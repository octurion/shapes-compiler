#include "ast.h"
#include "ir.h"

#include <llvm/ADT/None.h>

#include <llvm/Analysis/TypeBasedAliasAnalysis.h>

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/Interpreter.h>

#include <llvm/IR/AssemblyAnnotationWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Passes/PassBuilder.h>

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <sstream>
#include <string>
#include <unordered_map>

#include <cstdlib>

namespace Ir {

ClassSpecialization ClassSpecialization::specialize(
	const Ast::Class& clazz,
	const std::vector<Ast::PoolParameter>& params) const
{
	const auto& pools = m_class->pools();
	std::unordered_map<const Ast::Pool*, const Ast::Layout*> mapping;
	for (size_t i = 0; i < m_pool_param_types.size(); i++) {
		const Ast::Pool& pool = pools[i];
		mapping[&pool] = m_pool_param_types[i];
	}

	std::vector<const Ast::Layout*> param_types;
	for (const auto& e: params) {
		const auto* pool_ref = mpark::get_if<Ast::PoolRef>(&e);
		if (pool_ref == nullptr) {
			param_types.push_back(nullptr);
			continue;
		}
		const Ast::Pool& pool = pool_ref->pool();

		const auto* as_layout = mpark::get_if<Ast::LayoutType>(&pool.type());
		if (as_layout != nullptr) {
			param_types.push_back(&as_layout->layout());
			continue;
		}

		auto it = mapping.find(&pool);
		assert_msg(it != mapping.end(), "Could not find pool");
		param_types.push_back(it->second);
	}

	return ClassSpecialization(clazz, std::move(param_types));
}

std::ostream& operator<<(std::ostream& os, const ClassSpecialization& specialization)
{
	const auto& class_name = specialization.clazz().name();
	os << specialization.pool_param_types().size() << "C"
		<< class_name.length() << class_name;

	for (const auto* e: specialization.pool_param_types()) {
		if (e == nullptr) {
			os << "N";
		} else {
			os << e->name().length() << e->name();
		}
	}

	return os;
}

struct StandaloneSpecializationInfo
{
	llvm::StructType* type = nullptr;
	llvm::MDNode* tbaa_type = nullptr;
	llvm::MDNode* tbaa_ptr_type = nullptr;

	llvm::Function* ctor = nullptr;
};

struct PoolSpecializationInfo
{
	llvm::StructType* pool_type = nullptr;
	llvm::MDNode* pool_tbaa_ptr_type = nullptr;
	llvm::MDNode* pool_tbaa_type = nullptr;

	std::vector<llvm::StructType*> cluster_types;
	std::vector<llvm::MDNode*> cluster_tbaa_types;
	std::vector<llvm::MDNode*> cluster_tbaa_ptr_types;

	llvm::Function* pool_alloc = nullptr;
	llvm::Function* pool_free = nullptr;
	llvm::Function* obj_ctor = nullptr;
};

struct SpecializationInfo
{
	mpark::variant<StandaloneSpecializationInfo, PoolSpecializationInfo> type_info;

	std::unordered_map<const Ast::Method*, llvm::Function*> funcs;
};

struct MethodCodegenState;

class LLVMExpr
{
	llvm::IRBuilder<>* m_builder = nullptr;
	llvm::Value* m_value = nullptr;
	llvm::MDNode* m_tbaa_metadata = nullptr;
	bool m_lvalue = false;

public:
	LLVMExpr() = default;
	LLVMExpr(
			llvm::IRBuilder<>* builder,
			llvm::Value* value,
			llvm::MDNode* tbaa_metadata,
			bool lvalue)
		: m_builder(builder)
		, m_value(value)
		, m_tbaa_metadata(tbaa_metadata)
		, m_lvalue(lvalue)
	{}

	llvm::Value* value() const { return m_value; }
	llvm::MDNode* tbaa_metadata() const { return m_tbaa_metadata; }
	bool lvalue() const { return m_lvalue; }

	llvm::Value* to_rvalue() const
	{
		if (!m_lvalue) {
			return m_value;
		}

		auto* insn = m_builder->CreateLoad(m_value);
		if (m_tbaa_metadata != nullptr) {
			insn->setMetadata(llvm::LLVMContext::MD_tbaa, m_tbaa_metadata);
		}
		return insn;
	}
};

class Codegen::Impl
{
	llvm::LLVMContext m_ctx;
	const llvm::Target* m_target = nullptr;
	llvm::TargetMachine* m_target_machine = nullptr;

	std::unique_ptr<llvm::Module> m_mod = nullptr;

	llvm::Type* m_void = nullptr;
	llvm::Type* m_null = nullptr;

	llvm::Type* m_i1 = nullptr;
	llvm::Type* m_i8 = nullptr;
	llvm::Type* m_i16 = nullptr;
	llvm::Type* m_i32 = nullptr;
	llvm::Type* m_i64 = nullptr;

	llvm::Type* m_f32 = nullptr;
	llvm::Type* m_f64 = nullptr;

	llvm::Type* m_intptr = nullptr;

	llvm::MDNode* m_tbaa_root = nullptr;

	llvm::MDNode* m_tbaa_bool = nullptr;
	llvm::MDNode* m_tbaa_i8 = nullptr;
	llvm::MDNode* m_tbaa_u8 = nullptr;
	llvm::MDNode* m_tbaa_i16 = nullptr;
	llvm::MDNode* m_tbaa_u16 = nullptr;
	llvm::MDNode* m_tbaa_i32 = nullptr;
	llvm::MDNode* m_tbaa_u32 = nullptr;
	llvm::MDNode* m_tbaa_i64 = nullptr;
	llvm::MDNode* m_tbaa_u64 = nullptr;
	llvm::MDNode* m_tbaa_f32 = nullptr;
	llvm::MDNode* m_tbaa_f64 = nullptr;
	llvm::MDNode* m_tbaa_intptr = nullptr;

	llvm::FunctionType* m_malloc_type = nullptr;
	llvm::Function* m_malloc = nullptr;

	llvm::FunctionType* m_realloc_type = nullptr;
	llvm::Function* m_realloc = nullptr;

	llvm::FunctionType* m_free_type = nullptr;
	llvm::Function* m_free = nullptr;

	std::unordered_map<ClassSpecialization, SpecializationInfo> m_specialization_info;

	void generate_specializations(const Ast::Class& clazz);

	void generate_specializations_impl(
		const Ast::Class& clazz, std::vector<const Ast::Layout*>& curr_layouts);

	llvm::Type* type_of(
		const Ast::NoneType& type, const ClassSpecialization& specialization);
	llvm::Type* type_of(
		const Ast::LayoutType& type, const ClassSpecialization& specialization);
	llvm::Type* type_of(
		const Ast::BoundType& type, const ClassSpecialization& specialization);
	llvm::Type* type_of(
		const Ast::PoolType& type, const ClassSpecialization& specialization);

	llvm::Type* type_of(
		const Ast::ObjectType& type, const ClassSpecialization& specialization);
	llvm::Type* type_of(
		const Ast::PrimitiveType& type, const ClassSpecialization&);
	llvm::Type* type_of(
		const Ast::NullptrType&, const ClassSpecialization&);
	llvm::Type* type_of(
		const Ast::VoidType&, const ClassSpecialization&);
	llvm::Type* type_of(
		const Ast::Type& type, const ClassSpecialization& specialization);

	llvm::MDNode* tbaa_type_of(
		const Ast::PrimitiveType& type, const ClassSpecialization& specialization);
	llvm::MDNode* tbaa_type_of(
		const Ast::ObjectType& type, const ClassSpecialization& specialization);

	llvm::MDNode* tbaa_type_of(
		const Ast::VoidType& type, const ClassSpecialization& specialization);
	llvm::MDNode* tbaa_type_of(
		const Ast::NullptrType& type, const ClassSpecialization& specialization);

	llvm::Constant* zero(
		const Ast::ObjectType& type, const ClassSpecialization& specialization);
	llvm::Constant* zero(
		const Ast::PrimitiveType& type, const ClassSpecialization& specialization);
	llvm::Constant* zero(const Ast::NullptrType&, const ClassSpecialization&);
	llvm::Constant* zero(const Ast::VoidType&, const ClassSpecialization&);
	llvm::Constant* zero(
		const Ast::Type& type, const ClassSpecialization& specialization);

	void visit(const Ast::Assignment& e, MethodCodegenState& state);
	void visit(const Ast::OpAssignment& e, MethodCodegenState& state);
	void visit(const Ast::If& e, MethodCodegenState& state);
	void visit(const Ast::While& e, MethodCodegenState& state);
	void visit(const Ast::ForeachRange& e, MethodCodegenState& state);
	void visit(const Ast::ForeachPool& e, MethodCodegenState& state);
	void visit(const Ast::ExprStmt& e, MethodCodegenState& state);
	void visit(const Ast::Break& e, MethodCodegenState& state);
	void visit(const Ast::Continue& e, MethodCodegenState& state);
	void visit(const Ast::Return& e, MethodCodegenState& state);

	LLVMExpr visit(const Ast::IntegerConst& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::DoubleConst& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::BooleanConst& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::NullExpr& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::ThisExpr& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::CastExpr& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::UnaryExpr& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::BinaryExpr& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::VariableExpr& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::MethodCall& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::FieldAccess& e, MethodCodegenState& state);
	LLVMExpr visit(const Ast::NewExpr& e, MethodCodegenState& state);

	void generate_llvm_types();
	void generate_llvm_function_decls();
	void generate_llvm_functions();

public:
	bool ir(const Ast::Program& ast);
	bool emit(const char* filename);

	llvm::Function* find_method(const ClassSpecialization& spec, const Ast::Method& m) const;

	std::unique_ptr<llvm::Module> get_module();
};

std::string create_method_name(
	const ClassSpecialization& specialization, const Ast::Method& method)
{
	std::ostringstream os;
	os << "_shapes" << specialization
		<< "_M" << method.name().length() << method.name();

	return os.str();
}

std::string create_pool_ctor_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_shapes" << specialization << "_P";

	return os.str();
}

std::string create_pool_dtor_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_shapes" << specialization << "_D";

	return os.str();
}

std::string create_ctor_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_shapes" << specialization << "_C";

	return os.str();
}

std::string create_pool_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "struct.pool." << specialization;

	return os.str();
}

static std::string create_tbaa_pool_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_tbaa_pool" << specialization;

	return os.str();
}

static std::string create_tbaa_pool_ptr_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_tbaa_pool_ptr" << specialization;

	return os.str();
}

std::string create_cluster_name(const ClassSpecialization& specialization, size_t idx)
{
	std::ostringstream os;
	os << "struct.cluster." << idx << "." << specialization;

	return os.str();
}

static std::string create_tbaa_cluster_name(const ClassSpecialization& specialization, size_t idx)
{
	std::ostringstream os;
	os << "_tbaa_cluster" << idx << "_" << specialization;

	return os.str();
}

static std::string create_tbaa_cluster_ptr_name(const ClassSpecialization& specialization, size_t idx)
{
	std::ostringstream os;
	os << "_tbaa_cluster_ptr" << idx << "_" << specialization;

	return os.str();
}

std::string create_class_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "struct." << specialization;

	return os.str();
}

static std::string create_tbaa_class_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_tbaa_class" << specialization;

	return os.str();
}

static std::string create_tbaa_class_ptr_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_tbaa_class_ptr" << specialization;

	return os.str();
}

void init_llvm()
{
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmParser();
	LLVMInitializeX86AsmPrinter();
};

void Codegen::Impl::generate_specializations_impl(
	const Ast::Class& clazz, std::vector<const Ast::Layout*>& curr_layouts)
{
	auto idx = curr_layouts.size();
	if (idx == clazz.pools().size()) {
		m_specialization_info.emplace(
			ClassSpecialization(clazz, curr_layouts),
			SpecializationInfo());

		return;
	}

	curr_layouts.emplace_back(nullptr);
	generate_specializations_impl(clazz, curr_layouts);

	const Ast::Pool& idx_pool = clazz.pools()[idx];
	const auto& layouts = Ast::for_class(idx_pool.type()).layouts();

	for (const auto& e: layouts) {
		auto& back = curr_layouts.back();
		back = &e.get();

		generate_specializations_impl(clazz, curr_layouts);
	}

	curr_layouts.pop_back();
}

struct LLVMTypeFunctor
{
	llvm::Type* operator()(const StandaloneSpecializationInfo& info) const
	{
		return info.type;
	}

	llvm::Type* operator()(const PoolSpecializationInfo& info) const
	{
		return info.pool_type;
	}
};

llvm::Type* Codegen::Impl::type_of(const Ast::PoolType& type, const ClassSpecialization& specialization)
{
	return mpark::visit([this, &specialization](const auto& e) {
		return type_of(e, specialization);
	}, type);
}

llvm::Type* Codegen::Impl::type_of(const Ast::NoneType&, const ClassSpecialization&)
{
	return nullptr;
}

llvm::Type* Codegen::Impl::type_of(const Ast::LayoutType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	return mpark::visit(
		LLVMTypeFunctor(), m_specialization_info[new_spec].type_info);
}

llvm::Type* Codegen::Impl::type_of(const Ast::BoundType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	auto* as_pool = mpark::get_if<PoolSpecializationInfo>(&m_specialization_info[new_spec].type_info);
	if (as_pool == nullptr) {
		return nullptr;
	}
	return as_pool->pool_type;
}

void Codegen::Impl::generate_specializations(const Ast::Class& clazz)
{
	std::vector<const Ast::Layout*> curr_layouts;

	generate_specializations_impl(clazz, curr_layouts);
}

llvm::Type* Codegen::Impl::type_of(const Ast::NullptrType&, const ClassSpecialization&)
{
	unreachable("Null pointers do not have an LLVM type");
}

llvm::Type* Codegen::Impl::type_of(const Ast::VoidType&, const ClassSpecialization&)
{
	return m_void;
}

llvm::Type* Codegen::Impl::type_of(const Ast::ObjectType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	const auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
		&m_specialization_info[new_spec].type_info);
	if (as_standalone != nullptr) {
		return as_standalone->type->getPointerTo();
	}
	return m_intptr;
}

llvm::Type* Codegen::Impl::type_of(const Ast::PrimitiveType& type, const ClassSpecialization&)
{
	switch (type) {
	case Ast::PrimitiveType::BOOL:
		return m_i1;

	case Ast::PrimitiveType::I8:
	case Ast::PrimitiveType::U8:
		return m_i8;

	case Ast::PrimitiveType::I16:
	case Ast::PrimitiveType::U16:
		return m_i16;

	case Ast::PrimitiveType::I32:
	case Ast::PrimitiveType::U32:
		return m_i32;

	case Ast::PrimitiveType::I64:
	case Ast::PrimitiveType::U64:
		return m_i64;

	case Ast::PrimitiveType::F32:
		return m_f32;

	case Ast::PrimitiveType::F64:
		return m_f64;
	}
}

llvm::Type* Codegen::Impl::type_of(const Ast::Type& type, const ClassSpecialization& specialization)
{
	return mpark::visit([this, &specialization](const auto& e){
		return type_of(e, specialization);
	}, type);
}

llvm::MDNode* Codegen::Impl::tbaa_type_of(const Ast::PrimitiveType& type, const ClassSpecialization&)
{
	switch (type) {
	case Ast::PrimitiveType::BOOL:
		return m_tbaa_bool;

	case Ast::PrimitiveType::I8:
		return m_tbaa_i8;

	case Ast::PrimitiveType::U8:
		return m_tbaa_u8;

	case Ast::PrimitiveType::U16:
		return m_tbaa_i16;

	case Ast::PrimitiveType::I16:
		return m_tbaa_u16;

	case Ast::PrimitiveType::I32:
		return m_tbaa_i32;

	case Ast::PrimitiveType::U32:
		return m_tbaa_u32;

	case Ast::PrimitiveType::I64:
		return m_tbaa_i64;

	case Ast::PrimitiveType::U64:
		return m_tbaa_u64;

	case Ast::PrimitiveType::F32:
		return m_tbaa_f32;

	case Ast::PrimitiveType::F64:
		return m_tbaa_f64;
	}
}

llvm::MDNode* Codegen::Impl::tbaa_type_of(const Ast::ObjectType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);

	const auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
		&m_specialization_info[new_spec].type_info);
	const auto* as_pool = mpark::get_if<PoolSpecializationInfo>(
		&m_specialization_info[new_spec].type_info);
	if (as_standalone != nullptr) {
		return as_standalone->tbaa_ptr_type;
	}
	assert_msg(as_pool != nullptr, "Neither an object nor a pool?");

	return as_pool->pool_tbaa_ptr_type;
}

llvm::MDNode* Codegen::Impl::tbaa_type_of(const Ast::VoidType&, const ClassSpecialization&)
{
	unreachable("`void` has no TBAA type");
}

llvm::MDNode* Codegen::Impl::tbaa_type_of(const Ast::NullptrType&, const ClassSpecialization&)
{
	unreachable("`nullptr` has no TBAA type");
}

llvm::Constant* Codegen::Impl::zero(const Ast::ObjectType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	const auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
		&m_specialization_info[new_spec].type_info);
	if (as_standalone != nullptr) {
		return llvm::ConstantPointerNull::get(as_standalone->type->getPointerTo());
	}
	return llvm::ConstantInt::get(m_intptr, -1ull, true);
}

llvm::Constant* Codegen::Impl::zero(const Ast::PrimitiveType& type, const ClassSpecialization& spec)
{
	return llvm::ConstantInt::get(type_of(type, spec), 0);
}

llvm::Constant* Codegen::Impl::zero(const Ast::NullptrType&, const ClassSpecialization&)
{
	unreachable("Null pointers require an explicit check for them");
}

llvm::Constant* Codegen::Impl::zero(const Ast::VoidType&, const ClassSpecialization&)
{
	unreachable("Null pointers have no initial value!");
}

llvm::Constant* Codegen::Impl::zero(const Ast::Type& type, const ClassSpecialization& specialization)
{
	return mpark::visit([this, &specialization](const auto& e){
		return zero(e, specialization);
	}, type);
}

void Codegen::Impl::generate_llvm_types()
{
	llvm::MDBuilder tbaa_builder(m_ctx);
	for (auto& e: m_specialization_info) {
		const auto& spec = e.first;
		auto& info = e.second;

		if (spec.is_pooled_type()) {
			PoolSpecializationInfo pool_info;
			pool_info.pool_type =
				llvm::StructType::create(m_ctx, create_pool_name(spec));
			pool_info.pool_tbaa_ptr_type =
				tbaa_builder.createTBAAScalarTypeNode(
					create_tbaa_pool_ptr_name(spec),
					m_tbaa_root);
			info.type_info = std::move(pool_info);
		} else {
			StandaloneSpecializationInfo standalone_info;
			standalone_info.type =
				llvm::StructType::create(m_ctx, create_pool_name(spec));
			standalone_info.tbaa_ptr_type =
				tbaa_builder.createTBAAScalarTypeNode(
					create_tbaa_class_ptr_name(spec),
					m_tbaa_root);
			info.type_info = std::move(standalone_info);
		}
	}

	for (auto& e: m_specialization_info) {
		const auto& spec = e.first;
		auto& info = e.second;

		const auto& data_layout = m_mod->getDataLayout();
		llvm::MDBuilder tbaa_builder(m_ctx);

		const auto* layout = spec.first_pool_param_spec();
		if (layout != nullptr) {
			auto* pool_info = mpark::get_if<PoolSpecializationInfo>(&info.type_info);
			assert_msg(pool_info != nullptr, "Not a pool type?");

			const auto& clusters = layout->clusters();
			for (size_t i = 0; i < clusters.size(); i++) {
				const auto& cluster = clusters[i];
				std::vector<llvm::Type*> cluster_fields;
				for (const Ast::Field* e: cluster.fields()) {
					auto* type = mpark::visit(
						[this, &spec](const auto& e) {
							return type_of(e, spec);
					}, e->type());
					cluster_fields.push_back(type);
				}

				auto* cluster_type = llvm::StructType::create(
					m_ctx,
					cluster_fields,
					create_cluster_name(spec, i));
				pool_info->cluster_types.push_back(cluster_type);

				const auto& cluster_layout =
					data_layout.getStructLayout(cluster_type);

				std::vector<std::pair<llvm::MDNode*, uint64_t>> tbaa_fields;
				for (size_t j = 0; j < cluster.fields().size(); j++) {
					const Ast::Field* e = cluster.fields()[j];
					auto* tbaa_type = mpark::visit(
						[this, &spec](const auto& e) {
							return tbaa_type_of(e, spec);
					}, e->type());

					tbaa_fields.emplace_back(
						tbaa_type, cluster_layout->getElementOffset(j));
				}
				auto* tbaa_cluster = tbaa_builder.createTBAAStructTypeNode(
					create_tbaa_cluster_name(spec, i), tbaa_fields);
				pool_info->cluster_tbaa_types.push_back(tbaa_cluster);
			}

			std::vector<llvm::Type*> pool_fields { m_intptr, m_intptr };
			for (const auto& e: pool_info->cluster_types) {
				pool_fields.push_back(e->getPointerTo());
			}

			pool_info->pool_type->setBody(pool_fields);

			std::vector<std::pair<llvm::MDNode*, uint64_t>> tbaa_fields;
			const auto& pool_layout =
				data_layout.getStructLayout(pool_info->pool_type);
			tbaa_fields.emplace_back(
				m_tbaa_intptr,
				pool_layout->getElementOffset(0));
			tbaa_fields.emplace_back(
				m_tbaa_intptr,
				pool_layout->getElementOffset(1));
			for (size_t i = 0; i < layout->clusters().size(); i++) {
				auto* tbaa_ptr_type = tbaa_builder.createTBAAScalarTypeNode(
					create_tbaa_cluster_ptr_name(spec, i), m_tbaa_root);
				pool_info->cluster_tbaa_ptr_types.push_back(tbaa_ptr_type);

				tbaa_fields.emplace_back(
					tbaa_ptr_type, pool_layout->getElementOffset(i + 2));
			}
			pool_info->pool_tbaa_type = tbaa_builder.createTBAAStructTypeNode(
				create_tbaa_pool_name(spec), tbaa_fields);

		} else {
			auto* standalone_info =
				mpark::get_if<StandaloneSpecializationInfo>(&info.type_info);
			assert_msg(standalone_info != nullptr, "Not a standalone type?");

			const auto& ast_fields = spec.clazz().fields();
			std::vector<llvm::Type*> fields;
			for (const Ast::Field& e: ast_fields) {
				auto* type = mpark::visit(
					[this, &spec](const auto& e) {
						return type_of(e, spec);
				}, e.type());
				fields.push_back(type);
			}

			standalone_info->type->setBody(fields);
			const auto& struct_layout =
				data_layout.getStructLayout(standalone_info->type);

			std::vector<std::pair<llvm::MDNode*, uint64_t>> tbaa_fields;
			for (size_t i = 0; i < ast_fields.size(); i++) {
				const Ast::Field& field = ast_fields[i];
				auto* tbaa_type = mpark::visit(
					[this, &spec](const auto& e) {
						return tbaa_type_of(e, spec);
				}, field.type());
				tbaa_fields.emplace_back(
					tbaa_type, struct_layout->getElementOffset(i));
			}
			standalone_info->tbaa_type = tbaa_builder.createTBAAStructTypeNode(
				create_tbaa_class_name(spec), tbaa_fields);
		}
	}
}

void Codegen::Impl::generate_llvm_function_decls()
{
	for (auto& e: m_specialization_info) {
		const auto& specialization = e.first;
		auto& info = e.second;

		std::vector<llvm::Type*> pool_params;
		for (const Ast::Pool& pool: specialization.clazz().pools()) {
			auto* type = type_of(pool.type(), specialization);
			if (type == nullptr) {
				continue;
			}
			pool_params.push_back(type->getPointerTo());
		}

		for (const Ast::Method& method: specialization.clazz().methods()) {
			std::vector<llvm::Type*> params;
			// this parameter
			params.push_back(type_of(
					specialization.clazz().this_object_type(),
					specialization));

			// Actual function parameters
			for (const auto& e: method.params()) {
				params.push_back(type_of(e.type(), specialization));
			}

			params.insert(params.end(), pool_params.begin(), pool_params.end());
			auto* ret_type = type_of(method.return_type(), specialization);

			auto* func_type = llvm::FunctionType::get(ret_type, params, false);
			auto* func = llvm::Function::Create(
				func_type,
				llvm::GlobalValue::ExternalLinkage,
				create_method_name(specialization, method),
				m_mod.get());
			func->addFnAttr(llvm::Attribute::NoUnwind);

			info.funcs[&method] = func;
		}

		const auto& data_layout = m_mod->getDataLayout();
		llvm::MDBuilder tbaa_builder(m_ctx);

		auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
			&info.type_info);
		if (as_standalone != nullptr) {
			auto* ctor_func_type = llvm::FunctionType::get(
				as_standalone->type->getPointerTo(), {}, false);
			as_standalone->ctor = llvm::Function::Create(
					ctor_func_type,
					llvm::GlobalValue::ExternalLinkage,
					create_ctor_name(specialization),
					m_mod.get());
			as_standalone->ctor->setReturnDoesNotAlias();
			as_standalone->ctor->addFnAttr(llvm::Attribute::NoUnwind);

			auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", as_standalone->ctor);
			llvm::IRBuilder<> builder(bb);
			auto* class_ptr_type = as_standalone->type->getPointerTo();

			auto* size = llvm::ConstantInt::get(
				data_layout.getIntPtrType(m_ctx),
				data_layout.getStructLayout(as_standalone->type)->getSizeInBytes());
			auto* malloc_ptr = builder.CreateCall(m_malloc, {size});

			const auto& fields = specialization.clazz().fields();
			std::vector<llvm::Constant*> constants;
			for (size_t i = 0; i < fields.size(); i++) {
				const Ast::Field& e = fields[i];
				const auto& type = e.type();
				const auto* primitive_type = mpark::get_if<Ast::PrimitiveType>(&type);
				if (primitive_type != nullptr) {
					constants.push_back(
						llvm::ConstantInt::get(
							type_of(*primitive_type, specialization), 0));
					continue;
				}

				constants.push_back(zero(type, specialization));
			}

			auto* init = llvm::ConstantStruct::get(as_standalone->type, constants);
			auto* retval = builder.CreatePointerCast(malloc_ptr, class_ptr_type);

			builder.CreateStore(init, retval);
			builder.CreateRet(retval);

			continue;
		}

		auto* as_pool = mpark::get_if<PoolSpecializationInfo>(&info.type_info);
		if (as_pool != nullptr) {
			auto* layout = specialization.first_pool_param_spec();
			assert_msg(layout != nullptr, "Pool with no layout?");

			auto* pool_ctor_type = llvm::FunctionType::get(
				m_void, {as_pool->pool_type->getPointerTo()}, false);
			as_pool->pool_alloc = llvm::Function::Create(
				pool_ctor_type,
				llvm::GlobalValue::ExternalLinkage,
				create_pool_ctor_name(specialization),
				m_mod.get());
			as_pool->pool_alloc->addFnAttr(llvm::Attribute::NoUnwind);

			{
				auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", as_pool->pool_alloc);
				llvm::IRBuilder<> builder(bb);

				assert_msg(
					!as_pool->pool_alloc->arg_empty(),
					"Pool constructor has no arguments?");

				auto* pool_ptr = as_pool->pool_alloc->arg_begin();
				std::vector<llvm::Constant*> init_values = {
					llvm::ConstantInt::get(m_intptr, 0),
					llvm::ConstantInt::get(m_intptr, 0),
				};
				for (auto* e: as_pool->cluster_types) {
					init_values.push_back(
						llvm::Constant::getNullValue(e->getPointerTo()));
				}
				auto* init = llvm::ConstantStruct::get(as_pool->pool_type, init_values);
				builder.CreateStore(init, pool_ptr);
				builder.CreateRetVoid();
			}

			auto* pool_dtor_type = llvm::FunctionType::get(
				m_void, {as_pool->pool_type->getPointerTo()}, false);
			as_pool->pool_free = llvm::Function::Create(
				pool_dtor_type,
				llvm::GlobalValue::ExternalLinkage,
				create_pool_dtor_name(specialization),
				m_mod.get());
			as_pool->pool_free->addFnAttr(llvm::Attribute::NoUnwind);

			{
				auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", as_pool->pool_free);
				llvm::IRBuilder<> builder(bb);

				assert_msg(
					!as_pool->pool_free->arg_empty(),
					"Pool destructor has no arguments?");

				auto* pool_ptr = as_pool->pool_free->arg_begin();

				for (size_t i = 0; i < as_pool->cluster_types.size(); i++) {
					auto* cluster_type = as_pool->cluster_types[i];
					auto* cluster_ptr_type = cluster_type->getPointerTo();
					auto* cluster_ptr_field =
						builder.CreateStructGEP(
							as_pool->pool_type, pool_ptr, i + 2);
					auto* cluster_ptr = builder.CreateLoad(
						cluster_ptr_type, cluster_ptr_field);
					auto* cluster_ptr_cast = builder.CreatePointerCast(
						cluster_ptr, m_i8->getPointerTo());
					builder.CreateCall(m_free_type, m_free, {cluster_ptr_cast});
				}

				builder.CreateRetVoid();
			}

			auto* alloc_func_type = llvm::FunctionType::get(
				m_intptr, {as_pool->pool_type->getPointerTo()}, false);
			as_pool->obj_ctor = llvm::Function::Create(
				alloc_func_type,
				llvm::GlobalValue::ExternalLinkage,
				create_ctor_name(specialization),
				m_mod.get());
			as_pool->obj_ctor->addFnAttr(llvm::Attribute::NoUnwind);

			auto* bb_entry = llvm::BasicBlock::Create(
				m_ctx, "entry", as_pool->obj_ctor);
			auto* bb_call_realloc = llvm::BasicBlock::Create(
				m_ctx, "call_realloc", as_pool->obj_ctor);
			auto* bb_alloc = llvm::BasicBlock::Create(
				m_ctx, "alloc", as_pool->obj_ctor);

			assert_msg(
				!as_pool->obj_ctor->arg_empty(),
				"Pool object constructor has no arguments?");
			auto* pool_ptr = as_pool->obj_ctor->arg_begin();

			llvm::Value* size_ptr;
			llvm::Value* capacity_ptr;
			llvm::Instruction* size;
			llvm::Instruction* capacity;

			llvm::IRBuilder<> builder(bb_entry);

			size_ptr = builder.CreateStructGEP(as_pool->pool_type, pool_ptr, 0);
			const auto* pool_layout = data_layout.getStructLayout(as_pool->pool_type);
			auto* size_tbaa_node = tbaa_builder.createTBAAStructTagNode(
				as_pool->pool_tbaa_type,
				m_tbaa_intptr,
				pool_layout->getElementOffset(0));
			size = builder.CreateLoad(size_ptr);
			size->setMetadata(llvm::LLVMContext::MD_tbaa, size_tbaa_node);

			capacity_ptr = builder.CreateStructGEP(as_pool->pool_type, pool_ptr, 1);
			auto* capacity_tbaa_node = tbaa_builder.createTBAAStructTagNode(
				as_pool->pool_tbaa_type,
				m_tbaa_intptr,
				pool_layout->getElementOffset(1));
			capacity = builder.CreateLoad(capacity_ptr);
			capacity->setMetadata(llvm::LLVMContext::MD_tbaa, capacity_tbaa_node);

			auto* filled = builder.CreateICmpEQ(size, capacity);

			builder.CreateCondBr(filled, bb_call_realloc, bb_alloc);

			builder.SetInsertPoint(bb_call_realloc);

			auto* new_cap = builder.CreateShl(capacity, 1);
			auto* new_cap_nonzero = builder.CreateICmpNE(
				new_cap, llvm::ConstantInt::get(m_intptr, 0));
			new_cap = builder.CreateSelect(
				new_cap_nonzero,
				new_cap,
				llvm::ConstantInt::get(m_intptr, 1));
			auto* store_cap_insn = builder.CreateStore(new_cap, capacity_ptr);
			store_cap_insn->setMetadata(llvm::LLVMContext::MD_tbaa, capacity_tbaa_node);

			const auto& cluster_types = as_pool->cluster_types;
			for (size_t i = 0; i < cluster_types.size(); i++) {
				auto* cluster_type = cluster_types[i];
				auto* cluster_ptr_type = cluster_type->getPointerTo();

				auto* member_ptr = builder.CreateStructGEP(
					as_pool->pool_type, pool_ptr, i + 2);

				auto* cluster_tbaa_node = tbaa_builder.createTBAAStructTagNode(
					as_pool->pool_tbaa_type,
					as_pool->cluster_tbaa_ptr_types[i],
					pool_layout->getElementOffset(i + 2));
				auto* old_ptr = builder.CreateLoad(member_ptr);
				old_ptr->setMetadata(llvm::LLVMContext::MD_tbaa, cluster_tbaa_node);
				auto* old_ptr_cast =
					builder.CreatePointerCast(old_ptr, m_i8->getPointerTo());

				const auto& data_layout = m_mod->getDataLayout();
				auto* cluster_size = llvm::ConstantInt::get(
					data_layout.getIntPtrType(m_ctx),
					data_layout.getTypeAllocSize(m_intptr));

				auto* new_size = builder.CreateMul(new_cap, cluster_size);
				auto* realloc = builder.CreateCall(
					m_realloc, {old_ptr_cast, new_size});
				auto* store_insn = builder.CreateStore(
					builder.CreatePointerCast(realloc, cluster_ptr_type),
					member_ptr);
				store_insn->setMetadata(llvm::LLVMContext::MD_tbaa, cluster_tbaa_node);
			}
			builder.CreateBr(bb_alloc);

			builder.SetInsertPoint(bb_alloc);
			auto* new_idx = size;
			auto* store_size_insn = builder.CreateStore(
				builder.CreateAdd(size, llvm::ConstantInt::get(m_intptr, 1)),
				builder.CreateStructGEP(as_pool->pool_type, pool_ptr, 0));
			store_size_insn->setMetadata(llvm::LLVMContext::MD_tbaa, size_tbaa_node);

			const auto& clusters = layout->clusters();
			for (size_t i = 0; i < clusters.size(); i++) {
				const auto& fields = clusters[i].fields();
				auto* cluster_tbaa_node = tbaa_builder.createTBAAStructTagNode(
					as_pool->pool_tbaa_type,
					as_pool->cluster_tbaa_ptr_types[i],
					pool_layout->getElementOffset(i + 2));

				auto* cluster_ptr = builder.CreateStructGEP(
					as_pool->pool_type, pool_ptr, i + 2);
				auto* cluster = builder.CreateLoad(cluster_ptr);
				cluster->setMetadata(llvm::LLVMContext::MD_tbaa, cluster_tbaa_node);

				auto* offset_ptr = builder.CreateInBoundsGEP(cluster, new_idx);
				for (size_t j = 0; j < fields.size(); j++) {
					auto* cluster_layout = data_layout.getStructLayout(as_pool->cluster_types[i]);

					auto* tbaa_field_type = mpark::visit(
						[this, &specialization](const auto& e) {
							return tbaa_type_of(e, specialization);
						}, fields[j]->type());
					auto* record_tbaa_node = tbaa_builder.createTBAAStructTagNode(
						as_pool->cluster_tbaa_types[i],
						tbaa_field_type,
						cluster_layout->getElementOffset(j));

					auto* init_value = zero(fields[j]->type(), specialization);

					auto* field_ptr = builder.CreateStructGEP(
						as_pool->cluster_types[i], offset_ptr, j);
					auto* record_store_insn = builder.CreateStore(init_value, field_ptr);
					record_store_insn->setMetadata(
						llvm::LLVMContext::MD_tbaa, record_tbaa_node);
				}


			}

			builder.CreateRet(new_idx);

			continue;
		}

		unreachable("Neither standalone nor pooled?");
	}
}

struct LoopStackEntry
{
	llvm::BasicBlock* loop_continue;
	llvm::BasicBlock* loop_break;
};

struct MethodCodegenState
{
	ClassSpecialization spec;
	const Ast::Method* method = nullptr;
	llvm::Function* llvm_func = nullptr;

	llvm::IRBuilder<>* builder = nullptr;

	std::unordered_map<const Ast::Variable*, llvm::Value*> local_vars;
	std::unordered_map<const Ast::Pool*, llvm::Value*> local_pools;

	std::vector<LoopStackEntry> loop_stack;

	MethodCodegenState() = default;
	MethodCodegenState(const MethodCodegenState&) = delete;
	MethodCodegenState& operator=(const MethodCodegenState&) = delete;
};

void Codegen::Impl::generate_llvm_functions()
{
	for (auto& e: m_specialization_info) {
		const auto& spec = e.first;
		for (auto& e: e.second.funcs) {
			const auto& method = *e.first;
			auto* llvm_func = e.second;

			MethodCodegenState state;
			state.spec = spec;
			state.llvm_func = llvm_func;

			auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", llvm_func);
			llvm::IRBuilder<> builder(bb);

			state.builder = &builder;

			const auto& params = method.params();
			for (const auto& var: params) {
				auto* value = builder.CreateAlloca(
					type_of(var.type(), spec), nullptr, var.name());
				state.local_vars[&var] = value;
			}
			for (const auto& var: method.vars()) {
				auto* value = builder.CreateAlloca(
					type_of(var.type(), spec), nullptr, var.name());
				state.local_vars[&var] = value;
			}
			for (const auto& pool: method.pools()) {
				auto* value = builder.CreateAlloca(
					type_of(pool.type(), spec), nullptr, pool.name());
				state.local_pools[&pool] = value;
			}

			auto it = llvm_func->arg_begin() + params.size() + 1;
			const auto& class_params = spec.clazz().pools();
			const auto& spec_params = spec.pool_param_types();
			for (size_t i = 0; i < class_params.size(); i++) {
				if (spec_params[i] != nullptr) {
					const Ast::Pool& pool = class_params[i];
					state.local_pools[&pool] = it++;
				}
			}

			for (size_t i = 0; i < params.size(); i++) {
				builder.CreateStore(
					llvm_func->arg_begin() + i + 1,
					state.local_vars[&params[i]]);
			}
			for (const auto& var: method.vars()) {
				builder.CreateStore(zero(var.type(), spec), state.local_vars[&var]);
			}
			for (const auto& pool: method.pools()) {
				const auto* as_pool_type = mpark::get_if<Ast::LayoutType>(&pool.type());
				assert_msg(
					as_pool_type != nullptr, "Local pool has no layout type?");

				auto new_spec = spec.specialize_type(*as_pool_type);
				const auto& type_info = m_specialization_info[new_spec].type_info;
				const auto* as_pool =
					mpark::get_if<PoolSpecializationInfo>(&type_info);
				assert_msg(
					as_pool != nullptr, "Not a pool specialization?");

				builder.CreateCall(as_pool->pool_alloc, {state.local_pools[&pool]});
			}

			for (const auto& stmt: method.body()) {
				mpark::visit([this, &state](const auto& e) {
					this->visit(e, state);
				}, stmt);
			}

			if (mpark::holds_alternative<Ast::VoidType>(method.return_type())) {
				builder.CreateRetVoid();
				continue;
			}

			const auto* as_primitive =
				mpark::get_if<Ast::PrimitiveType>(&method.return_type());
			if (as_primitive != nullptr) {
				builder.CreateRet(zero(*as_primitive, state.spec));
				continue;
			}

			const auto* as_object =
				mpark::get_if<Ast::ObjectType>(&method.return_type());
			if (as_object != nullptr) {
				builder.CreateRet(zero(*as_object, state.spec));
				continue;
			}

			unreachable("Null return type?");
		}
	}
}

void Codegen::Impl::visit(const Ast::Assignment& e, MethodCodegenState& state)
{
	auto lhs = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.lhs());

	llvm::Value* rhs_value;

	if (mpark::holds_alternative<Ast::NullExpr>(e.rhs())) {
		rhs_value = llvm::Constant::getNullValue(lhs.value()->getType());
	} else {
		auto rhs = mpark::visit([this, &state](const auto& e) {
			return visit(e, state);
		}, e.rhs());
		rhs_value = rhs.to_rvalue();
	}

	auto* insn = state.builder->CreateStore(rhs_value, lhs.value());
	insn->setMetadata(llvm::LLVMContext::MD_tbaa, lhs.tbaa_metadata());
}

void Codegen::Impl::visit(const Ast::OpAssignment& e, MethodCodegenState& state)
{
	auto rhs = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.rhs());
	auto* rhs_value = rhs.to_rvalue();

	auto lhs = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.lhs());
	auto lhs_value = lhs.to_rvalue();

	auto type = Ast::expr_type(e.lhs());
	const auto* as_primitive = mpark::get_if<Ast::PrimitiveType>(&type);
	assert_msg(as_primitive != nullptr, "Must be a primitive type");

	bool is_floating_point = Ast::is_floating_point(*as_primitive);

	llvm::Value* value;
	switch (e.op()) {
	case Ast::BinOp::PLUS: {
		if (is_floating_point) {
			value = state.builder->CreateFAdd(lhs_value, rhs_value);
		} else {
			value = state.builder->CreateAdd(lhs_value, rhs_value);
		}
		break;
	}
	case Ast::BinOp::MINUS: {
		if (is_floating_point) {
			value = state.builder->CreateFSub(lhs_value, rhs_value);
		} else {
			value = state.builder->CreateSub(lhs_value, rhs_value);
		}
		break;
	}
	case Ast::BinOp::TIMES: {
		if (is_floating_point) {
			value = state.builder->CreateFMul(lhs_value, rhs_value);
		} else {
			value = state.builder->CreateMul(lhs_value, rhs_value);
		}
		break;
	}
	case Ast::BinOp::DIV: {
		if (is_floating_point) {
			value = state.builder->CreateFDiv(lhs_value, rhs_value);
		} else if (Ast::is_signed_integer(*as_primitive)) {
			value = state.builder->CreateSDiv(lhs_value, rhs_value);
		} else {
			value = state.builder->CreateUDiv(lhs_value, rhs_value);
		}
		break;
	}
	case Ast::BinOp::AND: {
		value = state.builder->CreateAnd(lhs_value, rhs_value);
		break;
	}
	case Ast::BinOp::OR: {
		value = state.builder->CreateOr(lhs_value, rhs_value);
		break;
	}
	case Ast::BinOp::XOR: {
		value = state.builder->CreateXor(lhs_value, rhs_value);
		break;
	}
	case Ast::BinOp::SHL: {
		value = state.builder->CreateShl(lhs_value, rhs_value);
		break;
	}
	case Ast::BinOp::SHR: {
		if (Ast::is_signed_integer(*as_primitive)) {
			value = state.builder->CreateAShr(lhs_value, rhs_value);
		} else {
			value = state.builder->CreateLShr(lhs_value, rhs_value);
		}
		break;
	}
	default:
		unreachable("Not applicable to op-assign statements");
	}

	auto* insn = state.builder->CreateStore(value, lhs.value());
	insn->setMetadata(llvm::LLVMContext::MD_tbaa, lhs.tbaa_metadata());
}

void Codegen::Impl::visit(const Ast::If& e, MethodCodegenState& state)
{
	auto cond = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.cond());
	auto cond_value = cond.to_rvalue();

	auto* then_bb = llvm::BasicBlock::Create(m_ctx, "if_then", state.llvm_func);
	auto* else_bb = llvm::BasicBlock::Create(m_ctx, "if_else", state.llvm_func);
	state.builder->CreateCondBr(cond_value, then_bb, else_bb);

	auto* next_bb = llvm::BasicBlock::Create(m_ctx, "if_fini", state.llvm_func);

	state.builder->SetInsertPoint(then_bb);
	for (const auto& stmt: e.then_stmts()) {
		mpark::visit([this, &state](const auto& e) { visit(e, state); }, stmt);
	}
	state.builder->CreateBr(next_bb);

	state.builder->SetInsertPoint(else_bb);
	for (const auto& stmt: e.else_stmts()) {
		mpark::visit([this, &state](const auto& e) { visit(e, state); }, stmt);
	}
	state.builder->CreateBr(next_bb);

	state.builder->SetInsertPoint(next_bb);
}

void Codegen::Impl::visit(const Ast::While& e, MethodCodegenState& state)
{
	auto* header_bb = llvm::BasicBlock::Create(m_ctx, "while_header", state.llvm_func);
	auto* body_bb = llvm::BasicBlock::Create(m_ctx, "while_body", state.llvm_func);
	auto* exit_bb = llvm::BasicBlock::Create(m_ctx, "while_exit", state.llvm_func);

	LoopStackEntry entry;

	entry.loop_continue = header_bb;
	entry.loop_break = exit_bb;

	state.loop_stack.push_back(entry);

	state.builder->CreateBr(header_bb);
	state.builder->SetInsertPoint(header_bb);

	auto cond = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.cond());
	auto* cond_value = cond.to_rvalue();

	state.builder->CreateCondBr(cond_value, body_bb, exit_bb);

	state.builder->SetInsertPoint(body_bb);
	for (const auto& stmt: e.body()) {
		mpark::visit([this, &state](const auto& e) { visit(e, state); }, stmt);
	}
	state.builder->CreateBr(header_bb);

	state.builder->SetInsertPoint(exit_bb);
	state.loop_stack.pop_back();
}

void Codegen::Impl::visit(const Ast::ForeachRange& e, MethodCodegenState& state)
{
	auto type = Ast::expr_type(e.range_begin());
	const auto* as_primitive = mpark::get_if<Ast::PrimitiveType>(&type);
	assert_msg(as_primitive != nullptr, "Range loop needs primitives");

	bool is_signed = Ast::is_signed_integer(*as_primitive);

	auto* range_begin = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.range_begin()).to_rvalue();

	auto* range_end = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.range_end()).to_rvalue();

	auto* var = state.local_vars[&e.var()];
	assert_msg(var != nullptr, "No LLVM variable for loop");

	auto* init_bb = llvm::BasicBlock::Create(m_ctx, "foreach_range_init", state.llvm_func);
	auto* exit_bb = llvm::BasicBlock::Create(m_ctx, "foreach_range_exit", state.llvm_func);
	auto* cond_bb = llvm::BasicBlock::Create(m_ctx, "foreach_range_cond", state.llvm_func);
	auto* body_bb = llvm::BasicBlock::Create(m_ctx, "foreach_range_body", state.llvm_func);
	auto* update_bb = llvm::BasicBlock::Create(m_ctx, "foreach_range_update", state.llvm_func);

	auto* initial_cond = is_signed
		? state.builder->CreateICmpSLT(range_begin, range_end)
		: state.builder->CreateICmpULT(range_begin, range_end);
	state.builder->CreateCondBr(initial_cond, init_bb, exit_bb);

	state.builder->SetInsertPoint(init_bb);
	state.builder->CreateStore(range_begin, var);
	state.builder->CreateBr(cond_bb);

	state.builder->SetInsertPoint(cond_bb);
	auto* var_value = state.builder->CreateLoad(var);
	auto* cond = is_signed
		? state.builder->CreateICmpSLT(var_value, range_end)
		: state.builder->CreateICmpULT(var_value, range_end);
	state.builder->CreateCondBr(cond, body_bb, exit_bb);

	LoopStackEntry entry;
	entry.loop_continue = update_bb;
	entry.loop_break = exit_bb;
	state.loop_stack.push_back(entry);

	state.builder->SetInsertPoint(body_bb);

	for (const auto& stmt: e.body()) {
		mpark::visit([this, &state](const auto& e) { visit(e, state); }, stmt);
	}
	state.builder->CreateBr(update_bb);

	state.builder->SetInsertPoint(update_bb);
	auto* const_one = llvm::ConstantInt::get(var_value->getType(), 1);
	auto* new_var_value = is_signed
		? state.builder->CreateNSWAdd(var_value, const_one)
		: state.builder->CreateNUWAdd(var_value, const_one);
	state.builder->CreateStore(new_var_value, var);
	state.builder->CreateBr(cond_bb);

	state.loop_stack.pop_back();

	state.builder->SetInsertPoint(exit_bb);
}

void Codegen::Impl::visit(const Ast::ForeachPool& e, MethodCodegenState& state)
{
	auto* pool = state.local_pools[&e.pool()];
	if (pool == nullptr) {
		return;
	}

	auto* var = state.local_vars[&e.var()];
	assert_msg(var != nullptr, "No LLVM variable for loop");

	auto* cond_bb = llvm::BasicBlock::Create(m_ctx, "foreach_pool_cond", state.llvm_func);
	auto* body_bb = llvm::BasicBlock::Create(m_ctx, "foreach_pool_body", state.llvm_func);
	auto* update_bb = llvm::BasicBlock::Create(m_ctx, "foreach_pool_update", state.llvm_func);
	auto* exit_bb = llvm::BasicBlock::Create(m_ctx, "foreach_pool_exit", state.llvm_func);

	state.builder->CreateStore(llvm::ConstantInt::get(m_intptr, 0), var);
	state.builder->CreateBr(cond_bb);

	state.builder->SetInsertPoint(cond_bb);

	auto* var_value = state.builder->CreateLoad(var);
	auto* size_ptr = state.builder->CreateStructGEP(pool, 0);
	auto* size = state.builder->CreateLoad(size_ptr);
	auto* cond = state.builder->CreateICmpULT(var_value, size);

	state.builder->CreateCondBr(cond, body_bb, exit_bb);

	LoopStackEntry entry;
	entry.loop_continue = update_bb;
	entry.loop_break = exit_bb;
	state.loop_stack.push_back(entry);

	state.builder->SetInsertPoint(body_bb);
	for (const auto& stmt: e.body()) {
		mpark::visit([this, &state](const auto& e) { visit(e, state); }, stmt);
	}
	state.builder->CreateBr(update_bb);

	state.builder->SetInsertPoint(update_bb);
	auto* const_one = llvm::ConstantInt::get(var_value->getType(), 1);
	auto* new_var_value = state.builder->CreateNUWAdd(var_value, const_one);
	state.builder->CreateStore(new_var_value, var);
	state.builder->CreateBr(cond_bb);

	state.loop_stack.pop_back();

	state.builder->SetInsertPoint(exit_bb);
}

void Codegen::Impl::visit(const Ast::ExprStmt& e, MethodCodegenState& state)
{
	if (mpark::holds_alternative<Ast::NullExpr>(e.expr())) {
		return;
	}
	mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr());
}

void Codegen::Impl::visit(const Ast::Break&, MethodCodegenState& state)
{
	assert_msg(!state.loop_stack.empty(), "Must be inside loop");

	auto& entry = state.loop_stack.back();
	state.builder->CreateBr(entry.loop_break);

	auto* bb = llvm::BasicBlock::Create(m_ctx, "post_break", state.llvm_func);
	state.builder->SetInsertPoint(bb);
}

void Codegen::Impl::visit(const Ast::Continue&, MethodCodegenState& state)
{
	assert_msg(!state.loop_stack.empty(), "Must be inside loop");

	auto& entry = state.loop_stack.back();
	state.builder->CreateBr(entry.loop_continue);

	auto* bb = llvm::BasicBlock::Create(m_ctx, "post_continue", state.llvm_func);
	state.builder->SetInsertPoint(bb);
}

void Codegen::Impl::visit(const Ast::Return& e, MethodCodegenState& state)
{
	if (e.expr() == nullptr) {
		state.builder->CreateRetVoid();
		return;
	}

	if (mpark::holds_alternative<Ast::NullExpr>(*e.expr())) {
		auto* as_obj_type = mpark::get_if<Ast::ObjectType>(&state.method->return_type());
		auto new_spec = state.spec.specialize_type(*as_obj_type);

		auto* nullptr_value = new_spec.is_pooled_type()
			? llvm::ConstantInt::get(m_intptr, -1ull, true)
			: llvm::ConstantPointerNull::get(
				llvm::cast<llvm::PointerType>(type_of(*as_obj_type, new_spec)));

		state.builder->CreateRet(nullptr_value);
		return;
	}

	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, *e.expr()).to_rvalue();
	state.builder->CreateRet(value);

	auto* new_bb = llvm::BasicBlock::Create(m_ctx, "after_ret", state.llvm_func);
	state.builder->SetInsertPoint(new_bb);
}

LLVMExpr Codegen::Impl::visit(const Ast::IntegerConst& e, MethodCodegenState& state)
{
	auto* value = llvm::ConstantInt::get(m_intptr, e.value());
	return LLVMExpr(state.builder, value, nullptr, false);
}

LLVMExpr Codegen::Impl::visit(const Ast::DoubleConst& e, MethodCodegenState& state)
{
	auto* value = llvm::ConstantFP::get(m_f64, e.value());
	return LLVMExpr(state.builder, value, nullptr, false);
}

LLVMExpr Codegen::Impl::visit(const Ast::BooleanConst& e, MethodCodegenState& state)
{
	auto* value = llvm::ConstantInt::get(m_i1, e.value());
	return LLVMExpr(state.builder, value, nullptr, false);
}

LLVMExpr Codegen::Impl::visit(const Ast::NullExpr&, MethodCodegenState&)
{
	unreachable("You must check explicitly for null expressions");
}

LLVMExpr Codegen::Impl::visit(const Ast::ThisExpr&, MethodCodegenState& state)
{
	return LLVMExpr(state.builder, state.llvm_func->arg_begin(), nullptr, false);
}

LLVMExpr Codegen::Impl::visit(const Ast::CastExpr& e, MethodCodegenState& state)
{
	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr()).to_rvalue();

	auto type = Ast::expr_type(e.expr());
	auto dest_type = e.type();
	auto* src_type = mpark::get_if<Ast::PrimitiveType>(&type);
	assert_msg(src_type != nullptr, "Not a primitive type?");

	auto llvm_dest_type = type_of(dest_type, state.spec);
	auto llvm_src_type = type_of(*src_type, state.spec);

	// Anything to boolean: Check if non-zero
	if (Ast::is_boolean(dest_type)) {
		if (Ast::is_floating_point(*src_type)) {
			auto* zero = llvm::ConstantFP::get(llvm_dest_type, 0.0);
			auto* cast_val = state.builder->CreateFCmpUNE(value, zero);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}

		auto* zero = llvm::ConstantInt::get(llvm_dest_type, 0);
		auto* cast_val = state.builder->CreateICmpNE(value, zero);
		return LLVMExpr(state.builder, cast_val, nullptr, false);
	}

	// Boolean to anything
	if (Ast::is_boolean(*src_type)) {
		if (Ast::is_floating_point(dest_type)) {
			auto* cast_val = state.builder->CreateSelect(value,
				llvm::ConstantFP::get(llvm_dest_type, 1.0),
				llvm::ConstantFP::get(llvm_dest_type, 0.0));
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}

		auto* cast_val = state.builder->CreateSelect(value,
			llvm::ConstantInt::get(llvm_dest_type, 1),
			llvm::ConstantInt::get(llvm_dest_type, 0));
		return LLVMExpr(state.builder, cast_val, nullptr, false);
	}

	// Anything to float
	if (Ast::is_floating_point(dest_type)) {
		if (Ast::is_floating_point(*src_type)) {
			auto* cast_val = state.builder->CreateFPCast(value, llvm_dest_type);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}
		if (Ast::is_signed_integer(*src_type)) {
			auto* cast_val = state.builder->CreateSIToFP(value, llvm_dest_type);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}
		if (Ast::is_unsigned_integer(*src_type) || Ast::is_boolean(*src_type)) {
			auto* cast_val = state.builder->CreateUIToFP(value, llvm_dest_type);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}
	}

	// Float to anything
	if (Ast::is_floating_point(*src_type)) {
		if (Ast::is_floating_point(dest_type)) {
			auto* cast_val = state.builder->CreateFPCast(value, llvm_dest_type);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}
		if (Ast::is_signed_integer(dest_type)) {
			auto* cast_val = state.builder->CreateFPToSI(value, llvm_dest_type);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}
		if (Ast::is_unsigned_integer(*src_type) || Ast::is_boolean(dest_type)) {
			auto* cast_val = state.builder->CreateFPToUI(value, llvm_dest_type);
			return LLVMExpr(state.builder, cast_val, nullptr, false);
		}
	}

	// Int truncation or bitcast
	if (llvm_dest_type->getScalarSizeInBits() <= llvm_src_type->getScalarSizeInBits()) {
		auto* cast_val = state.builder->CreateTruncOrBitCast(value, llvm_dest_type);
		return LLVMExpr(state.builder, cast_val, nullptr, false);
	}

	if (Ast::is_signed_integer(*src_type)) {
		auto* cast_val = state.builder->CreateSExt(value, llvm_dest_type);
		return LLVMExpr(state.builder, cast_val, nullptr, false);
	} else {
		auto* cast_val = state.builder->CreateZExt(value, llvm_dest_type);
		return LLVMExpr(state.builder, cast_val, nullptr, false);
	}
}

LLVMExpr Codegen::Impl::visit(const Ast::UnaryExpr& e, MethodCodegenState& state)
{
	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr()).to_rvalue();

	auto type = Ast::expr_type(e.expr());
	auto* src_type = mpark::get_if<Ast::PrimitiveType>(&type);
	assert_msg(src_type != nullptr, "Not a primitive type?");

	switch (e.op()) {
	case Ast::UnOp::PLUS: {
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::UnOp::MINUS: {
		if (Ast::is_floating_point(*src_type)) {
			return LLVMExpr(state.builder, state.builder->CreateFNeg(value), nullptr, false);
		}

		return LLVMExpr(state.builder, state.builder->CreateNeg(value), nullptr, false);
	}
	case Ast::UnOp::NOT: {
		assert_msg(!Ast::is_floating_point(*src_type), "Can't be a float");
		return LLVMExpr(state.builder, state.builder->CreateNot(value), nullptr, false);
	}
	}
}

LLVMExpr Codegen::Impl::visit(const Ast::BinaryExpr& e, MethodCodegenState& state)
{
	if (e.op() == Ast::BinOp::EQ || e.op() == Ast::BinOp::NE) {
		auto lhs_type = Ast::expr_type(e.lhs());
		auto rhs_type = Ast::expr_type(e.rhs());

		const auto* as_primitive = mpark::get_if<Ast::PrimitiveType>(&lhs_type);
		if (as_primitive != nullptr) {
			auto* lhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.lhs()).to_rvalue();

			auto* rhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.rhs()).to_rvalue();

			if (Ast::is_floating_point(*as_primitive)) {
				auto* value = state.builder->CreateFCmpUEQ(lhs_value, rhs_value);
				return LLVMExpr(state.builder, value, nullptr, false);
			}
			auto* value = state.builder->CreateICmpEQ(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}

		const auto* as_lhs_obj = mpark::get_if<Ast::ObjectType>(&lhs_type);
		const auto* as_rhs_obj = mpark::get_if<Ast::ObjectType>(&rhs_type);

		const auto& obj_type = as_lhs_obj != nullptr ? *as_lhs_obj : *as_rhs_obj;
		const auto new_spec = state.spec.specialize_type(obj_type);

		auto* nullptr_value = new_spec.is_pooled_type()
			? llvm::ConstantInt::get(m_intptr, -1ull, true)
			: llvm::ConstantPointerNull::get(
				llvm::cast<llvm::PointerType>(type_of(obj_type, new_spec)));

		llvm::Value* lhs_value;
		if (mpark::holds_alternative<Ast::NullptrType>(lhs_type)) {
			lhs_value = nullptr_value;
		} else {
			lhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.lhs()).to_rvalue();
		}

		llvm::Value* rhs_value;
		if (mpark::holds_alternative<Ast::NullptrType>(rhs_type)) {
			rhs_value = nullptr_value;
		} else {
			rhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.rhs()).to_rvalue();
		}

		if (lhs_value->getType()->isIntegerTy()) {
			auto* value = e.op() == Ast::BinOp::EQ
				? state.builder->CreateICmpEQ(lhs_value, rhs_value)
				: state.builder->CreateICmpNE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto lhs_intptr = state.builder->CreatePtrToInt(lhs_value, m_intptr);
			auto rhs_intptr = state.builder->CreatePtrToInt(rhs_value, m_intptr);
			auto* value = e.op() == Ast::BinOp::EQ
				? state.builder->CreateICmpEQ(lhs_intptr, rhs_intptr)
				: state.builder->CreateICmpNE(lhs_intptr, rhs_intptr);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}

	auto* lhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.lhs()).to_rvalue();

	if (e.op() == Ast::BinOp::LAND || e.op() == Ast::BinOp::LOR) {
		auto* curr_bb = state.builder->GetInsertBlock();
		auto* else_bb = llvm::BasicBlock::Create(m_ctx, "short_circuit", state.llvm_func);
		auto* next_bb = llvm::BasicBlock::Create(m_ctx, "short_circuit_end", state.llvm_func);

		if (e.op() == Ast::BinOp::LAND) {
			state.builder->CreateCondBr(lhs_value, else_bb, next_bb);
		} else {
			state.builder->CreateCondBr(lhs_value, next_bb, else_bb);
		}
		state.builder->SetInsertPoint(else_bb);

		auto* rhs_value = mpark::visit([this, &state](const auto& e) {
			return visit(e, state);
		}, e.rhs()).to_rvalue();

		state.builder->CreateBr(next_bb);

		state.builder->SetInsertPoint(next_bb);
		auto* phi = state.builder->CreatePHI(m_i1, 2);
		if (e.op() == Ast::BinOp::LAND) {
			phi->addIncoming(rhs_value, else_bb);
			phi->addIncoming(lhs_value, curr_bb);
		} else {
			phi->addIncoming(lhs_value, curr_bb);
			phi->addIncoming(rhs_value, else_bb);
		}

		return LLVMExpr(state.builder, phi, nullptr, false);
	}

	auto* rhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.rhs()).to_rvalue();

	bool is_floating_point = Ast::is_floating_point(e.type());
	bool is_signed = Ast::is_signed_integer(e.type());

	switch (e.op()) {
	case Ast::BinOp::EQ:
	case Ast::BinOp::NE:
		unreachable("Must be handled specifically for pointer equality");

	case Ast::BinOp::LAND:
	case Ast::BinOp::LOR:
		unreachable("Must be handled specifically for short circuiting");

	case Ast::BinOp::PLUS: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFAdd(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
		auto* value = state.builder->CreateAdd(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::MINUS: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFSub(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
		auto* value = state.builder->CreateSub(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::TIMES: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFMul(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
		auto* value = state.builder->CreateMul(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::DIV: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFDiv(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else if (is_signed) {
			auto* value = state.builder->CreateSDiv(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto* value = state.builder->CreateUDiv(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}
	case Ast::BinOp::AND: {
		auto* value = state.builder->CreateAnd(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::OR: {
		auto* value = state.builder->CreateOr(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::XOR: {
		auto* value = state.builder->CreateXor(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::SHL: {
		auto* value = state.builder->CreateShl(lhs_value, rhs_value);
		return LLVMExpr(state.builder, value, nullptr, false);
	}
	case Ast::BinOp::SHR: {
		if (is_signed) {
			auto* value = state.builder->CreateAShr(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto* value = state.builder->CreateLShr(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}
	case Ast::BinOp::LE: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFCmpULE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else if (is_signed) {
			auto* value = state.builder->CreateICmpSLE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto* value = state.builder->CreateICmpULE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}
	case Ast::BinOp::LT: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFCmpULT(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else if (is_signed) {
			auto* value = state.builder->CreateICmpSLT(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto* value = state.builder->CreateICmpULT(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}
	case Ast::BinOp::GE: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFCmpUGE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else if (is_signed) {
			auto* value = state.builder->CreateICmpSGE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto* value = state.builder->CreateICmpUGE(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}
	case Ast::BinOp::GT: {
		if (is_floating_point) {
			auto* value = state.builder->CreateFCmpUGT(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else if (is_signed) {
			auto* value = state.builder->CreateICmpSGT(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		} else {
			auto* value = state.builder->CreateICmpUGT(lhs_value, rhs_value);
			return LLVMExpr(state.builder, value, nullptr, false);
		}
	}
	}
}

LLVMExpr Codegen::Impl::visit(const Ast::VariableExpr& e, MethodCodegenState& state)
{
	auto* value = state.local_vars[&e.var()];
	assert_msg(value != nullptr, "Local variable does not exist? O_o");
	return LLVMExpr(state.builder, value, nullptr, true);
}

LLVMExpr Codegen::Impl::visit(const Ast::MethodCall& e, MethodCodegenState& state)
{
	std::vector<llvm::Value*> llvm_args;

	auto* this_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.this_expr()).to_rvalue();
	llvm_args.push_back(this_value);

	for (size_t i = 0; i < e.args().size(); i++) {
		const auto& arg = e.args()[i];
		if (mpark::holds_alternative<Ast::NullExpr>(arg)) {
			const auto& arg_type = e.method().params()[i].type();
			auto* obj_type = mpark::get_if<Ast::ObjectType>(&arg_type);
			assert_msg(obj_type != nullptr, "Should be an object type");

			llvm_args.push_back(zero(*obj_type, state.spec));
			continue;
		}

		auto* llvm_arg = mpark::visit([this, &state](const auto& e) {
			return visit(e, state);
		}, arg).to_rvalue();
		llvm_args.push_back(llvm_arg);
	}

	auto this_type = Ast::expr_type(e.this_expr());
	auto* this_obj_type = mpark::get_if<Ast::ObjectType>(&this_type);
	assert_msg(this_obj_type != nullptr, "Type of `this` hould be an object type");

	for (size_t i = 0; i < this_obj_type->num_params(); i++) {
		const auto& params = this_obj_type->params();
		const auto* maybe_pool = mpark::get_if<Ast::PoolRef>(&params[i]);
		if (maybe_pool == nullptr) {
			continue;
		}

		const Ast::Pool& pool = maybe_pool->pool();
		auto* value = state.local_pools[&pool];
		if (value == nullptr) {
			continue;
		}

		llvm_args.push_back(value);
	}

	auto new_spec = state.spec.specialize_type(*this_obj_type);
	auto* func = m_specialization_info[new_spec].funcs[&e.method()];

	auto* value = state.builder->CreateCall(func, llvm_args);
	return LLVMExpr(state.builder, value, nullptr, false);
}

LLVMExpr Codegen::Impl::visit(const Ast::FieldAccess& e, MethodCodegenState& state)
{
	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr()).to_rvalue();

	const auto& data_layout = m_mod->getDataLayout();
	llvm::MDBuilder tbaa_builder(m_ctx);

	auto type = Ast::expr_type(e.expr());
	auto* as_obj_type = mpark::get_if<Ast::ObjectType>(&type);
	assert_msg(as_obj_type != nullptr, "No object type for field access?");

	auto new_spec = state.spec.specialize_type(*as_obj_type);
	auto& info = m_specialization_info[new_spec];

	auto* as_pool = mpark::get_if<PoolSpecializationInfo>(&info.type_info);
	auto* as_obj = mpark::get_if<StandaloneSpecializationInfo>(&info.type_info);

	assert_msg(as_pool != nullptr || as_obj != nullptr, "Forgot a case?");

	if (as_pool != nullptr) {
		const auto* layout = new_spec.first_pool_param_spec();
		assert_msg(layout != nullptr, "Not a pool type?");

		const auto* indices = layout->field_pos(e.field());
		assert_msg(indices != nullptr, "Field not in layout?");

		const auto& pool_param = as_obj_type->params().front();
		const auto* pool_ref = mpark::get_if<Ast::PoolRef>(&pool_param);
		assert_msg(pool_ref != nullptr, "Pool is null?");

		const Ast::Pool& pool = pool_ref->pool();
		auto* llvm_pool = state.local_pools[&pool];
		assert_msg(llvm_pool != nullptr, "LLVM pool is null?");

		const auto* pool_layout = data_layout.getStructLayout(as_pool->pool_type);
		auto* pool_tbaa_node = tbaa_builder.createTBAAStructTagNode(
			as_pool->pool_tbaa_type,
			as_pool->cluster_tbaa_ptr_types[indices->cluster_idx],
			pool_layout->getElementOffset(indices->cluster_idx + 2));

		auto* cluster_ptr_ref = state.builder->CreateStructGEP(
			as_pool->pool_type, llvm_pool, indices->cluster_idx + 2);
		auto* cluster_type = as_pool->cluster_types[indices->cluster_idx];
		auto* cluster_ptr = state.builder->CreateLoad(
			cluster_type->getPointerTo(), cluster_ptr_ref);
		cluster_ptr->setMetadata(llvm::LLVMContext::MD_tbaa, pool_tbaa_node);

		const auto* cluster_layout = data_layout.getStructLayout(
			as_pool->cluster_types[indices->cluster_idx]);

		auto* record_ptr = state.builder->CreateInBoundsGEP(
			cluster_type, cluster_ptr, value);
		auto* field_ptr = state.builder->CreateStructGEP(
			cluster_type, record_ptr, indices->pos);

		auto* tbaa_field_type = mpark::visit(
			[this, &state](const auto& e) {
				return tbaa_type_of(e, state.spec);
			}, e.type());

		auto* cluster_tbaa_node = tbaa_builder.createTBAAStructTagNode(
			as_pool->cluster_tbaa_types[indices->cluster_idx],
			tbaa_field_type,
			cluster_layout->getElementOffset(indices->pos));

		return LLVMExpr(state.builder, field_ptr, cluster_tbaa_node, true);
	}

	auto idx = new_spec.clazz().index_of(e.field());
	assert_msg(idx != (size_t)-1, "Field not belonging to class?");

	auto* tbaa_field_type = mpark::visit(
		[this, &state](const auto& e) {
			return tbaa_type_of(e, state.spec);
		}, e.type());

	const auto* layout = data_layout.getStructLayout(as_obj->type);
	auto* tbaa_node = tbaa_builder.createTBAAStructTagNode(
		as_obj->tbaa_type,
		tbaa_field_type,
		layout->getElementOffset(idx));

	auto* field_ptr = state.builder->CreateStructGEP(as_obj->type, value, idx);
	return LLVMExpr(state.builder, field_ptr, tbaa_node, true);
}

LLVMExpr Codegen::Impl::visit(const Ast::NewExpr& e, MethodCodegenState& state)
{
	auto new_spec = state.spec.specialize_type(e.type());
	auto& info = m_specialization_info[new_spec];

	auto* as_pool = mpark::get_if<PoolSpecializationInfo>(&info.type_info);
	if (as_pool != nullptr) {
		const auto* maybe_pool =
			mpark::get_if<Ast::PoolRef>(&e.type().params().front());
		assert_msg(maybe_pool != nullptr, "Should have a pool parameter");

		const Ast::Pool& pool = maybe_pool->pool();
		auto* llvm_pool = state.local_pools[&pool];

		assert_msg(llvm_pool != nullptr, "No LLVM pointer to pool?");

		auto* retval = state.builder->CreateCall(as_pool->obj_ctor, {llvm_pool});
		return LLVMExpr(state.builder, retval, nullptr, false);
	}

	auto* as_obj = mpark::get_if<StandaloneSpecializationInfo>(&info.type_info);
	assert_msg(as_obj != nullptr, "Should be an object");

	auto* retval = state.builder->CreateCall(as_obj->ctor);
	return LLVMExpr(state.builder, retval, nullptr, false);
}

bool Codegen::Impl::ir(const Ast::Program& ast)
{
	Codegen::Impl state;
	std::string error;
	m_target = llvm::TargetRegistry::lookupTarget("x86_64-unknown-linux-gnu", error);

	if (m_target == nullptr) {
		fprintf(stderr, "Error while creating LLVM target machine: %s", error.c_str());
		return false;
	}

	m_target_machine = m_target->createTargetMachine(
		"x86_64-unknown-linux-gnu",
		"generic",
		"",
		llvm::TargetOptions(),
		llvm::None,
		llvm::None,
		llvm::CodeGenOpt::Aggressive);

	m_void = llvm::Type::getVoidTy(m_ctx);
	m_null = m_void->getPointerTo();

	m_i1 = llvm::Type::getInt1Ty(m_ctx);
	m_i8 = llvm::Type::getInt8Ty(m_ctx);
	m_i16 = llvm::Type::getInt16Ty(m_ctx);
	m_i32 = llvm::Type::getInt32Ty(m_ctx);
	m_i64 = llvm::Type::getInt64Ty(m_ctx);

	m_f32 = llvm::Type::getFloatTy(m_ctx);
	m_f64 = llvm::Type::getDoubleTy(m_ctx);

	m_mod.reset(new llvm::Module("shapes", m_ctx));
	m_mod->setDataLayout(m_target_machine->createDataLayout());

	const auto& data_layout = m_mod->getDataLayout();
	m_intptr = data_layout.getIntPtrType(m_ctx);

	m_malloc_type = llvm::FunctionType::get(
		m_i8->getPointerTo(), {m_intptr}, false);
	m_malloc = llvm::Function::Create(
		m_malloc_type,
		llvm::GlobalValue::ExternalLinkage,
		"malloc",
		m_mod.get());
	m_malloc->setReturnDoesNotAlias();
	m_malloc->addFnAttr(llvm::Attribute::NoUnwind);

	m_realloc_type = llvm::FunctionType::get(
		m_i8->getPointerTo(), {m_i8->getPointerTo(), m_intptr}, false);
	m_realloc = llvm::Function::Create(
		m_realloc_type,
		llvm::GlobalValue::ExternalLinkage,
		"realloc",
		m_mod.get());
	m_realloc->setReturnDoesNotAlias();
	m_realloc->addFnAttr(llvm::Attribute::NoUnwind);

	m_free_type = llvm::FunctionType::get(
		m_void, {m_i8->getPointerTo()}, false);
	m_free = llvm::Function::Create(
		m_free_type,
		llvm::GlobalValue::ExternalLinkage,
		"free",
		m_mod.get());
	m_free->addFnAttr(llvm::Attribute::NoUnwind);

	llvm::MDBuilder tbaa_builder(m_ctx);

	m_tbaa_root = tbaa_builder.createTBAARoot("shapes_tbaa_root");
	m_tbaa_bool = tbaa_builder.createTBAAScalarTypeNode("bool", m_tbaa_root);
	m_tbaa_i8 = tbaa_builder.createTBAAScalarTypeNode("i8", m_tbaa_root);
	m_tbaa_u8 = tbaa_builder.createTBAAScalarTypeNode("u8", m_tbaa_root);
	m_tbaa_i16 = tbaa_builder.createTBAAScalarTypeNode("i16", m_tbaa_root);
	m_tbaa_u16 = tbaa_builder.createTBAAScalarTypeNode("u16", m_tbaa_root);
	m_tbaa_i32 = tbaa_builder.createTBAAScalarTypeNode("i32", m_tbaa_root);
	m_tbaa_u32 = tbaa_builder.createTBAAScalarTypeNode("u32", m_tbaa_root);
	m_tbaa_i64 = tbaa_builder.createTBAAScalarTypeNode("i64", m_tbaa_root);
	m_tbaa_u64 = tbaa_builder.createTBAAScalarTypeNode("u64", m_tbaa_root);

	m_tbaa_intptr = tbaa_builder.createTBAAScalarTypeNode("intptr", m_tbaa_root);

	for (const Ast::Class& e: ast.ordered_classes()) {
		generate_specializations(e);
	}

	generate_llvm_types();
	generate_llvm_function_decls();
	generate_llvm_functions();

	llvm::verifyModule(*m_mod, &llvm::errs());
	return true;
}

bool Codegen::Impl::emit(const char* filename)
{
	auto opt_level = llvm::PassBuilder::OptimizationLevel::O3;

	llvm::PassBuilder pass_builder(m_target_machine);

	llvm::FunctionAnalysisManager fam;
	llvm::LoopAnalysisManager lam;
	llvm::CGSCCAnalysisManager cam;
	llvm::ModuleAnalysisManager mam;

	fam.registerPass([&] { return pass_builder.buildDefaultAAPipeline(); });

	pass_builder.registerFunctionAnalyses(fam);
	pass_builder.registerLoopAnalyses(lam);
	pass_builder.registerCGSCCAnalyses(cam);
	pass_builder.registerModuleAnalyses(mam);

	pass_builder.crossRegisterProxies(lam, fam, cam, mam);

	auto mod_pass_manager =
		pass_builder.buildPerModuleDefaultPipeline(opt_level, false);
	mod_pass_manager.addPass(llvm::PrintModulePass(llvm::errs()));

	mod_pass_manager.run(*m_mod, mam);

	std::error_code EC;
	llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);

	llvm::legacy::PassManager old_pm;

	m_target_machine->addPassesToEmitFile(old_pm, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile);
	old_pm.run(*m_mod);

	return true;
}

llvm::Function* Codegen::Impl::find_method(const ClassSpecialization& spec, const Ast::Method& m) const
{
	auto spec_it = m_specialization_info.find(spec);
	if (spec_it == m_specialization_info.end()) {
		return nullptr;
	}

	const auto& mapping = spec_it->second.funcs;
	auto method_it = mapping.find(&m);

	return (method_it != mapping.end())
		? method_it->second
		: nullptr;
}

Codegen::Codegen() {}
Codegen::~Codegen() {}

bool Codegen::ir(const Ast::Program& ast)
{
	if (m_impl != nullptr) {
		return false;
	}
	m_impl.reset(new Codegen::Impl);

	return m_impl->ir(ast);
}

bool Codegen::emit(const char* filename)
{
	return m_impl->emit(filename);
}

std::unique_ptr<llvm::Module> Codegen::Impl::get_module()
{
	return std::move(m_mod);
}

std::unique_ptr<llvm::Module> Codegen::get_module()
{
	return m_impl->get_module();
}

llvm::Function*
Codegen::find_method(const ClassSpecialization& spec, const Ast::Method& m) const
{
	return m_impl->find_method(spec, m);
}

class CodegenInterpreter::Impl
{
	const Codegen* m_codegen;
	llvm::ExecutionEngine* m_engine;

public:
	void init(Codegen& codegen);

	llvm::GenericValue
	run_function(llvm::Function* func, std::vector<llvm::GenericValue> values);

	llvm::Function*
	find_method(const ClassSpecialization& spec, const Ast::Method& m) const;
};

void CodegenInterpreter::Impl::init(Codegen& codegen)
{
	m_codegen = &codegen;

	std::string error_msg;

	llvm::EngineBuilder builder(codegen.get_module());
	builder.setErrorStr(&error_msg);
	builder.setEngineKind(llvm::EngineKind::Interpreter);
	builder.setVerifyModules(true);

	m_engine = builder.create();
	if (!error_msg.empty()) {
		fprintf(stderr, "Interpreter error message: %s\n", error_msg.c_str());
	}

	assert_msg(m_engine != nullptr, "Engine was not created? O_o");

	m_engine->addGlobalMapping("malloc", (uint64_t) &malloc);
	m_engine->addGlobalMapping("realloc", (uint64_t) &realloc);
	m_engine->addGlobalMapping("free", (uint64_t) &free);

	m_engine->finalizeObject();
}

llvm::GenericValue
CodegenInterpreter::Impl::run_function(
	llvm::Function* func, std::vector<llvm::GenericValue> values)
{
	return m_engine->runFunction(func, values);
}

llvm::Function*
CodegenInterpreter::Impl::find_method(const ClassSpecialization& spec, const Ast::Method& m) const
{
	return m_codegen->find_method(spec, m);
}

CodegenInterpreter::CodegenInterpreter(Codegen& codegen)
	: m_impl(new CodegenInterpreter::Impl)
{
	m_impl->init(codegen);
}
CodegenInterpreter::~CodegenInterpreter() {}

llvm::GenericValue
CodegenInterpreter::run_function(
	llvm::Function* func, std::vector<llvm::GenericValue> values)
{
	return m_impl->run_function(func, values);
}

llvm::Function*
CodegenInterpreter::find_method(const ClassSpecialization& spec, const Ast::Method& m) const
{
	return m_impl->find_method(spec, m);
}

} // namespace Ir

template<typename T>
static inline void hash_combine(std::size_t& seed, T&& v)
{
	using Type = std::remove_cv_t<std::remove_reference_t<T>>;

	std::hash<Type> hasher;
	seed ^= hasher(std::forward<T>(v)) + 0x9e3779b9u + (seed<<6) + (seed>>2);
}

size_t std::hash<Ir::ClassSpecialization>::operator()(
	const Ir::ClassSpecialization& specialization) const
{
	size_t hash = std::hash<const Ast::Class*>{}(&specialization.clazz());

	for (const auto& e: specialization.pool_param_types()) {
		hash_combine(hash, e);
	}

	return hash;
}
