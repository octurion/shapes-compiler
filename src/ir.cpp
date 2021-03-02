#include "ast.h"
#include "ir.h"

#include <llvm/ADT/None.h>

#include <llvm/IR/AssemblyAnnotationWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
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

class ClassSpecialization
{
	const Ast::Class* m_class = nullptr;
	std::vector<const Ast::Layout*> m_pool_param_types;

	ClassSpecialization specialize(
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
			if (mpark::holds_alternative<Ast::NoneType>(pool.type())) {
				param_types.push_back(nullptr);
				continue;
			}

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

public:
	ClassSpecialization() = default;

	ClassSpecialization(
			const Ast::Class& clazz,
			std::vector<const Ast::Layout*> pool_param_types)
		: m_class(&clazz)
		, m_pool_param_types(std::move(pool_param_types))
	{}

	const Ast::Class& clazz() const {
		assert_msg(m_class != nullptr, "Invalid class specialization");
		return *m_class;
	}
	const std::vector<const Ast::Layout*>& pool_param_types() const {
		return m_pool_param_types;
	}

	const Ast::Layout* first_pool_param_spec() const {
		assert_msg(!m_pool_param_types.empty(), "Specialization has no pool parameters");
		return m_pool_param_types.front();
	}

	bool is_pooled_type() const {
		assert_msg(!m_pool_param_types.empty(), "Specialization has no pool parameters");
		return m_pool_param_types.front() != nullptr;
	}

	bool operator==(const ClassSpecialization& rhs) const {
		return m_class == rhs.m_class
			&& m_pool_param_types == rhs.m_pool_param_types;
	}
	bool operator!=(const ClassSpecialization& rhs) const {
		return m_class != rhs.m_class
			|| !(m_pool_param_types == rhs.m_pool_param_types);
	}

	ClassSpecialization specialize_type(const Ast::LayoutType& type) const
	{
		return specialize(type.for_class(), type.params());
	}

	ClassSpecialization specialize_type(const Ast::BoundType& type) const
	{
		return specialize(type.of_class(), type.params());
	}

	ClassSpecialization specialize_type(const Ast::ObjectType& type) const
	{
		return specialize(type.of_class(), type.params());
	}
};

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

	llvm::Function* ctor = nullptr;
};

struct PoolSpecializationInfo
{
	llvm::StructType* pool_type = nullptr;
	std::vector<llvm::StructType*> cluster_types;

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

class CodegenState
{
	llvm::LLVMContext m_ctx;
	const llvm::Target* m_target = nullptr;
	llvm::TargetMachine* m_target_machine = nullptr;

	llvm::Module* m_mod = nullptr;

	llvm::Type* m_void = nullptr;
	llvm::Type* m_null = nullptr;

	llvm::Type* m_i1 = nullptr;
	llvm::Type* m_i8 = nullptr;
	llvm::Type* m_i16 = nullptr;
	llvm::Type* m_i32 = nullptr;
	llvm::Type* m_i64 = nullptr;

	llvm::Type* m_f32 = nullptr;
	llvm::Type* m_f64 = nullptr;

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

	llvm::Value* visit(const Ast::IntegerConst& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::DoubleConst& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::BooleanConst& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::NullExpr& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::ThisExpr& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::CastExpr& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::UnaryExpr& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::BinaryExpr& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::VariableExpr& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::MethodCall& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::FieldAccess& e, MethodCodegenState& state);
	llvm::Value* visit(const Ast::NewExpr& e, MethodCodegenState& state);

	void generate_llvm_types();
	void generate_llvm_function_decls();
	void generate_llvm_functions();

public:
	bool ir(const Ast::Program& ast);
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

std::string create_cluster_name(const ClassSpecialization& specialization, size_t idx)
{
	std::ostringstream os;
	os << "struct.cluster." << idx << "." << specialization;

	return os.str();
}

std::string create_class_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "struct." << specialization;

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

void CodegenState::generate_specializations_impl(
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

llvm::Type* CodegenState::type_of(const Ast::PoolType& type, const ClassSpecialization& specialization)
{
	return mpark::visit([this, &specialization](const auto& e) {
		return type_of(e, specialization);
	}, type);
}

llvm::Type* CodegenState::type_of(const Ast::NoneType&, const ClassSpecialization&)
{
	return nullptr;
}

llvm::Type* CodegenState::type_of(const Ast::LayoutType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	return mpark::visit(
		LLVMTypeFunctor(), m_specialization_info[new_spec].type_info);
}

llvm::Type* CodegenState::type_of(const Ast::BoundType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	return mpark::visit(
		LLVMTypeFunctor(), m_specialization_info[new_spec].type_info);
}

void CodegenState::generate_specializations(const Ast::Class& clazz)
{
	std::vector<const Ast::Layout*> curr_layouts;

	generate_specializations_impl(clazz, curr_layouts);
}

llvm::Type* CodegenState::type_of(const Ast::NullptrType&, const ClassSpecialization&)
{
	unreachable("Null pointers do not have an LLVM type");
}

llvm::Type* CodegenState::type_of(const Ast::VoidType&, const ClassSpecialization&)
{
	return m_void;
}

llvm::Type* CodegenState::type_of(const Ast::ObjectType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	const auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
		&m_specialization_info[new_spec].type_info);
	if (as_standalone != nullptr) {
		return as_standalone->type->getPointerTo();
	}
	return m_i64;
}

llvm::Type* CodegenState::type_of(const Ast::PrimitiveType& type, const ClassSpecialization&)
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

llvm::Type* CodegenState::type_of(const Ast::Type& type, const ClassSpecialization& specialization)
{
	return mpark::visit([this, &specialization](const auto& e){
		return type_of(e, specialization);
	}, type);
}

llvm::Constant* CodegenState::zero(const Ast::ObjectType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	const auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
		&m_specialization_info[new_spec].type_info);
	if (as_standalone != nullptr) {
		return llvm::ConstantPointerNull::get(as_standalone->type->getPointerTo());
	}
	return llvm::ConstantInt::get(m_i64, -1ull, true);
}

llvm::Constant* CodegenState::zero(const Ast::PrimitiveType& type, const ClassSpecialization& spec)
{
	return llvm::ConstantInt::get(type_of(type, spec), 0);
}

llvm::Constant* CodegenState::zero(const Ast::NullptrType&, const ClassSpecialization&)
{
	unreachable("Null pointers require an explicit check for them");
}

llvm::Constant* CodegenState::zero(const Ast::VoidType&, const ClassSpecialization&)
{
	unreachable("Null pointers have no initial value!");
}

llvm::Constant* CodegenState::zero(const Ast::Type& type, const ClassSpecialization& specialization)
{
	return mpark::visit([this, &specialization](const auto& e){
		return zero(e, specialization);
	}, type);
}

void CodegenState::generate_llvm_types()
{
	for (auto& e: m_specialization_info) {
		const auto& spec = e.first;
		auto& info = e.second;

		if (spec.is_pooled_type()) {
			PoolSpecializationInfo pool_info;
			pool_info.pool_type =
				llvm::StructType::create(m_ctx, create_pool_name(spec));
			info.type_info = std::move(pool_info);
		} else {
			StandaloneSpecializationInfo standalone_info;
			standalone_info.type =
				llvm::StructType::create(m_ctx, create_pool_name(spec));
			info.type_info = std::move(standalone_info);
		}
	}

	for (auto& e: m_specialization_info) {
		const auto& spec = e.first;
		auto& info = e.second;

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
			}

			std::vector<llvm::Type*> pool_fields { m_i64, m_i64 };
			for (const auto& e: pool_info->cluster_types) {
				pool_fields.push_back(e->getPointerTo());
			}

			pool_info->pool_type->setBody(pool_fields);
		} else {
			auto* standalone_info =
				mpark::get_if<StandaloneSpecializationInfo>(&info.type_info);
			assert_msg(standalone_info != nullptr, "Not a standalone type?");

			std::vector<llvm::Type*> fields;
			for (const Ast::Field& e: spec.clazz().fields()) {
				auto* type = mpark::visit(
					[this, &spec](const auto& e) {
						return type_of(e, spec);
				}, e.type());
				fields.push_back(type);
			}
			standalone_info->type->setBody(fields);
		}
	}
}

void CodegenState::generate_llvm_function_decls()
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
				m_mod);
			func->addFnAttr(llvm::Attribute::NoUnwind);

			info.funcs[&method] = func;
		}

		auto* as_standalone = mpark::get_if<StandaloneSpecializationInfo>(
			&info.type_info);
		if (as_standalone != nullptr) {
			auto* ctor_func_type = llvm::FunctionType::get(
				as_standalone->type->getPointerTo(), {}, false);
			as_standalone->ctor = llvm::Function::Create(
					ctor_func_type,
					llvm::GlobalValue::ExternalLinkage,
					create_ctor_name(specialization),
					m_mod);
			as_standalone->ctor->setReturnDoesNotAlias();
			as_standalone->ctor->addFnAttr(llvm::Attribute::NoUnwind);

			auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", as_standalone->ctor);
			llvm::IRBuilder<> builder(bb);
			auto* class_ptr_type = as_standalone->type->getPointerTo();

			auto* size = llvm::ConstantExpr::getSizeOf(as_standalone->type);
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
				m_mod);
			as_pool->pool_alloc->addFnAttr(llvm::Attribute::NoUnwind);

			{
				auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", as_pool->pool_alloc);
				llvm::IRBuilder<> builder(bb);

				assert_msg(
					!as_pool->pool_alloc->arg_empty(),
					"Pool constructor has no arguments?");

				auto* pool_ptr = as_pool->pool_alloc->arg_begin();
				std::vector<llvm::Constant*> init_values = {
					llvm::ConstantInt::get(m_i64, 0),
					llvm::ConstantInt::get(m_i64, 0),
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
				m_mod);
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
				m_i64, {as_pool->pool_type->getPointerTo()}, false);
			as_pool->obj_ctor = llvm::Function::Create(
				alloc_func_type,
				llvm::GlobalValue::ExternalLinkage,
				create_ctor_name(specialization),
				m_mod);
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
			llvm::Value* size;
			llvm::Value* capacity;

			llvm::IRBuilder<> builder(bb_entry);

			size_ptr = builder.CreateStructGEP(as_pool->pool_type, pool_ptr, 0);
			capacity_ptr = builder.CreateStructGEP(as_pool->pool_type, pool_ptr, 1);

			size = builder.CreateLoad(size_ptr);
			capacity = builder.CreateLoad(capacity_ptr);
			auto* filled = builder.CreateICmpEQ(size, capacity);

			builder.CreateCondBr(filled, bb_call_realloc, bb_alloc);

			builder.SetInsertPoint(bb_call_realloc);

			auto* new_cap = builder.CreateShl(capacity, 1);
			auto* new_cap_nonzero = builder.CreateICmpNE(
				new_cap, llvm::ConstantInt::get(m_i64, 0));
			new_cap = builder.CreateSelect(
				new_cap_nonzero,
				new_cap,
				llvm::ConstantInt::get(m_i64, 1));
			builder.CreateStore(new_cap, capacity_ptr);

			const auto& cluster_types = as_pool->cluster_types;
			for (size_t i = 0; i < cluster_types.size(); i++) {
				auto* cluster_type = cluster_types[i];
				auto* cluster_ptr_type = cluster_type->getPointerTo();

				auto* member_ptr = builder.CreateStructGEP(
					as_pool->pool_type, pool_ptr, i + 2);
				auto* old_ptr = builder.CreatePointerCast(
					builder.CreateLoad(member_ptr),
					m_i8->getPointerTo());

				auto* cluster_size = llvm::ConstantExpr::getSizeOf(m_i64);
				auto* new_size = builder.CreateMul(new_cap, cluster_size);
				auto* realloc = builder.CreateCall(
					m_realloc, {old_ptr, new_size});
				builder.CreateStore(
					builder.CreatePointerCast(realloc, cluster_ptr_type),
					member_ptr);
			}
			builder.CreateBr(bb_alloc);

			builder.SetInsertPoint(bb_alloc);
			auto* new_idx = size;
			builder.CreateStore(
				builder.CreateAdd(size, llvm::ConstantInt::get(m_i64, 1)),
				builder.CreateStructGEP(as_pool->pool_type, pool_ptr, 0));

			const auto& clusters = layout->clusters();
			for (size_t i = 0; i < clusters.size(); i++) {
				std::vector<llvm::Constant*> constants;
				const auto& fields = clusters[i].fields();
				for (size_t j = 0; j < fields.size(); j++) {
					constants.push_back(zero(fields[j]->type(), specialization));
				}
				auto* cluster_ptr = builder.CreateStructGEP(
					as_pool->pool_type, pool_ptr, i + 2);
				auto* cluster = builder.CreateLoad(cluster_ptr);
				auto* offset_ptr = builder.CreateGEP(cluster, new_idx);
				auto* const_val = llvm::ConstantStruct::get(
					cluster_types[i], constants);
				builder.CreateStore(const_val, offset_ptr);
			}

			builder.CreateRet(new_idx);

			continue;
		}

		unreachable("Neither standalone nor pooled?");
	}
}

struct MethodCodegenState
{
	ClassSpecialization spec;
	const Ast::Method* method = nullptr;
	llvm::Function* llvm_func = nullptr;

	llvm::IRBuilder<>* builder = nullptr;

	std::unordered_map<const Ast::Variable*, llvm::Value*> local_vars;
	std::unordered_map<const Ast::Pool*, llvm::Value*> local_pools;

	MethodCodegenState() = default;
	MethodCodegenState(const MethodCodegenState&) = delete;
	MethodCodegenState& operator=(const MethodCodegenState&) = delete;
};

void CodegenState::generate_llvm_functions()
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

			const auto* as_primitive = mpark::get_if<Ast::PrimitiveType>(&method.return_type());
			if (as_primitive != nullptr) {
				builder.CreateRet(zero(*as_primitive, state.spec));
				continue;
			}

			const auto* as_object = mpark::get_if<Ast::ObjectType>(&method.return_type());
			if (as_object != nullptr) {
				builder.CreateRet(zero(*as_object, state.spec));
				continue;
			}

			unreachable("Null return type?");
		}
	}
}

void CodegenState::visit(const Ast::Assignment& e, MethodCodegenState& state)
{
	auto* rhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.rhs());
	if (Ast::is_lvalue(e.rhs())) {
		rhs_value = state.builder->CreateLoad(rhs_value);
	}

	auto* lhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.lhs());

	state.builder->CreateStore(rhs_value, lhs_value);
}

void CodegenState::visit(const Ast::OpAssignment& e, MethodCodegenState& state)
{
	auto* rhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.rhs());
	if (Ast::is_lvalue(e.rhs())) {
		rhs_value = state.builder->CreateLoad(rhs_value);
	}

	auto* lhs = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.lhs());
	auto* lhs_value = state.builder->CreateLoad(lhs);

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

	state.builder->CreateStore(value, lhs);
}

void CodegenState::visit(const Ast::If& e, MethodCodegenState& state)
{
	auto* cond = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.cond());
	if (Ast::is_lvalue(e.cond())) {
		cond = state.builder->CreateLoad(cond);
	}

	auto* then_bb = llvm::BasicBlock::Create(m_ctx, "if_then", state.llvm_func);
	auto* else_bb = llvm::BasicBlock::Create(m_ctx, "if_else", state.llvm_func);
	state.builder->CreateCondBr(cond, then_bb, else_bb);

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

void CodegenState::visit(const Ast::While& e, MethodCodegenState& state)
{

}

void CodegenState::visit(const Ast::ForeachRange& e, MethodCodegenState& state)
{

}

void CodegenState::visit(const Ast::ForeachPool& e, MethodCodegenState& state)
{

}

void CodegenState::visit(const Ast::ExprStmt& e, MethodCodegenState& state)
{
	if (mpark::holds_alternative<Ast::NullExpr>(e.expr())) {
		return;
	}
	mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr());
}

void CodegenState::visit(const Ast::Break& e, MethodCodegenState& state)
{

}

void CodegenState::visit(const Ast::Continue& e, MethodCodegenState& state)
{

}

void CodegenState::visit(const Ast::Return& e, MethodCodegenState& state)
{
	if (e.expr() == nullptr) {
		state.builder->CreateRetVoid();
		return;
	}

	if (mpark::holds_alternative<Ast::NullExpr>(*e.expr())) {
		auto* as_obj_type = mpark::get_if<Ast::ObjectType>(&state.method->return_type());
		auto new_spec = state.spec.specialize_type(*as_obj_type);

		auto* nullptr_value = new_spec.is_pooled_type()
			? llvm::ConstantInt::get(m_i64, -1ull, true)
			: llvm::ConstantPointerNull::get(
				llvm::cast<llvm::PointerType>(type_of(*as_obj_type, new_spec)));

		state.builder->CreateRet(nullptr_value);
		return;
	}

	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, *e.expr());
	if (Ast::is_lvalue(*e.expr())) {
		value = state.builder->CreateLoad(value);
	}
	state.builder->CreateRet(value);

	auto* new_bb = llvm::BasicBlock::Create(m_ctx, "after_ret", state.llvm_func);
	state.builder->SetInsertPoint(new_bb);
}

llvm::Value* CodegenState::visit(const Ast::IntegerConst& e, MethodCodegenState&)
{
	return llvm::ConstantInt::get(m_i64, e.value());
}

llvm::Value* CodegenState::visit(const Ast::DoubleConst& e, MethodCodegenState&)
{
	return llvm::ConstantFP::get(m_f64, e.value());
}

llvm::Value* CodegenState::visit(const Ast::BooleanConst& e, MethodCodegenState&)
{
	return llvm::ConstantInt::get(m_i1, e.value());
}

llvm::Value* CodegenState::visit(const Ast::NullExpr&, MethodCodegenState&)
{
	unreachable("You must check explicitly for null expressions");
}

llvm::Value* CodegenState::visit(const Ast::ThisExpr&, MethodCodegenState& state)
{
	return state.llvm_func->arg_begin();
}

llvm::Value* CodegenState::visit(const Ast::CastExpr& e, MethodCodegenState& state)
{
	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr());
	if (Ast::is_lvalue(e.expr())) {
		value = state.builder->CreateLoad(value);
	}

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
			return state.builder->CreateFCmpUNE(value, zero);
		}

		auto* zero = llvm::ConstantInt::get(llvm_dest_type, 0);
		return state.builder->CreateICmpNE(value, zero);
	}

	// Boolean to anything
	if (Ast::is_boolean(*src_type)) {
		if (Ast::is_floating_point(dest_type)) {
			return state.builder->CreateSelect(value,
				llvm::ConstantFP::get(llvm_dest_type, 1.0),
				llvm::ConstantFP::get(llvm_dest_type, 0.0));
		}

		return state.builder->CreateSelect(value,
			llvm::ConstantInt::get(llvm_dest_type, 1),
			llvm::ConstantInt::get(llvm_dest_type, 0));
	}

	// Anything to float
	if (Ast::is_floating_point(dest_type)) {
		if (Ast::is_floating_point(*src_type)) {
			return state.builder->CreateFPCast(value, llvm_dest_type);
		}
		if (Ast::is_signed_integer(*src_type)) {
			return state.builder->CreateSIToFP(value, llvm_dest_type);
		}
		if (Ast::is_unsigned_integer(*src_type) || Ast::is_boolean(*src_type)) {
			return state.builder->CreateUIToFP(value, llvm_dest_type);
		}
	}

	// Float to anything
	if (Ast::is_floating_point(*src_type)) {
		if (Ast::is_floating_point(dest_type)) {
			return state.builder->CreateFPCast(value, llvm_dest_type);
		}
		if (Ast::is_signed_integer(dest_type)) {
			return state.builder->CreateFPToSI(value, llvm_dest_type);
		}
		if (Ast::is_unsigned_integer(*src_type) || Ast::is_boolean(dest_type)) {
			return state.builder->CreateFPToUI(value, llvm_dest_type);
		}
	}

	// Int truncation or bitcast
	if (llvm_dest_type->getScalarSizeInBits() <= llvm_src_type->getScalarSizeInBits()) {
		return state.builder->CreateTruncOrBitCast(value, llvm_dest_type);
	}

	if (Ast::is_signed_integer(*src_type)) {
		return state.builder->CreateSExt(value, llvm_dest_type);
	} else {
		return state.builder->CreateZExt(value, llvm_dest_type);
	}
}

llvm::Value* CodegenState::visit(const Ast::UnaryExpr& e, MethodCodegenState& state)
{
	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr());

	if (Ast::is_lvalue(e.expr())) {
		value = state.builder->CreateLoad(value);
	}
	auto type = Ast::expr_type(e.expr());
	auto* src_type = mpark::get_if<Ast::PrimitiveType>(&type);
	assert_msg(src_type != nullptr, "Not a primitive type?");

	switch (e.op()) {
	case Ast::UnOp::PLUS: {
		return value;
	}
	case Ast::UnOp::MINUS: {
		if (Ast::is_floating_point(*src_type)) {
			return state.builder->CreateFNeg(value);
		}

		return state.builder->CreateNeg(value);
	}
	case Ast::UnOp::NOT: {
		assert_msg(!Ast::is_floating_point(*src_type), "Can't be a float");
		return state.builder->CreateNot(value);
	}
	}
}

llvm::Value* CodegenState::visit(const Ast::BinaryExpr& e, MethodCodegenState& state)
{
	if (e.op() == Ast::BinOp::EQ || e.op() == Ast::BinOp::NE) {
		auto lhs_type = Ast::expr_type(e.lhs());
		auto rhs_type = Ast::expr_type(e.rhs());

		const auto* as_primitive = mpark::get_if<Ast::PrimitiveType>(&lhs_type);
		if (as_primitive != nullptr) {
			auto* lhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.lhs());
			if (Ast::is_lvalue(e.lhs())) {
				lhs_value = state.builder->CreateLoad(lhs_value);
			}

			auto* rhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.rhs());
			if (Ast::is_lvalue(e.rhs())) {
				rhs_value = state.builder->CreateLoad(rhs_value);
			}

			if (Ast::is_floating_point(*as_primitive)) {
				return state.builder->CreateFCmpUEQ(lhs_value, rhs_value);
			}
			return state.builder->CreateICmpEQ(lhs_value, rhs_value);
		}

		const auto* as_lhs_obj = mpark::get_if<Ast::ObjectType>(&lhs_type);
		const auto* as_rhs_obj = mpark::get_if<Ast::ObjectType>(&rhs_type);

		const auto& obj_type = as_lhs_obj != nullptr ? *as_lhs_obj : *as_rhs_obj;
		const auto new_spec = state.spec.specialize_type(obj_type);

		auto* nullptr_value = new_spec.is_pooled_type()
			? llvm::ConstantInt::get(m_i64, -1ull, true)
			: llvm::ConstantPointerNull::get(
				llvm::cast<llvm::PointerType>(type_of(obj_type, new_spec)));

		llvm::Value* lhs_value;
		if (mpark::holds_alternative<Ast::NullptrType>(lhs_type)) {
			lhs_value = nullptr_value;
		} else {
			lhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.lhs());
			if (Ast::is_lvalue(e.lhs())) {
				lhs_value = state.builder->CreateLoad(lhs_value);
			}
		}

		llvm::Value* rhs_value;
		if (mpark::holds_alternative<Ast::NullptrType>(rhs_type)) {
			rhs_value = nullptr_value;
		} else {
			rhs_value = mpark::visit([this, &state](const auto& e) {
				return visit(e, state);
			}, e.rhs());
			if (Ast::is_lvalue(e.rhs())) {
				rhs_value = state.builder->CreateLoad(rhs_value);
			}
		}

		if (lhs_value->getType()->isIntegerTy()) {
			return e.op() == Ast::BinOp::EQ
				? state.builder->CreateICmpEQ(lhs_value, rhs_value)
				: state.builder->CreateICmpNE(lhs_value, rhs_value);
		} else {
			auto lhs_intptr = state.builder->CreatePtrToInt(lhs_value, m_i64);
			auto rhs_intptr = state.builder->CreatePtrToInt(rhs_value, m_i64);
			return e.op() == Ast::BinOp::EQ
				? state.builder->CreateICmpEQ(lhs_intptr, rhs_intptr)
				: state.builder->CreateICmpNE(lhs_intptr, rhs_intptr);
		}
	}

	auto* lhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.lhs());
	if (Ast::is_lvalue(e.lhs())) {
		lhs_value = state.builder->CreateLoad(lhs_value);
	}

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
		}, e.rhs());

		if (Ast::is_lvalue(e.rhs())) {
			rhs_value = state.builder->CreateLoad(rhs_value);
		}
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

		return phi;
	}

	auto* rhs_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.rhs());
	if (Ast::is_lvalue(e.rhs())) {
		rhs_value = state.builder->CreateLoad(rhs_value);
	}

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
			return state.builder->CreateFAdd(lhs_value, rhs_value);
		}
		return state.builder->CreateAdd(lhs_value, rhs_value);
	}
	case Ast::BinOp::MINUS: {
		if (is_floating_point) {
			return state.builder->CreateFSub(lhs_value, rhs_value);
		}
		return state.builder->CreateSub(lhs_value, rhs_value);
	}
	case Ast::BinOp::TIMES: {
		if (is_floating_point) {
			return state.builder->CreateFMul(lhs_value, rhs_value);
		}
		return state.builder->CreateMul(lhs_value, rhs_value);
	}
	case Ast::BinOp::DIV: {
		if (is_floating_point) {
			return state.builder->CreateFDiv(lhs_value, rhs_value);
		} else if (is_signed) {
			return state.builder->CreateSDiv(lhs_value, rhs_value);
		} else {
			return state.builder->CreateUDiv(lhs_value, rhs_value);
		}
	}
	case Ast::BinOp::AND: {
		return state.builder->CreateAnd(lhs_value, rhs_value);
	}
	case Ast::BinOp::OR: {
		return state.builder->CreateOr(lhs_value, rhs_value);
	}
	case Ast::BinOp::XOR: {
		return state.builder->CreateXor(lhs_value, rhs_value);
	}
	case Ast::BinOp::SHL: {
		return state.builder->CreateShl(lhs_value, rhs_value);
	}
	case Ast::BinOp::SHR: {
		if (is_signed) {
			return state.builder->CreateAShr(lhs_value, rhs_value);
		} else {
			return state.builder->CreateLShr(lhs_value, rhs_value);
		}
	}
	case Ast::BinOp::LE: {
		if (is_floating_point) {
			return state.builder->CreateFCmpULE(lhs_value, rhs_value);
		} else if (is_signed) {
			return state.builder->CreateICmpSLE(lhs_value, rhs_value);
		} else {
			return state.builder->CreateICmpULE(lhs_value, rhs_value);
		}
	}
	case Ast::BinOp::LT: {
		if (is_floating_point) {
			return state.builder->CreateFCmpULT(lhs_value, rhs_value);
		} else if (is_signed) {
			return state.builder->CreateICmpSLT(lhs_value, rhs_value);
		} else {
			return state.builder->CreateICmpULT(lhs_value, rhs_value);
		}
	}
	case Ast::BinOp::GE: {
		if (is_floating_point) {
			return state.builder->CreateFCmpUGE(lhs_value, rhs_value);
		} else if (is_signed) {
			return state.builder->CreateICmpSGE(lhs_value, rhs_value);
		} else {
			return state.builder->CreateICmpUGE(lhs_value, rhs_value);
		}
	}
	case Ast::BinOp::GT: {
		if (is_floating_point) {
			return state.builder->CreateFCmpUGT(lhs_value, rhs_value);
		} else if (is_signed) {
			return state.builder->CreateICmpSGT(lhs_value, rhs_value);
		} else {
			return state.builder->CreateICmpUGT(lhs_value, rhs_value);
		}
	}
	}
}

llvm::Value* CodegenState::visit(const Ast::VariableExpr& e, MethodCodegenState& state)
{
	auto* value = state.local_vars[&e.var()];
	assert_msg(value != nullptr, "Local variable does not exist? O_o");
	return value;
}

llvm::Value* CodegenState::visit(const Ast::MethodCall& e, MethodCodegenState& state)
{
	std::vector<llvm::Value*> llvm_args;

	// TODO: Not done
	auto* this_value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.this_expr());
	if (Ast::is_lvalue(e.this_expr())) {
		this_value = state.builder->CreateLoad(this_value);
	}
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
		}, arg);
		if (Ast::is_lvalue(arg)) {
			llvm_arg = state.builder->CreateLoad(llvm_arg);
		}
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

	return state.builder->CreateCall(func, llvm_args);
}

llvm::Value* CodegenState::visit(const Ast::FieldAccess& e, MethodCodegenState& state)
{
	auto* value = mpark::visit([this, &state](const auto& e) {
		return visit(e, state);
	}, e.expr());
	if (Ast::is_lvalue(e.expr())) {
		value = state.builder->CreateLoad(value);
	}

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

		auto* cluster_ptr_ref = state.builder->CreateStructGEP(
			as_pool->pool_type, llvm_pool, indices->cluster_idx + 2);
		auto* cluster_type = as_pool->cluster_types[indices->cluster_idx];
		auto* cluster_ptr = state.builder->CreateLoad(
			cluster_type->getPointerTo(), cluster_ptr_ref);

		auto* record_ptr = state.builder->CreateInBoundsGEP(
			cluster_type, cluster_ptr, value);
		auto* field_ptr = state.builder->CreateStructGEP(
			cluster_type, record_ptr, indices->pos);

		return field_ptr;
	}

	auto idx = new_spec.clazz().index_of(e.field());
	assert_msg(idx != (size_t)-1, "Field not belonging to class?");

	auto* field_ptr = state.builder->CreateStructGEP(as_obj->type, value, idx);
	return field_ptr;
}

llvm::Value* CodegenState::visit(const Ast::NewExpr& e, MethodCodegenState& state)
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

		return state.builder->CreateCall(as_pool->obj_ctor, {llvm_pool});
	}

	auto* as_obj = mpark::get_if<StandaloneSpecializationInfo>(&info.type_info);
	assert_msg(as_obj != nullptr, "Should be an object");

	return state.builder->CreateCall(as_obj->ctor);
}

bool CodegenState::ir(const Ast::Program& ast)
{
	CodegenState state;
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

	llvm::Module mod("shapes", m_ctx);
	mod.setDataLayout(m_target_machine->createDataLayout());
	m_mod = &mod;

	m_malloc_type = llvm::FunctionType::get(
		m_i8->getPointerTo(), {m_i64}, false);
	m_malloc = llvm::Function::Create(
		m_malloc_type,
		llvm::GlobalValue::ExternalLinkage,
		"malloc",
		m_mod);
	m_malloc->setReturnDoesNotAlias();
	m_malloc->addFnAttr(llvm::Attribute::NoUnwind);

	m_realloc_type = llvm::FunctionType::get(
		m_i8->getPointerTo(), {m_i8->getPointerTo(), m_i64}, false);
	m_realloc = llvm::Function::Create(
		m_realloc_type,
		llvm::GlobalValue::ExternalLinkage,
		"realloc",
		m_mod);
	m_realloc->setReturnDoesNotAlias();
	m_realloc->addFnAttr(llvm::Attribute::NoUnwind);

	m_free_type = llvm::FunctionType::get(
		m_void, {m_i8->getPointerTo()}, false);
	m_free = llvm::Function::Create(
		m_free_type,
		llvm::GlobalValue::ExternalLinkage,
		"free",
		m_mod);
	m_free->addFnAttr(llvm::Attribute::NoUnwind);

	for (const Ast::Class& e: ast.ordered_classes()) {
		generate_specializations(e);
	}

	generate_llvm_types();
	generate_llvm_function_decls();
	generate_llvm_functions();

	// llvm::verifyModule(*m_mod, &llvm::errs());

	llvm::FunctionAnalysisManager fam;
	llvm::LoopAnalysisManager lam;
	llvm::CGSCCAnalysisManager cam;
	llvm::ModuleAnalysisManager mam;

	llvm::PassBuilder pass_builder(m_target_machine);
	pass_builder.registerFunctionAnalyses(fam);
	pass_builder.registerLoopAnalyses(lam);
	pass_builder.registerCGSCCAnalyses(cam);
	pass_builder.registerModuleAnalyses(mam);

	pass_builder.crossRegisterProxies(lam, fam, cam, mam);
	auto pass_manager = pass_builder.buildModuleOptimizationPipeline(
		llvm::PassBuilder::O3, false);
	pass_manager.addPass(llvm::PrintModulePass(llvm::errs()));
	pass_manager.addPass(llvm::VerifierPass());

	pass_manager.run(*m_mod, mam);

	const char* filename = "shapes.o";
	std::error_code EC;
	llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);

	llvm::legacy::PassManager old_pm;

	m_target_machine->addPassesToEmitFile(old_pm, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile);
	old_pm.run(*m_mod);

	return true;
}

bool ir(const Ast::Program& ast)
{
	CodegenState state;
	return state.ir(ast);
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
