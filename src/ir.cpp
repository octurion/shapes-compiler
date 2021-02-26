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
	ClassSpecialization(
			const Ast::Class& clazz,
			std::vector<const Ast::Layout*> pool_param_types)
		: m_class(&clazz)
		, m_pool_param_types(std::move(pool_param_types))
	{}

	const Ast::Class& clazz() const { return *m_class; }
	const std::vector<const Ast::Layout*>& pool_param_types() const {
		return m_pool_param_types;
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

struct SpecializationInfo
{
	llvm::StructType* type = nullptr;
	llvm::Function* ctor = nullptr;
	llvm::Function* alloc = nullptr;

	std::unordered_map<const Ast::Method*, llvm::Function*> funcs;
};

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

	llvm::Function* m_realloc = nullptr;
	llvm::Function* m_malloc = nullptr;

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

std::string create_alloc_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_shapes" << specialization << "_A";

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
	return m_specialization_info[new_spec].type;
}

llvm::Type* CodegenState::type_of(const Ast::BoundType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	if (new_spec.pool_param_types().front() == nullptr) {
		return nullptr;
	}
	return m_specialization_info[new_spec].type;
}

void CodegenState::generate_specializations(const Ast::Class& clazz)
{
	std::vector<const Ast::Layout*> curr_layouts;

	generate_specializations_impl(clazz, curr_layouts);
}

llvm::Type* CodegenState::type_of(const Ast::NullptrType&, const ClassSpecialization&)
{
	return m_null;
}

llvm::Type* CodegenState::type_of(const Ast::VoidType&, const ClassSpecialization&)
{
	return m_void;
}

llvm::Type* CodegenState::type_of(const Ast::ObjectType& type, const ClassSpecialization& specialization)
{
	auto new_spec = specialization.specialize_type(type);
	if (new_spec.pool_param_types().front() == nullptr) {
		return m_specialization_info[new_spec].type->getPointerTo();
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

	auto* malloc_type = llvm::FunctionType::get(
		m_i8->getPointerTo(), {m_i64}, false);
	m_malloc = llvm::Function::Create(
		malloc_type,
		llvm::GlobalValue::ExternalLinkage,
		"malloc",
		m_mod);
	m_malloc->setReturnDoesNotAlias();

	auto* realloc_type = llvm::FunctionType::get(
		m_i8->getPointerTo(), {m_i8->getPointerTo(), m_i64}, false);
	m_realloc = llvm::Function::Create(
		realloc_type,
		llvm::GlobalValue::ExternalLinkage,
		"realloc",
		m_mod);
	m_realloc->setReturnDoesNotAlias();

	for (const Ast::Class& e: ast.ordered_classes()) {
		generate_specializations(e);
	}

	for (auto& e: m_specialization_info) {
		const auto& specialization = e.first;
		auto& info = e.second;

		const auto* first = specialization.pool_param_types().front();
		auto type_name = (first == nullptr)
			? create_class_name(specialization)
			: create_pool_name(specialization);

		info.type = llvm::StructType::create(m_ctx, type_name);
	}

	for (auto& e: m_specialization_info) {
		const auto& specialization = e.first;
		auto& info = e.second;

		const auto* layout = specialization.pool_param_types().front();
		if (layout != nullptr) {
			std::vector<llvm::Type*> pool_fields { m_i64, m_i64 };

			size_t count = 0;
			for (const auto& cluster: layout->clusters()) {
				std::vector<llvm::Type*> cluster_fields;
				for (const Ast::Field* e: cluster.fields()) {
					auto* type = mpark::visit(
						[this, &specialization](const auto& e) {
							return type_of(e, specialization);
					}, e->type());
					cluster_fields.push_back(type);
				}

				auto* cluster_type = llvm::StructType::create(
					m_ctx,
					cluster_fields,
					create_cluster_name(specialization, count++));
				pool_fields.push_back(cluster_type->getPointerTo());
			}
			info.type->setBody(pool_fields);
		} else {
			std::vector<llvm::Type*> fields;
			for (const Ast::Field& e: specialization.clazz().fields()) {
				auto* type = mpark::visit(
					[this, &specialization](const auto& e) {
						return type_of(e, specialization);
				}, e.type());
				fields.push_back(type);
			}
			info.type->setBody(fields);
		}
	}

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
			params.push_back(type_of(
					specialization.clazz().this_object_type(),
					specialization));
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
			{
				auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", func);
				llvm::IRBuilder<> builder(bb);

				if (method.name() == "getter") {
					if (specialization.pool_param_types().front() == nullptr) {
						auto* gep = builder.CreateStructGEP(func->args().begin(), 0);
						auto* load = builder.CreateLoad(gep);

						builder.CreateRet(load);
					} else {
						auto* gep = builder.CreateStructGEP(func->args().begin() + 1, 2);
						auto* cluster = builder.CreateLoad(gep);
						auto* cluster_gep = builder.CreateGEP(cluster, func->args().begin());
						auto* field_gep = builder.CreateStructGEP(cluster_gep, 0);

						auto* load = builder.CreateLoad(field_gep);

						builder.CreateRet(load);
					}
				} else {
					if (specialization.pool_param_types().front() == nullptr) {
						auto* gep = builder.CreateStructGEP(func->args().begin(), 0);
						builder.CreateStore(func->args().begin() + 1, gep);
					} else {
						auto* gep = builder.CreateStructGEP(func->args().begin() + 2, 2);
						auto* cluster = builder.CreateLoad(gep);
						auto* cluster_gep = builder.CreateGEP(cluster, func->args().begin());
						auto* field_gep = builder.CreateStructGEP(cluster_gep, 0);

						builder.CreateStore(func->args().begin() + 1, field_gep);
					}

					builder.CreateRetVoid();
				}
			}

			info.funcs[&method] = func;
		}

		{
			if (specialization.pool_param_types().front() == nullptr) {
				auto* alloc_func_type = llvm::FunctionType::get(info.type->getPointerTo(), {}, false);
				auto* alloc_func = llvm::Function::Create(
					alloc_func_type,
					llvm::GlobalValue::ExternalLinkage,
					create_alloc_name(specialization),
					m_mod);
				alloc_func->setReturnDoesNotAlias();
				{
					auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", alloc_func);
					llvm::IRBuilder<> builder(bb);
					auto* class_ptr_type = info.type->getPointerTo();

					auto* unused_ptr = builder.CreateConstGEP1_64(
						llvm::ConstantPointerNull::get(class_ptr_type), 1);
					auto* size = builder.CreateBitCast(unused_ptr, m_i64);
					auto* malloc = builder.CreateCall(m_malloc, {size});
					auto* retval = builder.CreatePointerCast(malloc, class_ptr_type);

					builder.CreateRet(retval);
				}

				info.alloc = alloc_func;

				auto* ctor_func_type = llvm::FunctionType::get(
					m_void, {info.type->getPointerTo()}, false);
				auto* ctor_func = llvm::Function::Create(
					ctor_func_type,
					llvm::GlobalValue::ExternalLinkage,
					create_ctor_name(specialization),
					m_mod);
				{
					auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", ctor_func);
					llvm::IRBuilder<> builder(bb);

					std::vector<llvm::Constant*> constants;

					const auto& fields = specialization.clazz().fields();
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
						const auto* object_type = mpark::get_if<Ast::ObjectType>(&type);
						assert_msg(object_type != nullptr, "Not an object or primitive type");
						auto new_spec = specialization.specialize_type(*object_type);
						if (new_spec.pool_param_types().front() == nullptr) {
							constants.push_back(
								llvm::ConstantPointerNull::get(
									m_specialization_info[new_spec].type->getPointerTo()));
						} else {
							constants.push_back(llvm::ConstantInt::get(m_i64, -1ull));
						}
					}

					builder.CreateStore(
						llvm::ConstantStruct::get(info.type, constants),
						ctor_func->arg_begin());

					builder.CreateRetVoid();
				}

				info.ctor = ctor_func;
			} else {
				auto* layout = specialization.pool_param_types().front();
				auto* alloc_func_type = llvm::FunctionType::get(m_i64, {info.type->getPointerTo()}, false);
				auto* alloc_func = llvm::Function::Create(
					alloc_func_type,
					llvm::GlobalValue::ExternalLinkage,
					create_alloc_name(specialization),
					m_mod);
				{
					auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", alloc_func);
					llvm::IRBuilder<> builder(bb);
					auto* size_ptr = builder.CreateStructGEP(
						alloc_func->args().begin(), 0, "size_ptr");
					auto* capacity_ptr = builder.CreateStructGEP(
						alloc_func->args().begin(), 1, "cap_ptr");
					auto* size = builder.CreateLoad(size_ptr, "size");
					auto* capacity = builder.CreateLoad(capacity_ptr, "cap");
					auto* eq = builder.CreateICmpEQ(size, capacity, "cap_full");

					auto* bb_call_realloc = llvm::BasicBlock::Create(m_ctx, "call_realloc", alloc_func);
					auto* bb_alloc = llvm::BasicBlock::Create(m_ctx, "alloc", alloc_func);

					builder.CreateCondBr(eq, bb_call_realloc, bb_alloc);

					llvm::IRBuilder<> builder_call_realloc(bb_call_realloc);
					auto* maybe_new_cap = builder_call_realloc.CreateShl(capacity, 1, "new_cap");
					auto* maybe_new_cap_zero = builder_call_realloc.CreateICmpNE(
						maybe_new_cap, llvm::ConstantInt::get(m_i64, 0));
					auto* new_cap = builder_call_realloc.CreateSelect(
						maybe_new_cap_zero,
						maybe_new_cap,
						llvm::ConstantInt::get(m_i64, 1));
					builder_call_realloc.CreateStore(new_cap, capacity_ptr);

					const auto& members = info.type->elements();
					for (size_t i = 2; i < members.size(); i++) {
						auto* e = members[i];
						auto* member_ptr = builder_call_realloc.CreateStructGEP(
							alloc_func->args().begin(), i);
						auto* old_ptr = builder_call_realloc.CreatePointerCast(
							builder_call_realloc.CreateLoad(member_ptr),
							m_i8->getPointerTo());

						auto* unused_ptr = builder_call_realloc.CreateConstGEP1_64(
							llvm::ConstantPointerNull::getNullValue(e), 1);
						auto* cluster_size = builder_call_realloc.CreateBitCast(unused_ptr, m_i64);
						auto* new_size = builder_call_realloc.CreateMul(new_cap, cluster_size, "realloc_size");
						auto* realloc = builder_call_realloc.CreateCall(
							m_realloc, {old_ptr, new_size}, "realloc_ptr");
						builder_call_realloc.CreateStore(
							builder_call_realloc.CreatePointerCast(realloc, e),
							member_ptr);
					}
					builder_call_realloc.CreateBr(bb_alloc);

					llvm::IRBuilder<> builder_alloc(bb_alloc);
					auto* new_idx = size;
					builder_alloc.CreateStore(
						builder_alloc.CreateAdd(size, llvm::ConstantInt::get(m_i64, 1)),
						builder_alloc.CreateStructGEP(alloc_func->args().begin(), 1));
					builder_alloc.CreateRet(new_idx);
				}

				info.alloc = alloc_func;

				auto* ctor_func_type = llvm::FunctionType::get(
					m_void, {m_i64, info.type->getPointerTo()}, false);
				auto* ctor_func = llvm::Function::Create(
					ctor_func_type,
					llvm::GlobalValue::ExternalLinkage,
					create_ctor_name(specialization),
					m_mod);

				{
					auto* bb = llvm::BasicBlock::Create(m_ctx, "entry", ctor_func);
					llvm::IRBuilder<> builder(bb);

					auto* idx = ctor_func->args().begin();
					auto* pool = ctor_func->args().begin() + 1;

					const auto& clusters = layout->clusters();
					for (size_t i = 0; i < clusters.size(); i++) {
						std::vector<llvm::Constant*> constants;
						const auto& fields = clusters[i].fields();
						for (size_t j = 0; j < fields.size(); j++) {
							const auto& e = *fields[i];
							const auto& type = e.type();
							const auto* primitive_type = mpark::get_if<Ast::PrimitiveType>(&type);
							if (primitive_type != nullptr) {
								constants.push_back(
									llvm::ConstantInt::get(
										type_of(*primitive_type, specialization), 0));
								continue;
							}
							const auto* object_type = mpark::get_if<Ast::ObjectType>(&type);
							assert_msg(object_type != nullptr, "Not an object or primitive type");
							auto new_spec = specialization.specialize_type(*object_type);
							if (new_spec.pool_param_types().front() == nullptr) {
								constants.push_back(
									llvm::ConstantPointerNull::get(
										m_specialization_info[new_spec].type->getPointerTo()));
							} else {
								constants.push_back(llvm::ConstantInt::get(m_i64, -1ull));
							}
						}
						auto* cluster = builder.CreateStructGEP(pool, i + 2);
						auto* ptr = builder.CreateLoad(cluster);
						auto* offset_ptr = builder.CreateGEP(ptr, idx);
						auto* const_val = llvm::ConstantStruct::get(
							(llvm::StructType*)
								(cluster->getType()->getPointerElementType()->getPointerElementType()),
							constants);
						builder.CreateStore(const_val, offset_ptr);
						builder.CreateRetVoid();
					}
				}
				info.ctor = ctor_func;
			}
		}
	}

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
