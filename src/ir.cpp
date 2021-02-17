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
	std::unordered_map<const Ast::Method*, llvm::Function*> funcs;
};

class CodegenState
{
	llvm::LLVMContext m_ctx;
	const llvm::Target* m_target = nullptr;
	llvm::TargetMachine* m_target_machine = nullptr;

	llvm::Module* m_mod = nullptr;

	llvm::Type* m_i8 = nullptr;
	llvm::Type* m_i16 = nullptr;
	llvm::Type* m_i32 = nullptr;
	llvm::Type* m_i64 = nullptr;

	llvm::Type* m_f32 = nullptr;
	llvm::Type* m_f64 = nullptr;

	std::unordered_map<ClassSpecialization, SpecializationInfo> m_specialization_info;

	void generate_specializations(const Ast::Class& clazz);

	void generate_specializations_impl(
		const Ast::Class& clazz, std::vector<const Ast::Layout*>& curr_layouts);

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

std::string create_ctor_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_shapes" << specialization << "_C";

	return os.str();
}

std::string create_alloc_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "_shapes" << specialization << "_A";

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

void CodegenState::generate_specializations(const Ast::Class& clazz)
{
	std::vector<const Ast::Layout*> curr_layouts;

	generate_specializations_impl(clazz, curr_layouts);
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

	m_i8 = llvm::Type::getInt8Ty(m_ctx);
	m_i16 = llvm::Type::getInt16Ty(m_ctx);
	m_i32 = llvm::Type::getInt32Ty(m_ctx);
	m_i64 = llvm::Type::getInt64Ty(m_ctx);

	m_f32 = llvm::Type::getFloatTy(m_ctx);
	m_f64 = llvm::Type::getDoubleTy(m_ctx);

	llvm::Module mod("shapes", m_ctx);
	mod.setDataLayout(m_target_machine->createDataLayout());

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

	}

#if 0
	auto* struct_type = llvm::StructType::create(
		ctx, {state.i32(), state.f32()}, "struct.Meme");
	auto* struct_ptr_type = struct_type->getPointerTo();

	auto* func_type = llvm::FunctionType::get(
		state.f32(), {struct_ptr_type, state.i64()}, false);
	auto* func = llvm::Function::Create(
		func_type, llvm::Function::ExternalLinkage, "hello", &mod);

	auto* bb = llvm::BasicBlock::Create(ctx, "entry", func);
	{
		llvm::IRBuilder<> builder(bb);
		auto it = func->arg_begin();
		auto* param0 = &*it++;
		auto* param1 = &*it++;

		auto* record_split_ptr = builder.CreateInBoundsGEP(struct_type, param0, param1);
		auto* field = builder.CreateStructGEP(struct_type, record_split_ptr, 1);

		auto* val = builder.CreateLoad(state.f32(), field);

		builder.CreateRet(val);
	}
#endif

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

	pass_manager.run(mod, mam);

	auto Filename = "shapes.o";
	std::error_code EC;
	llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

	llvm::legacy::PassManager old_pm;

	m_target_machine->addPassesToEmitFile(old_pm, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile);
	old_pm.run(mod);

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
