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

class LLVMState
{
	llvm::LLVMContext m_ctx;
	const llvm::Target* m_target = nullptr;
	llvm::TargetMachine* m_target_machine = nullptr;

	llvm::Type* m_i8 = nullptr;
	llvm::Type* m_i16 = nullptr;
	llvm::Type* m_i32 = nullptr;
	llvm::Type* m_i64 = nullptr;

	llvm::Type* m_f32 = nullptr;
	llvm::Type* m_f64 = nullptr;

public:
	bool init();

	llvm::LLVMContext& ctx() { return m_ctx; }

	llvm::Type* i8()  { return m_i8;  }
	llvm::Type* i16() { return m_i16; }
	llvm::Type* i32() { return m_i32; }
	llvm::Type* i64() { return m_i64; }

	llvm::Type* f32() { return m_f32; }
	llvm::Type* f64() { return m_f64; }

	const llvm::Target* target() { return m_target; }
	llvm::TargetMachine* target_machine() { return m_target_machine; }
};

#if 0
using PoolType = mpark::variant<const Ast::Layout*, Ast::None>;

class ClassSpecialization
{
	const Ast::Class* m_class = nullptr;
	std::vector<PoolType> m_pool_param_types;

public:
	ClassSpecialization(
			const Ast::Class& clazz, std::vector<PoolType> pool_param_types)
		: m_class(&clazz)
		, m_pool_param_types(std::move(pool_param_types))
	{}

	const Ast::Class& clazz() const { return *m_class; }
	const std::vector<PoolType>& pool_param_types() const {
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

	friend std::ostream& operator<<(
		std::ostream& os, const ClassSpecialization& specialization);
};

struct FieldPos {
	size_t standalone_pos = -1;

	size_t cluster = -1;
	size_t clustered_pos = -1;

	static FieldPos standalone(size_t pos) {
		FieldPos retval;
		retval.standalone_pos = pos;

		return retval;
	}

	static FieldPos clustered(size_t cluster, size_t pos) {
		FieldPos retval;
		retval.cluster = cluster;
		retval.clustered_pos = pos;

		return retval;
	}
};

struct SpecializationDataTypes
{
	llvm::StructType* m_class_layout = nullptr;
	llvm::StructType* m_pool_layout = nullptr;
	std::vector<llvm::StructType*> m_clusters;

	std::unordered_map<const Ast::Field*, FieldPos> m_fields_pos;

	llvm::Function* m_ctor = nullptr;
	std::unordered_map<const Ast::Method*, llvm::Function*> m_methods;
};

struct IR
{
	LLVMState state;
	std::unordered_map<ClassSpecialization, SpecializationDataTypes> map;
};

std::ostream& operator<<(
	std::ostream& os, const ClassSpecialization& specialization)
{
	for (const auto& e: specialization.pool_param_types()) {
		os << "_";
		const auto* layout = mpark::get_if<const Ast::Layout*>(&e);
		if (layout != nullptr) {
			os << (*layout)->name().length();
			os << (*layout)->name();
		} else {
			os << "none";
		}
	}

	return os;
}

std::string create_method_name(
	const ClassSpecialization& specialization, const Ast::Method& method)
{
	std::ostringstream os;
	os << "shapes" << specialization
		<< "_" << method.name().length() << method.name();

	return os.str();
}

std::string create_ctor_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "shapes" << specialization << "_new";

	return os.str();
}

std::string create_pool_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "struct.pool" << specialization;

	return os.str();
}

std::string create_cluster_name(const ClassSpecialization& specialization, size_t idx)
{
	std::ostringstream os;
	os << "struct.cluster" << idx << specialization;

	return os.str();
}

std::string create_class_name(const ClassSpecialization& specialization)
{
	std::ostringstream os;
	os << "struct.standalone" << specialization;

	return os.str();
}

void create_specialization(IR& ir, ClassSpecialization specialization)
{
	auto& data_types = ir.map[specialization];
	const auto& maybe_layout = specialization.pool_param_types().front();
	if (mpark::holds_alternative<Ast::None>(maybe_layout)) {
		data_types.m_class_layout = llvm::StructType::create(
			ir.state.ctx(), create_class_name(specialization));

		const auto& fields = specialization.clazz().fields();
		for (size_t i = 0; i < fields.size(); i++) {
			data_types.m_fields_pos.emplace(fields[i], FieldPos::standalone(i));
		}
	} else {
		data_types.m_pool_layout = llvm::StructType::create(
			ir.state.ctx(), create_pool_name(specialization));

		const auto* layout = mpark::get_if<const Ast::LayoutType*>(&maybe_layout);
		const auto& clusters = (*layout)->layout().clusters();
		for (size_t i = 0; i < clusters.size(); i++) {
			data_types.m_clusters.push_back(llvm::StructType::create(
				ir.state.ctx(), create_cluster_name(specialization, i)));

			const auto& fields = clusters[i].fields();
			for (size_t j = 0; j < fields.size(); j++) {
				data_types.m_fields_pos.emplace(
					fields[i], FieldPos::clustered(i, j));
			}
		}
	}
}

void generate_specializations_impl(
		IR& ir, const Ast::Class& clazz, std::vector<PoolType>& types) {
	if (types.size() == clazz.num_pools()) {
		create_specialization(ir, ClassSpecialization(clazz, types));
		return;
	}

	types.emplace_back(Ast::None());
	generate_specializations_impl(ir, clazz, types);
	const Ast::Pool& param = clazz.pools()[types.size()];
	const auto* type = mpark::get_if<Ast::BoundType>(&param.type());
	assert_msg(type != nullptr, "Formal pool parameter is not a bound?");

	for (const auto& layout: type->of_class().layouts()) {
		types.back() = &layout.get();
		generate_specializations_impl(ir, clazz, types);
	}

	types.pop_back();
}

void generate_specializations(IR& ir, const Ast::Program& ast)
{
	for (const auto& e: ast.classes()) {
		const auto& clazz = e.second;
	}
}
#endif

void init_llvm()
{
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmParser();
	LLVMInitializeX86AsmPrinter();
};

bool ir(const Ast::Program& ast)
{
	LLVMState state;
	if (!state.init()) {
		return false;
	}

	auto& ctx = state.ctx();
	auto* target_machine = state.target_machine();

	llvm::Module mod("mod_shapes", state.ctx());
	mod.setDataLayout(target_machine->createDataLayout());

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

	llvm::FunctionAnalysisManager fam;
	llvm::LoopAnalysisManager lam;
	llvm::CGSCCAnalysisManager cam;
	llvm::ModuleAnalysisManager mam;

	llvm::PassBuilder pass_builder(target_machine);
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

	auto Filename = "output.o";
	std::error_code EC;
	llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

	llvm::legacy::PassManager old_pm;

	target_machine->addPassesToEmitFile(old_pm, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile);
	old_pm.run(mod);

	return true;
}

bool LLVMState::init()
{
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

	return true;
}

} // namespace Ir
