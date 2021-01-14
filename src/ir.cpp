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

#include <unordered_map>

#include <cstdlib>

using namespace llvm;

class LLVMState
{
	LLVMContext m_ctx;
	const Target* m_target = nullptr;
	TargetMachine* m_target_machine = nullptr;

	Type* m_i8 = nullptr;
	Type* m_i16 = nullptr;
	Type* m_i32 = nullptr;
	Type* m_i64 = nullptr;

	Type* m_f32 = nullptr;
	Type* m_f64 = nullptr;

public:
	bool init();

	LLVMContext& ctx() { return m_ctx; }

	Type* i8()  { return m_i8;  }
	Type* i16() { return m_i16; }
	Type* i32() { return m_i32; }
	Type* i64() { return m_i64; }

	Type* f32() { return m_f32; }
	Type* f64() { return m_f64; }

	const Target* target() { return m_target; }
	TargetMachine* target_machine() { return m_target_machine; }
};

class BitcodeGenerator: public Ast::DefaultVisitor {
	void visit(const Ast::Class& e) override
	{
	}
};

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

	Module mod("hello", state.ctx());
	mod.setDataLayout(target_machine->createDataLayout());

	auto* struct_type = StructType::create(ctx, {state.i32(), state.f32()}, "struct.Meme");
	auto* struct_ptr_type = struct_type->getPointerTo();

	auto* func_type = FunctionType::get(state.f32(), {struct_ptr_type, state.i64()}, false);
	auto* func = Function::Create(func_type, Function::ExternalLinkage, "hello", &mod);

	auto* bb = BasicBlock::Create(ctx, "entry", func);
	{
		IRBuilder<> builder(bb);
		auto it = func->arg_begin();
		auto* param0 = &*it++;
		auto* param1 = &*it++;

		auto* record_split_ptr = builder.CreateInBoundsGEP(struct_type, param0, param1);
		auto* field = builder.CreateStructGEP(struct_type, record_split_ptr, 1);

		auto* val = builder.CreateLoad(state.f32(), field);

		builder.CreateRet(val);
	}

	FunctionAnalysisManager fam;
	LoopAnalysisManager lam;
	CGSCCAnalysisManager cam;
	ModuleAnalysisManager mam;

	PassBuilder pass_builder(target_machine);
	pass_builder.registerFunctionAnalyses(fam);
	pass_builder.registerLoopAnalyses(lam);
	pass_builder.registerCGSCCAnalyses(cam);
	pass_builder.registerModuleAnalyses(mam);

	pass_builder.crossRegisterProxies(lam, fam, cam, mam);
	auto pass_manager = pass_builder.buildModuleOptimizationPipeline(PassBuilder::O3, false);
	pass_manager.addPass(PrintModulePass(errs()));
	pass_manager.addPass(VerifierPass());

	pass_manager.run(mod, mam);

	auto Filename = "output.o";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

	legacy::PassManager old_pm;

	target_machine->addPassesToEmitFile(old_pm, dest, nullptr, TargetMachine::CGFT_ObjectFile);
	old_pm.run(mod);

	return true;
}

bool LLVMState::init()
{
	std::string error;
	m_target = TargetRegistry::lookupTarget("x86_64-unknown-linux-gnu", error);

	if (m_target == nullptr) {
		fprintf(stderr, "Error while creating LLVM target machine: %s", error.c_str());
		return false;
	}

	m_target_machine = m_target->createTargetMachine(
		"x86_64-unknown-linux-gnu",
		"generic",
		"",
		TargetOptions(),
		None,
		None,
		CodeGenOpt::Aggressive);

	m_i8 = Type::getInt8Ty(m_ctx);
	m_i16 = Type::getInt16Ty(m_ctx);
	m_i32 = Type::getInt32Ty(m_ctx);
	m_i64 = Type::getInt64Ty(m_ctx);

	m_f32 = Type::getFloatTy(m_ctx);
	m_f64 = Type::getDoubleTy(m_ctx);

	return true;
}
