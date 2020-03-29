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

#include <cstdlib>

using namespace llvm;

int ir()
{
	LLVMContext ctx;

	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmParser();
	LLVMInitializeX86AsmPrinter();

	std::string error;
	const auto* target = TargetRegistry::lookupTarget("x86_64-unknown-linux-gnu", error);

	auto target_machine = target->createTargetMachine(
		"x86_64-unknown-linux-gnu",
		"generic",
		"",
		TargetOptions(),
		None,
		None,
		CodeGenOpt::Aggressive);

	Module mod("hello", ctx);
	mod.setDataLayout(target_machine->createDataLayout());

	auto* int_type = Type::getInt32Ty(ctx);
	auto* idx_type = Type::getInt64Ty(ctx);
	auto* float_type = Type::getFloatTy(ctx);

	auto* struct_type = StructType::create(ctx, {int_type, float_type}, "struct.Meme");
	auto* struct_ptr_type = struct_type->getPointerTo();

	auto* func_type = FunctionType::get(float_type, {struct_ptr_type, idx_type}, false);
	auto* func = Function::Create(func_type, Function::ExternalLinkage, "hello", &mod);

	auto* bb = BasicBlock::Create(ctx, "entry", func);
	{
		IRBuilder<> builder(bb);
		auto it = func->arg_begin();
		auto* param0 = &*it++;
		auto* param1 = &*it++;

		auto* record_split_ptr = builder.CreateInBoundsGEP(struct_type, param0, param1);
		auto* field = builder.CreateStructGEP(struct_type, record_split_ptr, 1);

		auto* val = builder.CreateLoad(float_type, field);

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

	return EXIT_SUCCESS;
}
