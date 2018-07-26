#include <llvm/ADT/None.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include <cstdlib>

using namespace llvm;

int ir()
{
	LLVMContext ctx;

	Module mod("hello", ctx);

	auto* void_type = Type::getVoidTy(ctx);
	auto* func_type = FunctionType::get(void_type, None, false);
	auto* func = Function::Create(func_type, Function::ExternalLinkage, "hello", &mod);

	auto* bb = BasicBlock::Create(ctx, "entry", func);
	{
		IRBuilder<> builder(bb);
		builder.CreateRetVoid();
	}

	mod.dump();

	return EXIT_SUCCESS;
}
