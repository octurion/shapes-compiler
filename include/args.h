#pragma once

#include <string>

struct CmdArgs
{
	std::string source_file;

	std::string header_file = "shapes.h";
	std::string obj_file = "shapes.o";

	std::string llvm_file = "shapes.ll";
};
