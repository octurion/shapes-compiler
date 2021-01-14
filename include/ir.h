#pragma once

#include "ast.h"

void init_llvm();
bool ir(const Ast::Program& ast);
