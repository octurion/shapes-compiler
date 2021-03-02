#pragma once

#include "ast.h"

namespace Ir {

void init_llvm();
bool ir(const Ast::Program& ast);

class ClassSpecialization;

} // namespace Ir

namespace std {
template <>
struct hash<Ir::ClassSpecialization> {
	size_t operator()(const Ir::ClassSpecialization& specialization) const;
};
}
