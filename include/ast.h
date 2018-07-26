#pragma once

#include <string>
#include <unordered_set>
#include <vector>

#include <cstddef>

struct Location {
	size_t first_line = 1, first_column = 1, last_line = 1, last_column = 1;
};

struct ClassDecl {
	const std::string* identifier;
	Location loc;

	ClassDecl(const std::string* identifier, Location loc)
		: identifier(identifier)
		, loc(loc)
	{}
};

struct Ast {
	std::vector<ClassDecl> class_decls;
	std::unordered_set<std::string> identifiers;
};
