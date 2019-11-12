#pragma once

#include <string>
#include <unordered_set>
#include <vector>

#include <cstddef>

struct Location {
	size_t first_line = 1, first_column = 1, last_line = 1, last_column = 1;
};

struct FormalPoolParameter
{
	std::string name;
};

struct ClassDecl {
	std::string identifier;
	std::vector<FormalPoolParameter> parameters;

	std::vector<void*> fields;
	std::vector<void*> methods;

	Location loc;

	ClassDecl(std::string&& identifier, Location loc)
		: identifier(identifier)
		, loc(loc)
	{}
};

struct Cst {
	std::vector<ClassDecl> class_decls;
};
