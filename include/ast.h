#pragma once

#include <string>
#include <vector>
#include <utility>

#include <cstddef>

struct Location {
	int first_line = 1, first_column = 1, last_line = 1, last_column = 1;
};

struct SyntaxError
{
	std::string message;
	Location loc;

	SyntaxError(std::string message, Location loc)
		: message(std::move(message))
		, loc(loc)
	{
	}
};

struct SemanticError
{
	// TODO: Fill this up for the sematic analysis section
};

struct ErrorList {
	std::vector<SyntaxError> syntax_errors;
	std::vector<SemanticError> semantic_errors;
};

struct Identifier
{
	std::string ident;
	Location loc;

	Identifier() = default;
	Identifier(std::string ident, Location loc)
		: ident(std::move(ident))
		, loc(loc)
	{
	}
};

struct CstClassType
{
	Identifier name;
	std::vector<Identifier> pool_parameters;
	bool is_bound = false;

	CstClassType() = default;
};

struct CstFormalPoolParameter
{
	Identifier ident;
	CstClassType bound;
};

struct CstClass
{
	Identifier name;
	std::vector<CstFormalPoolParameter> formal_pool_parameters;
};

struct CstRec
{
	std::vector<Identifier> fields;
};

struct CstLayout
{
	Identifier name;
	Identifier class_name;
	std::vector<CstRec> recs;
};

struct Cst
{
	std::vector<CstClass> classes;
	std::vector<CstLayout> layouts;
};
