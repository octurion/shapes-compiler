#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "syntactic_analysis.h"

#include "ast.h"
#include "semantic_analysis.h"

#include "ir.h"

#include <cerrno>
#include <cstdio>
#include <cstdlib>

void print_loc(const Location& loc)
{
	if (loc.first_line == loc.last_line) {
		fprintf(stderr, "[%d:%d,%d]",
			loc.first_line,
			loc.first_column,
			loc.last_column
		);
	} else {
		fprintf(stderr, "[%d:%d,%d:%d]",
			loc.first_line,
			loc.first_column,
			loc.last_line,
			loc.last_column
		);
	}
}

const char* kind_str(Ast::ErrorKind kind)
{
	switch (kind) {
	case Ast::ErrorKind::CLASS:
		return "Class";

	case Ast::ErrorKind::FIELD:
		return "Field";

	case Ast::ErrorKind::POOL:
		return "Pool";

	case Ast::ErrorKind::POOL_BOUND:
		return "Pool bound";

	case Ast::ErrorKind::VARIABLE:
		return "Variable";

	case Ast::ErrorKind::TYPE:
		return "Type";

	case Ast::ErrorKind::LAYOUT:
		return "Layout";

	case Ast::ErrorKind::METHOD:
		return "Method";
	}
}

struct SemanticErrorPrinter
{
	void operator()(const Ast::DuplicateDefinition& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": %s '%s' is already defined in ",
			kind_str(e.kind()), e.name().c_str()
		);
		print_loc(e.existing_loc());
		fprintf(stderr, ".");
	}

	void operator()(const Ast::MissingDefinition& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": %s '%s' has not been defined.",
			kind_str(e.kind()), e.name().c_str()
		);
	}

	void operator()(const Ast::MissingBound& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": No bound has been defined for pool '%s'.",
			e.pool_name().c_str()
		);
	}

	void operator()(const Ast::LayoutMissingField& e)
	{
		print_loc(e.layout_loc());
		fprintf(stderr, ": Missing field '%s' in layout '%s'.",
			e.field_name().c_str(), e.layout_name().c_str()
		);
	}

	void operator()(const Ast::LayoutNameClash& e)
	{
		print_loc(e.layout_loc());
		fprintf(stderr, ": Layout name '%s' clashes with class of the same name defined in ",
			e.layout_name().c_str()
		);
		print_loc(e.class_loc());
	}

	void operator()(const Ast::LayoutDuplicateField& e)
	{
		print_loc(e.field_loc());
		fprintf(stderr, ": Field '%s' is layout '%s' is already defined in ",
			e.field_name().c_str(), e.layout_name().c_str()
		);
		print_loc(e.existing_field_loc());
	}

	void operator()(const Ast::NoPoolParameters& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": No pool parameters defined for class '%s'.",
			e.name().c_str()
		);
	}

	void operator()(const Ast::EmptyCluster& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Cluster is empty.");
	}

	void operator()(const Ast::NotInsideLoop& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Statement is not inside loop.");
	}

	void operator()(const Ast::IntegerOutOfBounds& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Integer constant can't fit into a 64-bit unsigned integer.");
	}

	void operator()(const Ast::IncorrectFirstPoolParameter& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected pool parameter '%s', but got '%s'.",
			e.expected().c_str(), e.got().c_str()
		);
	}

	void operator()(const Ast::IncorrectType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected type '%s', but got '%s'.",
			e.expected_type().c_str(), e.got_type().c_str()
		);
	}

	void operator()(const Ast::IncompatibleBound& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Type '%s' is not compatible with bound '%s'.",
			e.type().c_str(), e.bound().c_str()
		);
	}

	void operator()(const Ast::IncorrectPoolsNumber& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected %zu pool parameters, but got %zu.",
			e.num_expected(), e.num_got()
		);
	}

	void operator()(const Ast::ExpectedObjectType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected an object type, but got '%s'.",
			e.type_got().c_str()
		);
	}

	void operator()(const Ast::ExpectedPrimitiveType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected a primitive type, but got '%s'.",
			e.type_got().c_str()
		);
	}

	void operator()(const Ast::ExpectedBooleanType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected a boolean type, but got '%s'.",
			e.type_got().c_str()
		);
	}

	void operator()(const Ast::ExpectedIntegerType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected an integer type, but got '%s'.",
			e.type_got().c_str()
		);
	}

	void operator()(const Ast::ExpectedNumericType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected an integer or floating-point type, but got '%s'.",
			e.type_got().c_str()
		);
	}

	void operator()(const Ast::ReturnWithExpression& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Return statement with an expression.");
	}

	void operator()(const Ast::ReturnWithoutExpression& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Return statement without an expression.");
	}

	void operator()(const Ast::NonAssignableType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Cannot assign expression of type '%s' into type '%s'.",
			e.assigned_from().c_str(), e.assigned_to().c_str()
		);
	}

	void operator()(const Ast::NotLvalue& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expression is not an lvalue.");
	}

	void operator()(const Ast::IncorrectArgsNumber& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Expected %zu method arguments, but got %zu.",
			e.num_expected(), e.num_got()
		);
	}

	void operator()(const Ast::NotAllPathsReturn& e)
	{
		print_loc(e.loc());
		fprintf(stderr, ": Not all paths return a value in method '%s'.",
			e.name().c_str()
		);
	}
};

int main(int argc, char** argv)
{
	if (argc < 2) {
		fprintf(stderr, "You must specify an input file\n");
		return EXIT_FAILURE;
	}

	FILE* in = fopen(argv[1], "r");
	if (in == NULL) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}

	Cst::Program cst;
	Cst::SyntaxErrorList syntax_errors;

	yyscan_t scanner;
	yylex_init(&scanner);
	yyset_in(in, scanner);

	yyparse(scanner, &cst, &syntax_errors);

	yylex_destroy(scanner);

	fclose(in);

	if (syntax_errors.has_errors()) {
		for (auto it = syntax_errors.begin(); it != syntax_errors.end(); it++) {
			const auto& e = *it;
			const auto& loc = e.loc();
			fprintf(stderr, "[%d:%d-%d:%d]: %s\n",
					loc.first_line, loc.first_column,
					loc.last_line,  loc.last_column,
					e.msg().c_str());
		}

		return EXIT_FAILURE;
	}

	Ast::Program ast;
	Ast::SemanticErrorList errors;

	Ast::run_semantic_analysis(cst, ast, errors);

	if (errors.has_errors()) {
		SemanticErrorPrinter printer;
		for (const auto& e: errors.errors()) {
			mpark::visit(printer, e);
			fprintf(stderr, "\n");
		}

		return EXIT_FAILURE;
	}

	Ir::init_llvm();
	Ir::Codegen codegen;
	codegen.ir(ast);
	codegen.emit("shapes.o");

	return EXIT_SUCCESS;
}
