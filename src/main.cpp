#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "cst_errors.h"

// #include "ast.h"
// #include "ast_errors.h"

#include <cerrno>
#include <cstdio>
#include <cstdlib>

#if 0
class SemanticErrorPrinter: public Ast::SemanticErrorVisitor
{
	void visit(const Ast::NotLvalue& e)                  override {}
	void visit(const Ast::MissingDefinition& e)          override {}
	void visit(const Ast::DuplicateDefinition& e)        override {}
	void visit(const Ast::UnexpectedTypeKind& e)         override {}
	void visit(const Ast::UnexpectedType& e)             override {}
	void visit(const Ast::MissingBound& e)               override {}
	void visit(const Ast::NoPoolParameters& e)           override {}
	void visit(const Ast::PoolParameterCountMismatch& e) override {}
	void visit(const Ast::FirstPoolParameterMismatch& e) override {}
	void visit(const Ast::ClassLayoutNameClash& e)       override {}
	void visit(const Ast::MissingFieldInLayout& e)       override {}
	void visit(const Ast::DuplicateFieldInLayout& e)     override {}
	void visit(const Ast::ClassPoolParameterNoNone& e)   override {}
	void visit(const Ast::ClassTypeMismatch& e)          override {}
	void visit(const Ast::PoolParameterMismatch& e)      override {}
	void visit(const Ast::NotInsideLoop& e)              override {}
	void visit(const Ast::NoReturnType& e)               override {}
	void visit(const Ast::ExpectedReturnType& e)         override {}
};
#endif

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

	if (syntax_errors.has_errors()) {
		for (auto it = syntax_errors.begin(); it != syntax_errors.end(); it++) {
			const auto& e = *it;
			const auto& loc = e.loc();
			fprintf(stderr, "[%d:%d-%d:%d]: %s\n",
					loc.first_line, loc.first_column,
					loc.last_line,  loc.last_column,
					e.msg().c_str());
		}

		fclose(in);
		return EXIT_SUCCESS;
	}

#if 0
	Ast::Program ast;
	Ast::SemanticErrorList errors;

	Ast::run_semantic_analysis(cst, &errors, &ast);

	if (errors.has_errors()) {
		// TODO: Print semantic errors
		fprintf(stderr, "We have semantic errors (TODO: Print them)\n");
		SemanticErrorPrinter printer;
		for (auto it = errors.begin(); it != errors.end(); it++) {
			(*it)->accept(printer);
		}

		fclose(in);
		return EXIT_SUCCESS;
	}

#endif
	fclose(in);
	return EXIT_SUCCESS;
}
