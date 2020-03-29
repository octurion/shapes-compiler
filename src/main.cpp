#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "ast.h"

#include <cerrno>
#include <cstdio>
#include <cstdlib>

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
			fprintf(stderr, "Parse error (%d,%d-%d,%d): %s\n",
					loc.first_line, loc.first_column,
					loc.last_line,  loc.last_column,
					e.msg().c_str());
		}
	}

#if 0
	run_semantic_analysis(ast, errors);

	if (!errors.semantic_errors.empty()) {
		for (const auto& e: errors.semantic_errors) {
			const auto& loc = e.loc;
			fprintf(stderr, "Semantic error (%d,%d - %d,%d): %s\n",
					loc.first_line, loc.first_column,
					loc.last_line,  loc.last_column,
					e.message.c_str());
		}
	}
#endif

	fclose(in);
	return EXIT_SUCCESS;
}
