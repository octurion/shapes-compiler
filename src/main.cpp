#include "ast.h"

#include "parser.tab.h"
#include "lexer.yy.h"

#include <cerrno>
#include <cstdio>

int main(int argc, char** argv)
{
	if (argc < 2) {
		fprintf(stderr, "Need an input file\n");
		return EXIT_FAILURE;
	}

	FILE* in = fopen(argv[1], "r");
	if (in == NULL) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}

	Ast ast;
	ErrorList errors;
	
	yyscan_t scanner;
	yylex_init(&scanner);
	yyset_in(in, scanner);

	yyparse(scanner, &ast, &errors);

	yylex_destroy(scanner);

	if (!errors.syntax_errors.empty()) {
		for (const auto& e: errors.syntax_errors) {
			const auto& loc = e.loc;
			fprintf(stderr, "Parse error (%d,%d - %d,%d): %s\n",
					loc.first_line, loc.first_column,
					loc.last_line,  loc.last_column,
					e.message.c_str());
		}
	}

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

	fclose(in);
	return EXIT_SUCCESS;
}
