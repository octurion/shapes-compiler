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

	Ast ast;

	FILE* in = fopen(argv[1], "r");
	if (in == NULL) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}
	
	{
		yyscan_t scanner;
		int err = yylex_init_extra(&ast, &scanner);
		if (err != 0) {
			fclose(in);
			return EXIT_FAILURE;
		}

		yyset_in(in, scanner);

		yyparse(scanner, &ast);

		yylex_destroy(scanner);
	}

	fclose(in);
	return EXIT_SUCCESS;
}
