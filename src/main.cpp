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
	
	{
		yyscan_t scanner;
#if 0
		int err = yylex_init_extra(&ast, &scanner);
		if (err != 0) {
			fclose(in);
			return EXIT_FAILURE;
		}
#endif
		yylex_init(&scanner);
		yyset_in(in, scanner);

		yyparse(scanner);

		yylex_destroy(scanner);
	}

	fclose(in);
	return EXIT_SUCCESS;
}
