%{
%}

%locations
%define api.pure full

%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner} {Ast* ast}

%define parse.error verbose

%union {
	const std::string* ident;
}

%start program

%token<ident> T_IDENT "identifier"

%token T_CLASS
%token T_OF
%token T_DEF

%token T_COMMA ","

%token T_LANGLE "<"
%token T_RANGLE ">"
%token T_LBRACE "{"
%token T_RBRACE "}"
%token T_LPAREN "("
%token T_RPAREN ")"

%token T_UNRECOGNIZED "unrecognized token"
%token T_END 0 "end of file"

%{
#include "lexer.yy.h"

void yyerror(YYLTYPE* loc, yyscan_t state, Ast* ast, const char* msg);
static Location from_yyltype(YYLTYPE loc);
%}

%code requires {
#include "ast.h"

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
}

%%

program
	: class_decl_list T_END {
	}

class_decl_list
	: class_decl_list class_decl {
	}
	| %empty {
	}

class_decl
	: T_CLASS T_IDENT T_LANGLE parameter_bound_list T_RANGLE T_LBRACE T_RBRACE {
		ast->class_decls.emplace_back($2, from_yyltype(@$));
	}

parameter_bound_list
	: parameter_bound {
	}
	| parameter_bound_list T_COMMA parameter_bound {
	}

parameter_bound
	: T_IDENT pool_bound {
	}

pool_bound
	: T_OF T_IDENT T_LANGLE identifier_list T_RANGLE {
	}

identifier_list
	: T_IDENT {
	}
	|  identifier_list T_COMMA T_IDENT {
	}

%%

void yyerror(YYLTYPE* loc, yyscan_t state, Ast* ast, const char* msg)
{
	(void) loc;
	(void) state;
	(void) ast;

	fprintf(stderr, "Parse error (%d,%d - %d,%d): %s\n",
		loc->first_line, loc->first_column, loc->last_line, loc->last_column,
		msg);
}

static Location from_yyltype(YYLTYPE yylloc)
{
	Location loc;

	loc.first_line = yylloc.first_line;
	loc.first_column = yylloc.first_column;

	loc.last_line = yylloc.last_line;
	loc.last_column = yylloc.last_column;

	return loc;
}
