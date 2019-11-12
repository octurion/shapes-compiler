%{
#include "parser.tab.h"
#include "lexer.yy.h"
%}

%define api.pure full

%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner}

%define parse.error verbose

%start program

%token T_IDENT "identifier"

%token T_CLASS "class"
%token T_DEF   "def"

%token T_COMMA     ","
%token T_COLON     ":"
%token T_SEMICOLON ";"

%token T_LANGLE "<"
%token T_RANGLE ">"
%token T_LBRACE "{"
%token T_RBRACE "}"
%token T_LPAREN "("
%token T_RPAREN ")"

%token T_UNRECOGNIZED "unrecognized token"
%token T_END 0 "end of file"

%code requires {
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

void yyerror(yyscan_t state, const char* msg);
}

%%

program
	: global_declaration_list T_END {
	}

global_declaration_list
    : global_declaration global_declaration_list
    | global_declaration
    ;

global_declaration
    : class_declaration
    ;

class_declaration
	: T_CLASS T_IDENT class_pool_parameters T_LBRACE field_method_decl_list T_RBRACE {
	}

class_pool_parameters
	: T_LANGLE class_pool_parameter_list T_RANGLE {
	}
	| T_LANGLE T_RANGLE {
	}
	| %empty {
	}

class_pool_parameter_list
	: T_IDENT T_COLON pool_bound {
	}
	| class_pool_parameter_list T_COMMA T_IDENT pool_bound {
	}

pool_bound
	: T_IDENT T_LANGLE identifier_list T_RANGLE {
	}

identifier_list
	: T_IDENT {
	}
	|  identifier_list T_COMMA T_IDENT {
	}

type
	: %empty {
	}

field_method_decl_list
	: field_method_decl_list field_decl {
	}
	| field_method_decl_list method_decl {
	}
	| %empty {
	}

field_decl
	: T_IDENT T_COLON type T_SEMICOLON {
	}

method_decl
    : %empty
    ;
%%

void yyerror(yyscan_t state, const char* msg)
{
	(void) state;
	fprintf(stderr, "Parse error: %s\n", msg);
}
