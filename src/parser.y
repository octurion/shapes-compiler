%{
#include "ast.h"
#include "parser.tab.h"
#include "lexer.yy.h"

extern void yyerror(
	YYLTYPE* loc,
	yyscan_t state,
	Cst* cst,
	ErrorList* errors,
	const char* msg);

static Location yyltype_to_location(YYLTYPE orig_loc);
%}

%define api.pure full
%locations

%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner} {Cst* cst} {ErrorList* errors}

%define parse.error verbose

%union {
	Cst*                                 cst;

	CstClass*                            class_definition;
	CstLayout*                           layout_definition;

	std::vector<CstFormalPoolParameter>* formal_pool_parameters;
	CstFormalPoolParameter*              formal_pool_parameter;
	CstClassBody*                        class_body;
	CstField*                            field_declaration;
	CstMethod*                           method_declaration;

	CstClassType*                        class_type;
	std::vector<Identifier>*             pool_parameters;

    std::vector<CstRec>*                 rec_list;
	CstRec*                              rec;

	Identifier*                          ident;
}

/* This will ensure that in the case of error recovery, the intermediate
 * products will be freed and no memory leaks will occur */
%destructor { delete $$; } <*>

/* Top-level goal */
%start program

/* List of all tokens (fed into lex) */

%token<ident> T_IDENT "identifier"

%token T_CLASS  "class"
%token T_POOL   "pool"
%token T_LAYOUT "layout"
%token T_REC    "rec"

%token T_LET    "let"
%token T_FN     "fn"

%token T_COMMA     ","
%token T_COLON     ":"
%token T_SEMICOLON ";"

%token T_LANGLE  "<"
%token T_RANGLE  ">"
%token T_LBRACE  "{"
%token T_RBRACE  "}"
%token T_LPAREN  "("
%token T_RPAREN  ")"
%token T_LSQUARE "["
%token T_RSQUARE "]"

%token T_PLUS "+"
%token T_EQ   "="

%token T_UNRECOGNIZED "unrecognized token"
%token T_END 0        "end of file"

%type<cst> program_definitions

%type<class_definition>  class_definition
%type<layout_definition> layout_definition

%type<formal_pool_parameters> formal_pool_parameters formal_pool_parameter_list
%type<formal_pool_parameter>  formal_pool_parameter
%type<class_body>             class_body class_members
%type<field_declaration>      field_declaration
%type<method_declaration>     method_declaration


%type<class_type>      pool_bound class_type
%type<pool_parameters> pool_parameters pool_parameter_list

%type<rec_list> recs rec_list
%type<rec> rec rec_field_list

%type<ident> identifier

%code requires {
#include <utility>

/* Ugly hack to allow both lex and bison to have yyscan_t as the scanner type */
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
}

%%

/*
 * How we do memory management: Unfortunately, Bison does not really let us use
 * C++11 goodies such as std::unique_ptr. This means that we have to manually
 * free whatever intermediate products are generated. To that extent, we use
 * the following pattern for memory management in the parser:
 *
 * ```
 * $$->foo = std::move(*$1); delete $1;
 * $$->bar = std::move(*$2); delete $2;
 * $$->baz = std::move(*$3); delete $3;
 * ```
 *
 * In the case of error recovery, the default `%destructor` we provided will
 * do all necessary cleanup.
 */

program
	: program_definitions T_END { *cst = std::move(*$1); delete $1; }

program_definitions
	: %empty { $$ = new Cst; }
	| program_definitions class_definition {
		$$ = $1;
		$$->classes.emplace_back(std::move(*$2)); delete $2;
	}
	| program_definitions layout_definition {
		$$ = $1;
		$$->layouts.emplace_back(std::move(*$2)); delete $2;
	}

class_definition
	: T_CLASS identifier formal_pool_parameters class_body {
		$$ = new CstClass();
		$$->name                   = std::move(*$2); delete $2;
		$$->formal_pool_parameters = std::move(*$3); delete $3;

		$$->fields  = std::move($4->fields);
		$$->methods = std::move($4->methods);
		delete $4;
	}

formal_pool_parameters
	: %empty                  { $$ = new std::vector<CstFormalPoolParameter>; }
	| T_LANGLE T_RANGLE       { $$ = new std::vector<CstFormalPoolParameter>; }
	| T_LANGLE formal_pool_parameter_list T_RANGLE         { $$ = $2; }
	| T_LANGLE formal_pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE { $$ = new std::vector<CstFormalPoolParameter>; }

formal_pool_parameter_list
	: formal_pool_parameter {
		$$ = new std::vector<CstFormalPoolParameter>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| formal_pool_parameter_list T_COMMA formal_pool_parameter {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}
formal_pool_parameter
	: identifier T_COLON pool_bound {
		$$ = new CstFormalPoolParameter;
		$$->ident = std::move(*$1); delete $1;
		$$->bound = std::move(*$3); delete $3;
	}

class_body
	: T_LBRACE T_RBRACE               { $$ = new CstClassBody; }
	| T_LBRACE class_members T_RBRACE { $$ = $2;               }
	| T_LBRACE error T_RBRACE         { $$ = new CstClassBody; }

class_members
	: field_declaration {
		$$ = new CstClassBody;
		$$->fields.emplace_back(std::move(*$1)); delete $1;
	}
	| method_declaration {
		$$ = new CstClassBody;
		$$->methods.emplace_back(std::move(*$1)); delete $1;
	}
	| class_members field_declaration {
		$$ = $1;
		$$->fields.emplace_back(std::move(*$2)); delete $2;
	}
	| class_members method_declaration {
		$$ = $1;
		$$->methods.emplace_back(std::move(*$2)); delete $2;
	}

field_declaration
	: T_LET T_LPAREN T_RPAREN T_SEMICOLON { $$ = new CstField; /* TODO */ }

method_declaration
	: T_FN T_LPAREN T_RPAREN T_LBRACE T_RBRACE { $$ = new CstMethod; /* TODO */ }

pool_bound
	: T_LSQUARE class_type T_RSQUARE { $$ = $2; $$->is_bound = true; }
	/* Just make up one for error recovery */
	| T_LSQUARE error T_RSQUARE      { $$ = new CstClassType; }

layout_definition
	: T_LAYOUT identifier T_COLON identifier T_EQ recs {
		$$ = new CstLayout;
		$$->name       = std::move(*$2); delete $2;
		$$->class_name = std::move(*$4); delete $4;
		$$->recs       = std::move(*$6); delete $6;
	}

recs
	: T_SEMICOLON          { $$ = new std::vector<CstRec>; }
	| rec_list T_SEMICOLON { $$ = $1;                      }
	/* Just make up one for error recovery */
	| error T_SEMICOLON    { $$ = new std::vector<CstRec>; }

rec_list
	: rec {
		$$ = new std::vector<CstRec>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| rec_list T_PLUS rec {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}

rec
	: T_REC T_LBRACE T_RBRACE                        { $$ = new CstRec; }
	| T_REC T_LBRACE rec_field_list T_RBRACE         { $$ = $3;         }
	| T_REC T_LBRACE rec_field_list T_COMMA T_RBRACE { $$ = $3;         }
	/* Just make up one for error recovery */
	| T_REC T_LBRACE error T_RBRACE                  { $$ = new CstRec; }

rec_field_list
	: identifier {
		$$ = new CstRec;
		$$->fields.emplace_back(std::move(*$1)); delete $1;
	}
	| rec_field_list T_COMMA identifier {
		$$ = $1;
		$$->fields.emplace_back(std::move(*$3)); delete $3;
	}

class_type
	: identifier pool_parameters {
		$$ = new CstClassType;
		$$->name            = std::move(*$1); delete $1;
		$$->pool_parameters = std::move(*$2); delete $2;
	}

pool_parameters
	: %empty                   { $$ = new std::vector<Identifier>; }
	| T_LANGLE T_RANGLE        { $$ = new std::vector<Identifier>; }
	| T_LANGLE pool_parameter_list T_RANGLE         { $$ = $2; }
	| T_LANGLE pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE  { $$ = new std::vector<Identifier>; }

pool_parameter_list
	: identifier {
		$$ = new std::vector<Identifier>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| pool_parameter_list T_COMMA identifier {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}

identifier
	: T_IDENT {
		$$ = $1;
		// Fix up the location that flex did not set up
		$$->loc = yyltype_to_location(@1);
	}
	/* These rules allow someone to use keywords as identifiers */
	| T_CLASS  { $$ = new Identifier("class",  yyltype_to_location(@1)); }
	| T_LAYOUT { $$ = new Identifier("layout", yyltype_to_location(@1)); }
	| T_REC    { $$ = new Identifier("rec",    yyltype_to_location(@1)); }
	| T_POOL   { $$ = new Identifier("pool",   yyltype_to_location(@1)); }
	| T_LET    { $$ = new Identifier("let",    yyltype_to_location(@1)); }
	| T_FN     { $$ = new Identifier("fn",     yyltype_to_location(@1)); }

%%

void yyerror(
	YYLTYPE* orig_loc,
	yyscan_t state,
	Cst* cst,
	ErrorList* errors,
	const char* msg)
{
	(void) state;
	(void) cst;

	Location loc;
	loc.first_line   = orig_loc->first_line;
	loc.first_column = orig_loc->first_column;
	loc.last_line    = orig_loc->last_line;
	loc.last_column  = orig_loc->last_column;

	errors->syntax_errors.emplace_back(std::string(msg), loc);
}

static Location yyltype_to_location(YYLTYPE orig_loc)
{
	Location loc;
	loc.first_line   = orig_loc.first_line;
	loc.first_column = orig_loc.first_column;
	loc.last_line    = orig_loc.last_line;
	loc.last_column  = orig_loc.last_column;

	return loc;
}
