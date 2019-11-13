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
	Cst*                          cst;

	CstClass*                     class_definition;
	CstLayout*                    layout_definition;

	CstClassBody*                 class_body;
	CstMethod*                    method_declaration;

	CstClassType*                 class_type;

	std::vector<CstVariableDecl>* variable_declarations;
	CstVariableDecl*              variable_declaration;

    std::vector<CstCluster>*      clusters;
	CstCluster*                   cluster;

	std::vector<Identifier>*      identifiers;
	Identifier*                   ident;
}

/* This will ensure that in the case of error recovery, the intermediate
 * products will be freed and no memory leaks will occur */
%destructor { delete $$; } <*>

/* Top-level goal */
%start program

/* List of all tokens (fed into lex) */

%token<ident> T_IDENT "identifier"

%token T_CLASS  "class"
%token T_WHERE  "where"
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

%type<variable_declarations> variable_declarations
                             variable_declaration_list
                             pool_bounds
                             field_declarations
                             method_arguments
%type<variable_declaration>  variable_declaration
%type<identifiers>           identifiers
                             identifier_list
                             pool_parameters
%type<class_body>            class_body
                             class_members
%type<method_declaration>    method_declaration

%type<class_type>      pool_bound class_type type

%type<clusters> clusters cluster_list
%type<cluster>  cluster

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
	: T_CLASS identifier pool_parameters pool_bounds class_body {
		$$ = new CstClass();
		$$->name                   = std::move(*$2); delete $2;
		$$->pool_parameters        = std::move(*$3); delete $3;
		$$->pool_bounds            = std::move(*$4); delete $4;

		$$->fields  = std::move($5->fields);
		$$->methods = std::move($5->methods);
		delete $5;
	}

pool_bounds
	: %empty                        { $$ = new std::vector<CstVariableDecl>; }
	| T_WHERE variable_declarations { $$ = $2; }

variable_declarations
	: %empty { $$ = new std::vector<CstVariableDecl>; }
	| variable_declaration_list         { $$ = $1; }
	| variable_declaration_list T_COMMA { $$ = $1; }

variable_declaration_list
	: variable_declaration {
		$$ = new std::vector<CstVariableDecl>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| variable_declaration_list T_COMMA variable_declaration {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}

variable_declaration
	: identifier T_COLON type {
		$$ = new CstVariableDecl;
		$$->name = std::move(*$1); delete $1;
		$$->type = std::move(*$3); delete $3;
	}

type
	: class_type { $$ = $1; }
	| pool_bound { $$ = $1; }

class_type
	: identifier pool_parameters {
		$$ = new CstClassType;
		$$->name            = std::move(*$1); delete $1;
		$$->pool_parameters = std::move(*$2); delete $2;
	}

pool_bound
	: T_LSQUARE class_type T_RSQUARE { $$ = $2; $$->is_bound = true; }
	/* Just make up one for error recovery */
	| T_LSQUARE error T_RSQUARE      { $$ = new CstClassType; }

class_body
	: T_LBRACE T_RBRACE               { $$ = new CstClassBody; }
	| T_LBRACE class_members T_RBRACE { $$ = $2;               }
	| T_LBRACE error T_RBRACE         { $$ = new CstClassBody; }

class_members
	: field_declarations {
		$$ = new CstClassBody;
		for (auto& e: *$1) {
			$$->fields.emplace_back(std::move(e));
		}
		delete $1;
	}
	| method_declaration {
		$$ = new CstClassBody;
		$$->methods.emplace_back(std::move(*$1)); delete $1;
	}
	| class_members field_declarations {
		$$ = $1;
		for (auto& e: *$2) {
			$$->fields.emplace_back(std::move(e));
		}
		delete $2;
	}
	| class_members method_declaration {
		$$ = $1;
		$$->methods.emplace_back(std::move(*$2)); delete $2;
	}

pool_parameters
	: %empty                        { $$ = new std::vector<Identifier>; }
	| T_LANGLE identifiers T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE       { $$ = new std::vector<Identifier>; }

field_declarations
	: variable_declarations T_SEMICOLON { $$ = $1; }

method_declaration
	: T_FN T_IDENT method_arguments T_LBRACE T_RBRACE {
		$$ = new CstMethod; /* TODO */
		$$->name      = std::move(*$2); delete $2;
		$$->arguments = std::move(*$3); delete $3;
	}

method_arguments
	: T_LPAREN variable_declarations T_RPAREN { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LPAREN error T_RPAREN { $$ = new std::vector<CstVariableDecl>; }

layout_definition
	: T_LAYOUT identifier T_COLON identifier T_EQ clusters {
		$$ = new CstLayout;
		$$->name       = std::move(*$2); delete $2;
		$$->class_name = std::move(*$4); delete $4;
		$$->clusters   = std::move(*$6); delete $6;
	}

clusters
	: T_SEMICOLON              { $$ = new std::vector<CstCluster>; }
	| cluster_list T_SEMICOLON { $$ = $1;                          }
	/* Just make up one for error recovery */
	| error T_SEMICOLON        { $$ = new std::vector<CstCluster>; }

cluster_list
	: cluster {
		$$ = new std::vector<CstCluster>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| cluster_list T_PLUS cluster {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}

cluster
	: T_REC T_LBRACE identifiers T_RBRACE {
		$$ = new CstCluster;
		$$->fields = std::move(*$3); delete $3;
	}
	/* Just make up one for error recovery */
	| T_REC T_LBRACE error T_RBRACE { $$ = new CstCluster; }

identifiers
	: %empty                  { $$ = new std::vector<Identifier>; }
	| identifier_list         { $$ = $1;                          }
	| identifier_list T_COMMA { $$ = $1; }

identifier_list
	: identifier {
		$$ = new std::vector<Identifier>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| identifier_list T_COMMA identifier {
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
	| T_WHERE  { $$ = new Identifier("where",  yyltype_to_location(@1)); }
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
