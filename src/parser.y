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
	Identifier* ident;
	Cst* cst;
	CstClass* class_definition;
	std::vector<CstFormalPoolParameter>* formal_pool_parameters;
	CstFormalPoolParameter* formal_pool_parameter;
	CstClassType* class_type;
	std::vector<Identifier>* pool_parameters;
}

%destructor { delete $$; } <*>

%start program

%token<ident> T_IDENT "identifier"

%token T_CLASS  "class"
%token T_POOL   "pool"
%token T_LAYOUT "layout"
%token T_REC    "rec"

%token T_COMMA     ","
%token T_COLON     ":"
%token T_SEMICOLON ";"

%token T_LANGLE "<"
%token T_RANGLE ">"
%token T_LBRACE "{"
%token T_RBRACE "}"
%token T_LPAREN "("
%token T_RPAREN ")"
%token T_LSQUARE "["
%token T_RSQUARE "]"

%token T_UNRECOGNIZED "unrecognized token"
%token T_END 0 "end of file"

%type<cst> class_definitions
%type<class_definition> class_definition
%type<formal_pool_parameters> formal_pool_parameters formal_pool_parameter_list
%type<formal_pool_parameter> formal_pool_parameter
%type<class_type> pool_bound class_type
%type<pool_parameters> pool_parameters pool_parameter_list

%type<ident> identifier

%code requires {
#include <utility>

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
}

%%

program
	: class_definitions T_END { *cst = std::move(*$1); delete $1; }

class_definitions
	: %empty { $$ = new Cst; }
	| class_definitions class_definition {
		$1->classes.emplace_back(std::move(*$2));
		$$ = $1;

		delete $2;
	}
	;

class_definition
	: T_CLASS identifier formal_pool_parameters T_LBRACE T_RBRACE {
		$$ = new CstClass();
		$$->name = std::move(*$2);
		$$->formal_pool_parameters = std::move(*$3);

		delete $2;
		delete $3;
	}

formal_pool_parameters
	: %empty                  { $$ = new std::vector<CstFormalPoolParameter>; }
	| T_LANGLE T_RANGLE       { $$ = new std::vector<CstFormalPoolParameter>; }
	| T_LANGLE formal_pool_parameter_list T_RANGLE         { $$ = $2; }
	| T_LANGLE formal_pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	| T_LANGLE error T_RANGLE { $$ = new std::vector<CstFormalPoolParameter>; }

formal_pool_parameter_list
	: formal_pool_parameter {
		$$ = new std::vector<CstFormalPoolParameter>;
		$$->emplace_back(std::move(*$1));

		delete $1;
	}
	| formal_pool_parameter_list T_COMMA formal_pool_parameter {
		$$ = $1;
		$$->emplace_back(std::move(*$3));

		delete $3;
	}
formal_pool_parameter
	: identifier T_COLON pool_bound {
		$$ = new CstFormalPoolParameter;
		$$->ident = std::move(*$1);
		$$->bound = std::move(*$3);

		delete $1;
		delete $3;
	}

pool_bound
	: T_LSQUARE class_type T_RSQUARE { $$ = $2; $$->is_bound = true; }
	/* Just make up a class type for error recovery */
	| T_LSQUARE error T_RSQUARE      { $$ = new CstClassType; }

class_type
	: identifier pool_parameters {
		$$ = new CstClassType;
		$$->name = std::move(*$1);
		$$->pool_parameters = std::move(*$2);

		delete $1;
		delete $2;
	}

pool_parameters
	: %empty                   { $$ = new std::vector<Identifier>; }
	| T_LANGLE T_RANGLE        { $$ = new std::vector<Identifier>; }
	| T_LANGLE pool_parameter_list T_RANGLE         { $$ = $2; }
	| T_LANGLE pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	| T_LANGLE error T_RANGLE  { $$ = new std::vector<Identifier>; }

pool_parameter_list
	: identifier {
		$$ = new std::vector<Identifier>;
		$$->emplace_back(std::move(*$1));

		delete $1;
	}
	| pool_parameter_list T_COMMA identifier {
		$$ = $1;
		$$->emplace_back(std::move(*$3));

		delete $3;
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
