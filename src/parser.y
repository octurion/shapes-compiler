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
	Cst* cst;

	Class*  class_definition;
	Layout* layout_definition;

	TmpClassBody* class_body;
	Method*       method_declaration;

	Type* type;

	Stmt*      stmt;
	BlockStmt* stmt_list;

	Expr* expr;

	BinOp bin_op;

	std::vector<VariableDecl>* variable_declarations;
	VariableDecl*              variable_declaration;

	std::vector<Cluster>* clusters;
	Cluster*              cluster;

	std::vector<Identifier>* identifiers;
	Identifier*              ident;
	Number*                  num;
}

/* This will ensure that in the case of error recovery, the intermediate
 * products will be freed and no memory leaks will occur */
%destructor { delete $$; } <*>
%destructor { } <bin_op>

/* Top-level goal */
%start program

/* List of all tokens (fed into lex) */

%token<ident> T_IDENT "identifier"
%token<num>   T_NUM   "number"

%token T_CLASS  "class"
%token T_EXPORT "export"
%token T_WHERE  "where"
%token T_POOL   "pool"
%token T_POOLS  "pools"
%token T_LAYOUT "layout"
%token T_REC    "rec"

%token T_LET    "let"
%token T_FN     "fn"

%token T_IF       "if"
%token T_ELSE     "else"
%token T_FOR      "for"
%token T_FOREACH  "foreach"
%token T_SIMD     "simd"
%token T_WHILE    "while"
%token T_BREAK    "break"
%token T_CONTINUE "continue"
%token T_RETURN   "return"

%token T_NEW  "new"
%token T_NULL "null"
%token T_THIS "this"
%token T_AS   "as"
%token T_NONE "none"

%token T_UNIFORM "uniform"
%token T_VARYING "varying"

%token T_VOID "void"
%token T_BOOL "bool"
%token T_I8   "i8"
%token T_U8   "u8"
%token T_I16  "i16"
%token T_U16  "u16"
%token T_I32  "i32"
%token T_U32  "u32"
%token T_I64  "i64"
%token T_U64  "u64"

%token T_F32 "f32"
%token T_F64 "f64"

%token T_COMMA     ","
%token T_COLON     ":"
%token T_SEMICOLON ";"
%token T_DOT       "."

%token T_DOTDOT    ".."

%token T_LANGLE  "<"
%token T_RANGLE  ">"
%token T_LBRACE  "{"
%token T_RBRACE  "}"
%token T_LPAREN  "("
%token T_RPAREN  ")"
%token T_LSQUARE "["
%token T_RSQUARE "]"

%token T_PLUS_ASSIGN  "+="
%token T_MINUS_ASSIGN "-="
%token T_TIMES_ASSIGN "*="
%token T_DIV_ASSIGN   "/="
%token T_SHL_ASSIGN   "<<="
%token T_SHR_ASSIGN   ">>="

%token T_AND_ASSIGN "&="
%token T_OR_ASSIGN  "|="
%token T_XOR_ASSIGN "^="

%token T_EQ "=="
%token T_NE "!="
%token T_LE "<="
%token T_GE ">="

%token T_PLUS  "+"
%token T_MINUS "-"
%token T_TIMES "*"
%token T_DIV   "/"

%token T_SHL   ">>"
%token T_SHR   "<<"

%token T_LAND "&&"
%token T_LOR  "||"
%token T_NOT  "!"

%token T_AND "&"
%token T_OR  "|"
%token T_XOR "^"

%token T_ASSIGN "="

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

%type<type>  type return_type

%type<stmt>      stmt else_branch
%type<stmt_list> stmt_list block_stmt

%type<bin_op> op_assign
%type<expr>   expr

%type<clusters> clusters cluster_list
%type<cluster>  cluster

%type<ident> identifier

%left T_TIMES T_DIV
%left T_PLUS T_MINUS
%left T_SHL T_SHR
%left T_AND
%left T_XOR
%left T_OR
/* No associativity for comparison operators */
%nonassoc T_EQ T_NE T_LANGLE T_LE T_RANGLE T_GE
%left T_LAND
%left T_LOR

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
		$$ = new Class(
			std::move(*$2), std::move(*$3), std::move(*$4), std::move(*$5)
		);

		delete $2;
		delete $3;
		delete $4;
		delete $5;
	}

pool_bounds
	: %empty                        { $$ = new std::vector<VariableDecl>; }
	| T_WHERE variable_declarations { $$ = $2; }

variable_declarations
	: %empty { $$ = new std::vector<VariableDecl>; }
	| variable_declaration_list         { $$ = $1; }
	| variable_declaration_list T_COMMA { $$ = $1; }

variable_declaration_list
	: variable_declaration {
		$$ = new std::vector<VariableDecl>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| variable_declaration_list T_COMMA variable_declaration {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}

variable_declaration
	: identifier T_COLON type {
		$$ = new VariableDecl(std::move(*$1), $3); delete $1;
	}

type
	: identifier pool_parameters {
		$$ = new TmpClassType(std::move(*$1), std::move(*$2), TmpClassType::Kind::CLASS_LAYOUT);
		delete $1; delete $2;
		}
	| T_LSQUARE identifier pool_parameters T_RSQUARE {
		$$ = new TmpClassType(std::move(*$2), std::move(*$3), TmpClassType::Kind::BOUND);
		delete $2; delete $3;
	}
	/* Just make up one for error recovery */
	| T_LSQUARE error T_RSQUARE {
		$$ = new InvalidType;
	}
	| T_AND type {
		$$ = new ReferenceType($2);
	}
	| T_BOOL { $$ = new PrimitiveType(PrimitiveKind::BOOL); }
	| T_I8   { $$ = new PrimitiveType(PrimitiveKind::I8);   }
	| T_U8   { $$ = new PrimitiveType(PrimitiveKind::U8);   }
	| T_I16  { $$ = new PrimitiveType(PrimitiveKind::I16);  }
	| T_U16  { $$ = new PrimitiveType(PrimitiveKind::U16);  }
	| T_I32  { $$ = new PrimitiveType(PrimitiveKind::I32);  }
	| T_U32  { $$ = new PrimitiveType(PrimitiveKind::U32);  }
	| T_I64  { $$ = new PrimitiveType(PrimitiveKind::I64);  }
	| T_U64  { $$ = new PrimitiveType(PrimitiveKind::U64);  }
	| T_F32  { $$ = new PrimitiveType(PrimitiveKind::F32);  }
	| T_F64  { $$ = new PrimitiveType(PrimitiveKind::F64);  }

expr
	: T_LPAREN expr T_RPAREN { $$ = $2; }

	/* Unary expressions; must be inline for Bison to do its precedence magic */
	| T_PLUS  expr { $$ = new UnaryExpr(UnOp::PLUS,  $2); }
	| T_MINUS expr { $$ = new UnaryExpr(UnOp::MINUS, $2); }
	| T_NOT   expr { $$ = new UnaryExpr(UnOp::NOT,   $2); }

	/* Binary expressions; must be inline for Bison to do its precedence magic */
	| expr T_PLUS   expr { $$ = new BinaryExpr($1, BinOp::PLUS,  $3); }
	| expr T_MINUS  expr { $$ = new BinaryExpr($1, BinOp::MINUS, $3); }
	| expr T_TIMES  expr { $$ = new BinaryExpr($1, BinOp::TIMES, $3); }
	| expr T_DIV    expr { $$ = new BinaryExpr($1, BinOp::DIV,   $3); }
	| expr T_SHL    expr { $$ = new BinaryExpr($1, BinOp::SHL,   $3); }
	| expr T_SHR    expr { $$ = new BinaryExpr($1, BinOp::SHR,   $3); }
	| expr T_AND    expr { $$ = new BinaryExpr($1, BinOp::AND,   $3); }
	| expr T_OR     expr { $$ = new BinaryExpr($1, BinOp::OR,    $3); }
	| expr T_XOR    expr { $$ = new BinaryExpr($1, BinOp::XOR,   $3); }
	| expr T_LAND   expr { $$ = new BinaryExpr($1, BinOp::LAND,  $3); }
	| expr T_LOR    expr { $$ = new BinaryExpr($1, BinOp::LOR,   $3); }
	| expr T_LANGLE expr { $$ = new BinaryExpr($1, BinOp::LT,    $3); }
	| expr T_RANGLE expr { $$ = new BinaryExpr($1, BinOp::GT,    $3); }
	| expr T_LE     expr { $$ = new BinaryExpr($1, BinOp::LE,    $3); }
	| expr T_GE     expr { $$ = new BinaryExpr($1, BinOp::GE,    $3); }
	| expr T_EQ     expr { $$ = new BinaryExpr($1, BinOp::EQ,    $3); }
	| expr T_NE     expr { $$ = new BinaryExpr($1, BinOp::NE,    $3); }

	| expr T_LSQUARE expr T_RSQUARE { $$ = new IndexExpr($1, $3); }
	| expr T_LSQUARE error T_RSQUARE {
		// Just make up one for error recovery
		$$ = new IndexExpr($1, new IntConst(0));
	}
	| expr T_DOT identifier { $$ = new FieldExpr($1, std::move(*$3)); delete $3; }
/*
	| expr T_DOT identifier T_LPAREN expr_list T_RPAREN { $$ = $1; }
	| expr T_DOT identifier T_LPAREN error T_RPAREN { $$ = $1; }
*/
	| identifier { $$ = new IdentifierExpr(std::move(*$1)); delete $1; }
	| expr T_AS type {
		$$ = new CastExpr($1, $3);
	}
	| T_NEW type {
		$$ = new NewExpr($2);
	}
	| T_NUM {
		$$ = new IntConst(0); delete $1; // TODO: Handle integers
	}
	| T_THIS { $$ = new ThisExpr; }
	| T_NULL { $$ = new NullExpr; }

/*
expr_list
	: %empty
	| expr_list T_COMMA expr
*/

stmt
	: block_stmt  { $$ = $1; }
	| T_SEMICOLON { $$ = new NoopStmt; }
	| T_LET variable_declaration_list T_SEMICOLON {
		$$ = new VariableDeclsStmt(std::move(*$2), VariableDeclsStmt::Kind::VARS);
		delete $2;
	}
	| T_POOLS variable_declaration_list T_SEMICOLON {
		$$ = new VariableDeclsStmt(std::move(*$2), VariableDeclsStmt::Kind::POOLS);
		delete $2;
	}
	| T_POOL variable_declaration T_SEMICOLON {
		std::vector<VariableDecl> list;
		list.emplace_back(std::move(*$2));
		delete $2;

		$$ = new VariableDeclsStmt(std::move(list), VariableDeclsStmt::Kind::POOLS);
	}
	| expr T_ASSIGN expr T_SEMICOLON {
		$$ = new AssignStmt($1, $3);
	}
	| expr op_assign expr T_SEMICOLON {
		$$ = new OpAssignStmt($1, $2, $3);
	}
	| T_IF expr block_stmt else_branch {
		$$ = new IfStmt($2, $3, $4);
	}
	| T_WHILE expr block_stmt {
		$$ = new WhileStmt($2, $3);
	}
	| T_FOREACH identifier T_ASSIGN expr T_DOTDOT expr block_stmt {
		$$ = new ForeachRangeStmt(std::move(*$2), $4, $6, $7);
		delete $2;
	}
	| T_FOREACH identifier T_COLON identifier block_stmt {
		$$ = new ForeachPoolStmt(std::move(*$2), std::move(*$4), $5);
		delete $2;
		delete $4;
	}
	| T_BREAK T_SEMICOLON {
		$$ = new BreakStmt;
	}
	| T_CONTINUE T_SEMICOLON {
		$$ = new ContinueStmt;
	}
	| T_RETURN T_SEMICOLON {
		$$ = new ReturnVoidStmt;
	}
	| T_RETURN expr T_SEMICOLON {
		$$ = new ReturnStmt($2);
	}
	| expr T_SEMICOLON {
		$$ = new ExprStmt($1);
	}
	| error T_SEMICOLON { $$ = new NoopStmt; }

block_stmt
	: T_LBRACE stmt_list T_RBRACE { $$ = $2; }
	/* Just make one up for error recovery */
	| T_LBRACE error T_RBRACE { $$ = new BlockStmt; }

stmt_list
	: %empty { $$ = new BlockStmt; }
	| stmt_list stmt {
		$$ = $1;
		$$->stmts.emplace_back($2);
	}

else_branch
	: %empty { $$ = new NoopStmt; }
	| T_ELSE block_stmt { $$ = $2; }

op_assign
	: T_PLUS_ASSIGN  { $$ = BinOp::PLUS;  }
	| T_MINUS_ASSIGN { $$ = BinOp::MINUS; }
	| T_TIMES_ASSIGN { $$ = BinOp::TIMES; }
	| T_DIV_ASSIGN   { $$ = BinOp::DIV;   }
	| T_SHL_ASSIGN   { $$ = BinOp::SHL;   }
	| T_SHR_ASSIGN   { $$ = BinOp::SHR;   }
	| T_AND_ASSIGN   { $$ = BinOp::AND;   }
	| T_OR_ASSIGN    { $$ = BinOp::OR;    }
	| T_XOR_ASSIGN   { $$ = BinOp::XOR;   }

class_body
	: T_LBRACE T_RBRACE               { $$ = new TmpClassBody; }
	| T_LBRACE class_members T_RBRACE { $$ = $2;               }
	| T_LBRACE error T_RBRACE         { $$ = new TmpClassBody; }

class_members
	: field_declarations {
		$$ = new TmpClassBody;
		for (auto& e: *$1) {
			$$->fields.emplace_back(std::move(e));
		}
		delete $1;
	}
	| method_declaration {
		$$ = new TmpClassBody;
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
	: T_FN T_IDENT method_arguments return_type block_stmt {
		$$ = new Method(
			std::move(*$2),
			std::move(*$3),
			$4,
			std::move($5->stmts)
		);
		delete $2;
		delete $3;
		delete $5;
	}

method_arguments
	: T_LPAREN variable_declarations T_RPAREN { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LPAREN error T_RPAREN { $$ = new std::vector<VariableDecl>; }

return_type
	: type   { $$ = $1; }
	| %empty { $$ = new VoidType; }

layout_definition
	: T_LAYOUT identifier T_COLON identifier T_ASSIGN clusters {
		$$ = new Layout(
			std::move(*$2), std::move(*$4), std::move(*$6)
		);

		delete $2;
		delete $4;
		delete $6;
	}

clusters
	: T_SEMICOLON              { $$ = new std::vector<Cluster>; }
	| cluster_list T_SEMICOLON { $$ = $1;                          }
	/* Just make up one for error recovery */
	| error T_SEMICOLON        { $$ = new std::vector<Cluster>; }

cluster_list
	: cluster {
		$$ = new std::vector<Cluster>;
		$$->emplace_back(std::move(*$1)); delete $1;
	}
	| cluster_list T_PLUS cluster {
		$$ = $1;
		$$->emplace_back(std::move(*$3)); delete $3;
	}

cluster
	: T_REC T_LBRACE identifiers T_RBRACE {
		$$ = new Cluster(std::move(*$3)); delete $3;
	}
	/* Just make up one for error recovery */
	| T_REC T_LBRACE error T_RBRACE { $$ = new Cluster; }

identifiers
	: %empty                  { $$ = new std::vector<Identifier>; }
	| identifier_list         { $$ = $1; }
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
