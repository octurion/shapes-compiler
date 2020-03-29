%{
#include "cst.h"
#include "parser.tab.h"
#include "lexer.yy.h"

#include <climits>
#include <cstdlib>

extern void yyerror(
	YYLTYPE* loc,
	yyscan_t state,
	Cst::Program* cst,
	Cst::SyntaxErrorList* syntax_errors,
	const char* msg);

static Location yyltype_to_location(const YYLTYPE& orig_loc);

/*
 * How we do memory management: Unfortunately, Bison does not really let us use
 * C++11 goodies such as std::unique_ptr. This means that we have to manually
 * free whatever intermediate products are generated. To that extent, we use
 * the following two functions pattern for memory management in the parser:
 */

/**
 * Moves the object pointed to by `ptr` (via move semantics), then deletes
 * `ptr`.
 */
template<typename T>
T move_and_delete(T* ptr) {
	T moved(std::move(*ptr));
	delete ptr;
	return moved;
}

/**
 * Wraps a raw `ptr` object into a `std::unique_ptr`. This `std::unique_ptr`
 * takes ownership of the heap-allocated object.
 */
template<typename T>
std::unique_ptr<T> ptr_to_unique(T* ptr) {
	return std::unique_ptr<T>(ptr);
}
%}

%define api.pure full
%locations

%lex-param   {yyscan_t scanner}
%parse-param {yyscan_t scanner} {Cst::Program* cst} {Cst::SyntaxErrorList* errors}

%define parse.error verbose

%union {
	Cst::Program* program;

	Cst::Class* class_def;
	Cst::ClassBody* class_body;

	Cst::Layout* layout;

	Cst::Method* method;

	Cst::Type* type;

	Cst::Stmt* stmt;
	Cst::BlockStmt* block;

	Cst::Expr* expr;

	Cst::BinOp bin_op;

	Cst::Variable* variable;
	std::vector<Cst::Variable>* variable_list;

	std::vector<Cst::Field>* field_list;

	Cst::Cluster* cluster;
	std::vector<Cst::Cluster>* cluster_list;

	Cst::Identifier* identifier;
	std::vector<Cst::Identifier>* identifier_list;

	std::vector<std::unique_ptr<Cst::PoolParameter>>* pool_param_list;

	Cst::IntegerConst* integer;
}

/* This will ensure that in the case of error recovery, the intermediate
 * products will be freed and no memory leaks will occur */
%destructor { delete $$; } <*>
%destructor { } <bin_op>

/* Top-level goal */
%start program

/* List of all tokens (fed into lex) */

%token<identifier> T_IDENT     "identifier"
%token<integer>    T_INT_CONST "integer constant"

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
%token T_NONE "none"

%token T_UNIFORM "uniform"
%token T_VARYING "varying"

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

%type<program> program_definitions

%type<class_def> class_definition
%type<layout>    layout_definition

%type<variable_list> variable_declarations
                     variable_declaration_list
                     pool_bounds
                     method_arguments
%type<field_list>    field_declarations
%type<variable> variable_declaration
%type<identifier_list> identifiers
                       identifier_list
                       class_pool_parameters
%type<pool_param_list> pool_parameters
                       pool_param_list_header
                       pool_param_list
%type<class_body> class_body
                  class_members
%type<method> method_declaration

%type<type> type return_type

%type<stmt>  stmt else_branch
%type<block> stmt_list block_stmt

%type<bin_op> op_assign
%type<expr>   expr

%type<cluster_list> clusters cluster_list
%type<cluster>  cluster

%type<identifier> identifier

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
#include "cst.h"

/* Ugly hack to allow both lex and bison to have yyscan_t as the scanner type */
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
}

%%

program
	: program_definitions T_END { *cst = move_and_delete($1); }

program_definitions
	: %empty { $$ = new Cst::Program; }
	| program_definitions class_definition {
		$$ = $1;
		$$->add_class(move_and_delete($2));
	}
	| program_definitions layout_definition {
		$$ = $1;
		$$->add_layout(move_and_delete($2));
	}

class_definition
	: T_CLASS identifier class_pool_parameters pool_bounds class_body {
		auto body = move_and_delete($5);
		$$ = new Cst::Class(
			move_and_delete($2),
			move_and_delete($3),
			move_and_delete($4),
			body.consume_fields(),
			body.consume_methods());
	}

pool_bounds
	: %empty                        { $$ = new std::vector<Cst::Variable>; }
	| T_WHERE variable_declarations { $$ = $2; }

variable_declarations
	: %empty { $$ = new std::vector<Cst::Variable>; }
	| variable_declaration_list         { $$ = $1; }
	| variable_declaration_list T_COMMA { $$ = $1; }

variable_declaration_list
	: variable_declaration {
		$$ = new std::vector<Cst::Variable>;
		$$->emplace_back(move_and_delete($1));
	}
	| variable_declaration_list T_COMMA variable_declaration {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

variable_declaration
	: identifier T_COLON type {
		$$ = new Cst::Variable(move_and_delete($1), ptr_to_unique($3));
	}

type
	: identifier pool_parameters {
		$$ = new Cst::ClassType(move_and_delete($1), move_and_delete($2));
		}
	| T_LSQUARE identifier pool_parameters T_RSQUARE {
		$$ = new Cst::BoundType(move_and_delete($2), move_and_delete($3));
	}
	/* Just make up one for error recovery */
	| T_LSQUARE error T_RSQUARE {
		$$ = new Cst::InvalidType;
	}
	/* TODO: Booleans */
	/* | T_BOOL { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::BOOL); } */
	| T_I8   { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::I8);   }
	| T_U8   { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::U8);   }
	| T_I16  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::I16);  }
	| T_U16  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::U16);  }
	| T_I32  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::I32);  }
	| T_U32  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::U32);  }
	| T_I64  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::I64);  }
	| T_U64  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::U64);  }
	| T_F32  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::F32);  }
	| T_F64  { $$ = new Cst::PrimitiveType(Cst::PrimitiveType::Kind::F64);  }

expr
	: T_LPAREN expr T_RPAREN { $$ = $2; }

	/* Unary expressions; must be inline for Bison to do its precedence magic */
	| T_PLUS  expr { $$ = new Cst::UnaryExpr(Cst::UnOp::PLUS,  ptr_to_unique($2), yyltype_to_location(@$)); }
	| T_MINUS expr { $$ = new Cst::UnaryExpr(Cst::UnOp::MINUS, ptr_to_unique($2), yyltype_to_location(@$)); }
	| T_NOT   expr { $$ = new Cst::UnaryExpr(Cst::UnOp::NOT,   ptr_to_unique($2), yyltype_to_location(@$)); }

	/* Binary expressions; must be inline for Bison to do its precedence magic */
	| expr T_PLUS   expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::PLUS,  ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_MINUS  expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::MINUS, ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_TIMES  expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::TIMES, ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_DIV    expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::DIV,   ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_SHL    expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::SHL,   ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_SHR    expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::SHR,   ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_AND    expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::AND,   ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_OR     expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::OR,    ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_XOR    expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::XOR,   ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_LAND   expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::LAND,  ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_LOR    expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::LOR,   ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_LANGLE expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::LT,    ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_RANGLE expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::GT,    ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_LE     expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::LE,    ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_GE     expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::GE,    ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_EQ     expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::EQ,    ptr_to_unique($3), yyltype_to_location(@$)); }
	| expr T_NE     expr { $$ = new Cst::BinaryExpr(ptr_to_unique($1), Cst::BinOp::NE,    ptr_to_unique($3), yyltype_to_location(@$)); }

	| expr T_DOT identifier {
		$$ = new Cst::FieldAccess(
			ptr_to_unique($1),
			move_and_delete($3),
			yyltype_to_location(@$));
	}
	| identifier {
		$$ = new Cst::IdentifierExpr(move_and_delete($1));
	}
	| T_NEW type {
		$$ = new Cst::NewExpr(ptr_to_unique($2), yyltype_to_location(@$));
	}
	| T_INT_CONST {
		$1->set_loc(yyltype_to_location(@$));
		$$ = $1;
	}
	| T_THIS { $$ = new Cst::ThisExpr(yyltype_to_location(@$)); }
	| T_NULL { $$ = new Cst::NullExpr(yyltype_to_location(@$)); }

stmt
	: block_stmt  { $$ = $1; }
	| T_SEMICOLON { $$ = new Cst::NoopStmt; }
	| T_LET variable_declaration_list T_SEMICOLON {
		$$ = new Cst::VariableDeclsStmt(
			move_and_delete($2),
			Cst::VariableDeclsStmt::Kind::VARS);
	}
	| T_POOLS variable_declaration_list T_SEMICOLON {
		$$ = new Cst::VariableDeclsStmt(
			move_and_delete($2),
			Cst::VariableDeclsStmt::Kind::POOLS);
	}
	| T_POOL variable_declaration T_SEMICOLON {
		std::vector<Cst::Variable> tmp;
		tmp.emplace_back(move_and_delete($2));
		$$ = new Cst::VariableDeclsStmt(std::move(tmp), Cst::VariableDeclsStmt::Kind::POOLS);
	}
	| expr T_ASSIGN expr T_SEMICOLON {
		$$ = new Cst::AssignStmt(ptr_to_unique($1), ptr_to_unique($3));
	}
	| expr op_assign expr T_SEMICOLON {
		$$ = new Cst::OpAssignStmt(ptr_to_unique($1), $2, ptr_to_unique($3));
	}
	| T_IF expr block_stmt else_branch {
		$$ = new Cst::IfStmt(
			ptr_to_unique($2),
			ptr_to_unique($3),
			ptr_to_unique($4));
	}
	| T_WHILE expr block_stmt {
		$$ = new Cst::WhileStmt(ptr_to_unique($2), ptr_to_unique($3));
	}
	| T_FOREACH identifier T_ASSIGN expr T_DOTDOT expr block_stmt {
		$$ = new Cst::ForeachRangeStmt(
			move_and_delete($2),
			ptr_to_unique($4),
			ptr_to_unique($6),
			ptr_to_unique($7));
	}
	| T_FOREACH identifier T_COLON identifier block_stmt {
		$$ = new Cst::ForeachPoolStmt(
			move_and_delete($2),
			move_and_delete($4),
			ptr_to_unique($5));
	}
	| T_BREAK T_SEMICOLON {
		$$ = new Cst::BreakStmt(yyltype_to_location(@1));
	}
	| T_CONTINUE T_SEMICOLON {
		$$ = new Cst::ContinueStmt(yyltype_to_location(@1));
	}
	| T_RETURN T_SEMICOLON {
		$$ = new Cst::ReturnVoidStmt(yyltype_to_location(@1));
	}
	| T_RETURN expr T_SEMICOLON {
		$$ = new Cst::ReturnStmt(ptr_to_unique($2));
	}
	| expr T_SEMICOLON {
		$$ = new Cst::ExprStmt(ptr_to_unique($1));
	}
	| error T_SEMICOLON { $$ = new Cst::NoopStmt; }

block_stmt
	: T_LBRACE stmt_list T_RBRACE { $$ = $2; }
	/* Just make one up for error recovery */
	| T_LBRACE error T_RBRACE { $$ = new Cst::BlockStmt; }

stmt_list
	: %empty { $$ = new Cst::BlockStmt; }
	| stmt_list stmt {
		$$ = $1;
		$$->add(ptr_to_unique($2));
	}

else_branch
	: %empty { $$ = new Cst::NoopStmt; }
	| T_ELSE block_stmt { $$ = $2; }

op_assign
	: T_PLUS_ASSIGN  { $$ = Cst::BinOp::PLUS;  }
	| T_MINUS_ASSIGN { $$ = Cst::BinOp::MINUS; }
	| T_TIMES_ASSIGN { $$ = Cst::BinOp::TIMES; }
	| T_DIV_ASSIGN   { $$ = Cst::BinOp::DIV;   }
	| T_SHL_ASSIGN   { $$ = Cst::BinOp::SHL;   }
	| T_SHR_ASSIGN   { $$ = Cst::BinOp::SHR;   }
	| T_AND_ASSIGN   { $$ = Cst::BinOp::AND;   }
	| T_OR_ASSIGN    { $$ = Cst::BinOp::OR;    }
	| T_XOR_ASSIGN   { $$ = Cst::BinOp::XOR;   }

class_body
	: T_LBRACE T_RBRACE               { $$ = new Cst::ClassBody; }
	| T_LBRACE class_members T_RBRACE { $$ = $2;                 }
	| T_LBRACE error T_RBRACE         { $$ = new Cst::ClassBody; }

class_members
	: field_declarations {
		$$ = new Cst::ClassBody;
		auto fields = move_and_delete($1);
		for (auto& e: fields) {
			$$->add_field(std::move(e));
		}
	}
	| method_declaration {
		$$ = new Cst::ClassBody;
		$$->add_method(move_and_delete($1));
	}
	| class_members field_declarations {
		$$ = $1;
		auto fields = move_and_delete($2);
		for (auto& e: fields) {
			$$->add_field(std::move(e));
		}
	}
	| class_members method_declaration {
		$$ = $1;
		$$->add_method(move_and_delete($2));
	}

pool_parameters
	: %empty { $$ = new std::vector<std::unique_ptr<Cst::PoolParameter>>; }
	| T_LANGLE pool_param_list_header T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE { $$ = new std::vector<std::unique_ptr<Cst::PoolParameter>>; }

pool_param_list_header
	: %empty                  { $$ = new std::vector<std::unique_ptr<Cst::PoolParameter>>; }
	| pool_param_list         { $$ = $1; }
	| pool_param_list T_COMMA { $$ = $1; }

pool_param_list
	: identifier {
		$$ = new std::vector<std::unique_ptr<Cst::PoolParameter>>;
		$$->emplace_back($1);
	}
	| T_NONE {
		$$ = new std::vector<std::unique_ptr<Cst::PoolParameter>>;
		$$->emplace_back(new Cst::NoneParam);
	}
	| pool_param_list T_COMMA identifier {
		$$ = $1;
		$$->emplace_back($3);
	}
	| pool_param_list T_COMMA T_NONE {
		$$ = $1;
		$$->emplace_back(new Cst::NoneParam);
	}

class_pool_parameters
	: %empty                        { $$ = new std::vector<Cst::Identifier>; }
	| T_LANGLE identifiers T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE       { $$ = new std::vector<Cst::Identifier>; }

field_declarations
	: variable_declarations T_SEMICOLON {
		$$ = new std::vector<Cst::Field>;
		auto fields = move_and_delete($1);

		for (auto& e: fields) {
			$$->emplace_back(e.consume_name(), e.consume_type());
		}
	}

method_declaration
	: T_FN T_IDENT method_arguments T_COLON return_type block_stmt {
		$$ = new Cst::Method(
			move_and_delete($2),
			move_and_delete($3),
			ptr_to_unique($5),
			move_and_delete($6));
	}
	| T_FN T_IDENT method_arguments block_stmt {
		$$ = new Cst::Method(
			move_and_delete($2),
			move_and_delete($3),
			nullptr,
			move_and_delete($4));
	}

method_arguments
	: T_LPAREN variable_declarations T_RPAREN { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LPAREN error T_RPAREN { $$ = new std::vector<Cst::Variable>; }

return_type
	: type   { $$ = $1; }
	| %empty { $$ = nullptr; }

layout_definition
	: T_LAYOUT identifier T_COLON identifier T_ASSIGN clusters {
		$$ = new Cst::Layout(
			move_and_delete($2), move_and_delete($4), move_and_delete($6));
	}

clusters
	: T_SEMICOLON              { $$ = new std::vector<Cst::Cluster>; }
	| cluster_list T_SEMICOLON { $$ = $1;                            }
	/* Just make up one for error recovery */
	| error T_SEMICOLON        { $$ = new std::vector<Cst::Cluster>; }

cluster_list
	: cluster {
		$$ = new std::vector<Cst::Cluster>;
		$$->emplace_back(move_and_delete($1));
	}
	| cluster_list T_PLUS cluster {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

cluster
	: T_REC T_LBRACE identifiers T_RBRACE {
		$$ = new Cst::Cluster(move_and_delete($3));
	}
	/* Just make up one for error recovery */
	| T_REC T_LBRACE error T_RBRACE { $$ = new Cst::Cluster; }

identifiers
	: %empty                  { $$ = new std::vector<Cst::Identifier>; }
	| identifier_list         { $$ = $1; }
	| identifier_list T_COMMA { $$ = $1; }

identifier_list
	: identifier {
		$$ = new std::vector<Cst::Identifier>;
		$$->emplace_back(move_and_delete($1));
	}
	| identifier_list T_COMMA identifier {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

identifier
	: T_IDENT {
		$$ = $1;
		// Fix up the location that flex did not set up
		$$->set_loc(yyltype_to_location(@1));
	}
%%

void yyerror(
	YYLTYPE* orig_loc,
	yyscan_t state,
	Cst::Program* program,
	Cst::SyntaxErrorList* errors,
	const char* msg)
{
	(void) state;
	(void) program;

	errors->add(yyltype_to_location(*orig_loc), std::string(msg));
}

static Location yyltype_to_location(const YYLTYPE& orig_loc)
{
	Location loc;
	loc.first_line   = orig_loc.first_line;
	loc.first_column = orig_loc.first_column;
	loc.last_line    = orig_loc.last_line;
	loc.last_column  = orig_loc.last_column;

	return loc;
}
