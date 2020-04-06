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

static Location make_loc(const YYLTYPE& orig_loc);

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
	Cst::Type::ObjectType* object_type;
	Cst::Type::PrimitiveType* primitive_type;

	Cst::LayoutType* layout_type;
	Cst::BoundType* bound_type;

	Cst::Stmt* stmt;
	Cst::Stmt::Block* block;

	Cst::Expr* expr;

	Cst::BinOp bin_op;

	Cst::VariableDeclaration* variable_decl;
	std::vector<Cst::VariableDeclaration>* variable_decl_list;

	Cst::PoolDeclaration* pool_decl;
	std::vector<Cst::PoolDeclaration>* pool_decl_list;

	Cst::FormalPoolBound* pool_bound;
	std::vector<Cst::FormalPoolBound>* pool_bound_list;

	Cst::FormalPoolParameter* formal_pool_parameter;
	std::vector<Cst::FormalPoolParameter>* formal_pool_parameter_list;

	std::vector<Cst::PoolParameter>* pool_parameter_list;

	Cst::PoolParameter::Pool* bound_pool_parameter;
	std::vector<Cst::PoolParameter::Pool>* bound_pool_parameter_list;

	Cst::MethodParameter* method_parameter;
	std::vector<Cst::MethodParameter>* method_parameter_list;

	Cst::Field* field;
	std::vector<Cst::Field>* field_list;

	Cst::ClusterField* cluster_field;
	std::vector<Cst::ClusterField>* cluster_field_list;

	Cst::Cluster* cluster;
	std::vector<Cst::Cluster>* cluster_list;

	std::vector<Cst::Expr>* expr_list;

	Cst::Identifier* identifier;

	std::string* str;
}

/* This will ensure that in the case of error recovery, the intermediate
 * products will be freed and no memory leaks will occur */
%destructor { delete $$; } <*>
%destructor { } <bin_op>

/* Top-level goal */
%start program

/* List of all tokens (fed into lex) */

%token<str> T_IDENT     "identifier"
%token<str> T_INT_CONST "integer constant"

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

%token T_TRUE  "true"
%token T_FALSE "false"

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

%token T_UNTERMINATED_COMMENT  "unterminated comment"
%token T_UNPRINTABLE_CHARACTER "unprintable character"
%token T_UNRECOGNIZED_TOKEN    "unrecognized token"
%token T_END 0                 "end of file"

%type<program> program_definitions

%type<class_def> class_definition
%type<layout>    layout_definition

%type<field> field_declaration
%type<field_list> field_declarations
                  field_declaration_list

%type<variable_decl> variable_declaration
%type<variable_decl_list> variable_declaration_list

%type<pool_decl> pool_declaration
%type<pool_decl_list> pool_declaration_list

%type<formal_pool_parameter> formal_pool_parameter
%type<formal_pool_parameter_list> formal_pool_parameters
                                  formal_pool_parameter_list

%type<bound_pool_parameter> bound_pool_parameter
%type<bound_pool_parameter_list> bound_pool_parameters
                                 bound_pool_parameter_list

%type<method_parameter> method_parameter
%type<method_parameter_list> method_parameters
                             method_parameter_list

%type<cluster_field> cluster_field
%type<cluster_field_list> cluster_field_list

%type<pool_bound> pool_bound
%type<pool_bound_list> pool_bounds pool_bound_list

%type<pool_parameter_list> pool_parameters
                           pool_parameter_list

%type<class_body> class_body
                  class_members
%type<method> method_declaration

%type<type> type
%type<object_type> object_type
%type<primitive_type> primitive_type

%type<bound_type> bound_type
%type<layout_type> layout_type

%type<stmt>  stmt else_branch
%type<block> stmt_list block_stmt

%type<bin_op> op_assign

%type<expr> expr

%type<expr_list> method_call_args method_call_arg_list

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
#include "syntactic_analysis.h"

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
	: T_CLASS identifier formal_pool_parameters pool_bounds class_body {
		auto body = move_and_delete($5);
		$$ = new Cst::Class(
			move_and_delete($2),
			move_and_delete($3),
			move_and_delete($4),
			body.consume_fields(),
			body.consume_methods());
	}

pool_bounds
	: %empty { $$ = new std::vector<Cst::FormalPoolBound>; }
	| T_WHERE pool_bound_list { $$ = $2; }
	| T_WHERE pool_bound_list T_COMMA { $$ = $2; }

pool_bound_list
	: pool_bound {
		$$ = new std::vector<Cst::FormalPoolBound>;
		$$->emplace_back(move_and_delete($1));
	}
	| pool_bound_list T_COMMA pool_bound {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

pool_bound
	: formal_pool_parameter T_COLON bound_type {
		$$ = new Cst::FormalPoolBound(move_and_delete($1), move_and_delete($3), make_loc(@$));
	}

bound_type
	: T_LSQUARE identifier bound_pool_parameters T_RSQUARE {
		$$ = new Cst::BoundType(move_and_delete($2), move_and_delete($3));
	}
	/* Just make up one for error recovery */
	| T_LSQUARE error T_RSQUARE {
		$$ = new Cst::BoundType;
	}

bound_pool_parameters
	: T_LANGLE T_RANGLE { $$ = new std::vector<Cst::PoolParameter::Pool>; }
	| T_LANGLE bound_pool_parameter_list T_RANGLE { $$ = $2; }
	| T_LANGLE bound_pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	| T_LANGLE error T_RANGLE { $$ = new std::vector<Cst::PoolParameter::Pool>; }

bound_pool_parameter_list
	: bound_pool_parameter {
		$$ = new std::vector<Cst::PoolParameter::Pool>;
		$$->emplace_back(move_and_delete($1));
	}
	| bound_pool_parameter_list T_COMMA bound_pool_parameter {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

bound_pool_parameter
	: identifier { $$ = new Cst::PoolParameter::Pool(move_and_delete($1)); }

formal_pool_parameter
	: identifier { $$ = new Cst::FormalPoolParameter(move_and_delete($1)); }

variable_declaration_list
	: variable_declaration {
		$$ = new std::vector<Cst::VariableDeclaration>;
		$$->emplace_back(move_and_delete($1));
	}
	| variable_declaration_list T_COMMA variable_declaration {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

variable_declaration
	: identifier T_COLON type {
		$$ = new Cst::VariableDeclaration(
			move_and_delete($1), move_and_delete($3), make_loc(@$));
	}

pool_declaration_list
	: pool_declaration {
		$$ = new std::vector<Cst::PoolDeclaration>;
		$$->emplace_back(move_and_delete($1));
	}
	| pool_declaration_list T_COMMA pool_declaration {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

pool_declaration
	: identifier T_COLON layout_type {
		$$ = new Cst::PoolDeclaration(
			move_and_delete($1), move_and_delete($3), make_loc(@$));
	}

layout_type
	: identifier pool_parameters {
		$$ = new Cst::LayoutType(
			move_and_delete($1), move_and_delete($2), make_loc(@$));
	}

object_type
	: identifier pool_parameters {
		$$ = new Cst::Type::ObjectType(move_and_delete($1), move_and_delete($2));
	}

primitive_type
	: T_BOOL { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::BOOL); }
	| T_I8   { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::I8);   }
	| T_U8   { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::U8);   }
	| T_I16  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::I16);  }
	| T_U16  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::U16);  }
	| T_I32  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::I32);  }
	| T_U32  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::U32);  }
	| T_I64  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::I64);  }
	| T_U64  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::U64);  }
	| T_F32  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::F32);  }
	| T_F64  { $$ = new Cst::Type::PrimitiveType(Cst::Type::PrimitiveKind::F64);  }

type
	: object_type    { $$ = new Cst::Type(move_and_delete($1), make_loc(@$)); }
	| primitive_type { $$ = new Cst::Type(move_and_delete($1), make_loc(@$)); }

expr
	: T_LPAREN expr T_RPAREN { $$ = $2; }

	/* Unary expressions; must be inline for Bison to do its precedence magic */
	| T_PLUS  expr { $$ = new Cst::Expr(Cst::Expr::Unary(Cst::UnOp::PLUS,  move_and_delete($2)), make_loc(@$)); }
	| T_MINUS expr { $$ = new Cst::Expr(Cst::Expr::Unary(Cst::UnOp::MINUS, move_and_delete($2)), make_loc(@$)); }
	| T_NOT   expr { $$ = new Cst::Expr(Cst::Expr::Unary(Cst::UnOp::NOT,   move_and_delete($2)), make_loc(@$)); }

	/* Binary expressions; must be inline for Bison to do its precedence magic */
	| expr T_PLUS   expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::PLUS, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_MINUS  expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::MINUS, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_TIMES expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::TIMES, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_DIV expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::DIV, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_SHL expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::SHL, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_SHR expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::SHR, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_AND expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::AND, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_OR expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::OR, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_XOR expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::XOR, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_LAND expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::LAND, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_LOR expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::LOR, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_LANGLE expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::LT, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_RANGLE expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::GT, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_LE expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::LE, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_GE expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::GE, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_EQ expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::EQ, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_NE expr {
		$$ = new Cst::Expr(
			Cst::Expr::Binary(move_and_delete($1), Cst::BinOp::NE, move_and_delete($3)),
			make_loc(@$)
		);
	}

	| expr T_DOT identifier {
		$$ = new Cst::Expr(
			Cst::Expr::FieldAccess(move_and_delete($1), move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr T_DOT identifier method_call_args {
		$$ = new Cst::Expr(
			Cst::Expr::MemberMethodCall(
				move_and_delete($1), move_and_delete($3), move_and_delete($4)
			),
			make_loc(@$)
		);
	}
	| identifier method_call_args {
		$$ = new Cst::Expr(
			Cst::Expr::MethodCall(move_and_delete($1), move_and_delete($2)),
			make_loc(@$)
		);
	}
	| identifier {
		$$ = new Cst::Expr(Cst::Expr::VariableExpr(move_and_delete($1)), make_loc(@$));
	}
	| T_NEW object_type {
		$$ = new Cst::Expr(Cst::Expr::New(move_and_delete($2)), make_loc(@$));
	}
	| T_INT_CONST {
		$$ = new Cst::Expr(Cst::Expr::IntegerConst(move_and_delete($1)), make_loc(@$));
	}
	| T_THIS  { $$ = new Cst::Expr(Cst::Expr::This(), make_loc(@$)); }
	| T_NULL  { $$ = new Cst::Expr(Cst::Expr::Null(), make_loc(@$)); }

	| T_TRUE  { $$ = new Cst::Expr(Cst::Expr::BooleanConst(true),  make_loc(@$)); }
	| T_FALSE { $$ = new Cst::Expr(Cst::Expr::BooleanConst(false), make_loc(@$)); }

method_call_args
	: T_LPAREN T_RPAREN { $$ = new std::vector<Cst::Expr>; }
	| T_LPAREN method_call_arg_list T_RPAREN         { $$ = $2; }
	| T_LPAREN method_call_arg_list T_COMMA T_RPAREN { $$ = $2; }
	| T_LPAREN error T_RPAREN { $$ = new std::vector<Cst::Expr>; }
	;

method_call_arg_list
	: expr {
		$$ = new std::vector<Cst::Expr>;
		$$->emplace_back(move_and_delete($1));
	}
	| method_call_arg_list T_COMMA expr {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}
	;

stmt
	: block_stmt  { $$ = new Cst::Stmt(move_and_delete($1), make_loc(@$)); }
	| T_SEMICOLON { $$ = new Cst::Stmt(Cst::Stmt::Noop(), make_loc(@$)); }
	| T_LET variable_declaration_list T_SEMICOLON {
		$$ = new Cst::Stmt(
			Cst::Stmt::VariableDeclarations(move_and_delete($2)),
			make_loc(@$)
		);
	}
	| T_POOLS pool_declaration_list T_SEMICOLON {
		$$ = new Cst::Stmt(
			Cst::Stmt::PoolDeclarations(move_and_delete($2)),
			make_loc(@$)
		);
	}
	| T_POOL pool_declaration T_SEMICOLON {
		std::vector<Cst::PoolDeclaration> tmp;
		tmp.emplace_back(move_and_delete($2));
		$$ = new Cst::Stmt(
			Cst::Stmt::PoolDeclarations(std::move(tmp)),
			make_loc(@$)
		);
	}
	| expr T_ASSIGN expr T_SEMICOLON {
		$$ = new Cst::Stmt(
			Cst::Stmt::Assignment(move_and_delete($1), move_and_delete($3)),
			make_loc(@$)
		);
	}
	| expr op_assign expr T_SEMICOLON {
		$$ = new Cst::Stmt(
			Cst::Stmt::OpAssignment(move_and_delete($1), $2, move_and_delete($3)),
			make_loc(@$)
		);
	}
	| T_IF expr block_stmt else_branch {
		$$ = new Cst::Stmt(
			Cst::Stmt::If(
				move_and_delete($2),
				Cst::Stmt(move_and_delete($3), make_loc(@3)),
				move_and_delete($4)
			),
			make_loc(@$)
		);
	}
	| T_WHILE expr block_stmt {
		$$ = new Cst::Stmt(
			Cst::Stmt::While(
				move_and_delete($2),
				Cst::Stmt(move_and_delete($3), make_loc(@3))
			),
			make_loc(@$)
		);
	}
	| T_FOREACH identifier T_ASSIGN expr T_DOTDOT expr block_stmt {
		$$ = new Cst::Stmt(
			Cst::Stmt::ForeachRange(
				move_and_delete($2),
				move_and_delete($4),
				move_and_delete($6),
				Cst::Stmt(move_and_delete($7), make_loc(@7))
			),
			make_loc(@$)
		);
	}
	| T_FOREACH identifier T_COLON identifier block_stmt {
		$$ = new Cst::Stmt(
			Cst::Stmt::ForeachPool(
				move_and_delete($2),
				move_and_delete($4),
				Cst::Stmt(move_and_delete($5), make_loc(@5))
			),
			make_loc(@$)
		);
	}
	| T_BREAK T_SEMICOLON {
		$$ = new Cst::Stmt(Cst::Stmt::Break(), make_loc(@$));
	}
	| T_CONTINUE T_SEMICOLON {
		$$ = new Cst::Stmt(Cst::Stmt::Continue(), make_loc(@$));
	}
	| T_RETURN T_SEMICOLON {
		$$ = new Cst::Stmt(Cst::Stmt::ReturnVoid(), make_loc(@$));
	}
	| T_RETURN expr T_SEMICOLON {
		$$ = new Cst::Stmt(Cst::Stmt::Return(move_and_delete($2)), make_loc(@$));
	}
	| expr T_SEMICOLON {
		$$ = new Cst::Stmt(Cst::Stmt::ExprStmt(move_and_delete($1)), make_loc(@$));
	}
	/* Just make one up for error recovery */
	| error T_SEMICOLON {
		$$ = new Cst::Stmt(Cst::Stmt::Noop(), make_loc(@$));
	}

block_stmt
	: T_LBRACE stmt_list T_RBRACE { $$ = $2; }
	/* Just make one up for error recovery */
	| T_LBRACE error T_RBRACE { $$ = new Cst::Stmt::Block; }

stmt_list
	: %empty { $$ = new Cst::Stmt::Block; }
	| stmt_list stmt {
		$$ = $1;
		$$->add(move_and_delete($2));
	}

else_branch
	: %empty { $$ = new Cst::Stmt(Cst::Stmt::Noop(), Location()); }
	| T_ELSE block_stmt { $$ = new Cst::Stmt(move_and_delete($2), make_loc(@$)); }

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
	: T_LANGLE T_RANGLE { $$ = new std::vector<Cst::PoolParameter>; }
	| T_LANGLE pool_parameter_list T_RANGLE { $$ = $2; }
	| T_LANGLE pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE { $$ = new std::vector<Cst::PoolParameter>; }

pool_parameter_list
	: identifier {
		$$ = new std::vector<Cst::PoolParameter>;
		$$->emplace_back(Cst::PoolParameter::Pool(move_and_delete($1)), make_loc(@1));
	}
	| T_NONE {
		$$ = new std::vector<Cst::PoolParameter>;
		$$->emplace_back(Cst::PoolParameter::None(), make_loc(@1));
	}
	| pool_parameter_list T_COMMA identifier {
		$$ = $1;
		$$->emplace_back(Cst::PoolParameter::Pool(move_and_delete($3)), make_loc(@3));
	}
	| pool_parameter_list T_COMMA T_NONE {
		$$ = $1;
		$$->emplace_back(Cst::PoolParameter::None(), make_loc(@3));
	}

formal_pool_parameters
	: %empty { $$ = new std::vector<Cst::FormalPoolParameter>; }
	| T_LANGLE T_RANGLE { $$ = new std::vector<Cst::FormalPoolParameter>; }
	| T_LANGLE formal_pool_parameter_list T_RANGLE { $$ = $2; }
	| T_LANGLE formal_pool_parameter_list T_COMMA T_RANGLE { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LANGLE error T_RANGLE { $$ = new std::vector<Cst::FormalPoolParameter>; }

formal_pool_parameter_list
	: formal_pool_parameter {
		$$ = new std::vector<Cst::FormalPoolParameter>;
		$$->emplace_back(move_and_delete($1));
	}
	| formal_pool_parameter_list T_COMMA formal_pool_parameter {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

field_declarations
	: field_declaration_list T_SEMICOLON { $$ = $1; }

field_declaration_list
	: field_declaration {
		$$ = new std::vector<Cst::Field>;
		$$->emplace_back(move_and_delete($1));
	}
	| field_declaration_list T_COMMA field_declaration {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

field_declaration
	: identifier T_COLON type {
		$$ = new Cst::Field(move_and_delete($1), move_and_delete($3));
	}

method_declaration
	: T_FN identifier method_parameters T_COLON type block_stmt {
		$$ = new Cst::Method(
			move_and_delete($2),
			move_and_delete($3),
			move_and_delete($5),
			move_and_delete($6)
		);
	}
	| T_FN identifier method_parameters block_stmt {
		$$ = new Cst::Method(
			move_and_delete($2),
			move_and_delete($3),
			move_and_delete($4)
		);
	}

method_parameters
	: T_LPAREN T_RPAREN { $$ = new std::vector<Cst::MethodParameter>; }
	| T_LPAREN method_parameter_list T_RPAREN { $$ = $2; }
	| T_LPAREN method_parameter_list T_COMMA T_RPAREN { $$ = $2; }
	/* Just make up one for error recovery */
	| T_LPAREN error T_RPAREN { $$ = new std::vector<Cst::MethodParameter>; }

method_parameter_list
	: method_parameter {
		$$ = new std::vector<Cst::MethodParameter>;
		$$->emplace_back(move_and_delete($1));
	}
	| method_parameter_list T_COMMA method_parameter {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

method_parameter
	: identifier T_COLON type {
		$$ = new Cst::MethodParameter(
			move_and_delete($1), move_and_delete($3), make_loc(@$)
		);
	}

layout_definition
	: T_LAYOUT identifier T_COLON identifier T_ASSIGN clusters {
		$$ = new Cst::Layout(
			move_and_delete($2),
			move_and_delete($4),
			move_and_delete($6),
			make_loc(@2)
		);
	}

clusters
	: T_SEMICOLON { $$ = new std::vector<Cst::Cluster>; }
	| cluster_list T_SEMICOLON { $$ = $1; }
	/* Just make up one for error recovery */
	| error T_SEMICOLON { $$ = new std::vector<Cst::Cluster>; }

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
	: T_REC T_LBRACE cluster_field_list T_RBRACE {
		$$ = new Cst::Cluster(move_and_delete($3), make_loc(@3));
	}
	/* Just make up one for error recovery */
	| T_REC T_LBRACE error T_RBRACE { $$ = new Cst::Cluster; }

cluster_field_list
	: cluster_field {
		$$ = new std::vector<Cst::ClusterField>;
		$$->emplace_back(move_and_delete($1));
	}
	| cluster_field_list T_COMMA cluster_field {
		$$ = $1;
		$$->emplace_back(move_and_delete($3));
	}

cluster_field
	: identifier { $$ = new Cst::ClusterField(move_and_delete($1)); }

identifier
	: T_IDENT { $$ = new Cst::Identifier(move_and_delete($1), make_loc(@1)); }
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

	errors->add(make_loc(*orig_loc), std::string(msg));
}

static Location make_loc(const YYLTYPE& orig_loc)
{
	Location loc;
	loc.first_line   = orig_loc.first_line;
	loc.first_column = orig_loc.first_column;
	loc.last_line    = orig_loc.last_line;
	loc.last_column  = orig_loc.last_column;

	return loc;
}
