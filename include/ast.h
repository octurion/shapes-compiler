#pragma once

#include <memory>
#include <string>
#include <vector>
#include <utility>

#include <cstddef>
#include <cstdint>

constexpr size_t NONE_IDENT = -1;

struct Location {
	int first_line = 1;
	int first_column = 1;
	int last_line = 1;
	int last_column = 1;
};

struct SyntaxError
{
	std::string message;
	Location loc;

	SyntaxError(std::string message, Location loc)
		: message(std::move(message))
		, loc(loc)
	{
	}
};

struct SemanticError
{
	// TODO: Fill this up for the sematic analysis section
};

struct ErrorList {
	std::vector<SyntaxError> syntax_errors;
	std::vector<SemanticError> semantic_errors;
};

#if 0
class Visitor;

class BaseVisitable
{
	virtual void accept(Visitor& v) = 0;
};

template<class T>
struct Visitable: public BaseVisitable
{
	void accept(Visitor& v) override
	{
		v.visit(static_cast<T&>(*this));
	}
};
#endif

struct Identifier
{
	std::string ident;
	Location loc;

	Identifier() = default;
	Identifier(std::string ident, Location loc)
		: ident(std::move(ident))
		, loc(loc)
	{}
};

struct Number
{
	std::string num;
	Location loc;

	Number() = default;
	Number(std::string num, Location loc)
		: num(std::move(num))
		, loc(loc)
	{}
};

struct VariableRef
{
	size_t id;
	Location loc;
};

struct Type {
	virtual ~Type() {}
};

enum class PrimitiveKind
{
	BOOL, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64
};

struct InvalidType: public Type {};
struct VoidType: public Type {};
struct PrimitiveType: public Type
{
	PrimitiveKind kind;

	PrimitiveType(PrimitiveKind kind)
		: kind(kind)
	{
	}
};
struct TmpClassType: public Type
{
	enum class Kind
	{
		CLASS_LAYOUT, BOUND
	};

	Identifier name;
	std::vector<Identifier> pool_parameters;
	Kind kind;

	TmpClassType(Identifier name, std::vector<Identifier> pool_parameters, Kind kind)
		: name(std::move(name))
		, pool_parameters(std::move(pool_parameters))
		, kind(kind)
	{
	}
};
struct ReferenceType: public Type
{
	std::unique_ptr<Type> type;

	ReferenceType(Type* type)
		: type(type)
	{
	}
};

struct VariableDecl
{
	Identifier name;
	std::unique_ptr<Type> type;

	VariableDecl(Identifier name, Type* type)
		: name(std::move(name))
		, type(type)
	{}
};

enum class BinOp
{
	PLUS, MINUS, TIMES, DIV, LAND, LOR, AND, OR, XOR, SHL, SHR, EQ, NE, LT, LE, GT, GE
};
enum class UnOp
{
	PLUS, MINUS, NOT
};

struct Expr {
	virtual ~Expr() {}
};

struct InvalidExpr: public Expr {};
struct ThisExpr: public Expr {};
struct NullExpr: public Expr {};
struct BinaryExpr: public Expr
{
	std::unique_ptr<Expr> lhs;
	BinOp op;
	std::unique_ptr<Expr> rhs;

	BinaryExpr(Expr* lhs, BinOp op, Expr* rhs)
		: lhs(lhs)
		, op(op)
		, rhs(rhs)
	{
	}
};

struct UnaryExpr: public Expr
{
	UnOp op;
	std::unique_ptr<Expr> expr;

	UnaryExpr(UnOp op, Expr* expr)
		: op(op)
		, expr(expr)
	{}
};
struct IndexExpr: public Expr
{
	std::unique_ptr<Expr> expr;
	std::unique_ptr<Expr> idx;

	IndexExpr(Expr* expr, Expr* idx)
		: expr(expr)
		, idx(idx)
	{}
};
struct FieldExpr: public Expr
{
	std::unique_ptr<Expr> expr;
	Identifier field;

	FieldExpr(Expr* expr, Identifier field)
		: expr(expr)
		, field(std::move(field))
	{}
};
struct IdentifierExpr: public Expr
{
	Identifier ident;

	IdentifierExpr(Identifier ident)
		: ident(std::move(ident))
	{}
};
struct IntConst: public Expr
{
	uint64_t val;

	IntConst(uint64_t val)
		: val(val)
	{}
};
struct CastExpr: public Expr
{
	std::unique_ptr<Expr> expr;
	std::unique_ptr<Type> type;

	CastExpr(Expr* expr, Type* type)
		: expr(expr)
		, type(type)
	{}
};
struct NewExpr: public Expr
{
	std::unique_ptr<Type> type;

	NewExpr(Type* type)
		: type(type)
	{}
};

struct Stmt {
	virtual ~Stmt() {}
};

struct NoopStmt: public Stmt {};

struct VariableDeclsStmt: public Stmt
{
	enum class Kind { POOLS, VARS };

	std::vector<VariableDecl> decls;
	Kind kind;

	VariableDeclsStmt(std::vector<VariableDecl> decls, Kind kind)
		: decls(std::move(decls))
		, kind(kind)
	{}
};

struct AssignStmt: public Stmt
{
	std::unique_ptr<Expr> lhs;
	std::unique_ptr<Expr> rhs;

	AssignStmt(Expr* lhs, Expr* rhs)
		: lhs(lhs)
		, rhs(rhs)
	{}
};

struct OpAssignStmt: public Stmt
{
	std::unique_ptr<Expr> lhs;
	BinOp op;
	std::unique_ptr<Expr> rhs;

	OpAssignStmt(Expr* lhs, BinOp op, Expr* rhs)
		: lhs(lhs)
		, op(op)
		, rhs(rhs)
	{}
};

struct IfStmt: public Stmt
{
	std::unique_ptr<Expr> cond;
	std::unique_ptr<Stmt> then_branch;
	std::unique_ptr<Stmt> else_branch;

	IfStmt(Expr* cond, Stmt* then_branch, Stmt* else_branch)
		: cond(cond)
		, then_branch(then_branch)
		, else_branch(else_branch)
	{}
};

struct WhileStmt: public Stmt
{
	std::unique_ptr<Expr> cond;
	std::unique_ptr<Stmt> body;

	WhileStmt(Expr* cond, Stmt* body)
		: cond(cond)
		, body(body)
	{}
};

struct ForeachRangeStmt: public Stmt
{
	Identifier ident;
	std::unique_ptr<Expr> begin;
	std::unique_ptr<Expr> end;
	std::unique_ptr<Stmt> body;

	ForeachRangeStmt(Identifier ident, Expr* begin, Expr* end, Stmt* body)
		: ident(std::move(ident))
		, begin(begin)
		, end(end)
		, body(body)
	{}
};

struct ForeachPoolStmt: public Stmt
{
	Identifier var;
	Identifier pool;
	std::unique_ptr<Stmt> body;

	ForeachPoolStmt(Identifier var, Identifier pool, Stmt* body)
		: var(std::move(var))
		, pool(std::move(pool))
		, body(body)
	{}
};

struct BlockStmt: public Stmt
{
	std::vector<std::unique_ptr<Stmt>> stmts;
};

struct ExprStmt: public Stmt
{
	std::unique_ptr<Expr> expr;

	ExprStmt(Expr* expr)
		: expr(expr)
	{}
};

struct BreakStmt: public Stmt {};

struct ContinueStmt: public Stmt {};

struct ReturnVoidStmt: public Stmt {};

struct ReturnStmt: public Stmt
{
	std::unique_ptr<Expr> expr;

	ReturnStmt(Expr* expr)
		: expr(expr)
	{}
};

struct Method
{
	Identifier name;
	std::vector<VariableDecl> arguments;
	std::unique_ptr<Type> return_type;
	std::vector<std::unique_ptr<Stmt>> statements;

	Method( Identifier name,
			std::vector<VariableDecl> arguments,
			Type* return_type,
			std::vector<std::unique_ptr<Stmt>> statements)
		: name(std::move(name))
		, arguments(std::move(arguments))
		, return_type(return_type)
		, statements(std::move(statements))
	{}
};

struct TmpClassBody
{
	std::vector<VariableDecl> fields;
	std::vector<Method> methods;
};

struct Class
{
	Identifier name;
	std::vector<Identifier> pool_parameters;
	std::vector<VariableDecl> pool_bounds;
	std::vector<VariableDecl> fields;
	std::vector<Method> methods;

	Class(
			Identifier name,
			std::vector<Identifier> pool_parameters,
			std::vector<VariableDecl> pool_bounds,
			TmpClassBody body)
		: name(name)
		, pool_parameters(std::move(pool_parameters))
		, pool_bounds(std::move(pool_bounds))
		, fields(std::move(body.fields))
		, methods(std::move(body.methods))
	{
	}
};

struct Cluster
{
	std::vector<Identifier> fields;

	Cluster() = default;
	Cluster(std::vector<Identifier> fields)
		: fields(std::move(fields))
	{}
};

struct Layout
{
	Identifier name;
	Identifier class_name;
	std::vector<Cluster> clusters;

	Layout(Identifier name, Identifier class_name, std::vector<Cluster> clusters)
		: name(std::move(name))
		, class_name(std::move(class_name))
		, clusters(std::move(clusters))
	{}
};

struct Cst
{
	std::vector<Class> classes;
	std::vector<Layout> layouts;
};

class Visitor
{
	virtual void visit(Identifier& ident);
	virtual void visit(Number& ident);
};
