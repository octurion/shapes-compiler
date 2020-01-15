#pragma once

#include <deque>
#include <memory>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
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
	std::string message;
	Location loc;

	SemanticError(std::string message, Location loc)
		: message(std::move(message))
		, loc(loc)
	{
	}
};

struct ErrorList {
	std::vector<SyntaxError> syntax_errors;
	std::vector<SemanticError> semantic_errors;
};

struct Identifier;
struct Number;
struct VariableDecl;

struct InvalidType;
struct VoidType;
struct PrimitiveType;
struct TmpClassType;
struct ClassType;
struct LayoutType;
struct BoundType;

struct InvalidExpr;
struct ThisExpr;
struct NullExpr;
struct BinaryExpr;
struct UnaryExpr;
struct FieldExpr;
struct IdentifierExpr;
struct IntConst;
struct NewExpr;

struct NoopStmt;
struct VariableDeclsStmt;
struct AssignStmt;
struct OpAssignStmt;
struct IfStmt;
struct WhileStmt;
struct ForeachRangeStmt;
struct ForeachPoolStmt;
struct BlockStmt;
struct ExprStmt;
struct BreakStmt;
struct ContinueStmt;
struct ReturnVoidStmt;
struct ReturnStmt;

struct Method;
struct Class;
struct Field;
struct Layout;
struct Cluster;

struct Ast;

struct Visitor
{
	virtual void visit(Identifier&)   { }
	virtual void visit(Number&)       { }
	virtual void visit(VariableDecl&) { }

	virtual void visit(InvalidType&)   { }
	virtual void visit(VoidType&)      { }
	virtual void visit(PrimitiveType&) { }
	virtual void visit(TmpClassType&)  { }
	virtual void visit(ClassType&)     { }
	virtual void visit(LayoutType&)    { }
	virtual void visit(BoundType&)     { }

	virtual void visit(InvalidExpr&)    { }
	virtual void visit(ThisExpr&)       { }
	virtual void visit(NullExpr&)       { }
	virtual void visit(BinaryExpr&)     { }
	virtual void visit(UnaryExpr&)      { }
	virtual void visit(FieldExpr&)      { }
	virtual void visit(IdentifierExpr&) { }
	virtual void visit(IntConst&)       { }
	virtual void visit(NewExpr&)        { }

	virtual void visit(NoopStmt&)          { }
	virtual void visit(VariableDeclsStmt&) { }
	virtual void visit(AssignStmt&)        { }
	virtual void visit(OpAssignStmt&)      { }
	virtual void visit(IfStmt&)            { }
	virtual void visit(WhileStmt&)         { }
	virtual void visit(ForeachRangeStmt&)  { }
	virtual void visit(ForeachPoolStmt&)   { }
	virtual void visit(BlockStmt&)         { }
	virtual void visit(ExprStmt&)          { }
	virtual void visit(BreakStmt&)         { }
	virtual void visit(ContinueStmt&)      { }
	virtual void visit(ReturnVoidStmt&)    { }
	virtual void visit(ReturnStmt&)        { }

	virtual void visit(Method&)  { }
	virtual void visit(Class&)   { }
	virtual void visit(Field&)   { }
	virtual void visit(Layout&)  { }
	virtual void visit(Cluster&) { }

	virtual void visit(Ast&) { }
};

struct BaseVisitable
{
	virtual void accept(Visitor& v) = 0;
};
template<typename T>
struct Visitable: public BaseVisitable
{
	void accept(Visitor& v) override { v.visit(static_cast<T&>(*this)); }
};

struct Identifier: public Visitable<Identifier>
{
	std::string ident;
	Location loc;

	Identifier() = default;
	Identifier(std::string ident, Location loc)
		: ident(std::move(ident))
		, loc(loc)
	{}
};

struct Number: public Visitable<Number>
{
	std::string num;
	Location loc;

	Number() = default;
	Number(std::string num, Location loc)
		: num(std::move(num))
		, loc(loc)
	{}
};

struct Type: public BaseVisitable {
	virtual ~Type() {}
};

enum class PrimitiveKind
{
	BOOL, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64
};

struct InvalidType: public Type {
	void accept(Visitor& v) override { v.visit(*this); }
};
struct VoidType: public Type {
	void accept(Visitor& v) override { v.visit(*this); }
};
struct PrimitiveType: public Type
{
	PrimitiveKind kind;

	PrimitiveType(PrimitiveKind kind)
		: kind(kind)
	{
	}

	void accept(Visitor& v) override { v.visit(*this); }
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

	void accept(Visitor& v) override { v.visit(*this); }
};
struct ClassType: public Type
{
	Class* clazz;
	std::vector<VariableDecl*> pool_parameters;

	void accept(Visitor& v) override { v.visit(*this); }
};
struct BoundType: public Type
{
	Class* clazz;
	std::vector<VariableDecl*> pool_parameters;

	void accept(Visitor& v) override { v.visit(*this); }
};
struct LayoutType: public Type
{
	Layout* layout;
	std::vector<VariableDecl*> pool_parameters;

	void accept(Visitor& v) override { v.visit(*this); }
};

struct VariableDecl: public Visitable<VariableDecl>
{
	Identifier name;
	std::unique_ptr<Type> type;

	VariableDecl(Identifier name, Type* type = nullptr)
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

struct Expr: public BaseVisitable {
	virtual bool is_lvalue() { return false; }
	virtual ~Expr() {}
};

struct InvalidExpr: public Expr {
	void accept(Visitor& v) override { v.visit(*this); }
};
struct ThisExpr: public Expr {
	bool is_lvalue() override {
		return true;
	}

	void accept(Visitor& v) override { v.visit(*this); }
};
struct NullExpr: public Expr {
	void accept(Visitor& v) override { v.visit(*this); }
};
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

	void accept(Visitor& v) override { v.visit(*this); }
};

struct UnaryExpr: public Expr, public Visitable<UnaryExpr>
{
	UnOp op;
	std::unique_ptr<Expr> expr;

	UnaryExpr(UnOp op, Expr* expr)
		: op(op)
		, expr(expr)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};
struct FieldExpr: public Expr, public Visitable<FieldExpr>
{
	std::unique_ptr<Expr> expr;
	Identifier field;

	bool is_lvalue() override {
		return true;
	}

	FieldExpr(Expr* expr, Identifier field)
		: expr(expr)
		, field(std::move(field))
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};
struct IdentifierExpr: public Expr, public Visitable<IdentifierExpr>
{
	Identifier ident;
	VariableDecl* var = nullptr;

	bool is_lvalue() override {
		return true;
	}

	IdentifierExpr(Identifier ident)
		: ident(std::move(ident))
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};
struct IntConst: public Expr, public Visitable<IntConst>
{
	uint64_t val;

	IntConst(uint64_t val)
		: val(val)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};
struct NewExpr: public Expr, public Visitable<NewExpr>
{
	std::unique_ptr<Type> type;

	bool is_lvalue() override {
		return true;
	}

	NewExpr(Type* type)
		: type(type)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};

struct Stmt: public BaseVisitable {
	virtual ~Stmt() {}
};

struct NoopStmt: public Stmt {
	void accept(Visitor& v) override { v.visit(*this); }
};

struct VariableDeclsStmt: public Stmt
{
	enum class Kind { POOLS, VARS };

	std::vector<VariableDecl> decls;
	Kind kind;

	VariableDeclsStmt(std::vector<VariableDecl> decls, Kind kind)
		: decls(std::move(decls))
		, kind(kind)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};

struct AssignStmt: public Stmt
{
	std::unique_ptr<Expr> lhs;
	std::unique_ptr<Expr> rhs;

	AssignStmt(Expr* lhs, Expr* rhs)
		: lhs(lhs)
		, rhs(rhs)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
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

	void accept(Visitor& v) override { v.visit(*this); }
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

	void accept(Visitor& v) override { v.visit(*this); }
};

struct WhileStmt: public Stmt
{
	std::unique_ptr<Expr> cond;
	std::unique_ptr<Stmt> body;

	WhileStmt(Expr* cond, Stmt* body)
		: cond(cond)
		, body(body)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
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

	void accept(Visitor& v) override { v.visit(*this); }
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

	void accept(Visitor& v) override { v.visit(*this); }
};

struct BlockStmt: public Stmt
{
	std::vector<std::unique_ptr<Stmt>> stmts;

	void accept(Visitor& v) override { v.visit(*this); }
};

struct ExprStmt: public Stmt
{
	std::unique_ptr<Expr> expr;

	ExprStmt(Expr* expr)
		: expr(expr)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};

struct BreakStmt: public Stmt {
	void accept(Visitor& v) override { v.visit(*this); }
};

struct ContinueStmt: public Stmt {
	void accept(Visitor& v) override { v.visit(*this); }
};

struct ReturnVoidStmt: public Stmt {
	void accept(Visitor& v) override { v.visit(*this); }
};

struct ReturnStmt: public Stmt
{
	std::unique_ptr<Expr> expr;

	ReturnStmt(Expr* expr)
		: expr(expr)
	{}

	void accept(Visitor& v) override { v.visit(*this); }
};

struct Field: public Visitable<Field>
{
	Identifier name;
};

struct Method: public Visitable<Method>
{
	Identifier name;
	std::vector<VariableDecl> arguments;
	std::unique_ptr<Type> return_type;
	std::vector<std::unique_ptr<Stmt>> statements;

	std::vector<VariableDecl*> ast_arguments;
	std::deque<VariableDecl> local_variables;
	std::deque<VariableDecl> pool_variables;

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

struct Class: public Visitable<Class>
{
	Identifier name;

	std::vector<Identifier> pool_parameters;
	std::vector<VariableDecl> pool_bounds;
	std::vector<VariableDecl> fields;
	std::vector<Method> methods;

	std::unordered_map<std::string, VariableDecl> ast_pool_parameters;
	std::unordered_map<std::string, VariableDecl*> ast_fields;
	std::unordered_map<std::string, Method*> ast_methods;

	Class(  Identifier name,
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

struct Cluster: public Visitable<Cluster>
{
	std::vector<Identifier> fields;

	std::vector<VariableDecl*> field_refs;

	Cluster() = default;
	Cluster(std::vector<Identifier> fields)
		: fields(std::move(fields))
	{}
};

struct Layout: public Visitable<Layout>
{
	Identifier name;
	Identifier class_name;
	std::vector<Cluster> clusters;

	Class* clazz;

	Layout(Identifier name, Identifier class_name, std::vector<Cluster> clusters)
		: name(std::move(name))
		, class_name(std::move(class_name))
		, clusters(std::move(clusters))
	{}
};

struct Ast: public Visitable<Ast>
{
	std::vector<Class> classes;
	std::vector<Layout> layouts;

	std::unordered_map<std::string, Class*> ast_classes;
	std::unordered_map<std::string, Layout*> ast_layouts;
};

extern void run_semantic_analysis(Ast& ast, ErrorList& errors);
