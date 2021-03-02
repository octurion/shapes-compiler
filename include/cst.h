#pragma once

#include "common.h"

#include "variant.h"

#include <iosfwd>
#include <memory>
#include <vector>
#include <utility>

namespace Cst
{

class Identifier
{
	std::string m_ident;
	Location m_loc;

public:
	Identifier() = default;
	explicit Identifier(std::string ident, const Location& loc)
		: m_ident(std::move(ident))
		, m_loc(std::move(loc))
	{}

	const std::string& ident() const { return m_ident; }
	const Location& loc() const { return m_loc; }
};

class FormalPoolParameter
{
	Identifier m_name;

public:
	explicit FormalPoolParameter(Identifier name)
		: m_name(std::move(name))
	{}

	const std::string& ident() const { return m_name.ident(); }
	const Location& loc() const { return m_name.loc(); }
};

class Pool
{
	Identifier m_name;

public:
	explicit Pool(Identifier name)
		: m_name(std::move(name))
	{}

	const std::string& ident() const { return m_name.ident(); }
	const Location& loc() const { return m_name.loc(); }
};
std::ostream& operator<<(std::ostream& os, const Pool& pool);

class None
{
	Location m_loc;

public:
	explicit None(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};
std::ostream& operator<<(std::ostream& os, const None& none);

using PoolParameter = mpark::variant<None, Pool>;
std::ostream& operator<<(std::ostream& os, const PoolParameter& param);

enum class PrimitiveKind
{
	BOOL, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64
};
std::ostream& operator<<(std::ostream& os, PrimitiveKind kind);

class PrimitiveType
{
	PrimitiveKind m_kind;
	Location m_loc;

public:
	explicit PrimitiveType(PrimitiveKind kind, const Location& loc)
		: m_kind(kind)
		, m_loc(loc)
	{}

	PrimitiveKind kind() const { return m_kind; }
	const Location& loc() const { return m_loc; }
};
std::ostream& operator<<(std::ostream& os, const PrimitiveType& type);

class ObjectType
{
	Identifier m_class_name;
	std::vector<PoolParameter> m_params;
	Location m_loc;

public:
	ObjectType(Identifier class_name,
			   std::vector<PoolParameter> params,
			   const Location& loc)
		: m_class_name(std::move(class_name))
		, m_params(std::move(params))
		, m_loc(loc)
	{}

	const Identifier& class_name() const { return m_class_name; }
	const Location& loc() const { return m_loc; }

	const std::vector<PoolParameter>& params() const { return m_params; }
};
std::ostream& operator<<(std::ostream& os, const ObjectType& type);

using Type = mpark::variant<PrimitiveType, ObjectType>;
std::ostream& operator<<(std::ostream& os, const Type& type);

class BoundType
{
	Identifier m_class_name;
	std::vector<Pool> m_params;

	Location m_loc;

public:
	BoundType() = default;
	BoundType(Identifier class_name, std::vector<Pool> params)
		: m_class_name(std::move(class_name))
		, m_params(std::move(params))
	{}

	const Identifier& class_name() const { return m_class_name; }
	const std::vector<Pool>& params() const { return m_params; }
	const Location& loc() const { return m_loc; }
};
std::ostream& operator<<(std::ostream& os, const BoundType& type);

class LayoutType
{
	Identifier m_layout_name;
	std::vector<PoolParameter> m_params;

	Location m_loc;

public:
	LayoutType() = default;
	LayoutType(Identifier layout_name,
			   std::vector<PoolParameter> params,
			   const Location& loc)
		: m_layout_name(std::move(layout_name))
		, m_params(std::move(params))
		, m_loc(loc)
	{}

	const Identifier& layout_name() const { return m_layout_name; }
	const std::vector<PoolParameter>& params() const { return m_params; }
	const Location& loc() const { return m_loc; }
};
std::ostream& operator<<(std::ostream& os, const LayoutType& type);

class FormalPoolBound
{
	FormalPoolParameter m_pool;
	BoundType m_type;

	Location m_loc;

public:
	FormalPoolBound(FormalPoolParameter pool, BoundType type, const Location& loc)
		: m_pool(std::move(pool))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const FormalPoolParameter& pool() const { return m_pool; }
	const BoundType& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class VariableDeclaration
{
	Identifier m_name;
	Type m_type;

	Location m_loc;

public:
	VariableDeclaration(Identifier name, Type type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const Identifier& name() const { return m_name; }
	const Type& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class PoolDeclaration
{
	Identifier m_name;
	LayoutType m_type;

	Location m_loc;

public:
	PoolDeclaration(Identifier name, LayoutType type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const Identifier& name() const { return m_name; }
	const LayoutType& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class IntegerConst;
class BooleanConst;
class NullExpr;
class ThisExpr;
class CastExpr;
class UnaryExpr;
class BinaryExpr;
class VariableExpr;
class MethodCall;
class MemberMethodCall;
class FieldAccess;
class NewExpr;

using Expr = mpark::variant<IntegerConst, BooleanConst, NullExpr, ThisExpr,
	  CastExpr, UnaryExpr, BinaryExpr, VariableExpr, MethodCall,
	  MemberMethodCall, FieldAccess, NewExpr>;
const Location& location(const Expr& expr);

enum class BinOp
{
	PLUS, MINUS, TIMES, DIV,
	LAND, LOR,
	AND, OR, XOR, SHL, SHR,
	EQ, NE, LT, LE, GT, GE
};

enum class UnOp { PLUS, MINUS, NOT };

class IntegerConst
{
	std::string m_value;
	Location m_loc;

public:
	explicit IntegerConst(std::string value, const Location& loc)
		: m_value(std::move(value))
		, m_loc(loc)
	{}

	const std::string& value() const { return m_value; }
	const Location& loc() const { return m_loc; }
};

class BooleanConst
{
	bool m_value;
	Location m_loc;

public:
	explicit BooleanConst(bool value, const Location& loc)
		: m_value(std::move(value))
		, m_loc(loc)
	{}

	bool value() const { return m_value; }
	const Location& loc() const { return m_loc; }
};

class NullExpr
{
	Location m_loc;

public:
	explicit NullExpr(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class ThisExpr
{
	Location m_loc;

public:
	explicit ThisExpr(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class CastExpr
{
	std::unique_ptr<Expr> m_expr;
	PrimitiveType m_type;

	Location m_loc;

public:
	CastExpr(Expr expr, PrimitiveType type, const Location& loc);

	const Expr& expr() const;
	const Location& loc() const { return m_loc; }
	const PrimitiveType& type() const { return m_type; }
};

class BinaryExpr
{
	std::unique_ptr<Expr> m_lhs;
	std::unique_ptr<Expr> m_rhs;
	BinOp m_op;
	Location m_loc;

public:
	BinaryExpr(Expr lhs, BinOp op, Expr rhs, const Location& loc);

	const Expr& lhs() const;
	const Expr& rhs() const;

	BinOp op() const { return m_op; }
	const Location& loc() const { return m_loc; }
};

class VariableExpr
{
	Identifier m_name;
	Location m_loc;

public:
	VariableExpr(Identifier name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const Identifier& name() const { return m_name; }
	const Location& loc() const { return m_loc; }
};

class UnaryExpr
{
	UnOp m_op;
	std::unique_ptr<Expr> m_expr;
	Location m_loc;

public:
	UnaryExpr(UnOp op, Expr expr, const Location& loc);

	const Expr& expr() const;
	UnOp op() const { return m_op; }
	const Location& loc() const { return m_loc; }
};

class MethodCall
{
	Identifier m_name;
	std::vector<Expr> m_params;
	Location m_loc;

public:
	MethodCall(Identifier name, std::vector<Expr> params, const Location& loc);

	const Identifier& name() const { return m_name; }
	const Location& loc() const { return m_loc; }

	const std::vector<Expr>& params() const { return m_params; }

	size_t num_params() const { return m_params.size(); }
};

class MemberMethodCall
{
	std::unique_ptr<Expr> m_this_expr;
	Identifier m_name;
	std::vector<Expr> m_args;
	Location m_loc;

public:
	MemberMethodCall(
			Expr this_expr,
			Identifier name,
			std::vector<Expr> args,
			const Location& loc);

	const Expr& this_expr() const;
	const Identifier& name() const { return m_name; }
	const Location& loc() const { return m_loc; }

	const std::vector<Expr>& args() const { return m_args; }
	size_t num_args() const { return m_args.size(); }
};

class FieldAccess
{
	std::unique_ptr<Expr> m_expr;
	Identifier m_field;
	Location m_loc;

public:
	FieldAccess(Expr expr, Identifier field, const Location& loc);

	const Expr& expr() const;
	const Identifier& field() const { return m_field; }
	const Location& loc() const { return m_loc; }
};

class NewExpr
{
	ObjectType m_type;
	Location m_loc;

public:
	explicit NewExpr(ObjectType type, const Location& loc)
		: m_type(std::move(type))
		, m_loc(loc)
	{}

	const ObjectType& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class Noop;
class VariableDeclarations;
class PoolDeclarations;
class Assignment;
class OpAssignment;
class IfStmt;
class WhileStmt;
class ForeachRange;
class ForeachPool;
class Block;
class ExprStmt;
class Break;
class Continue;
class Return;
class ReturnVoid;

using Stmt = mpark::variant<Noop, VariableDeclarations, PoolDeclarations,
	  Assignment, OpAssignment, IfStmt, WhileStmt, ForeachRange, ForeachPool, Block,
	  ExprStmt, Break, Continue, Return, ReturnVoid>;

class Noop {};

class VariableDeclarations
{
	std::vector<VariableDeclaration> m_vars;

public:
	explicit VariableDeclarations(std::vector<VariableDeclaration> vars)
		: m_vars(std::move(vars))
	{}

	const std::vector<VariableDeclaration>& vars() const { return m_vars; }
};

class PoolDeclarations
{
	std::vector<PoolDeclaration> m_pools;

public:
	explicit PoolDeclarations(std::vector<PoolDeclaration> pools)
		: m_pools(std::move(pools))
	{}

	const std::vector<PoolDeclaration>& pools() const { return m_pools; }
};

class Assignment
{
	std::unique_ptr<Expr> m_lhs;
	std::unique_ptr<Expr> m_rhs;

public:
	Assignment(Expr lhs, Expr rhs);

	const Expr& lhs() const { return *m_lhs; }
	const Expr& rhs() const { return *m_rhs; }
};

class OpAssignment
{
	std::unique_ptr<Expr> m_lhs;
	std::unique_ptr<Expr> m_rhs;
	BinOp m_op;

public:
	OpAssignment(Expr lhs, BinOp op, Expr rhs)
		: m_lhs(std::unique_ptr<Expr>(new Expr(std::move(lhs))))
		, m_rhs(std::unique_ptr<Expr>(new Expr(std::move(rhs))))
		, m_op(op)
	{}

	const Expr& lhs() const { return *m_lhs; }
	const Expr& rhs() const { return *m_rhs; }
	BinOp op() const { return m_op; }
};

class IfStmt
{
	Expr m_cond;
	std::unique_ptr<Stmt> m_then_branch;
	std::unique_ptr<Stmt> m_else_branch;

public:
	IfStmt(Expr cond, Stmt then_branch, Stmt else_branch);

	const Expr& cond() const { return m_cond; }

	const Stmt& then_branch() const;
	const Stmt& else_branch() const;
};

class WhileStmt
{
	Expr m_cond;
	std::unique_ptr<Stmt> m_body;

public:
	WhileStmt(Expr cond, Stmt body);

	const Expr& cond() const { return m_cond; }
	const Stmt& body() const;
};

class ForeachRange
{
	Identifier m_var;
	Expr m_range_begin;
	Expr m_range_end;
	std::unique_ptr<Stmt> m_body;

public:
	ForeachRange(Identifier var, Expr range_begin, Expr range_end, Stmt body);

	const Identifier& var() const { return m_var; }

	const Expr& range_begin() const { return m_range_begin; }
	const Expr& range_end() const { return m_range_end;   }

	const Stmt& body() const;
};

class ForeachPool
{
	Identifier m_var;
	Identifier m_pool;
	std::unique_ptr<Stmt> m_body;

public:
	ForeachPool(Identifier var, Identifier pool, Stmt body);

	const Identifier& var()  const { return m_var;  }
	const Identifier& pool() const { return m_pool; }

	const Stmt& body() const;
};

class Block
{
	std::vector<Stmt> m_stmts;

public:
	Block();
	explicit Block(std::vector<Stmt> stmts);

	const std::vector<Stmt>& stmts() const { return m_stmts; }

	void add(Stmt stmt);
};

class ExprStmt
{
	Expr m_expr;

public:
	explicit ExprStmt(Expr expr)
		: m_expr(std::move(expr))
	{}

	const Expr& expr() const { return m_expr; }
};

class Break
{
	Location m_loc;

public:
	explicit Break(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class Continue
{
	Location m_loc;

public:
	explicit Continue(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class Return
{
	Expr m_expr;

public:
	explicit Return(Expr expr)
		: m_expr(std::move(expr))
	{}

	const Expr& expr() const { return m_expr; }
};

class ReturnVoid
{
	Location m_loc;

public:
	explicit ReturnVoid(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class Field
{
	Identifier m_name;
	Type m_type;

public:
	Field(Identifier name, Type type)
		: m_name(std::move(name))
		, m_type(std::move(type))
	{}

	const Identifier& name() const { return m_name; }
	const Type&       type() const { return m_type; }
};

class MethodParameter
{
	Identifier m_name;
	Type m_type;

	Location m_loc;

public:
	MethodParameter(Identifier name, Type type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const Identifier& name() const { return m_name; }
	const Type& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class Method
{
	Identifier m_name;

	std::unique_ptr<Type> m_return_type;
	std::vector<MethodParameter> m_params;
	Block m_body;

public:
	Method(Identifier name, std::vector<MethodParameter> params, Type return_type, Block body)
		: m_name(std::move(name))
		, m_return_type(new Type(std::move(return_type)))
		, m_params(std::move(params))
		, m_body(std::move(body))
	{}

	Method(Identifier name, std::vector<MethodParameter> params, Block body)
		: m_name(std::move(name))
		, m_return_type()
		, m_params(std::move(params))
		, m_body(std::move(body))
	{}

	const Identifier& name() const { return m_name; }
	const Type* type() const { return m_return_type.get(); }
	const std::vector<MethodParameter>& params() const { return m_params; }
	const Block& body() const { return m_body; }
};

class Class
{
	Identifier m_name;

	std::vector<FormalPoolParameter> m_pool_params;
	std::vector<FormalPoolBound> m_pool_param_bounds;

	std::vector<Field> m_fields;
	std::vector<Method> m_methods;

public:
	Class(Identifier name,
			std::vector<FormalPoolParameter> pool_params,
			std::vector<FormalPoolBound> pool_param_bounds,
			std::vector<Field> fields,
			std::vector<Method> methods)
		: m_name(std::move(name))
		, m_pool_params(std::move(pool_params))
		, m_pool_param_bounds(std::move(pool_param_bounds))
		, m_fields(std::move(fields))
		, m_methods(std::move(methods))
	{
	}

	const Identifier& name() const { return m_name; }

	const std::vector<FormalPoolParameter>& pool_params() const { return m_pool_params; }
	const std::vector<FormalPoolBound>& pool_param_bounds() const { return m_pool_param_bounds; }
	const std::vector<Field>& fields() const { return m_fields; }
	const std::vector<Method>& methods() const { return m_methods; }
};

class ClassBody
{
	std::vector<Field> m_fields;
	std::vector<Method> m_methods;

public:
	ClassBody() = default;
	ClassBody(std::vector<Field> fields, std::vector<Method> methods)
		: m_fields(std::move(fields))
		, m_methods(std::move(methods))
	{
	}

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.begin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.end();   }

	decltype(m_methods)::const_iterator methods_begin() const { return m_methods.begin(); }
	decltype(m_methods)::const_iterator methods_end()   const { return m_methods.end();   }

	void add_field(Field field)
	{
		m_fields.emplace_back(std::move(field));
	}

	void add_method(Method method)
	{
		m_methods.emplace_back(std::move(method));
	}

	std::vector<Field>&& consume_fields()   { return std::move(m_fields);  }
	std::vector<Method>&& consume_methods() { return std::move(m_methods); }
};

class ClusterField
{
	Identifier m_name;

public:
	explicit ClusterField(Identifier name)
		: m_name(std::move(name))
	{}

	const std::string& ident() const { return m_name.ident(); }
	const Location& loc() const { return m_name.loc(); }
};

class Cluster
{
	std::vector<ClusterField> m_fields;
	Location m_loc;

public:
	Cluster() = default;
	explicit Cluster(std::vector<ClusterField> fields, const Location& loc)
		: m_fields(std::move(fields))
		, m_loc(loc)
	{}

	const std::vector<ClusterField>& fields() const { return m_fields; }
	const Location& loc() const { return m_loc; }
};

class Layout
{
	Identifier m_name;
	Identifier m_class;
	std::vector<Cluster> m_clusters;

	Location m_loc;

public:
	Layout(Identifier name,
		   Identifier for_class,
		   std::vector<Cluster> clusters,
		   const Location& loc)
		: m_name(std::move(name))
		, m_class(std::move(for_class))
		, m_clusters(std::move(clusters))
		, m_loc(loc)
	{}

	const Identifier& name() const { return m_name; }
	const Identifier& for_class() const { return m_class; }

	const std::vector<Cluster>& clusters() const { return m_clusters; }

	const Location& loc() { return m_loc; }
};

class Program
{
	std::vector<Class> m_classes;
	std::vector<Layout> m_layouts;

public:
	Program() = default;
	Program(std::vector<Class> classes, std::vector<Layout> layouts)
		: m_classes(std::move(classes))
		, m_layouts(std::move(layouts))
	{
	}

	const std::vector<Class>& classes() const { return m_classes; }
	const std::vector<Layout>& layouts() const { return m_layouts; }

	void add_class(Class new_class) {
		m_classes.emplace_back(std::move(new_class));
	}

	void add_layout(Layout new_layout) {
		m_layouts.emplace_back(std::move(new_layout));
	}
};

} // namespace Cst
