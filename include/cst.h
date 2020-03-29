#pragma once

#include "cst_decls.h"

#include <memory>
#include <vector>
#include <utility>

namespace Cst
{
#define DEFINE_VISITOR_DISPATCH \
	void accept(Visitor& v) const override { v.visit(*this); }

class Visitable
{
public:
	virtual void accept(Visitor& v) const = 0;
	virtual ~Visitable() {}
};

class PoolParameter: public Visitable
{
	virtual const Location& loc() const = 0;
};
class Expr: public Visitable {
	virtual const Location& loc() const = 0;
};
class Stmt: public Visitable {};
class Type: public Visitable {};

enum class BinOp
{
	PLUS, MINUS, TIMES, DIV, LAND, LOR, AND, OR, XOR, SHL, SHR, EQ, NE, LT, LE, GT, GE
};

enum class UnOp { PLUS, MINUS, NOT };

class Identifier: public PoolParameter
{
	std::string m_ident;
	Location m_loc;

public:
	explicit Identifier(std::string ident, const Location& loc = Location())
		: m_ident(std::move(ident))
		, m_loc(std::move(loc))
	{}

	const std::string& ident() const { return m_ident; }

	const Location& loc() const override { return m_loc; }

	void set_loc(const Location& loc) { m_loc = loc; }

	DEFINE_VISITOR_DISPATCH
};

class NoneParam: public PoolParameter
{
	Location m_loc;

public:
	explicit NoneParam(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class InvalidType: public Type
{
public:
	DEFINE_VISITOR_DISPATCH
};

class PrimitiveType: public Type
{
public:
	enum class Kind { I8, U8, I16, U16, I32, U32, I64, U64, F32, F64 };

private:
	Cst::PrimitiveType::Kind m_kind;

public:
	explicit PrimitiveType(Cst::PrimitiveType::Kind kind)
		: m_kind(kind)
	{}

	Cst::PrimitiveType::Kind kind() const { return m_kind; }

	DEFINE_VISITOR_DISPATCH
};

class ClassType: public Type
{
	Cst::Identifier m_class_name;
	std::vector<std::unique_ptr<Cst::PoolParameter>> m_pool_params;

public:
	ClassType(Cst::Identifier class_name,
			  std::vector<std::unique_ptr<Cst::PoolParameter>> pool_params)
		: m_class_name(std::move(class_name))
		, m_pool_params(std::move(pool_params))
	{}

	const Cst::Identifier& class_name() const { return m_class_name; }

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.cbegin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class BoundType: public Type
{
	Cst::Identifier m_class_name;
	std::vector<std::unique_ptr<Cst::PoolParameter>> m_pool_params;

public:
	BoundType(Cst::Identifier class_name,
			  std::vector<std::unique_ptr<Cst::PoolParameter>> pool_params)
		: m_class_name(std::move(class_name))
		, m_pool_params(std::move(pool_params))
	{}

	const Cst::Identifier& class_name() const { return m_class_name; }

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.cbegin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class Variable: public Visitable
{
	Cst::Identifier m_name;
	std::unique_ptr<Cst::Type> m_type;

public:
	Variable(Cst::Identifier name, std::unique_ptr<Cst::Type> type)
		: m_name(std::move(name))
		, m_type(std::move(type))
	{
	}

	const Cst::Identifier& name() const { return m_name;  }
	const Cst::Type&       type() const { return *m_type; }

	Cst::Identifier&&            consume_name() { return std::move(m_name); }
	std::unique_ptr<Cst::Type>&& consume_type() { return std::move(m_type); }

	DEFINE_VISITOR_DISPATCH
};

class IntegerConst: public Expr
{
	std::string m_value;
	Location m_loc;

public:
	explicit IntegerConst(std::string value, const Location& loc = Location())
		: m_value(std::move(value))
		, m_loc(std::move(loc))
	{}

	const std::string& value() const { return m_value; }

	const Location& loc() const override { return m_loc; }
	void set_loc(const Location& loc) { m_loc = loc; }

	DEFINE_VISITOR_DISPATCH
};

class NullExpr: public Expr
{
	Location m_loc;

public:
	explicit NullExpr(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class ThisExpr: public Expr
{
	Location m_loc;

public:
	explicit ThisExpr(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class BinaryExpr: public Expr
{
	std::unique_ptr<Expr> m_lhs;
	Cst::BinOp m_op;
	std::unique_ptr<Expr> m_rhs;
	Location m_loc;

public:
	BinaryExpr(std::unique_ptr<Cst::Expr> lhs,
			   Cst::BinOp op,
			   std::unique_ptr<Cst::Expr> rhs,
			   const Location& loc = Location())
		: m_lhs(std::move(lhs))
		, m_op(op)
		, m_rhs(std::move(rhs))
		, m_loc(loc)
	{}

	const Expr& lhs() const { return *m_lhs; }
	const Expr& rhs() const { return *m_rhs; }

	Cst::BinOp op() const { return m_op; }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class IdentifierExpr: public Expr
{
	Cst::Identifier m_name;

public:
	explicit IdentifierExpr(Cst::Identifier name)
		: m_name(std::move(name))
	{}

	const Cst::Identifier& name() const { return m_name; }

	const Location& loc() const override { return m_name.loc(); }

	DEFINE_VISITOR_DISPATCH
};

class UnaryExpr: public Expr
{
	Cst::UnOp m_op;
	std::unique_ptr<Expr> m_expr;
	Location m_loc;

public:
	UnaryExpr(Cst::UnOp op, std::unique_ptr<Cst::Expr> expr, const Location& loc = Location())
		: m_op(op)
		, m_expr(std::move(expr))
		, m_loc(loc)
	{}

	const Expr& expr() const { return *m_expr; }

	Cst::UnOp op() const { return m_op; }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class IndexExpr: public Expr
{
	std::unique_ptr<Cst::Expr> m_expr;
	std::unique_ptr<Cst::Expr> m_idx;
	Location m_loc;

public:
	IndexExpr(std::unique_ptr<Cst::Expr> expr, std::unique_ptr<Cst::Expr> idx, const Location& loc = Location())
		: m_expr(std::move(expr))
		, m_idx(std::move(idx))
		, m_loc(loc)
	{}

	const Cst::Expr& expr() const { return *m_expr; }
	const Cst::Expr& idx()  const { return *m_idx; }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class MethodCall: public Expr
{
	Cst::Identifier m_name;
	std::vector<std::unique_ptr<Cst::Expr>> m_params;
	Location m_loc;

public:
	explicit MethodCall(Cst::Identifier name,
						std::vector<std::unique_ptr<Cst::Expr>> expr,
						const Location& loc = Location())
		: m_name(std::move(name))
		, m_params(std::move(expr))
		, m_loc(loc)
	{}

	const Cst::Identifier& name() const { return m_name; }

	decltype(m_params)::const_iterator params_begin() const { return m_params.cbegin(); }
	decltype(m_params)::const_iterator params_end()   const { return m_params.cend();   }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class MemberMethodCall: public Expr
{
	std::unique_ptr<Expr> m_this_expr;
	Cst::Identifier m_name;
	std::vector<std::unique_ptr<Expr>> m_params;
	Location m_loc;

public:
	MemberMethodCall(std::unique_ptr<Expr> this_expr,
					 Cst::Identifier name,
					 std::vector<std::unique_ptr<Cst::Expr>> params,
					 const Location& loc = Location())
		: m_this_expr(std::move(this_expr))
		, m_name(std::move(name))
		, m_params(std::move(params))
		, m_loc(loc)
	{}

	const Expr& this_expr() const { return *m_this_expr; }

	const Cst::Identifier& name() const { return m_name; }

	decltype(m_params)::const_iterator params_begin() const { return m_params.cbegin(); }
	decltype(m_params)::const_iterator params_end()   const { return m_params.cend();   }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class FieldAccess: public Expr
{
	std::unique_ptr<Expr> m_expr;
	Cst::Identifier m_field;
	Location m_loc;

public:
	FieldAccess(std::unique_ptr<Expr> expr,
				Cst::Identifier field,
				const Location& loc = Location())
		: m_expr(std::move(expr))
		, m_field(std::move(field))
		, m_loc(loc)
	{}

	const Expr& expr() const { return *m_expr; }

	const Cst::Identifier& field() const { return m_field; }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class NewExpr: public Expr
{
	std::unique_ptr<Type> m_type;
	Location m_loc;

public:
	NewExpr(std::unique_ptr<Type> type,
			const Location& loc = Location())
		: m_type(std::move(type))
		, m_loc(loc)
	{}

	const Type& type() const { return *m_type; }

	const Location& loc() const override { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class InvalidExpr: public Expr
{
public:
	DEFINE_VISITOR_DISPATCH
};

class NoopStmt: public Stmt
{
public:
	DEFINE_VISITOR_DISPATCH
};

class VariableDeclsStmt: public Stmt
{
public:
	enum class Kind { POOLS, VARS };

private:
	std::vector<Cst::Variable> m_vars;
	Kind m_kind;

public:
	explicit VariableDeclsStmt(std::vector<Cst::Variable> vars, Kind kind)
		: m_vars(std::move(vars))
		, m_kind(kind)
	{}

	Kind kind() const { return m_kind; }

	decltype(m_vars)::const_iterator begin() const { return m_vars.cbegin(); }
	decltype(m_vars)::const_iterator end()   const { return m_vars.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class AssignStmt: public Stmt
{
	std::unique_ptr<Cst::Expr> m_lhs;
	std::unique_ptr<Cst::Expr> m_rhs;

public:
	AssignStmt(std::unique_ptr<Cst::Expr> lhs,
			   std::unique_ptr<Cst::Expr> rhs)
		: m_lhs(std::move(lhs))
		, m_rhs(std::move(rhs))
	{}

	const Cst::Expr& lhs() const { return *m_lhs; }
	const Cst::Expr& rhs() const { return *m_rhs; }

	DEFINE_VISITOR_DISPATCH
};

class OpAssignStmt: public Stmt
{
	std::unique_ptr<Cst::Expr> m_lhs;
	Cst::BinOp m_op;
	std::unique_ptr<Cst::Expr> m_rhs;

public:
	OpAssignStmt(std::unique_ptr<Cst::Expr> lhs,
				 Cst::BinOp op,
				 std::unique_ptr<Cst::Expr> rhs)
		: m_lhs(std::move(lhs))
		, m_op(op)
		, m_rhs(std::move(rhs))
	{}

	const Cst::Expr& lhs() const { return *m_lhs; }
	const Cst::Expr& rhs() const { return *m_rhs; }

	Cst::BinOp op() const { return m_op; }

	DEFINE_VISITOR_DISPATCH
};

class IfStmt: public Stmt
{
	std::unique_ptr<Cst::Expr> m_cond;
	std::unique_ptr<Cst::Stmt> m_then_branch;
	std::unique_ptr<Cst::Stmt> m_else_branch;

public:
	IfStmt(std::unique_ptr<Cst::Expr> cond,
		   std::unique_ptr<Cst::Stmt> then_branch,
		   std::unique_ptr<Cst::Stmt> else_branch)
		: m_cond(std::move(cond))
		, m_then_branch(std::move(then_branch))
		, m_else_branch(std::move(else_branch))
	{}

	const Cst::Expr& cond() const { return *m_cond; }

	const Cst::Stmt& then_branch() const { return *m_then_branch; }
	const Cst::Stmt& else_branch() const { return *m_else_branch; }

	DEFINE_VISITOR_DISPATCH
};

class WhileStmt: public Stmt
{
	std::unique_ptr<Cst::Expr> m_cond;
	std::unique_ptr<Cst::Stmt> m_body;

public:
	WhileStmt(std::unique_ptr<Cst::Expr> cond,
			  std::unique_ptr<Cst::Stmt> body)
		: m_cond(std::move(cond))
		, m_body(std::move(body))
	{}

	const Cst::Expr& cond() const { return *m_cond; }

	const Cst::Stmt& body() const { return *m_body; }

	DEFINE_VISITOR_DISPATCH
};

class ForeachRangeStmt: public Stmt
{
	Cst::Identifier m_var;
	std::unique_ptr<Cst::Expr> m_range_begin;
	std::unique_ptr<Cst::Expr> m_range_end;
	std::unique_ptr<Cst::Stmt> m_body;

public:
	ForeachRangeStmt(Cst::Identifier var,
					 std::unique_ptr<Cst::Expr> range_begin,
					 std::unique_ptr<Cst::Expr> range_end,
					 std::unique_ptr<Cst::Stmt> body)
		: m_var(std::move(var))
		, m_range_begin(std::move(range_begin))
		, m_range_end(std::move(range_end))
		, m_body(std::move(body))
	{}

	const Cst::Identifier& var() const { return m_var; }

	const Cst::Expr& range_begin() const { return *m_range_begin; }
	const Cst::Expr& range_end()   const { return *m_range_end;   }

	const Cst::Stmt& body() const { return *m_body; }

	DEFINE_VISITOR_DISPATCH
};

class ForeachPoolStmt: public Stmt
{
	Cst::Identifier m_var;
	Cst::Identifier m_pool;
	std::unique_ptr<Cst::Stmt> m_body;

public:
	ForeachPoolStmt(Cst::Identifier var, Cst::Identifier pool, std::unique_ptr<Cst::Stmt> body)
		: m_var(std::move(var))
		, m_pool(std::move(pool))
		, m_body(std::move(body))
	{}

	const Cst::Identifier& var()  const { return m_var;  }
	const Cst::Identifier& pool() const { return m_pool; }

	const Cst::Stmt& body() const { return *m_body; }

	DEFINE_VISITOR_DISPATCH
};

class BlockStmt: public Stmt
{
	std::vector<std::unique_ptr<Cst::Stmt>> m_stmts;

public:
	BlockStmt() = default;
	explicit BlockStmt(std::vector<std::unique_ptr<Cst::Stmt>> stmts)
		: m_stmts(std::move(stmts))
	{}

	decltype(m_stmts)::const_iterator begin() const { return m_stmts.cbegin(); }
	decltype(m_stmts)::const_iterator end()   const { return m_stmts.cend();   }

	void add(std::unique_ptr<Cst::Stmt> stmt)
	{
		m_stmts.emplace_back(std::move(stmt));
	}

	DEFINE_VISITOR_DISPATCH
};

class ExprStmt: public Stmt
{
	std::unique_ptr<Cst::Expr> m_expr;

public:
	explicit ExprStmt(std::unique_ptr<Cst::Expr> expr)
		: m_expr(std::move(expr))
	{}

	const Cst::Expr& expr() const { return *m_expr; }

	DEFINE_VISITOR_DISPATCH
};

class BreakStmt: public Stmt
{
	Location m_loc;

public:
	BreakStmt(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class ContinueStmt: public Stmt
{
	Location m_loc;

public:
	ContinueStmt(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class ReturnStmt: public Stmt
{
	std::unique_ptr<Cst::Expr> m_expr;

public:
	explicit ReturnStmt(std::unique_ptr<Cst::Expr> expr)
		: m_expr(std::move(expr))
	{}

	const Cst::Expr& expr() const { return *m_expr; }

	DEFINE_VISITOR_DISPATCH
};

class ReturnVoidStmt: public Stmt
{
	Location m_loc;

public:
	explicit ReturnVoidStmt(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class Class: public Visitable
{
	Cst::Identifier m_name;

	std::vector<Cst::Identifier> m_pool_params;
	std::vector<Cst::Variable> m_pool_param_bounds;

	std::vector<Cst::Field> m_fields;
	std::vector<Cst::Method> m_methods;

public:
	Class(Cst::Identifier name,
		  std::vector<Cst::Identifier> pool_params,
		  std::vector<Cst::Variable> pool_param_bounds,
		  std::vector<Cst::Field> fields,
		  std::vector<Cst::Method> methods)
		: m_name(std::move(name))
		, m_pool_params(std::move(pool_params))
		, m_pool_param_bounds(std::move(pool_param_bounds))
		, m_fields(std::move(fields))
		, m_methods(std::move(methods))
	{
	}

	const Cst::Identifier& name() const { return m_name; }

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.cbegin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.cend();   }

	decltype(m_pool_param_bounds)::const_iterator pool_param_bounds_begin() const { return m_pool_param_bounds.cbegin(); }
	decltype(m_pool_param_bounds)::const_iterator pool_param_bounds_end()   const { return m_pool_param_bounds.cend();   }

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }

	decltype(m_methods)::const_iterator methods_begin() const { return m_methods.cbegin(); }
	decltype(m_methods)::const_iterator methods_end()   const { return m_methods.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class Field: public Visitable
{
	Cst::Identifier m_name;
	std::unique_ptr<Cst::Type> m_type;

public:
	Field(Cst::Identifier name, std::unique_ptr<Cst::Type> type)
		: m_name(std::move(name))
		, m_type(std::move(type))
	{}

	const Cst::Identifier& name() const { return m_name;  }
	const Cst::Type&       type() const { return *m_type; }

	DEFINE_VISITOR_DISPATCH
};

class Method: public Visitable
{
	Cst::Identifier m_name;
	std::unique_ptr<Cst::Type> m_type;
	std::vector<Cst::Variable> m_params;
	Cst::BlockStmt m_body;

public:
	Method(Cst::Identifier name,
		   std::vector<Cst::Variable> params,
		   std::unique_ptr<Cst::Type> type,
		   Cst::BlockStmt body)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_params(std::move(params))
		, m_body(std::move(body))
	{}

	const Cst::Identifier& name() const { return m_name; }

	const Cst::Type* type() const { return m_type.get(); }

	decltype(m_params)::const_iterator params_begin() { return m_params.cbegin(); }
	decltype(m_params)::const_iterator params_end()   { return m_params.cend();   }

	const Cst::BlockStmt& body() const { return m_body; }

	DEFINE_VISITOR_DISPATCH
};

class ClassBody
{
	std::vector<Cst::Field> m_fields;
	std::vector<Cst::Method> m_methods;

public:
	ClassBody() = default;
	ClassBody(std::vector<Cst::Field> fields, std::vector<Cst::Method> methods)
		: m_fields(std::move(fields))
		, m_methods(std::move(methods))
	{
	}

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }

	decltype(m_methods)::const_iterator methods_begin() const { return m_methods.cbegin(); }
	decltype(m_methods)::const_iterator methods_end()   const { return m_methods.cend();   }

	void add_field(Cst::Field field)
	{
		m_fields.emplace_back(std::move(field));
	}

	void add_method(Cst::Method method)
	{
		m_methods.emplace_back(std::move(method));
	}

	std::vector<Cst::Field>&& consume_fields()   { return std::move(m_fields);  }
	std::vector<Cst::Method>&& consume_methods() { return std::move(m_methods); }
};

class Layout: public Visitable
{
	Cst::Identifier m_name;
	Cst::Identifier m_class;
	std::vector<Cst::Cluster> m_clusters;

public:
	Layout(Cst::Identifier name,
		   Cst::Identifier klass,
		   std::vector<Cst::Cluster> clusters)
		: m_name(std::move(name))
		, m_class(std::move(klass))
		, m_clusters(std::move(clusters))
	{
	}

	const Cst::Identifier& name() const { return m_name; }
	const Cst::Identifier& for_class() const { return m_class; }

	decltype(m_clusters)::const_iterator clusters_begin() const { return m_clusters.cbegin(); }
	decltype(m_clusters)::const_iterator clusters_end()   const { return m_clusters.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class Cluster: public Visitable
{
	std::vector<Cst::Identifier> m_fields;

public:
	Cluster() = default;
	explicit Cluster(std::vector<Cst::Identifier> fields)
		: m_fields(std::move(fields))
	{
	}

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class Program: public Visitable
{
	std::vector<Cst::Class> m_classes;
	std::vector<Cst::Layout> m_layouts;

public:
	Program() = default;
	Program(std::vector<Cst::Class> classes, std::vector<Cst::Layout> layouts)
		: m_classes(std::move(classes))
		, m_layouts(std::move(layouts))
	{
	}

	decltype(m_classes)::const_iterator classes_begin() const { return m_classes.cbegin(); }
	decltype(m_classes)::const_iterator classes_end()   const { return m_classes.cend();   }

	decltype(m_layouts)::const_iterator layouts_begin() const { return m_layouts.cbegin(); }
	decltype(m_layouts)::const_iterator layouts_end()   const { return m_layouts.cend();   }

	void add_class(Cst::Class klass) {
		m_classes.emplace_back(std::move(klass));
	}
	void add_layout(Cst::Layout layout) {
		m_layouts.emplace_back(std::move(layout));
	}

	DEFINE_VISITOR_DISPATCH
};

#undef DEFINE_VISITOR_DISPATCH

class SyntaxError
{
	Location m_loc;
	std::string m_msg;

public:
	SyntaxError(const Location& loc, std::string msg)
		: m_loc(std::move(loc))
		, m_msg(std::move(msg))
	{}

	const Location&    loc() const { return m_loc; }
	const std::string& msg() const { return m_msg; }
};

class SyntaxErrorList
{
	std::vector<SyntaxError> m_list;

public:
	decltype(m_list)::const_iterator begin() const { return m_list.cbegin(); }
	decltype(m_list)::const_iterator end()   const { return m_list.cend();   }

	void add(Location loc, std::string msg)
	{
		m_list.emplace_back(std::move(loc), std::move(msg));
	}

	bool has_errors() const
	{
		return !m_list.empty();
	}
};

}
