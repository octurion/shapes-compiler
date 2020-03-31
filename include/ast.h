#pragma once

#include "parse_tree_common.h"
#include "cst.h"
#include "ast_decls.h"
#include "ast_errors.h"

#include <cassert>
#include <cstdint>
#include <deque>
#include <memory>
#include <utility>
#include <vector>
#include <unordered_map>

namespace Ast
{
#define DEFINE_VISITOR_DISPATCH \
	void accept(Visitor& v) const override { v.visit(*this); }

class Visitable
{
public:
	virtual void accept(Visitor& v) const = 0;
	virtual ~Visitable() {}
};

class Expr: public Visitable
{
public:
	virtual const Ast::Type& type() const = 0;
	virtual bool is_lvalue()        const = 0;
	virtual const Location& loc()   const = 0;
};

class Stmt: public Visitable {};

class Type: public Visitable
{
public:
	enum class Kind { CLASS, POOL, BOUND, PRIMITIVE, NULLPTR };

	virtual Ast::Type::Kind kind() const = 0;
};

enum class BinOp
{
	PLUS, MINUS, TIMES, DIV, LAND, LOR, AND, OR, XOR, SHL, SHR, EQ, NE, LT, LE, GT, GE
};

enum class UnOp { PLUS, MINUS, NOT };

class Variable: public Visitable
{
	std::string m_name;
	std::unique_ptr<Ast::Type> m_type;

	Location m_loc;

public:
	explicit Variable(std::string name, std::unique_ptr<Ast::Type> type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }

	const Ast::Type& type() const { return *m_type; }

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class Pool: public Visitable
{
	std::string m_name;
	std::unique_ptr<Ast::Type> m_type;

	Location m_loc;

public:
	explicit Pool(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	void set_type(std::unique_ptr<Ast::Type> type) { m_type = std::move(type); }
	bool type_init() const { return m_type != nullptr; }

	const std::string& name() const { return m_name; }
	const Ast::Type& type() const { return *m_type; }
	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class PrimitiveType: public Type
{
public:
	enum class Kind { I8, U8, I16, U16, I32, U32, I64, U64, F32, F64 };

private:
	Ast::PrimitiveType::Kind m_kind;

public:
	explicit PrimitiveType(Ast::PrimitiveType::Kind kind)
		: m_kind(kind)
	{}

	Ast::PrimitiveType::Kind primitive_kind() const { return m_kind; }

	Ast::Type::Kind kind() const override { return Ast::Type::Kind::PRIMITIVE; };

	DEFINE_VISITOR_DISPATCH
};

class ClassType: public Type
{
	const Ast::Class* m_klass;
	std::vector<const Ast::Pool*> m_pool_params;

public:
	ClassType(const Ast::Class* klass, std::vector<const Ast::Pool*> pool_params)
		: m_klass(klass)
		, m_pool_params(std::move(pool_params))
	{
		assert(klass != nullptr);
	}

	const Ast::Class& klass() const { return *m_klass; }

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.cbegin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.cend();   }

	Ast::Type::Kind kind() const override { return Ast::Type::Kind::CLASS; };

	DEFINE_VISITOR_DISPATCH
};

class PoolType: public Type
{
	const Ast::Layout* m_layout;
	std::vector<const Ast::Pool*> m_pool_params;

public:
	PoolType(const Ast::Layout* layout, std::vector<const Ast::Pool*> pool_params)
		: m_layout(layout)
		, m_pool_params(std::move(pool_params))
	{
		assert(layout != nullptr);
	}

	const Ast::Layout& layout() const { return *m_layout; }

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.cbegin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.cend();   }

	Ast::Type::Kind kind() const override { return Ast::Type::Kind::POOL; };

	DEFINE_VISITOR_DISPATCH
};

class BoundType: public Type
{
	const Ast::Class* m_klass;
	std::vector<const Ast::Pool*> m_pool_params;

public:
	BoundType(const Ast::Class* klass, std::vector<const Ast::Pool*> pool_params)
		: m_klass(klass)
		, m_pool_params(std::move(pool_params))
	{
		assert(klass != nullptr);
	}

	const Ast::Class& klass() const { return *m_klass; }

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.cbegin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.cend();   }

	Ast::Type::Kind kind() const override { return Ast::Type::Kind::BOUND; };

	DEFINE_VISITOR_DISPATCH
};

class NullType: public Type
{
public:
	Ast::Type::Kind kind() const override { return Ast::Type::Kind::NULLPTR; };

	DEFINE_VISITOR_DISPATCH
};

class IntegerConst: public Expr
{
	uint64_t m_value;
	Location m_loc;
	Ast::PrimitiveType m_type;

public:
	explicit IntegerConst(uint64_t value, const Location& loc = Location())
		: m_value(value)
		, m_loc(loc)
		, m_type(Ast::PrimitiveType::Kind::U64)
	{}

	const Ast::PrimitiveType& type() const override { return m_type; }
	bool is_lvalue()                 const override { return false;  }
	const Location& loc()            const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class NullExpr: public Expr
{
	Location m_loc;
	NullType m_type;

public:
	explicit NullExpr(const Location& loc = Location())
		: m_loc(loc)
	{}

	const Ast::NullType& type() const override { return m_type; }
	bool is_lvalue()            const override { return false;  }
	const Location& loc()       const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class ThisExpr: public Expr
{
	ClassType m_type;
	Location m_loc;

public:
	explicit ThisExpr(ClassType type, const Location& loc = Location())
		: m_type(std::move(type))
		, m_loc(loc)
	{}

	const Ast::ClassType& type() const override { return m_type; }
	bool is_lvalue()             const override { return true;   }
	const Location& loc()        const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class BinaryExpr: public Expr
{
	std::unique_ptr<Ast::Expr> m_lhs;
	Ast::BinOp m_op;
	std::unique_ptr<Ast::Expr> m_rhs;
	Ast::PrimitiveType m_type;

	Location m_loc;

public:
	explicit BinaryExpr(std::unique_ptr<Ast::Expr> lhs,
						Ast::BinOp op,
						std::unique_ptr<Ast::Expr> rhs,
						Ast::PrimitiveType type,
						const Location& loc = Location())
		: m_lhs(std::move(lhs))
		, m_op(op)
		, m_rhs(std::move(rhs))
		, m_type(type)
		, m_loc(loc)
	{}

	const Ast::Expr& lhs() const { return *m_lhs; }
	const Ast::Expr& rhs() const { return *m_rhs; }

	Ast::BinOp op() const { return m_op; }

	const Ast::PrimitiveType& type() const override { return m_type; }
	bool is_lvalue()                 const override { return false;   }
	const Location& loc()            const override { return m_loc;   }

	DEFINE_VISITOR_DISPATCH
};

class UnaryExpr: public Expr
{
	Ast::UnOp m_op;
	std::unique_ptr<Ast::Expr> m_expr;
	Ast::PrimitiveType m_type;

	Location m_loc;

public:
	explicit UnaryExpr(Ast::UnOp op,
					   std::unique_ptr<Ast::Expr> expr,
					   Ast::PrimitiveType type,
					   const Location& loc = Location())
		: m_op(op)
		, m_expr(std::move(expr))
		, m_type(type)
		, m_loc(loc)
	{}

	const Ast::Expr& expr() const { return *m_expr; }

	Ast::UnOp op() const { return m_op; }

	const Ast::PrimitiveType& type() const override { return m_type; }
	bool is_lvalue()                 const override { return false;  }
	const Location& loc()            const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class IndexExpr: public Expr
{
	std::unique_ptr<Ast::Expr> m_expr;
	Ast::BinOp m_op;
	std::unique_ptr<Ast::Expr> m_idx;
	Ast::PrimitiveType m_type;

	Location m_loc;

public:
	explicit IndexExpr(std::unique_ptr<Ast::Expr> expr,
					   Ast::BinOp op,
					   std::unique_ptr<Ast::Expr> idx,
					   Ast::PrimitiveType type,
					   const Location& loc = Location())
		: m_expr(std::move(expr))
		, m_op(op)
		, m_idx(std::move(idx))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const Ast::Expr& expr() const { return *m_expr; }
	const Ast::Expr& idx()  const { return *m_idx;  }

	Ast::BinOp op() const { return m_op; }

	const Ast::PrimitiveType& type() const override { return m_type; }
	bool is_lvalue()      const override { return true;   }
	const Location& loc() const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class VariableExpr: public Expr
{
	const Ast::Variable* m_var;

	Location m_loc;

public:
	explicit VariableExpr(const Variable* var,
						  const Location& loc = Location())
		: m_var(var)
		, m_loc(loc)
	{
		assert(var != nullptr);
	}

	const Ast::Variable& var() const { return *m_var; }

	const Ast::Type& type() const override { return m_var->type(); }
	bool is_lvalue()        const override { return true;   }
	const Location& loc()   const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class MethodCall: public Expr
{
	std::unique_ptr<Ast::Expr> m_this_expr;
	const Ast::Method* m_method;
	std::vector<std::unique_ptr<Ast::Expr>> m_args;
	std::unique_ptr<Ast::Type> m_return_type;

	Location m_loc;

public:
	explicit MethodCall(std::unique_ptr<Ast::Expr> this_expr,
						const Ast::Method* method,
						std::vector<std::unique_ptr<Ast::Expr>> args,
						std::unique_ptr<Ast::Type> return_type,
						const Location& loc = Location())
		: m_this_expr(std::move(this_expr))
		, m_method(method)
		, m_args(std::move(args))
		, m_return_type(std::move(return_type))
		, m_loc(loc)
	{
		assert(method != nullptr);
	}

	const Ast::Expr& this_expr() const { return *m_this_expr; }

	const Ast::Method& method() const { return *m_method; }

	decltype(m_args)::const_iterator args_begin() const { return m_args.cbegin(); }
	decltype(m_args)::const_iterator args_end()   const { return m_args.cend();   }

	const Ast::Type& type() const override { return *m_return_type; }
	bool is_lvalue()        const override {
		return m_return_type != nullptr && m_return_type->kind() == Ast::Type::Kind::CLASS;
	}
	const Location& loc()   const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class FieldAccess: public Expr
{
	std::unique_ptr<Ast::Expr> m_expr;
	const Ast::Field* m_field;
	std::unique_ptr<Ast::Type> m_type;

	Location m_loc;

public:
	explicit FieldAccess(std::unique_ptr<Ast::Expr> expr,
						const Ast::Field* field,
						std::unique_ptr<Ast::Type> type,
						const Location& loc = Location())
		: m_expr(std::move(expr))
		, m_field(field)
		, m_type(std::move(type))
		, m_loc(loc)
	{
		assert(field != nullptr);
	}

	const Ast::Expr& expr()   const { return *m_expr;  }
	const Ast::Field& field() const { return *m_field; }

	const Ast::Type& type() const override { return *m_type; }
	bool is_lvalue()        const override { return true;    }
	const Location& loc()   const override { return m_loc;   }

	DEFINE_VISITOR_DISPATCH
};

class NewExpr: public Expr
{
	Ast::ClassType m_type;

	Location m_loc;

public:
	explicit NewExpr(Ast::ClassType type, const Location& loc = Location())
		: m_type(std::move(type))
		, m_loc(loc)
	{}

	const Ast::ClassType& type() const override { return m_type; }
	bool is_lvalue()             const override { return true;   }
	const Location& loc()        const override { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class AssignStmt: public Stmt
{
	std::unique_ptr<Ast::Expr> m_lhs;
	std::unique_ptr<Ast::Expr> m_rhs;

public:
	explicit AssignStmt(std::unique_ptr<Ast::Expr> lhs,
						std::unique_ptr<Ast::Expr> rhs)
		: m_lhs(std::move(lhs))
		, m_rhs(std::move(rhs))
	{}

	const Ast::Expr& lhs() const { return *m_lhs; }
	const Ast::Expr& rhs() const { return *m_rhs; }

	DEFINE_VISITOR_DISPATCH
};

class OpAssignStmt: public Stmt
{
	std::unique_ptr<Ast::Expr> m_lhs;
	Ast::BinOp m_op;
	std::unique_ptr<Ast::Expr> m_rhs;

public:
	explicit OpAssignStmt(std::unique_ptr<Ast::Expr> lhs,
						  Ast::BinOp op,
						  std::unique_ptr<Ast::Expr> rhs)
		: m_lhs(std::move(lhs))
		, m_op(op)
		, m_rhs(std::move(rhs))
	{}

	const Ast::Expr& lhs() const { return *m_lhs; }
	const Ast::Expr& rhs() const { return *m_rhs; }

	Ast::BinOp op()  const { return m_op; }

	DEFINE_VISITOR_DISPATCH
};

class IfStmt: public Stmt
{
	std::unique_ptr<Ast::Expr> m_cond;
	std::vector<std::unique_ptr<Ast::Stmt>> m_then_branch;
	std::vector<std::unique_ptr<Ast::Stmt>> m_else_branch;

public:
	explicit IfStmt(std::unique_ptr<Ast::Expr> cond,
					std::vector<std::unique_ptr<Ast::Stmt>> then_branch,
					std::vector<std::unique_ptr<Ast::Stmt>> else_branch)
		: m_cond(std::move(cond))
		, m_then_branch(std::move(then_branch))
		, m_else_branch(std::move(else_branch))
	{}

	const Ast::Expr& cond() const { return *m_cond; }

	decltype(m_then_branch)::const_iterator then_branch_begin() const { return m_then_branch.cbegin(); }
	decltype(m_then_branch)::const_iterator then_branch_end()   const { return m_then_branch.cend();   }

	decltype(m_else_branch)::const_iterator else_branch_begin() const { return m_else_branch.cbegin(); }
	decltype(m_else_branch)::const_iterator else_branch_end()   const { return m_else_branch.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class WhileStmt: public Stmt
{
	std::unique_ptr<Ast::Expr> m_cond;
	std::vector<std::unique_ptr<Ast::Stmt>> m_body;

public:
	explicit WhileStmt(std::unique_ptr<Ast::Expr> cond,
					   std::vector<std::unique_ptr<Ast::Stmt>> body)
		: m_cond(std::move(cond))
		, m_body(std::move(body))
	{}

	const Ast::Expr& cond() const { return *m_cond; }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class ForeachRangeStmt: public Stmt
{
	const Ast::Variable* m_var;
	std::unique_ptr<Ast::Expr> m_range_begin;
	std::unique_ptr<Ast::Expr> m_range_end;
	std::vector<std::unique_ptr<Ast::Stmt>> m_body;

public:
	explicit ForeachRangeStmt(const Ast::Variable* var,
							  std::unique_ptr<Ast::Expr> range_begin,
							  std::unique_ptr<Ast::Expr> range_end,
							  std::vector<std::unique_ptr<Ast::Stmt>> body)
		: m_var(var)
		, m_range_begin(std::move(range_begin))
		, m_range_end(std::move(range_end))
		, m_body(std::move(body))
	{
		assert(var != nullptr);
	}

	const Ast::Variable& var() const { return *m_var; }

	const Ast::Expr& range_begin() const { return *m_range_begin; }
	const Ast::Expr& range_end()   const { return *m_range_end;   }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class ForeachPoolStmt: public Stmt
{
	const Ast::Variable* m_var;
	const Ast::Pool* m_pool;
	std::vector<std::unique_ptr<Ast::Stmt>> m_body;

public:
	explicit ForeachPoolStmt(const Ast::Variable* var,
							 const Ast::Pool* pool,
							 std::vector<std::unique_ptr<Ast::Stmt>> body)
		: m_var(var)
		, m_pool(pool)
		, m_body(std::move(body))
	{
		assert(var != nullptr);
		assert(pool != nullptr);
	}

	const Ast::Variable& var() const { return *m_var;  }
	const Ast::Pool& pool()    const { return *m_pool; }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class ExprStmt: public Stmt
{
	std::unique_ptr<Ast::Expr> m_expr;

public:
	explicit ExprStmt(std::unique_ptr<Ast::Expr> expr)
		: m_expr(std::move(expr))
	{}

	const Ast::Expr& expr() const { return *m_expr; }

	DEFINE_VISITOR_DISPATCH
};

class BreakStmt: public Stmt
{
	const Ast::Stmt* m_stmt;

public:
	explicit BreakStmt(const Ast::Stmt* stmt)
		: m_stmt(stmt)
	{
		assert(stmt != nullptr);
	}

	const Ast::Stmt& stmt() const { return *m_stmt; }

	DEFINE_VISITOR_DISPATCH
};

class ContinueStmt: public Stmt
{
	const Ast::Stmt* m_stmt;

public:
	explicit ContinueStmt(const Ast::Stmt* stmt)
		: m_stmt(stmt)
	{
		assert(stmt != nullptr);
	}

	const Ast::Stmt& stmt() const { return *m_stmt; }

	DEFINE_VISITOR_DISPATCH
};

class ReturnStmt: public Stmt
{
	std::unique_ptr<Ast::Expr> m_expr;

public:
	explicit ReturnStmt(std::unique_ptr<Ast::Expr> expr)
		: m_expr(std::move(expr))
	{}

	const Ast::Expr* expr() const { return m_expr.get(); }

	DEFINE_VISITOR_DISPATCH
};

class Field: public Visitable
{
	std::string m_name;
	std::unique_ptr<Ast::Type> m_type;

	Location m_loc;

public:
	explicit Field(std::string name, std::unique_ptr<Ast::Type> type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Ast::Type& type() const { return *m_type; }
	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class Method: public Visitable
{
	std::string m_name;

	std::vector<Ast::Pool> m_pools;
	std::deque<Ast::Variable> m_vars;

	size_t m_param_count;

	std::unique_ptr<Ast::Type> m_return_type;
	std::vector<std::unique_ptr<Ast::Type>> m_body;

	Location m_loc;

public:
	explicit Method(std::string name,
					std::unique_ptr<Ast::Type> return_type,
					const Location& loc)
		: m_name(std::move(name))
		, m_param_count(0)
		, m_return_type(std::move(return_type))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }

	decltype(m_vars)::const_iterator vars_begin() const { return m_vars.cbegin(); }
	decltype(m_vars)::const_iterator vars_end()   const { return m_vars.cend();   }

	decltype(m_pools)::const_iterator pools_begin() const { return m_pools.cbegin(); }
	decltype(m_pools)::const_iterator pools_end()   const { return m_pools.cend();   }

	decltype(m_vars)::const_iterator params_begin() const { return vars_begin(); }
	decltype(m_vars)::const_iterator params_end()   const { return params_begin() + m_param_count; }

	size_t num_params() const { return m_param_count; }

	const Ast::Type* return_type() const { return m_return_type.get(); }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	const Location& loc() const { return m_loc; }

	void add_parameter(Ast::Variable param)
	{
		m_vars.emplace_back(std::move(param));
		m_param_count++;
	}

	void add_variable(Ast::Variable var)
	{
		m_vars.emplace_back(std::move(var));
	}

	DEFINE_VISITOR_DISPATCH
};

class Layout: public Visitable
{
	std::string m_name;
	const Ast::Class* m_klass;
	std::vector<Ast::Cluster> m_clusters;
	Location m_loc;

public:
	explicit Layout(std::string name,
					const Ast::Class* klass,
					std::vector<Ast::Cluster> clusters,
					const Location& loc)
		: m_name(std::move(name))
		, m_klass(klass)
		, m_clusters(std::move(clusters))
		, m_loc(loc)
	{
		assert(klass != nullptr);
	}

	const std::string& name() const { return m_name;   }
	const Ast::Class& klass() const { return *m_klass; }

	decltype(m_clusters)::const_iterator clusters_begin() const { return m_clusters.cbegin(); }
	decltype(m_clusters)::const_iterator clusters_end()   const { return m_clusters.cend();   }

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class Cluster: public Visitable
{
	std::vector<const Ast::Field*> m_fields;

public:
	explicit Cluster(std::vector<const Ast::Field*> fields)
		: m_fields(std::move(fields))
	{}

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class Class: public Visitable
{
	std::string m_name;

	std::unordered_map<std::string, Ast::Pool> m_pool_map;
	std::vector<const Ast::Pool*> m_pools;

	std::unordered_map<std::string, Ast::Field> m_field_map;
	std::vector<const Ast::Field*> m_fields;

	std::unordered_map<std::string, Ast::Method> m_method_map;
	std::vector<const Ast::Method*> m_methods;

	Location m_loc;

public:
	explicit Class(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	void set_pools(std::unordered_map<std::string, Ast::Pool> pool_map,
				   std::vector<const Ast::Pool*> pools)
	{
		m_pool_map = std::move(pool_map);
		m_pools    = std::move(pools);
	}

	void set_methods(std::unordered_map<std::string, Ast::Method> method_map,
					 std::vector<const Ast::Method*> methods)
	{
		m_method_map = std::move(method_map);
		m_methods    = std::move(methods);
	}

	void set_fields(std::unordered_map<std::string, Ast::Field> field_map,
					std::vector<const Ast::Field*> fields)
	{
		m_field_map = std::move(field_map);
		m_fields    = std::move(fields);
	}

	size_t num_pools() const { return m_pools.size(); }

	const Ast::Pool* find_pool(const std::string& name) const
	{
		auto it = m_pool_map.find(name);
		if (it == m_pool_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	Ast::Pool* find_pool(const std::string& name)
	{
		auto it = m_pool_map.find(name);
		if (it == m_pool_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	const Ast::Field* find_field(const std::string& name) const
	{
		auto it = m_field_map.find(name);
		if (it == m_field_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	decltype(m_pools)::const_iterator pools_begin() const { return m_pools.cbegin(); }
	decltype(m_pools)::const_iterator pools_end()   const { return m_pools.cend();   }

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }

	const std::string& name() const { return m_name; }
	const Location& loc()     const { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class Program: public Visitable
{
	std::unordered_map<std::string, Ast::Class> m_class_map;
	std::vector<const Ast::Class*> m_classes;

	std::unordered_map<std::string, Ast::Layout> m_layout_map;
	std::vector<const Ast::Layout*> m_layouts;

public:
	decltype(m_classes)::const_iterator classes_begin() const { return m_classes.cbegin(); }
	decltype(m_classes)::const_iterator classes_end()   const { return m_classes.cend();   }

	decltype(m_layouts)::const_iterator layouts_begin() const { return m_layouts.cbegin(); }
	decltype(m_layouts)::const_iterator layouts_end()   const { return m_layouts.cend();   }

	const Ast::Class* find_class(const std::string& name) const
	{
		auto it = m_class_map.find(name);
		if (it == m_class_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	const Ast::Layout* find_layout(const std::string& name) const
	{
		auto it = m_layout_map.find(name);
		if (it == m_layout_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	Ast::Class* find_class(const std::string& name)
	{
		auto it = m_class_map.find(name);
		if (it == m_class_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	void set_classes(std::unordered_map<std::string, Ast::Class> class_map,
					 std::vector<const Ast::Class*> classes)
	{
		m_class_map = std::move(class_map);
		m_classes = std::move(classes);
	}

	void set_layouts(std::unordered_map<std::string, Ast::Layout> layout_map,
					 std::vector<const Ast::Layout*> layouts)
	{
		m_layout_map = std::move(layout_map);
		m_layouts = std::move(layouts);
	}

	DEFINE_VISITOR_DISPATCH
};

extern void run_semantic_analysis(const Cst::Program& cst, Ast::SemanticErrorList* errors, Ast::Program* ast);

#undef DEFINE_VISITOR_DISPATCH
}
