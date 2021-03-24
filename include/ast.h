#pragma once

#include "common.h"

#include "variant.h"

#include <cstdint>
#include <deque>
#include <iosfwd>
#include <memory>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Ast
{

class Class;
class Field;
class Method;
class Layout;
class Pool;

class ObjectType;

class PoolRef
{
	const Pool* m_pool;

public:
	explicit PoolRef(const Pool& pool)
		: m_pool(&pool)
	{}

	const Pool& pool() const { return *m_pool; }

	bool operator==(const PoolRef& rhs) const;
	bool operator!=(const PoolRef& rhs) const;
};
std::ostream& operator<<(std::ostream& os, const PoolRef& pool);

class None {
public:
	bool operator==(const None&) const { return true; }
	bool operator!=(const None&) const { return false; }
};
std::ostream& operator<<(std::ostream& os, const None&);

using PoolParameter = mpark::variant<None, PoolRef>;
std::ostream& operator<<(std::ostream& os, const PoolParameter& parameter);

class BoundType
{
	const Class* m_class;
	std::vector<PoolParameter> m_params;

	Location m_loc;

public:
	BoundType(const Class& of_class,
			  std::vector<PoolParameter> params,
			  const Location& loc)
		: m_class(&of_class)
		, m_params(std::move(params))
		, m_loc(loc)
	{}

	const Class& of_class() const { return *m_class; }

	const std::vector<PoolParameter>& params() const { return m_params; }
	size_t num_params() const { return m_params.size(); }

	const Location& loc() const { return m_loc; }

	bool operator==(const BoundType& rhs) const;
	bool operator!=(const BoundType& rhs) const;

	bool compatible_with_bound(const BoundType& bound) const;
	BoundType to_bound_type() const;
	ObjectType to_object_type() const;

	BoundType bound_of_param(size_t idx) const;
	bool first_pool_param_is(const Pool& pool) const;
};
std::ostream& operator<<(std::ostream& os, const BoundType& type);

class LayoutType
{
	const Layout* m_layout;
	std::vector<PoolParameter> m_params;

	Location m_loc;

public:
	LayoutType(const Layout& layout,
			   std::vector<PoolParameter> params,
			   const Location& loc)
		: m_layout(&layout)
		, m_params(std::move(params))
		, m_loc(loc)
	{}

	const Layout& layout() const { return *m_layout; }
	const Class& for_class() const;

	const std::vector<PoolParameter>& params() const { return m_params; }
	size_t num_params() const { return m_params.size(); }

	const Location& loc() const { return m_loc; }

	bool operator==(const LayoutType& rhs) const;
	bool operator!=(const LayoutType& rhs) const;

	bool compatible_with_bound(const BoundType& bound) const;
	ObjectType to_object_type() const;
	BoundType to_bound_type() const;

	BoundType bound_of_param(size_t idx) const;
	bool first_pool_param_is(const Pool& pool) const;
};
std::ostream& operator<<(std::ostream& os, const LayoutType& type);

class NoneType {
public:
	bool operator==(const NoneType&) const { return true; }
	bool operator!=(const NoneType&) const { return false; }

	bool compatible_with_bound(const BoundType&) const { return true; }
	NoneType to_bound_type() const { return NoneType(); }
	bool first_pool_param_is(const Pool&) const { return false; }
};

using PoolType = mpark::variant<LayoutType, BoundType, NoneType>;
const Class& for_class(const PoolType& type);
std::ostream& operator<<(std::ostream& os, const PoolType& type);
bool compatible_with_bound(const PoolType& type, const BoundType& bound);
bool first_pool_param_is(const PoolType& type, const Pool& pool);

ObjectType from_pool_type(const PoolType& type);

class Pool
{
	std::string m_name;
	std::unique_ptr<PoolType> m_type;

	Location m_loc;

public:
	explicit Pool(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	explicit Pool(std::string name, PoolType type, const Location& loc)
		: m_name(std::move(name))
		, m_type(new PoolType(std::move(type)))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Location& loc() const { return m_loc; }

	bool has_type() const { return m_type != nullptr; }
	const PoolType& type() const {
		assert_msg(m_type != nullptr, "Pool type not set");
		return *m_type;
	}
	void set_type(PoolType type) {
		assert_msg(m_type == nullptr, "Pool type already set");
		m_type.reset(new PoolType(std::move(type)));
	}
};

enum class PrimitiveType
{
	BOOL, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64
};

std::ostream& operator<<(std::ostream& os, PrimitiveType type);

inline bool is_floating_point(PrimitiveType type)
{
	return type == PrimitiveType::F32 || type == PrimitiveType::F64;
}

inline bool is_boolean(PrimitiveType type)
{
	return type == PrimitiveType::BOOL;
}

inline bool is_signed_integer(PrimitiveType type)
{
	switch (type) {
	case PrimitiveType::I8:
	case PrimitiveType::I16:
	case PrimitiveType::I32:
	case PrimitiveType::I64:
		return true;
	default:
		return false;
	}
}

inline bool is_unsigned_integer(PrimitiveType type)
{
	switch (type) {
	case PrimitiveType::U8:
	case PrimitiveType::U16:
	case PrimitiveType::U32:
	case PrimitiveType::U64:
		return true;
	default:
		return false;
	}
}

inline bool is_integer(PrimitiveType type)
{
	return is_signed_integer(type) || is_unsigned_integer(type);
}
inline bool is_integer_or_bool(PrimitiveType type)
{
	return is_integer(type) || type == PrimitiveType::BOOL;
}

class NullptrType {
public:
	bool operator==(const NullptrType&) const { return true; }
	bool operator!=(const NullptrType&) const { return false; }
};
std::ostream& operator<<(std::ostream& os, const NullptrType&);

class VoidType {
public:
	bool operator==(const VoidType&) const { return true; }
	bool operator!=(const VoidType&) const { return false; }
};
std::ostream& operator<<(std::ostream& os, const VoidType&);

class ObjectType;

using Type = mpark::variant<PrimitiveType, ObjectType, VoidType, NullptrType>;

class ObjectType
{
	const Class* m_class;
	std::vector<PoolParameter> m_params;

	Location m_loc;

public:
	ObjectType(const Class& of_class,
			   std::vector<PoolParameter> params,
			   const Location& loc)
		: m_class(&of_class)
		, m_params(std::move(params))
		, m_loc(loc)
	{}

	const Class& of_class() const { return *m_class; }

	const std::vector<PoolParameter>& params() const { return m_params; }
	size_t num_params() const { return m_params.size(); }

	const Location& loc() const { return m_loc; }

	bool operator==(const ObjectType& rhs) const;
	bool operator!=(const ObjectType& rhs) const;

	ObjectType remap_formal_pool_params(const ObjectType& type_with_new_pools) const;
	Type remap_formal_pool_params(const Type& type_with_new_pools) const;

	bool compatible_with_bound(const BoundType& bound) const;

	BoundType to_bound_type() const;
	BoundType bound_of_param(size_t idx) const;
};
std::ostream& operator<<(std::ostream& os, const ObjectType&);

std::ostream& operator<<(std::ostream& os, const Type&);
bool assignable_from(const Type& lhs, const Type& rhs);

class Variable
{
	std::string m_name;
	Type m_type;

	Location m_loc;

public:
	explicit Variable(std::string name, Type type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Type& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class InvalidExpr;
class IntegerConst;
class DoubleConst;
class BooleanConst;
class NullExpr;
class ThisExpr;
class CastExpr;
class UnaryExpr;
class BinaryExpr;
class VariableExpr;
class MethodCall;
class FieldAccess;
class NewExpr;

using Expr = mpark::variant<
	InvalidExpr, IntegerConst, DoubleConst, BooleanConst, NullExpr, ThisExpr,
	CastExpr, UnaryExpr, BinaryExpr, VariableExpr, MethodCall, FieldAccess,
	NewExpr>;
Type expr_type(const Expr& expr);
bool is_lvalue(const Expr& expr);

class InvalidExpr
{
public:
	PrimitiveType type() const;
	bool is_lvalue() const;
};

class IntegerConst
{
	uint64_t m_value = 0;
	PrimitiveType m_type;

public:
	explicit IntegerConst(uint64_t value, PrimitiveType type)
		: m_value(value)
		, m_type(type)
	{}

	uint64_t value() const { return m_value; }
	PrimitiveType type() const { return m_type; }
	bool is_lvalue() const { return false; }
};

class DoubleConst
{
	double m_value = 0;
	PrimitiveType m_type;

public:
	explicit DoubleConst(double value, PrimitiveType type)
		: m_value(value)
		, m_type(type)
	{}

	double value() const { return m_value; }
	PrimitiveType type() const { return m_type; }
	bool is_lvalue() const { return false; }
};

class BooleanConst
{
	bool m_value = false;

public:
	explicit BooleanConst(bool value)
		: m_value(value)
	{}

	bool value() const { return m_value; }
	PrimitiveType type() const { return PrimitiveType::BOOL; }
	bool is_lvalue() const { return false; }
};

class NullExpr {
public:
	NullptrType type() const { return NullptrType(); }
	bool is_lvalue() const { return false; }
};

class ThisExpr
{
	ObjectType m_type;

	public:
	explicit ThisExpr(ObjectType type)
		: m_type(std::move(type))
	{}

	const ObjectType& type() const { return m_type; }
	bool is_lvalue() const { return false; }
};

class CastExpr
{
	std::unique_ptr<Expr> m_expr;
	PrimitiveType m_type;

	Location m_loc;

public:
	CastExpr(Expr expr, PrimitiveType type);

	const Expr& expr() const;
	PrimitiveType type() const { return m_type; }
	bool is_lvalue() const { return false; }
};

enum class UnOp { PLUS, MINUS, NOT };
std::ostream& operator<<(std::ostream& os, UnOp op);
inline bool is_bitwise_operator(UnOp op) { return op == UnOp::NOT; }

class UnaryExpr
{
	std::unique_ptr<Expr> m_expr;
	UnOp m_op;

public:
	UnaryExpr(UnOp op, Expr expr);

	const Expr& expr() const;
	UnOp op() const { return m_op; }

	Type type() const;
	bool is_lvalue() const { return false; }
};

enum class BinOp
{
	PLUS, MINUS, TIMES, DIV,
	LAND, LOR,
	AND, OR, XOR, SHL, SHR,
	EQ, NE, LT, LE, GT, GE,
};
std::ostream& operator<<(std::ostream& os, BinOp op);
inline bool is_bitwise_operator(BinOp op) {
	switch (op) {
	case BinOp::AND:
	case BinOp::OR:
	case BinOp::XOR:
	case BinOp::SHL:
	case BinOp::SHR:
		return true;
	default:
		return false;
	}
}

class BinaryExpr
{
	std::unique_ptr<Expr> m_lhs;
	std::unique_ptr<Expr> m_rhs;
	BinOp m_op;
	PrimitiveType m_type;

public:
	BinaryExpr(Expr lhs, BinOp op, Expr rhs, PrimitiveType type);

	const Expr& lhs() const;
	const Expr& rhs() const;
	BinOp op() const { return m_op; }

	PrimitiveType type() const { return m_type; }
	bool is_lvalue() const { return false; }
};

class VariableExpr
{
	const Variable& m_var;
	Location m_loc;

public:
	explicit VariableExpr(const Variable& var, const Location& loc)
		: m_var(var)
		, m_loc(loc)
	{}

	const Variable& var() const { return m_var; }
	const Location& loc() const { return m_loc; }
	const Type& type() const { return m_var.type(); }
	bool is_lvalue() const { return true; }
};

class MethodCall
{
	const Method& m_method;
	std::unique_ptr<Expr> m_this_expr;
	std::vector<Expr> m_args;
	Type m_type;

public:
	MethodCall(const Method& method, Expr this_expr, std::vector<Expr> args, Type type);

	const Method& method() const { return m_method; }
	const Expr& this_expr() const;
	const Type& type() const;

	const std::vector<Expr>& args() const { return m_args; }
	size_t num_args() const { return m_args.size(); }
	bool is_lvalue() const { return false; }
};

class FieldAccess
{
	std::unique_ptr<Expr> m_expr;
	const Field& m_field;
	Type m_type;

public:
	explicit FieldAccess(Expr expr, const Field& field, Type type);

	const Expr& expr() const;
	const Field& field() const { return m_field; }
	const Type& type() const { return m_type; }
	bool is_lvalue() const { return true; }
};

class NewExpr
{
	ObjectType m_type;

public:
	explicit NewExpr(ObjectType type)
		: m_type(std::move(type))
	{}

	const ObjectType& type() const { return m_type; }
	bool is_lvalue() const { return false; }
};

class Field
{
	std::string m_name;
	std::unique_ptr<Type> m_type;

	Location m_loc;

public:
	Field(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Type& type() const {
		assert_msg(m_type != nullptr, "Type has not been set for this field");
		return *m_type;
	}
	const Location& loc() const { return m_loc; }

	void set_type(Type type) {
		assert_msg(m_type == nullptr, "Type has been already set for this field");
		m_type.reset(new Type(std::move(type)));
	}
};

class Cluster
{
	std::vector<const Field*> m_fields;
	Location m_loc;

public:
	explicit Cluster(const Location& loc)
		: m_loc(loc)
	{}

	const std::vector<const Field*>& fields() const { return m_fields; }

	void add_field(const Field& field) { m_fields.emplace_back(&field); }
};

struct FieldPos
{
	size_t cluster_idx;
	size_t pos;
};

class Layout
{
	std::string m_name;
	const Class& m_class;
	std::vector<Cluster> m_clusters;
	std::unordered_map<const Field*, FieldPos> m_field_map;
	Location m_loc;

public:
	explicit Layout(std::string name,
					const Class& for_class,
					const Location& loc)
		: m_name(std::move(name))
		, m_class(for_class)
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name;  }
	const Class& for_class() const { return m_class; }

	const std::vector<Cluster>& clusters() const { return m_clusters; }
	size_t num_clusters() const { return m_clusters.size(); }

	const Location& loc() const { return m_loc; }
	const FieldPos* field_pos(const Field& field) const
	{
		auto it = m_field_map.find(&field);
		return it != m_field_map.end()
			? &it->second
			: nullptr;
	}

	Cluster& add_cluster(const Location& loc)
	{
		m_clusters.emplace_back(loc);
		return m_clusters.back();
	}

	void build_field_map();
};

class Assignment;
class OpAssignment;
class If;
class While;
class ForeachRange;
class ForeachPool;
class ExprStmt;
class Break;
class Continue;
class Return;

using Stmt = mpark::variant<Assignment, OpAssignment, If, While, ForeachRange,
	  ForeachPool, ExprStmt, Break, Continue, Return>;

class Assignment
{
	std::unique_ptr<Expr> m_lhs;
	std::unique_ptr<Expr> m_rhs;

public:
	Assignment(Expr lhs, Expr rhs);

	const Expr& lhs() const;
	const Expr& rhs() const;
};

class OpAssignment
{
	Expr m_lhs;
	Expr m_rhs;
	BinOp m_op;

public:
	OpAssignment(Expr lhs, BinOp op, Expr rhs);

	const Expr& lhs() const { return m_lhs; }
	const Expr& rhs() const { return m_rhs; }
	BinOp op() const { return m_op; }
};

class If
{
	Expr m_cond;
	std::vector<Stmt> m_then_branch;
	std::vector<Stmt> m_else_branch;

public:
	If(Expr cond, std::vector<Stmt> then_branch, std::vector<Stmt> else_branch);

	const Expr& cond() const { return m_cond; }

	const std::vector<Stmt>& then_stmts() const { return m_then_branch; }
	const std::vector<Stmt>& else_stmts() const { return m_else_branch; }
};

class While
{
	Expr m_cond;
	std::vector<Stmt> m_body;

public:
	While(Expr cond, std::vector<Stmt> body);

	const Expr& cond() const { return m_cond; }

	const std::vector<Stmt>& body() const { return m_body; }
};

class ForeachRange
{
	std::reference_wrapper<const Variable> m_var;
	Expr m_range_begin;
	Expr m_range_end;
	std::vector<Stmt> m_body;

public:
	ForeachRange(const Variable& var,
				 Expr range_begin,
				 Expr range_end,
				 std::vector<Stmt> body);

	const Variable& var() const { return m_var; }

	const Expr& range_begin() const { return m_range_begin; }
	const Expr& range_end()   const { return m_range_end;   }

	const std::vector<Stmt>& body() const { return m_body; }
};

class ForeachPool
{
	std::reference_wrapper<const Variable> m_var;
	std::reference_wrapper<const Pool> m_pool;
	std::vector<Stmt> m_body;

public:
	ForeachPool(const Variable& var, const Pool& pool, std::vector<Stmt> body);

	// No clue why gcc requires this for compilation
	ForeachPool(ForeachPool&&) = default;
	ForeachPool& operator=(ForeachPool&&) = default;

	const Variable& var() const { return m_var;  }
	const Pool& pool()    const { return m_pool; }

	const std::vector<Stmt>& body() const { return m_body; }
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

class Break { };

class Continue { };

class Return
{
	std::unique_ptr<Expr> m_expr;

public:
	explicit Return(Expr expr)
		: m_expr(new Expr(std::move(expr)))
	{}

	Return()
		: m_expr()
	{}

	const Expr* expr() const { return m_expr.get(); }
};

class Method
{
	std::string m_name;

	std::deque<Pool> m_pools;
	std::deque<Variable> m_params;
	std::deque<Variable> m_vars;

	Type m_return_type;
	std::vector<Stmt> m_body;

	Location m_loc;

public:
	explicit Method(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }

	const std::deque<Pool>& pools() const { return m_pools; }
	const std::deque<Variable>& vars() const { return m_vars; }

	const std::deque<Variable>& params() const { return m_params; }
	size_t num_params() const { return m_params.size(); }

	void set_params(std::deque<Variable> params) { m_params = std::move(params); }
	void set_return_type(Type type) { m_return_type = std::move(type); }
	void set_body(std::vector<Stmt> body) { m_body = std::move(body); }

	const Type& return_type() const { return m_return_type; }

	const std::vector<Stmt>& body() const { return m_body; }

	const Location& loc() const { return m_loc; }

	const Variable& add_parameter(std::string name, Type type, const Location& loc)
	{
		m_params.emplace_back(std::move(name), std::move(type), loc);
		return m_params.back();
	}

	const Variable& add_variable(std::string name, Type type, const Location& loc)
	{
		m_vars.emplace_back(std::move(name), std::move(type), loc);
		return m_vars.back();
	}

	Pool& add_pool(std::string name, const Location& loc)
	{
		m_pools.emplace_back(std::move(name), loc);
		return m_pools.back();
	}
};

class Class
{
	std::string m_name;

	std::unordered_map<std::string, Pool> m_pool_map;
	std::vector<std::reference_wrapper<Pool>> m_pools;

	std::unordered_map<std::string, Field> m_field_map;
	std::unordered_map<const Field*, size_t> m_field_indices;
	std::vector<std::reference_wrapper<Field>> m_fields;

	std::unordered_map<std::string, Method> m_method_map;
	std::vector<std::reference_wrapper<Method>> m_methods;

	std::vector<std::reference_wrapper<const Layout>> m_layouts;

	Location m_loc;

public:
	using pools_iterator = decltype(m_pools)::iterator;
	using pools_const_iterator = decltype(m_pools)::const_iterator;

	Class(const Class&) = delete;
	Class& operator=(const Class&) = delete;

	Class(Class&&) = default;
	Class& operator=(Class&&) = default;

	explicit Class(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	size_t num_pools() const { return m_pools.size(); }

	const Pool* find_pool(const std::string& name) const;
	Pool* find_pool(const std::string& name);

	const Field* find_field(const std::string& name) const;
	Field* find_field(const std::string& name);

	const Method* find_method(const std::string& name) const;
	Method* find_method(const std::string& name);

	std::pair<Pool*, bool> add_pool(std::string name, const Location& loc);
	std::pair<Field*, bool> add_field(std::string name, const Location& loc);
	std::pair<Method*, bool> add_method(std::string name, const Location& loc);

	const std::vector<std::reference_wrapper<Pool>>& pools() const {
		return m_pools;
	}
	const std::vector<std::reference_wrapper<Field>>& fields() const {
		return m_fields;
	}
	const std::vector<std::reference_wrapper<Method>>& methods() const {
		return m_methods;
	}
	const std::vector<std::reference_wrapper<const Layout>>& layouts() const {
		return m_layouts;
	}

	size_t index_of(const Field& field) const;

	void add_layout(const Layout& layout) { m_layouts.emplace_back(layout); }

	const std::string& name() const { return m_name; }
	const Location& loc()     const { return m_loc;  }

	ObjectType this_object_type() const;
	BoundType this_bound_type() const;
};

class Program
{
	std::unordered_map<std::string, Class> m_class_map;
	std::vector<std::reference_wrapper<const Class>> m_classes;

	std::unordered_map<std::string, Layout> m_layout_map;
	std::vector<std::reference_wrapper<const Layout>> m_layouts;

public:
	const std::unordered_map<std::string, Class>& classes() const { return m_class_map; }
	const std::unordered_map<std::string, Layout>& layouts() const { return m_layout_map; }

	const std::vector<std::reference_wrapper<const Class>>& ordered_classes() const { return m_classes; }
	const std::vector<std::reference_wrapper<const Layout>>& ordered_layouts() const { return m_layouts; }

	const Layout* find_layout(const std::string& name) const;

	const Class* find_class(const std::string& name) const;
	Class* find_class(const std::string& name);

	std::pair<Class*, bool> add_class(std::string name, const Location& loc);
	std::pair<Layout*, bool> add_layout(
		std::string name, const Class& for_class, const Location& loc);
};

void debug_ast(const Program& ast);

} // namespace Ast
