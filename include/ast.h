#pragma once

#include "parse_tree_common.h"

#include <cstdint>
#include <deque>
#include <memory>
#include <vector>
#include <unordered_map>
#include <utility>

namespace Ast
{

class Class;
class Field;
class Method;
class Layout;

class PoolParameter;
class Pool;
class Type;

class PoolType
{
public:
	class BoundType: public BaseVisitable<BoundType>
	{
		const Class& m_class;
		std::vector<std::reference_wrapper<const Pool>> m_params;

	public:
		explicit BoundType(const Class& of_class,
						   std::vector<std::reference_wrapper<const Pool>> params)
			: m_class(of_class)
			, m_params(std::move(params))
		{}

		const Class& of_class() const { return m_class; }

		decltype(m_params)::iterator begin() { return m_params.begin(); }
		decltype(m_params)::iterator end()   { return m_params.end();   }

		decltype(m_params)::const_iterator begin() const { return m_params.begin(); }
		decltype(m_params)::const_iterator end()   const { return m_params.end();   }
	};

	class LayoutType: public BaseVisitable<LayoutType>
	{
		const Layout& m_layout;
		std::vector<std::reference_wrapper<const PoolParameter>> m_params;

	public:
		explicit LayoutType(const Layout& layout,
							std::vector<std::reference_wrapper<const PoolParameter>> params)
			: m_layout(layout)
			, m_params(std::move(params))
		{}

		const Layout& layout() const { return m_layout; }
		const Class& of_class() const;

		decltype(m_params)::iterator begin() { return m_params.begin(); }
		decltype(m_params)::iterator end()   { return m_params.end();   }

		decltype(m_params)::const_iterator begin() const { return m_params.begin(); }
		decltype(m_params)::const_iterator end()   const { return m_params.end();   }

		bool is_compatible_with(const LayoutType& type) const;
	};

private:
	enum class Tag { BOUND, LAYOUT };

	Tag m_tag;
	union
	{
		BoundType m_bound;
		LayoutType m_layout;
	};

	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(PoolType& other);

	PoolType() = default;

public:
	PoolType(const PoolType&) = delete;
	PoolType& operator=(const PoolType&) = delete;

	PoolType(PoolType&&);
	PoolType& operator=(PoolType&&);

	~PoolType();

	const Location& loc() const { return m_loc; }
};

class Pool: public BaseVisitable<Pool>
{
	std::string m_name;
	PoolType m_type;

	Location m_loc;

public:
	explicit Pool(std::string name, PoolType type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const PoolType& type() const { return m_type; }
	const Location& loc() const { return m_loc; }
};

class PoolParameter
{
public:
	class PoolRef: public BaseVisitable<PoolRef>
	{
		const Pool& m_pool;

	public:
		explicit PoolRef(const Pool& pool)
			: m_pool(pool)
		{}

		const Pool& pool() const { return m_pool; }
	};

	class None: public BaseVisitable<None>
	{
	};

private:
	enum class Tag { POOL, NONE };

	Tag m_tag;
	union
	{
		PoolRef m_pool;
		None m_none;
	};

	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(PoolParameter& other);

	PoolParameter() = default;

public:
	PoolParameter(const PoolParameter&) = delete;
	PoolParameter& operator=(const PoolParameter&) = delete;

	PoolParameter(PoolParameter&&);
	PoolParameter& operator=(PoolParameter&&);

	~PoolParameter();

	const Location& loc() const { return m_loc; }

	const PoolRef* as_pool() const;
	const None* as_none() const;

	bool operator==(const PoolParameter& rhs) const;
	bool operator!=(const PoolParameter& rhs) const { return !(*this == rhs); }
};

class Type
{
public:
	enum class PrimitiveKind
	{
		BOOL, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64
	};

	class PrimitiveType: public BaseVisitable<PrimitiveType>
	{
		PrimitiveKind m_kind;

	public:
		explicit PrimitiveType(PrimitiveKind kind)
			: m_kind(kind)
		{}

		PrimitiveKind kind() const { return m_kind; }

		bool operator==(const PrimitiveType& oth) const { return m_kind == oth.m_kind; }
		bool operator!=(const PrimitiveType& oth) const { return m_kind != oth.m_kind; }

		bool is_floating_point() const
		{
			return m_kind == PrimitiveKind::F32 || m_kind == PrimitiveKind::F64;
		}

		bool is_boolean() const
		{
			return m_kind == PrimitiveKind::BOOL;
		}

		bool is_signed_integer() const
		{
			switch (m_kind) {
			case PrimitiveKind::I8:
			case PrimitiveKind::I16:
			case PrimitiveKind::I32:
			case PrimitiveKind::I64:
				return true;
			default:
				return false;
			}
		}

		bool is_unsigned_integer() const
		{
			switch (m_kind) {
			case PrimitiveKind::U8:
			case PrimitiveKind::U16:
			case PrimitiveKind::U32:
			case PrimitiveKind::U64:
				return true;
			default:
				return false;
			}
		}

		bool is_integer() const
		{
			return is_signed_integer() || is_unsigned_integer();
		}

		const char* to_string() const;
	};

	class NullType: public BaseVisitable<NullType>
	{
		const char* to_string() const;
	};

	class ObjectType: public BaseVisitable<ObjectType>
	{
		const Class& m_class;
		std::vector<PoolParameter> m_params;

	public:
		explicit ObjectType(const Class& of_class, std::vector<PoolParameter> params)
			: m_class(of_class)
			, m_params(std::move(params))
		{}

		const Class& of_class() const { return m_class; }

		decltype(m_params)::iterator begin() { return m_params.begin(); }
		decltype(m_params)::iterator end()   { return m_params.end();   }

		decltype(m_params)::const_iterator begin() const { return m_params.begin(); }
		decltype(m_params)::const_iterator end()   const { return m_params.end();   }

		bool operator==(const ObjectType& rhs) const;
		bool operator!=(const ObjectType& rhs) const { return !(*this == rhs); }
	};

private:
	enum class Tag { PRIMITIVE, OBJECT, NULLPTR };

	Tag m_tag;
	union
	{
		PrimitiveType m_primitive_type;
		ObjectType m_object_type;
		NullType m_null_type;
	};

	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(Type& other);

	Type() = default;

public:
	Type(const Type&) = delete;
	Type& operator=(const Type&) = delete;

	Type(Type&&);
	Type& operator=(Type&&);

	~Type();

	const PrimitiveType* as_primitive_type() const;
	const ObjectType* as_object_type() const;
	const NullType* as_null_type() const;

	static Type make_primitive_type(PrimitiveKind kind, const Location& loc);
	static Type make_object_type(const Class& of_class,
								 std::vector<PoolParameter> params,
								 const Location& loc);
	static Type make_null_type(const Location& loc);

	const Location& loc() const { return m_loc; }

	bool operator==(const Type& rhs) const;
	bool operator!=(const Type& rhs) const { return !(*this == rhs); }

	template <typename T>
	void accept(T& visitor) {
		switch (m_tag) {
		case Tag::PRIMITIVE:
			m_primitive_type.accept(visitor);
			break;

		case Tag::OBJECT:
			m_object_type.accept(visitor);
			break;

		case Tag::NULLPTR:
			m_null_type.accept(visitor);
			break;
		}
	}
};

class Variable: public BaseVisitable<Variable>
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

class Expr
{
public:
	enum class BinOp
	{
		PLUS, MINUS, TIMES, DIV, LAND, LOR, AND, OR, XOR, SHL, SHR, EQ, NE, LT, LE, GT, GE
	};

	enum class UnOp { PLUS, MINUS, NOT };

	class IntegerConst: public BaseVisitable<IntegerConst>
	{
		uint64_t m_value;

	public:
		explicit IntegerConst(uint64_t value);

		uint64_t value() const { return m_value; }
	};

	class BooleanConst: public BaseVisitable<BooleanConst>
	{
		bool m_value;

	public:
		explicit BooleanConst(bool value);

		uint64_t value() const { return m_value; }
	};

	class Null: public BaseVisitable<Null>
	{
	};

	class This: public BaseVisitable<This>
	{
		Type::ObjectType m_type;

	public:
		explicit This(Type::ObjectType type);

		const Type::ObjectType& type() const { return m_type; }
	};

	class Unary: public BaseVisitable<Unary>
	{
		std::unique_ptr<Expr> m_expr;
		UnOp m_op;

	public:
		explicit Unary(Expr expr, UnOp op);

		const Expr& expr() const { return *m_expr; }
		UnOp op() const { return m_op; }
	};

	class Binary: public BaseVisitable<Binary>
	{
		std::unique_ptr<Expr> m_lhs;
		std::unique_ptr<Expr> m_rhs;
		BinOp m_op;

	public:
		explicit Binary(Expr lhs, Expr::BinOp op, Expr rhs);

		const Expr& lhs() const { return *m_lhs; }
		const Expr& rhs() const { return *m_rhs; }
		BinOp op() const { return m_op; }
	};

	class IndexExpr: public BaseVisitable<IndexExpr>
	{
		const Pool& pool;
		std::unique_ptr<Expr> idx;

	public:
		explicit IndexExpr(const Pool& pool, Expr idx);
	};

	class VariableExpr: public BaseVisitable<VariableExpr>
	{
		const Variable& m_var;

	public:
		explicit VariableExpr(const Variable& var);

		const Variable& var() const { return m_var; }
	};

	class MethodCall: public BaseVisitable<MethodCall>
	{
		const Method& m_method;
		std::vector<Expr> m_args;

	public:
		explicit MethodCall(const Method& method, std::vector<Expr> args);

		const Method& method() const { return m_method; }

		decltype(m_args)::iterator begin() { return m_args.begin(); }
		decltype(m_args)::iterator end()   { return m_args.end();   }

		decltype(m_args)::const_iterator begin() const { return m_args.begin(); }
		decltype(m_args)::const_iterator end()   const { return m_args.end();   }

		size_t num_args() const { return m_args.size(); }
	};

	class FieldAccess: public BaseVisitable<FieldAccess>
	{
		std::unique_ptr<Expr> m_expr;
		const Field& m_field;

	public:
		explicit FieldAccess(Expr expr, const Field& field);

		const Expr& expr() const   { return *m_expr; }
		const Field& field() const { return m_field; }
	};

	class New: public BaseVisitable<New>
	{
		Type::ObjectType m_type;

	public:
		explicit New(Type::ObjectType type);

		const Type::ObjectType& type() const { return m_type; }
	};

private:
	enum class Tag {
		INTEGER_CONST,
		BOOLEAN_CONST,
		NULL_EXPR,
		THIS,
		UNARY,
		BINARY,
		INDEX,
		VARIABLE,
		METHOD_CALL,
		FIELD_ACCESS,
		NEW,
	};

	Tag m_tag;
	union
	{
		IntegerConst m_integer_const;
		BooleanConst m_boolean_const;
		Null m_null_expr;
		This m_this_expr;
		Unary m_unary;
		Binary m_binary;
		IndexExpr m_index_expr;
		VariableExpr m_variable_expr;
		MethodCall m_method_call;
		FieldAccess m_field_access;
		New m_new_expr;
	};
	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(Expr& other);

	Expr() = default;

public:

	Expr(const Expr&) = delete;
	Expr& operator=(const Expr&) = delete;

	Expr(Expr&& other);
	Expr& operator=(Expr&& other);

	static Expr make_integer_const(uint64_t value, const Location& loc);
	static Expr make_boolean_const(bool value, const Location& loc);
	static Expr make_null_expr(const Location& loc);
	static Expr make_this_expr(Type::ObjectType type, const Location& loc);
	static Expr make_unary(Expr lhs, UnOp op, const Location& loc);
	static Expr make_binary(Expr lhs, BinOp op, Expr rhs, const Location& loc);
	static Expr make_index_expr(const Pool& pool, Expr idx, const Location& loc);
	static Expr make_variable_expr(const Variable& var, const Location& loc);
	static Expr make_method_call(const Method& method,
								 std::vector<Expr> args,
								 const Location& loc);
	static Expr make_field_access(Expr expr, const Field& field, const Location& loc);
	static Expr make_new_expr(Type::ObjectType type, const Location& loc);

	~Expr();

	bool is_lvalue() const;
	Type type() const;
	const Location& loc() const { return m_loc; }

	const IntegerConst* as_integer_const() const;
	const BooleanConst* as_boolean_const() const;
	const Null* as_null_expr() const;
	const This* as_this_expr() const;
	const Unary* as_unary() const;
	const Binary* as_binary() const;
	const IndexExpr* as_index_expr() const;
	const VariableExpr* as_variable_expr() const;
	const MethodCall* as_method_call() const;
	const FieldAccess* as_field_access() const;
	const New* as_new_expr() const;

	template <typename T>
	void accept(T& visitor) {
		switch (m_tag) {
		case Tag::INTEGER_CONST:
			m_integer_const.accept(visitor);
			break;

		case Tag::BOOLEAN_CONST:
			m_boolean_const.accept(visitor);
			break;

		case Tag::NULL_EXPR:
			m_null_expr.accept(visitor);
			break;

		case Tag::THIS:
			m_this_expr.accept(visitor);
			break;

		case Tag::UNARY:
			m_unary.accept(visitor);
			break;

		case Tag::BINARY:
			m_binary.accept(visitor);
			break;

		case Tag::INDEX:
			m_index_expr.accept(visitor);
			break;

		case Tag::VARIABLE:
			m_variable_expr.accept(visitor);
			break;

		case Tag::METHOD_CALL:
			m_method_call.accept(visitor);
			break;

		case Tag::FIELD_ACCESS:
			m_field_access.accept(visitor);
			break;

		case Tag::NEW:
			m_new_expr.accept(visitor);
			break;
		}
	}
};

class Field: public BaseVisitable<Field>
{
	std::string m_name;
	std::unique_ptr<Type> m_type;

	Location m_loc;

public:
	explicit Field(std::string name, Type type, const Location& loc);

	const std::string& name() const { return m_name; }
	const Type& type() const { return *m_type; }
	const Location& loc() const { return m_loc; }
};

class Cluster: public BaseVisitable<Cluster>
{
	std::vector<std::reference_wrapper<const Field>> m_fields;

public:
	explicit Cluster(std::vector<std::reference_wrapper<const Field>> fields)
		: m_fields(std::move(fields))
	{}

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }
};

class Layout: public BaseVisitable<Layout>
{
	std::string m_name;
	const Class& m_class;
	std::vector<Cluster> m_clusters;
	Location m_loc;

public:
	explicit Layout(std::string name,
					const Class& for_class,
					std::vector<Cluster> clusters,
					const Location& loc)
		: m_name(std::move(name))
		, m_class(for_class)
		, m_clusters(std::move(clusters))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name;  }
	const Class& for_class() const { return m_class; }

	decltype(m_clusters)::const_iterator clusters_begin() const { return m_clusters.cbegin(); }
	decltype(m_clusters)::const_iterator clusters_end() const { return m_clusters.cend(); }

	const Location& loc() const { return m_loc; }
};

#if 0

class AssignStmt: public Stmt
{
	std::unique_ptr<Expr> m_lhs;
	std::unique_ptr<Expr> m_rhs;

public:
	explicit AssignStmt(std::unique_ptr<Expr> lhs,
						std::unique_ptr<Expr> rhs)
		: m_lhs(std::move(lhs))
		, m_rhs(std::move(rhs))
	{}

	const Expr& lhs() const { return *m_lhs; }
	const Expr& rhs() const { return *m_rhs; }

	DEFINE_VISITOR_DISPATCH
};

class OpAssignStmt: public Stmt
{
	std::unique_ptr<Expr> m_lhs;
	BinOp m_op;
	std::unique_ptr<Expr> m_rhs;

public:
	explicit OpAssignStmt(std::unique_ptr<Expr> lhs,
						  BinOp op,
						  std::unique_ptr<Expr> rhs)
		: m_lhs(std::move(lhs))
		, m_op(op)
		, m_rhs(std::move(rhs))
	{}

	const Expr& lhs() const { return *m_lhs; }
	const Expr& rhs() const { return *m_rhs; }

	BinOp op()  const { return m_op; }

	DEFINE_VISITOR_DISPATCH
};

class IfStmt: public Stmt
{
	std::unique_ptr<Expr> m_cond;
	std::vector<std::unique_ptr<Stmt>> m_then_branch;
	std::vector<std::unique_ptr<Stmt>> m_else_branch;

public:
	explicit IfStmt(std::unique_ptr<Expr> cond,
					std::vector<std::unique_ptr<Stmt>> then_branch,
					std::vector<std::unique_ptr<Stmt>> else_branch)
		: m_cond(std::move(cond))
		, m_then_branch(std::move(then_branch))
		, m_else_branch(std::move(else_branch))
	{}

	const Expr& cond() const { return *m_cond; }

	decltype(m_then_branch)::const_iterator then_branch_begin() const { return m_then_branch.cbegin(); }
	decltype(m_then_branch)::const_iterator then_branch_end()   const { return m_then_branch.cend();   }

	decltype(m_else_branch)::const_iterator else_branch_begin() const { return m_else_branch.cbegin(); }
	decltype(m_else_branch)::const_iterator else_branch_end()   const { return m_else_branch.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class WhileStmt: public Stmt
{
	std::unique_ptr<Expr> m_cond;
	std::vector<std::unique_ptr<Stmt>> m_body;

public:
	explicit WhileStmt(std::unique_ptr<Expr> cond,
					   std::vector<std::unique_ptr<Stmt>> body)
		: m_cond(std::move(cond))
		, m_body(std::move(body))
	{}

	const Expr& cond() const { return *m_cond; }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class ForeachRangeStmt: public Stmt
{
	const Variable* m_var;
	std::unique_ptr<Expr> m_range_begin;
	std::unique_ptr<Expr> m_range_end;
	std::vector<std::unique_ptr<Stmt>> m_body;

public:
	explicit ForeachRangeStmt(const Variable* var,
							  std::unique_ptr<Expr> range_begin,
							  std::unique_ptr<Expr> range_end,
							  std::vector<std::unique_ptr<Stmt>> body)
		: m_var(var)
		, m_range_begin(std::move(range_begin))
		, m_range_end(std::move(range_end))
		, m_body(std::move(body))
	{
		assert(var != nullptr);
	}

	const Variable& var() const { return *m_var; }

	const Expr& range_begin() const { return *m_range_begin; }
	const Expr& range_end()   const { return *m_range_end;   }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class ForeachPoolStmt: public Stmt
{
	const Variable* m_var;
	const Pool* m_pool;
	std::vector<std::unique_ptr<Stmt>> m_body;

public:
	explicit ForeachPoolStmt(const Variable* var,
							 const Pool* pool,
							 std::vector<std::unique_ptr<Stmt>> body)
		: m_var(var)
		, m_pool(pool)
		, m_body(std::move(body))
	{
		assert(var != nullptr);
		assert(pool != nullptr);
	}

	const Variable& var() const { return *m_var;  }
	const Pool& pool()    const { return *m_pool; }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	DEFINE_VISITOR_DISPATCH
};

class ExprStmt: public Stmt
{
	std::unique_ptr<Expr> m_expr;

public:
	explicit ExprStmt(std::unique_ptr<Expr> expr)
		: m_expr(std::move(expr))
	{}

	const Expr& expr() const { return *m_expr; }

	DEFINE_VISITOR_DISPATCH
};

class BreakStmt: public Stmt
{
	const Stmt* m_stmt = nullptr;

public:
	void set_stmt(const Stmt& stmt) { m_stmt = &stmt; }
	const Stmt& stmt() const { return *m_stmt; }

	DEFINE_VISITOR_DISPATCH
};

class ContinueStmt: public Stmt
{
	const Stmt* m_stmt;

public:
	void set_stmt(const Stmt& stmt) { m_stmt = &stmt; }
	const Stmt& stmt() const { return *m_stmt; }

	DEFINE_VISITOR_DISPATCH
};

class ReturnStmt: public Stmt
{
	std::unique_ptr<Expr> m_expr;

public:
	explicit ReturnStmt(std::unique_ptr<Expr> expr = nullptr)
		: m_expr(std::move(expr))
	{}

	const Expr* expr() const { return m_expr.get(); }

	DEFINE_VISITOR_DISPATCH
};

class Method: public Visitable
{
	std::string m_name;

	std::vector<Pool> m_pools;
	std::deque<Variable> m_vars;

	size_t m_param_count;

	std::unique_ptr<Type> m_return_type;
	std::vector<std::unique_ptr<Type>> m_body;

	Location m_loc;

public:
	explicit Method(std::string name,
					std::unique_ptr<Type> return_type,
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

	const Type* return_type() const { return m_return_type.get(); }

	decltype(m_body)::const_iterator body_begin() const { return m_body.cbegin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.cend();   }

	const Location& loc() const { return m_loc; }

	void add_parameter(Variable param)
	{
		m_vars.emplace_back(std::move(param));
		m_param_count++;
	}

	void add_variable(Variable var)
	{
		m_vars.emplace_back(std::move(var));
	}

	DEFINE_VISITOR_DISPATCH
};

class Class: public Visitable
{
	std::string m_name;

	std::unordered_map<std::string, Pool> m_pool_map;
	std::vector<const Pool*> m_pools;

	std::unordered_map<std::string, Field> m_field_map;
	std::vector<const Field*> m_fields;

	std::unordered_map<std::string, Method> m_method_map;
	std::vector<const Method*> m_methods;

	Location m_loc;

public:
	explicit Class(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	void set_pools(std::unordered_map<std::string, Pool> pool_map,
				   std::vector<const Pool*> pools)
	{
		m_pool_map = std::move(pool_map);
		m_pools    = std::move(pools);
	}

	void set_methods(std::unordered_map<std::string, Method> method_map,
					 std::vector<const Method*> methods)
	{
		m_method_map = std::move(method_map);
		m_methods    = std::move(methods);
	}

	void set_fields(std::unordered_map<std::string, Field> field_map,
					std::vector<const Field*> fields)
	{
		m_field_map = std::move(field_map);
		m_fields    = std::move(fields);
	}

	size_t num_pools() const { return m_pools.size(); }

	const Pool* find_pool(const std::string& name) const
	{
		auto it = m_pool_map.find(name);
		if (it == m_pool_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	Pool* find_pool(const std::string& name)
	{
		auto it = m_pool_map.find(name);
		if (it == m_pool_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	const Field* find_field(const std::string& name) const
	{
		auto it = m_field_map.find(name);
		if (it == m_field_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	const Method* find_method(const std::string& name) const
	{
		auto it = m_method_map.find(name);
		if (it == m_method_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	Method* find_method(const std::string& name)
	{
		auto it = m_method_map.find(name);
		if (it == m_method_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	decltype(m_pools)::const_iterator pools_begin() const { return m_pools.cbegin(); }
	decltype(m_pools)::const_iterator pools_end()   const { return m_pools.cend();   }

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.cbegin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.cend();   }

	decltype(m_methods)::const_iterator methods_begin() const { return m_methods.cbegin(); }
	decltype(m_methods)::const_iterator methods_end()   const { return m_methods.cend();   }

	const std::string& name() const { return m_name; }
	const Location& loc()     const { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class Program: public Visitable
{
	std::unordered_map<std::string, Class> m_class_map;
	std::vector<const Class*> m_classes;

	std::unordered_map<std::string, Layout> m_layout_map;
	std::vector<const Layout*> m_layouts;

public:
	decltype(m_classes)::const_iterator classes_begin() const { return m_classes.cbegin(); }
	decltype(m_classes)::const_iterator classes_end()   const { return m_classes.cend();   }

	decltype(m_layouts)::const_iterator layouts_begin() const { return m_layouts.cbegin(); }
	decltype(m_layouts)::const_iterator layouts_end()   const { return m_layouts.cend();   }

	const Class* find_class(const std::string& name) const
	{
		auto it = m_class_map.find(name);
		if (it == m_class_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	const Layout* find_layout(const std::string& name) const
	{
		auto it = m_layout_map.find(name);
		if (it == m_layout_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	Class* find_class(const std::string& name)
	{
		auto it = m_class_map.find(name);
		if (it == m_class_map.end()) {
			return nullptr;
		}
		return &it->second;
	}

	void set_classes(std::unordered_map<std::string, Class> class_map,
					 std::vector<const Class*> classes)
	{
		m_class_map = std::move(class_map);
		m_classes = std::move(classes);
	}

	void set_layouts(std::unordered_map<std::string, Layout> layout_map,
					 std::vector<const Layout*> layouts)
	{
		m_layout_map = std::move(layout_map);
		m_layouts = std::move(layouts);
	}

	DEFINE_VISITOR_DISPATCH
};

extern void run_semantic_analysis(const Cst::Program& cst, SemanticErrorList* errors, Program* ast);

#endif

#undef DEFINE_VISITOR_DISPATCH
}
