#pragma once

#include "parse_tree_common.h"

#include <cassert>
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
class Pool;

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

public:
	explicit PoolParameter(PoolRef pool, const Location& loc)
		: m_tag(Tag::POOL)
		, m_pool(pool)
		, m_loc(loc)
	{}

	explicit PoolParameter(None none, const Location& loc)
		: m_tag(Tag::NONE)
		, m_none(none)
		, m_loc(loc)
	{}

	PoolParameter(const PoolParameter&);
	PoolParameter& operator=(const PoolParameter&);

	PoolParameter(PoolParameter&&);
	PoolParameter& operator=(PoolParameter&&);

	~PoolParameter();

	const Location& loc() const { return m_loc; }

	const PoolRef* as_pool() const
	{
		return (m_tag == Tag::POOL)
			? &m_pool
			: nullptr;
	}

	const None* as_none() const
	{
		return (m_tag == Tag::NONE)
			? &m_none
			: nullptr;
	}

	bool operator==(const PoolParameter& rhs) const;
	bool operator!=(const PoolParameter& rhs) const { return !(*this == rhs); }

	template <typename T>
	void accept(T& visitor) const {
		switch (m_tag) {
		case Tag::POOL:
			m_pool.accept(visitor);
			break;

		case Tag::NONE:
			m_none.accept(visitor);
			break;
		}
	}
};

class PoolType
{
public:
	class BoundType: public BaseVisitable<BoundType>
	{
		const Class& m_class;
		std::vector<PoolParameter> m_params;

		Location m_loc;

	public:
		using iterator = decltype(m_params)::iterator;
		using const_iterator = decltype(m_params)::const_iterator;

		explicit BoundType(const Class& of_class, std::vector<PoolParameter> params, const Location& loc)
			: m_class(of_class)
			, m_params(std::move(params))
			, m_loc(loc)
		{}

		const Class& of_class() const { return m_class; }

		iterator begin() { return m_params.begin(); }
		iterator end()   { return m_params.end();   }

		const_iterator begin() const { return m_params.begin(); }
		const_iterator end()   const { return m_params.end();   }

		size_t num_params() const { return m_params.size(); }

		const Location& loc() const { return m_loc; }

		bool operator==(const BoundType& rhs) const
		{
			if (&m_class != &rhs.m_class) {
				return false;
			}

			if (num_params() != rhs.num_params()) {
				return false;
			}

			return std::equal(begin(), end(), rhs.begin());
		}

		bool operator!=(const BoundType& rhs) const { return !(*this == rhs); }

		bool compatible_with_bound(const BoundType& bound) const
		{
			if (&m_class != &bound.m_class) {
				return false;
			}

			if (num_params() != bound.num_params()) {
				return false;
			}

			return std::equal(begin(), end(), bound.begin());
		}
	};

	class LayoutType: public BaseVisitable<LayoutType>
	{
		const Layout& m_layout;
		std::vector<PoolParameter> m_params;

		Location m_loc;

	public:
		using iterator = decltype(m_params)::iterator;
		using const_iterator = decltype(m_params)::const_iterator;

		explicit LayoutType(const Layout& layout,
							std::vector<PoolParameter> params,
							const Location& loc)
			: m_layout(layout)
			, m_params(std::move(params))
			, m_loc(loc)
		{}

		const Layout& layout() const { return m_layout; }
		const Class& of_class() const;

		const_iterator begin() const { return m_params.begin(); }
		const_iterator end()   const { return m_params.end();   }

		size_t num_params() const { return m_params.size(); }

		iterator begin() { return m_params.begin(); }
		iterator end()   { return m_params.end();   }

		const Location& loc() const { return m_loc; }

		bool compatible_with_bound(const BoundType& bound) const
		{
			if (&of_class() != &bound.of_class()) {
				return false;
			}

			if (num_params() != bound.num_params()) {
				return false;
			}

			return std::equal(begin(), end(), bound.begin());
		}
	};

private:
	enum class Tag { INVALID, BOUND, LAYOUT };

	Tag m_tag;
	union
	{
		BoundType m_bound;
		LayoutType m_layout;
	};

	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(PoolType& other);

public:
	PoolType()
		: m_tag(Tag::INVALID)
	{}

	explicit PoolType(BoundType bound, const Location& loc)
		: m_tag(Tag::BOUND)
		, m_bound(std::move(bound))
		, m_loc(loc)
	{}

	explicit PoolType(LayoutType layout, const Location& loc)
		: m_tag(Tag::LAYOUT)
		, m_layout(std::move(layout))
		, m_loc(loc)
	{}

	PoolType(const PoolType&) = delete;
	PoolType& operator=(const PoolType&) = delete;

	PoolType(PoolType&&);
	PoolType& operator=(PoolType&&);

	~PoolType();

	bool valid() const { return m_tag != Tag::INVALID; }

	bool compatible_with_bound(const BoundType& bound) const;

	const BoundType* as_bound_type() const
	{
		return (m_tag == Tag::BOUND)
			? &m_bound
			: nullptr;
	}

	const LayoutType* as_layout_type() const
	{
		return (m_tag == Tag::LAYOUT)
			? &m_layout
			: nullptr;
	}

	const Location& loc() const { return m_loc; }

	template <typename T>
	void accept(T& visitor) const {
		switch (m_tag) {
		case Tag::INVALID:
			assert(false && "Type is in an invalid state");
			break;

		case Tag::BOUND:
			m_bound.accept(visitor);
			break;

		case Tag::LAYOUT:
			m_layout.accept(visitor);
			break;
		}
	}
};

class Pool: public BaseVisitable<Pool>
{
	std::string m_name;
	PoolType m_type;

	Location m_loc;

public:
	explicit Pool(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	explicit Pool(std::string name, PoolType type, const Location& loc)
		: m_name(std::move(name))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const PoolType& type() const { return m_type; }
	const Location& loc() const { return m_loc; }

	void set_type(PoolType type)
	{
		assert(type.valid());
		m_type = std::move(type);
	}
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
	};

	class NullType: public BaseVisitable<NullType>
	{
	};

	class VoidType: public BaseVisitable<VoidType>
	{
	};

	class ObjectType: public BaseVisitable<ObjectType>
	{
		const Class& m_class;
		std::vector<PoolParameter> m_params;

		Location m_loc;

	public:
		using iterator = decltype(m_params)::iterator;
		using const_iterator = decltype(m_params)::const_iterator;

		explicit ObjectType(const Class& of_class, std::vector<PoolParameter> params, const Location& loc)
			: m_class(of_class)
			, m_params(std::move(params))
			, m_loc(loc)
		{}

		const Class& of_class() const { return m_class; }

		size_t num_params() const { return m_params.size(); }

		const Location& loc() const { return m_loc; }

		iterator begin() { return m_params.begin(); }
		iterator end()   { return m_params.end();   }

		const_iterator begin() const { return m_params.begin(); }
		const_iterator end()   const { return m_params.end();   }

		bool operator==(const ObjectType& rhs) const;
		bool operator!=(const ObjectType& rhs) const { return !(*this == rhs); }
	};

private:
	enum class Tag { INVALID, PRIMITIVE, OBJECT, NULLPTR, VOID };

	Tag m_tag;
	union
	{
		PrimitiveType m_primitive_type;
		ObjectType m_object_type;
		NullType m_null_type;
		VoidType m_void_type;
	};

	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(Type& other);

public:
	Type()
		: m_tag(Tag::INVALID)
	{}

	explicit Type(PrimitiveType primitive_type, const Location& loc)
		: m_tag(Tag::PRIMITIVE)
		, m_primitive_type(std::move(primitive_type))
		, m_loc(loc)
	{}

	explicit Type(ObjectType object_type, const Location& loc)
		: m_tag(Tag::OBJECT)
		, m_object_type(std::move(object_type))
		, m_loc(loc)
	{}

	explicit Type(NullType null_type, const Location& loc)
		: m_tag(Tag::NULLPTR)
		, m_null_type(std::move(null_type))
		, m_loc(loc)
	{}

	explicit Type(VoidType void_type)
		: m_tag(Tag::VOID)
		, m_void_type(std::move(void_type))
		, m_loc(Location())
	{}

	Type(const Type&);
	Type& operator=(const Type&);

	Type(Type&&);
	Type& operator=(Type&&);

	~Type();

	const PrimitiveType* as_primitive_type() const;
	const ObjectType* as_object_type() const;
	const NullType* as_null_type() const;

	bool valid() const { return m_tag != Tag::INVALID; }
	const Location& loc() const { return m_loc; }

	bool assignable_to(const Type& assigned_to) const
	{
		switch (m_tag) {
		case Tag::INVALID:
			assert(false);
			break;

		case Tag::PRIMITIVE: {
			const auto* primitive_type = assigned_to.as_primitive_type();
			return primitive_type != nullptr && this->m_primitive_type == *primitive_type;
		}

		case Tag::OBJECT: {
			const auto* object_type = assigned_to.as_object_type();
			return object_type != nullptr && this->m_object_type == *object_type;
		}

		case Tag::NULLPTR:
			return assigned_to.as_object_type() != nullptr;

		default:
			return false;
		}
	}

	bool operator==(const Type& rhs) const;
	bool operator!=(const Type& rhs) const { return !(*this == rhs); }

	template <typename T>
	void accept(T& visitor) const {
		switch (m_tag) {
		case Tag::INVALID:
			assert(false);
			break;

		case Tag::PRIMITIVE:
			m_primitive_type.accept(visitor);
			break;

		case Tag::OBJECT:
			m_object_type.accept(visitor);
			break;

		case Tag::NULLPTR:
			m_null_type.accept(visitor);
			break;

		case Tag::VOID:
			m_void_type.accept(visitor);
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
		explicit IntegerConst(uint64_t value)
			: m_value(value)
		{}

		uint64_t value() const { return m_value; }
	};

	class BooleanConst: public BaseVisitable<BooleanConst>
	{
		bool m_value;

	public:
		explicit BooleanConst(bool value)
			: m_value(value)
		{}

		uint64_t value() const { return m_value; }
	};

	class Null: public BaseVisitable<Null>
	{
	};

	class This: public BaseVisitable<This>
	{
		Type::ObjectType m_type;

	public:
		explicit This(Type::ObjectType type)
			: m_type(std::move(type))
		{}

		const Type::ObjectType& type() const { return m_type; }
	};

	class Cast: public BaseVisitable<Cast>
	{
		std::unique_ptr<Expr> m_expr;
		Type::PrimitiveType m_type;

		Location m_loc;

	public:
		explicit Cast(Expr expr, Type::PrimitiveType type)
			: m_expr(std::unique_ptr<Expr>(new Expr(std::move(expr))))
			, m_type(std::move(type))
		{}

		const Expr& expr() const { return *m_expr; }
		const Type::PrimitiveType& type() const { return m_type; }
	};

	class Unary: public BaseVisitable<Unary>
	{
		std::unique_ptr<Expr> m_expr;
		UnOp m_op;

	public:
		explicit Unary(UnOp op, Expr expr)
			: m_expr(std::unique_ptr<Expr>(new Expr(std::move(expr))))
			, m_op(op)
		{}

		const Expr& expr() const { return *m_expr; }
		UnOp op() const { return m_op; }
	};

	class Binary: public BaseVisitable<Binary>
	{
		std::unique_ptr<Expr> m_lhs;
		std::unique_ptr<Expr> m_rhs;
		BinOp m_op;

	public:
		explicit Binary(Expr lhs, BinOp op, Expr rhs)
			: m_lhs(std::unique_ptr<Expr>(new Expr(std::move(lhs))))
			, m_rhs(std::unique_ptr<Expr>(new Expr(std::move(rhs))))
			, m_op(op)
		{}

		const Expr& lhs() const { return *m_lhs; }
		const Expr& rhs() const { return *m_rhs; }
		BinOp op() const { return m_op; }
	};

	class IndexExpr: public BaseVisitable<IndexExpr>
	{
		const Pool& m_pool;
		std::unique_ptr<Expr> m_idx;

	public:
		explicit IndexExpr(const Pool& pool, Expr idx)
			: m_pool(pool)
			, m_idx(std::unique_ptr<Expr>(new Expr(std::move(idx))))
		{}

		const Pool& pool() const { return m_pool; }
		const Expr& idx() const { return *m_idx; }
	};

	class VariableExpr: public BaseVisitable<VariableExpr>
	{
		const Variable& m_var;

	public:
		explicit VariableExpr(const Variable& var)
			: m_var(var)
		{}

		const Variable& var() const { return m_var; }
	};

	class MethodCall: public BaseVisitable<MethodCall>
	{
		const Method& m_method;
		std::unique_ptr<Expr> m_this_expr;
		std::vector<Expr> m_args;

	public:
		using iterator = decltype(m_args)::iterator;
		using const_iterator = decltype(m_args)::const_iterator;

		explicit MethodCall(const Method& method, Expr this_expr, std::vector<Expr> args)
			: m_method(method)
			, m_this_expr(std::unique_ptr<Expr>(new Expr(std::move(this_expr))))
			, m_args(std::move(args))
		{}

		const Method& method() const { return m_method; }

		const Expr& this_expr() const { return *m_this_expr; }

		iterator begin() { return m_args.begin(); }
		iterator end()   { return m_args.end();   }

		const_iterator begin() const { return m_args.begin(); }
		const_iterator end()   const { return m_args.end();   }

		size_t num_args() const { return m_args.size(); }
	};

	class FieldAccess: public BaseVisitable<FieldAccess>
	{
		std::unique_ptr<Expr> m_expr;
		const Field& m_field;

	public:
		explicit FieldAccess(Expr expr, const Field& field)
			: m_expr(std::unique_ptr<Expr>(new Expr(std::move(expr))))
			, m_field(field)
		{}

		const Expr& expr() const   { return *m_expr; }
		const Field& field() const { return m_field; }
	};

	class New: public BaseVisitable<New>
	{
		Type::ObjectType m_type;

	public:
		explicit New(Type::ObjectType type)
			: m_type(std::move(type))
		{}

		const Type::ObjectType& type() const { return m_type; }
	};

private:
	enum class Tag
	{
		INVALID,
		INTEGER_CONST,
		BOOLEAN_CONST,
		NULL_EXPR,
		THIS,
		CAST,
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
		Cast m_cast;
		Unary m_unary;
		Binary m_binary;
		IndexExpr m_index_expr;
		VariableExpr m_variable_expr;
		MethodCall m_method_call;
		FieldAccess m_field_access;
		New m_new_expr;
	};
	Type m_type;
	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(Expr& other);

public:
	Expr()
		: m_tag(Tag::INVALID)
	{}

	explicit Expr(IntegerConst integer_const, Type type, const Location& loc)
		: m_tag(Tag::INTEGER_CONST)
		, m_integer_const(std::move(integer_const))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(BooleanConst boolean_const, Type type, const Location& loc)
		: m_tag(Tag::BOOLEAN_CONST)
		, m_boolean_const(std::move(boolean_const))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(Null null_expr, Type type, const Location& loc)
		: m_tag(Tag::NULL_EXPR)
		, m_null_expr(std::move(null_expr))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(This this_expr, Type type, const Location& loc)
		: m_tag(Tag::THIS)
		, m_this_expr(std::move(this_expr))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(Cast cast, Type type, const Location& loc)
		: m_tag(Tag::CAST)
		, m_cast(std::move(cast))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(Unary unary, Type type, const Location& loc)
		: m_tag(Tag::UNARY)
		, m_unary(std::move(unary))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(Binary binary, Type type, const Location& loc)
		: m_tag(Tag::BINARY)
		, m_binary(std::move(binary))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(IndexExpr index_expr, Type type, const Location& loc)
		: m_tag(Tag::INDEX)
		, m_index_expr(std::move(index_expr))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(VariableExpr variable_expr, Type type, const Location& loc)
		: m_tag(Tag::VARIABLE)
		, m_variable_expr(std::move(variable_expr))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(MethodCall method_call, Type type, const Location& loc)
		: m_tag(Tag::METHOD_CALL)
		, m_method_call(std::move(method_call))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(FieldAccess field_access, Type type, const Location& loc)
		: m_tag(Tag::FIELD_ACCESS)
		, m_field_access(std::move(field_access))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	explicit Expr(New new_expr, Type type, const Location& loc)
		: m_tag(Tag::NEW)
		, m_new_expr(std::move(new_expr))
		, m_type(std::move(type))
		, m_loc(loc)
	{}

	Expr(const Expr&) = delete;
	Expr& operator=(const Expr&) = delete;

	Expr(Expr&& other);
	Expr& operator=(Expr&& other);

	~Expr();

	bool valid() const { return m_tag != Tag::INVALID; }

	bool is_lvalue() const;
	const Type& type() const { return m_type; }
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
	void accept(T& visitor) const {
		switch (m_tag) {
		case Tag::INVALID:
			assert(false && "Expression is in an invalid state");
			break;

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

		case Tag::CAST:
			m_cast.accept(visitor);
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
	Type m_type;

	Location m_loc;

public:
	explicit Field(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Type& type() const { return m_type; }
	const Location& loc() const { return m_loc; }

	void set_type(Type type) { m_type = std::move(type); }
};

class Cluster: public BaseVisitable<Cluster>
{
	std::vector<std::reference_wrapper<const Field>> m_fields;
	Location m_loc;

public:
	using const_iterator = decltype(m_fields)::const_iterator;
	using iterator = decltype(m_fields)::iterator;

	explicit Cluster(const Location& loc)
		: m_loc(loc)
	{}

	const_iterator begin() const { return m_fields.begin(); }
	const_iterator end()   const { return m_fields.end();   }

	iterator begin() { return m_fields.begin(); }
	iterator end()   { return m_fields.end();   }

	void add_field(const Field& field) { m_fields.emplace_back(field); }
};

class Layout: public BaseVisitable<Layout>
{
	std::string m_name;
	const Class& m_class;
	std::vector<Cluster> m_clusters;
	Location m_loc;

public:
	using const_iterator = decltype(m_clusters)::const_iterator;
	using iterator = decltype(m_clusters)::iterator;

	explicit Layout(std::string name,
					const Class& for_class,
					const Location& loc)
		: m_name(std::move(name))
		, m_class(for_class)
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name;  }
	const Class& for_class() const { return m_class; }

	const_iterator begin() const { return m_clusters.begin(); }
	const_iterator end()   const { return m_clusters.end();   }

	iterator begin() { return m_clusters.begin(); }
	iterator end()   { return m_clusters.end();   }

	const Location& loc() const { return m_loc; }

	Cluster& add_cluster(const Location& loc)
	{
		m_clusters.emplace_back(loc);
		return m_clusters.back();
	}
};

class Stmt
{
public:
	class Assignment: public BaseVisitable<Assignment>
	{
		std::unique_ptr<Expr> m_lhs;
		std::unique_ptr<Expr> m_rhs;

	public:
		explicit Assignment(Expr lhs, Expr rhs)
			: m_lhs(std::unique_ptr<Expr>(new Expr(std::move(lhs))))
			, m_rhs(std::unique_ptr<Expr>(new Expr(std::move(rhs))))
		{}

		const Expr& lhs() const { return *m_lhs; }
		const Expr& rhs() const { return *m_rhs; }
	};

	class OpAssignment: public BaseVisitable<OpAssignment>
	{
		std::unique_ptr<Expr> m_lhs;
		std::unique_ptr<Expr> m_rhs;
		Expr::BinOp m_op;

	public:
		explicit OpAssignment(Expr lhs, Expr::BinOp op, Expr rhs)
			: m_lhs(std::unique_ptr<Expr>(new Expr(std::move(lhs))))
			, m_rhs(std::unique_ptr<Expr>(new Expr(std::move(rhs))))
			, m_op(op)
		{}

		const Expr& lhs() const { return *m_lhs; }
		const Expr& rhs() const { return *m_rhs; }
		Expr::BinOp op()  const { return m_op; }
	};

	class If: public BaseVisitable<If>
	{
		Expr m_cond;
		std::vector<Stmt> m_then_branch;
		std::vector<Stmt> m_else_branch;

	public:
		explicit If(Expr cond, std::vector<Stmt> then_branch, std::vector<Stmt> else_branch)
			: m_cond(std::move(cond))
			, m_then_branch(std::move(then_branch))
			, m_else_branch(std::move(else_branch))
		{}

		const Expr& cond() const { return m_cond; }

		decltype(m_then_branch)::const_iterator then_begin() const { return m_then_branch.begin(); }
		decltype(m_then_branch)::const_iterator then_end()   const { return m_then_branch.end();   }

		decltype(m_else_branch)::const_iterator else_begin() const { return m_else_branch.begin(); }
		decltype(m_else_branch)::const_iterator else_end()   const { return m_else_branch.end();   }
	};

	class While: public BaseVisitable<While>
	{
		Expr m_cond;
		std::vector<Stmt> m_body;

	public:
		using iterator = decltype(m_body)::iterator;
		using const_iterator = decltype(m_body)::const_iterator;

		explicit While(Expr cond, std::vector<Stmt> body)
			: m_cond(std::move(cond))
			, m_body(std::move(body))
		{}

		const Expr& cond() const { return m_cond; }

		const_iterator begin() const { return m_body.begin(); }
		const_iterator end()   const { return m_body.end();   }

		iterator begin() { return m_body.begin(); }
		iterator end()   { return m_body.end();   }
	};

	class ForeachRange: public BaseVisitable<ForeachRange>
	{
		const Variable& m_var;
		Expr m_range_begin;
		Expr m_range_end;
		std::vector<Stmt> m_body;

	public:
		using iterator = decltype(m_body)::iterator;
		using const_iterator = decltype(m_body)::const_iterator;

		explicit ForeachRange(const Variable& var,
							  Expr range_begin,
							  Expr range_end,
							  std::vector<Stmt> body)
			: m_var(var)
			, m_range_begin(std::move(range_begin))
			, m_range_end(std::move(range_end))
			, m_body(std::move(body))
		{}

		const Variable& var() const { return m_var; }

		const Expr& range_begin() const { return m_range_begin; }
		const Expr& range_end()   const { return m_range_end;   }

		const_iterator begin() const { return m_body.begin(); }
		const_iterator end()   const { return m_body.end();   }

		iterator begin() { return m_body.begin(); }
		iterator end()   { return m_body.end();   }
	};

	class ForeachPool: public BaseVisitable<ForeachPool>
	{
		const Variable& m_var;
		const Pool& m_pool;
		std::vector<Stmt> m_body;

	public:
		using iterator = decltype(m_body)::iterator;
		using const_iterator = decltype(m_body)::const_iterator;

		explicit ForeachPool(const Variable& var, const Pool& pool, std::vector<Stmt> body)
			: m_var(var)
			, m_pool(pool)
			, m_body(std::move(body))
		{}

		const Variable& var() const { return m_var;  }
		const Pool& pool()    const { return m_pool; }

		const_iterator begin() const { return m_body.begin(); }
		const_iterator end()   const { return m_body.end();   }

		iterator begin() { return m_body.begin(); }
		iterator end()   { return m_body.end();   }
	};

	class ExprStmt: public BaseVisitable<ExprStmt>
	{
		Expr m_expr;

	public:
		explicit ExprStmt(Expr expr)
			: m_expr(std::move(expr))
		{}

		const Expr& expr() const { return m_expr; }
	};

	class Break: public BaseVisitable<Break> { };

	class Continue: public BaseVisitable<Continue> { };

	class Return: public BaseVisitable<Return>
	{
		using ReturnExpr = Optional<Expr>;

		ReturnExpr m_expr;

	public:
		explicit Return(Expr expr)
			: m_expr(std::move(expr))
		{}

		explicit Return()
			: m_expr()
		{}

		const Expr* expr() const { return m_expr.get(); }
	};

	Stmt(const Stmt&) = delete;
	Stmt& operator=(const Stmt&) = delete;

	Stmt(Stmt&& other);
	Stmt& operator=(Stmt&& other);

	~Stmt();

private:
	enum class Tag
	{
		ASSIGNMENT,
		OP_ASSIGNMENT,
		IF,
		WHILE,
		FOREACH_RANGE,
		FOREACH_POOL,
		EXPR_STMT,
		BREAK,
		CONTINUE,
		RETURN,
	};

	Tag m_tag;
	union
	{
		Assignment m_assignment;
		OpAssignment m_op_assignment;
		If m_if;
		While m_while;
		ForeachRange m_foreach_range;
		ForeachPool m_foreach_pool;
		ExprStmt m_expr_stmt;
		Break m_break;
		Continue m_continue;
		Return m_return;
	};

	void destroy_variant();
	void construct_variant_from_other(Stmt& other);

public:
	explicit Stmt(Assignment assignment)
		: m_tag(Tag::ASSIGNMENT)
		, m_assignment(std::move(assignment))
	{}

	explicit Stmt(OpAssignment op_assignment)
		: m_tag(Tag::OP_ASSIGNMENT)
		, m_op_assignment(std::move(op_assignment))
	{}

	explicit Stmt(If if_stmt)
		: m_tag(Tag::IF)
		, m_if(std::move(if_stmt))
	{}

	explicit Stmt(While while_stmt)
		: m_tag(Tag::WHILE)
		, m_while(std::move(while_stmt))
	{}

	explicit Stmt(ForeachRange foreach_range)
		: m_tag(Tag::FOREACH_RANGE)
		, m_foreach_range(std::move(foreach_range))
	{}

	explicit Stmt(ForeachPool foreach_pool)
		: m_tag(Tag::FOREACH_POOL)
		, m_foreach_pool(std::move(foreach_pool))
	{}

	explicit Stmt(ExprStmt expr_stmt)
		: m_tag(Tag::EXPR_STMT)
		, m_expr_stmt(std::move(expr_stmt))
	{}

	explicit Stmt(Break break_stmt)
		: m_tag(Tag::BREAK)
		, m_break(std::move(break_stmt))
	{}

	explicit Stmt(Continue continue_stmt)
		: m_tag(Tag::CONTINUE)
		, m_continue(std::move(continue_stmt))
	{}

	explicit Stmt(Return return_stmt)
		: m_tag(Tag::RETURN)
		, m_return(std::move(return_stmt))
	{}

	template<typename T>
	void accept(T& visitor) const
	{
		switch (m_tag) {
		case Tag::ASSIGNMENT:
			visitor.visit(m_assignment);
			break;

		case Tag::OP_ASSIGNMENT:
			visitor.visit(m_op_assignment);
			break;

		case Tag::IF:
			visitor.visit(m_if);
			break;

		case Tag::WHILE:
			visitor.visit(m_while);
			break;

		case Tag::FOREACH_RANGE:
			visitor.visit(m_foreach_range);
			break;

		case Tag::FOREACH_POOL:
			visitor.visit(m_foreach_pool);
			break;

		case Tag::EXPR_STMT:
			visitor.visit(m_expr_stmt);
			break;

		case Tag::BREAK:
			visitor.visit(m_break);
			break;

		case Tag::CONTINUE:
			visitor.visit(m_continue);
			break;

		case Tag::RETURN:
			visitor.visit(m_return);
			break;
		}
	}
};

class Method: public BaseVisitable<Method>
{
	using ReturnType = Optional<Type>;

	std::string m_name;

	std::deque<Pool> m_pools;
	std::deque<Variable> m_vars;

	size_t m_param_count = 0;

	ReturnType m_return_type;
	std::vector<Stmt> m_body;

	Location m_loc;

public:
	explicit Method(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }

	decltype(m_vars)::const_iterator vars_begin() const { return m_vars.begin(); }
	decltype(m_vars)::const_iterator vars_end()   const { return m_vars.end();   }

	decltype(m_pools)::iterator pools_begin() { return m_pools.begin(); }
	decltype(m_pools)::iterator pools_end()   { return m_pools.end();   }

	decltype(m_pools)::const_iterator pools_begin() const { return m_pools.begin(); }
	decltype(m_pools)::const_iterator pools_end()   const { return m_pools.end();   }

	decltype(m_vars)::const_iterator params_begin() const { return vars_begin(); }
	decltype(m_vars)::const_iterator params_end()   const { return params_begin() + m_param_count; }

	size_t num_params() const { return m_param_count; }

	void set_return_type(Type type)
	{
		m_return_type = std::move(type);
	}

	void set_body(std::vector<Stmt> body)
	{
		m_body = std::move(body);
	}

	const Type* return_type() const { return m_return_type.get(); }

	decltype(m_body)::const_iterator body_begin() const { return m_body.begin(); }
	decltype(m_body)::const_iterator body_end()   const { return m_body.end();   }

	const Location& loc() const { return m_loc; }

	const Variable& add_parameter(std::string name, Type type, const Location& loc)
	{
		m_vars.emplace_back(std::move(name), std::move(type), loc);
		m_param_count++;
		return m_vars.back();
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

class Class: public BaseVisitable<Class>
{
	std::string m_name;

	std::unordered_map<std::string, Pool> m_pool_map;
	std::vector<std::reference_wrapper<Pool>> m_pools;

	std::unordered_map<std::string, Field> m_field_map;
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

	Field* find_field(const std::string& name)
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

	std::pair<Pool*, bool> add_pool(std::string name, const Location& loc)
	{
		auto name_copy = name;
		auto res = m_pool_map.emplace(
			std::move(name_copy),
			Pool(std::move(name), loc)
		);
		if (res.second) {
			m_pools.emplace_back(res.first->second);
		}
		return std::make_pair(&res.first->second, res.second);
	}

	std::pair<Field*, bool> add_field(std::string name, const Location& loc)
	{
		auto name_copy = name;
		auto res = m_field_map.emplace(
			std::move(name_copy),
			Field(std::move(name), loc)
		);
		if (res.second) {
			m_fields.emplace_back(res.first->second);
		}
		return std::make_pair(&res.first->second, res.second);
	}

	std::pair<Method*, bool> add_method(std::string name, const Location& loc)
	{
		auto name_copy = name;
		auto res = m_method_map.emplace(
			std::move(name_copy),
			Method(std::move(name), loc)
		);
		if (res.second) {
			m_methods.emplace_back(res.first->second);
		}
		return std::make_pair(&res.first->second, res.second);
	}

	pools_const_iterator pools_begin() const { return m_pools.begin(); }
	pools_const_iterator pools_end()   const { return m_pools.end();   }

	pools_iterator pools_begin() { return m_pools.begin(); }
	pools_iterator pools_end()   { return m_pools.end();   }

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.begin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.end();   }

	decltype(m_methods)::const_iterator methods_begin() const { return m_methods.begin(); }
	decltype(m_methods)::const_iterator methods_end()   const { return m_methods.end();   }

	decltype(m_layouts)::const_iterator layouts_begin() const { return m_layouts.begin(); }
	decltype(m_layouts)::const_iterator layouts_end()   const { return m_layouts.end();   }

	void add_layout(const Layout& layout) { m_layouts.emplace_back(layout); }

	const std::string& name() const { return m_name; }
	const Location& loc()     const { return m_loc;  }
};

class Program: public BaseVisitable<Program>
{
	std::unordered_map<std::string, Class> m_class_map;
	std::vector<std::reference_wrapper<const Class>> m_classes;

	std::unordered_map<std::string, Layout> m_layout_map;
	std::vector<std::reference_wrapper<const Layout>> m_layouts;

public:
	decltype(m_classes)::const_iterator classes_begin() const { return m_classes.begin(); }
	decltype(m_classes)::const_iterator classes_end()   const { return m_classes.end();   }

	decltype(m_layouts)::const_iterator layouts_begin() const { return m_layouts.begin(); }
	decltype(m_layouts)::const_iterator layouts_end()   const { return m_layouts.end();   }

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

	std::pair<Class*, bool> add_class(std::string name, const Location& loc)
	{
		auto name_copy = name;
		auto res = m_class_map.emplace(
			std::move(name_copy),
			Class(std::move(name), loc));
		if (res.second) {
			m_classes.emplace_back(res.first->second);
		}
		return std::make_pair(&res.first->second, res.second);
	}

	std::pair<Layout*, bool> add_layout(
		std::string name, const Class& for_class, const Location& loc)
	{
		auto name_copy = name;
		auto res = m_layout_map.emplace(
			std::move(name_copy),
			Layout(std::move(name), for_class, loc)
		);
		if (res.second) {
			m_layouts.emplace_back(res.first->second);
		}
		return std::make_pair(&res.first->second, res.second);
	}
};

class AstVisitor: public BaseVisitor
	, public Visitor<PoolType::BoundType>
	, public Visitor<PoolType::LayoutType>
	, public Visitor<Pool>
	, public Visitor<PoolParameter::PoolRef>
	, public Visitor<PoolParameter::None>
	, public Visitor<Type::PrimitiveType>
	, public Visitor<Type::NullType>
	, public Visitor<Type::ObjectType>
	, public Visitor<Type::VoidType>
	, public Visitor<Variable>
	, public Visitor<Expr::IntegerConst>
	, public Visitor<Expr::BooleanConst>
	, public Visitor<Expr::Null>
	, public Visitor<Expr::This>
	, public Visitor<Expr::Cast>
	, public Visitor<Expr::Unary>
	, public Visitor<Expr::Binary>
	, public Visitor<Expr::IndexExpr>
	, public Visitor<Expr::VariableExpr>
	, public Visitor<Expr::MethodCall>
	, public Visitor<Expr::FieldAccess>
	, public Visitor<Expr::New>
	, public Visitor<Field>
	, public Visitor<Cluster>
	, public Visitor<Layout>
	, public Visitor<Stmt::Assignment>
	, public Visitor<Stmt::OpAssignment>
	, public Visitor<Stmt::If>
	, public Visitor<Stmt::While>
	, public Visitor<Stmt::ForeachRange>
	, public Visitor<Stmt::ForeachPool>
	, public Visitor<Stmt::ExprStmt>
	, public Visitor<Stmt::Break>
	, public Visitor<Stmt::Continue>
	, public Visitor<Stmt::Return>
	, public Visitor<Method>
	, public Visitor<Class>
	, public Visitor<Program>
{
};

class DefaultVisitor: public AstVisitor
{
public:
	template<typename Iter>
	void visit(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			it->accept(*this);
		}
	}

	template<typename Iter>
	void visit_ref(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			it->get().accept(*this);
		}
	}

	void visit(const PoolType::BoundType& e)    override;
	void visit(const PoolType::LayoutType& e)   override;
	void visit(const Pool& e)                   override;
	void visit(const PoolParameter::PoolRef& e) override;
	void visit(const PoolParameter::None& e)    override;
	void visit(const Type::PrimitiveType& e)    override;
	void visit(const Type::NullType& e)         override;
	void visit(const Type::ObjectType& e)       override;
	void visit(const Type::VoidType& e)         override;
	void visit(const Variable& e)               override;
	void visit(const Expr::IntegerConst& e)     override;
	void visit(const Expr::BooleanConst& e)     override;
	void visit(const Expr::Null& e)             override;
	void visit(const Expr::This& e)             override;
	void visit(const Expr::Cast& e)             override;
	void visit(const Expr::Unary& e)            override;
	void visit(const Expr::Binary& e)           override;
	void visit(const Expr::IndexExpr& e)        override;
	void visit(const Expr::VariableExpr& e)     override;
	void visit(const Expr::MethodCall& e)       override;
	void visit(const Expr::FieldAccess& e)      override;
	void visit(const Expr::New& e)              override;
	void visit(const Field& e)                  override;
	void visit(const Cluster& e)                override;
	void visit(const Layout& e)                 override;
	void visit(const Stmt::Assignment& e)       override;
	void visit(const Stmt::OpAssignment& e)     override;
	void visit(const Stmt::If& e)               override;
	void visit(const Stmt::While& e)            override;
	void visit(const Stmt::ForeachRange& e)     override;
	void visit(const Stmt::ForeachPool& e)      override;
	void visit(const Stmt::ExprStmt& e)         override;
	void visit(const Stmt::Break& e)            override;
	void visit(const Stmt::Continue& e)         override;
	void visit(const Stmt::Return& e)           override;
	void visit(const Method& e)                 override;
	void visit(const Class& e)                  override;
	void visit(const Program& e)                override;
};

} // namespace Ast
