#pragma once

#include "parse_tree_common.h"

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

class FormalPoolParameter: public BaseVisitable<FormalPoolParameter>
{
	Identifier m_name;

public:
	explicit FormalPoolParameter(Identifier name)
		: m_name(std::move(name))
	{}

	const std::string& ident() const { return m_name.ident(); }
	const Location& loc() const { return m_name.loc(); }
};

class PoolParameter
{
public:
	class Pool: public BaseVisitable<Pool>
	{
		Identifier m_name;

	public:
		explicit Pool(Identifier name)
			: m_name(std::move(name))
		{}

		const std::string& ident() const { return m_name.ident(); }
		const Location& loc() const { return m_name.loc(); }
	};

	class None: public BaseVisitable<None>
	{
		Location m_loc;

	public:
		explicit None(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

private:
	void destroy_variant();
	void construct_variant_from_other(PoolParameter& other);

	enum class Tag { POOL, NONE };

	Tag m_tag;
	union
	{
		Pool m_pool;
		None m_none;
	};

	Location m_loc;

public:
	PoolParameter(Pool pool, const Location& loc)
		: m_tag(Tag::POOL)
		, m_pool(std::move(pool))
		, m_loc(loc)
	{}

	PoolParameter(None none, const Location& loc)
		: m_tag(Tag::NONE)
		, m_none(std::move(none))
		, m_loc(loc)
	{}

	PoolParameter(const PoolParameter&) = delete;
	PoolParameter& operator=(const PoolParameter&) = delete;

	PoolParameter(PoolParameter&&);
	PoolParameter& operator=(PoolParameter&&);

	~PoolParameter();

	template<typename T>
	void accept(T& visitor) const
	{
		switch (m_tag) {
		case Tag::POOL:
			visitor.visit(m_pool);
			break;

		case Tag::NONE:
			visitor.visit(m_none);
			break;
		}
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
		Location m_loc;

	public:
		explicit PrimitiveType(PrimitiveKind kind, const Location& loc)
			: m_kind(kind)
			, m_loc(loc)
		{}

		PrimitiveKind kind() const { return m_kind; }
		const Location& loc() const { return m_loc; }
	};

	class ObjectType: public BaseVisitable<ObjectType>
	{
		Identifier m_class_name;
		std::vector<PoolParameter> m_params;
		Location m_loc;

	public:
		ObjectType(Identifier class_name, std::vector<PoolParameter> params, const Location& loc)
			: m_class_name(std::move(class_name))
			, m_params(std::move(params))
			, m_loc(loc)
		{}

		const Identifier& class_name() const { return m_class_name; }
		const Location& loc() const { return m_loc; }

		decltype(m_params)::const_iterator begin() const { return m_params.begin(); }
		decltype(m_params)::const_iterator end()   const { return m_params.end();   }

		decltype(m_params)::iterator begin() { return m_params.begin(); }
		decltype(m_params)::iterator end()   { return m_params.end();   }
	};

	Type(const Type&) = delete;
	Type& operator=(const Type&) = delete;

	Type(Type&&);
	Type& operator=(Type&&);

	~Type();

private:
	enum class Tag { PRIMITIVE, OBJECT };

	Tag m_tag;
	union
	{
		PrimitiveType m_primitive_type;
		ObjectType m_object_type;
	};

	void destroy_variant();
	void construct_variant_from_other(Type& other);

public:
	Type(PrimitiveType primitive_type)
		: m_tag(Tag::PRIMITIVE)
		, m_primitive_type(std::move(primitive_type))
	{}

	Type(ObjectType object_type)
		: m_tag(Tag::OBJECT)
		, m_object_type(std::move(object_type))
	{}

	template<typename T>
	void accept(T& visitor) const
	{
		switch (m_tag) {
		case Tag::PRIMITIVE:
			visitor.visit(m_primitive_type);
			break;

		case Tag::OBJECT:
			visitor.visit(m_object_type);
			break;
		}
	}
};

class BoundType: public BaseVisitable<BoundType>
{
	Identifier m_class_name;
	std::vector<PoolParameter::Pool> m_params;

	Location m_loc;

public:
	BoundType() = default;
	BoundType(Identifier class_name, std::vector<PoolParameter::Pool> params)
		: m_class_name(std::move(class_name))
		, m_params(std::move(params))
	{}

	const Identifier& class_name() const { return m_class_name; }

	decltype(m_params)::const_iterator begin() const { return m_params.begin(); }
	decltype(m_params)::const_iterator end()   const { return m_params.end();   }

	decltype(m_params)::iterator begin() { return m_params.begin(); }
	decltype(m_params)::iterator end()   { return m_params.end();   }

	const Location& loc() const { return m_loc; }
};

class LayoutType: public BaseVisitable<LayoutType>
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

	decltype(m_params)::const_iterator begin() const { return m_params.begin(); }
	decltype(m_params)::const_iterator end()   const { return m_params.end();   }

	decltype(m_params)::iterator begin() { return m_params.begin(); }
	decltype(m_params)::iterator end()   { return m_params.end();   }

	const Location& loc() const { return m_loc; }
};

class FormalPoolBound: public BaseVisitable<FormalPoolBound>
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

class VariableDeclaration: public BaseVisitable<VariableDeclaration>
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

class PoolDeclaration: public BaseVisitable<PoolDeclaration>
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

enum class BinOp
{
	PLUS, MINUS, TIMES, DIV, LAND, LOR, AND, OR, XOR, SHL, SHR, EQ, NE, LT, LE, GT, GE
};

enum class UnOp { PLUS, MINUS, NOT };

class Expr
{
public:
	class IntegerConst: public BaseVisitable<IntegerConst>
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

	class BooleanConst: public BaseVisitable<BooleanConst>
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

	class Null: public BaseVisitable<Null>
	{
		Location m_loc;

	public:
		explicit Null(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class This: public BaseVisitable<This>
	{
		Location m_loc;

	public:
		explicit This(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class Cast: public BaseVisitable<Cast>
	{
		std::unique_ptr<Expr> m_expr;
		Type::PrimitiveType m_type;

		Location m_loc;

	public:
		Cast(Expr expr, Type::PrimitiveType type, const Location& loc)
			: m_expr(std::unique_ptr<Expr>(new Expr(std::move(expr))))
			, m_type(std::move(type))
			, m_loc(loc)
		{}

		const Expr& expr() const { return *m_expr; }
		const Location& loc() const { return m_loc; }
		const Type::PrimitiveType& type() const { return m_type; }
	};

	class Binary: public BaseVisitable<Binary>
	{
		std::unique_ptr<Expr> m_lhs;
		std::unique_ptr<Expr> m_rhs;
		BinOp m_op;
		Location m_loc;

	public:
		explicit Binary(Expr lhs, BinOp op, Expr rhs, const Location& loc)
			: m_lhs(std::unique_ptr<Expr>(new Expr(std::move(lhs))))
			, m_rhs(std::unique_ptr<Expr>(new Expr(std::move(rhs))))
			, m_op(op)
			, m_loc(loc)
		{}

		const Expr& lhs() const { return *m_lhs; }
		const Expr& rhs() const { return *m_rhs; }

		BinOp op() const { return m_op; }
		const Location& loc() const { return m_loc; }
	};

	class VariableExpr: public BaseVisitable<VariableExpr>
	{
		Identifier m_name;
		Location m_loc;

	public:
		explicit VariableExpr(Identifier name, const Location& loc)
			: m_name(std::move(name))
			, m_loc(loc)
		{}

		const Identifier& name() const { return m_name; }
		const Location& loc() const { return m_loc; }
	};

	class Unary: public BaseVisitable<Unary>
	{
		UnOp m_op;
		std::unique_ptr<Expr> m_expr;
		Location m_loc;

	public:
		explicit Unary(UnOp op, Expr expr, const Location& loc)
			: m_op(op)
			, m_expr(std::unique_ptr<Expr>(new Expr(std::move(expr))))
			, m_loc(loc)
		{}

		const Expr& expr() const { return *m_expr; }
		UnOp op() const { return m_op; }
		const Location& loc() const { return m_loc; }
	};

	class IndexExpr: public BaseVisitable<IndexExpr>
	{
		PoolParameter::Pool m_pool;
		std::unique_ptr<Expr> m_idx;
		Location m_loc;

	public:
		explicit IndexExpr(PoolParameter::Pool pool, Expr idx, const Location& loc)
			: m_pool(std::move(pool))
			, m_idx(std::unique_ptr<Expr>(new Expr(std::move(idx))))
			, m_loc(loc)
		{}

		const PoolParameter::Pool& pool() const { return m_pool; }
		const Expr& idx()  const { return *m_idx; }
		const Location& loc() const { return m_loc; }
	};

	class MethodCall: public BaseVisitable<MethodCall>
	{
		Identifier m_name;
		std::vector<Expr> m_params;
		Location m_loc;

	public:
		using iterator = decltype(m_params)::iterator;
		using const_iterator = decltype(m_params)::const_iterator;

		explicit MethodCall(Identifier name, std::vector<Expr> params, const Location& loc)
			: m_name(std::move(name))
			, m_params(std::move(params))
			, m_loc(loc)
		{}

		const Identifier& name() const { return m_name; }
		const Location& loc() const { return m_loc; }

		const_iterator begin() const { return m_params.begin(); }
		const_iterator end()   const { return m_params.end();   }
		
		size_t num_args() const { return m_params.size(); }

		iterator begin() { return m_params.begin(); }
		iterator end()   { return m_params.end();   }
	};

	class MemberMethodCall: public BaseVisitable<MemberMethodCall>
	{
		std::unique_ptr<Expr> m_this_expr;
		Identifier m_name;
		std::vector<Expr> m_args;
		Location m_loc;

	public:
		using iterator = decltype(m_args)::iterator;
		using const_iterator = decltype(m_args)::const_iterator;

		explicit MemberMethodCall(Expr this_expr, Identifier name, std::vector<Expr> args, const Location& loc)
			: m_this_expr(std::unique_ptr<Expr>(new Expr(std::move(this_expr))))
			, m_name(std::move(name))
			, m_args(std::move(args))
			, m_loc(loc)
		{}

		const Expr& this_expr() const { return *m_this_expr; }
		const Identifier& method_name() const { return m_name; }
		const Location& loc() const { return m_loc; }

		const_iterator begin() const { return m_args.begin(); }
		const_iterator end()   const { return m_args.end();   }

		size_t num_args() const { return m_args.size(); }

		iterator begin() { return m_args.begin(); }
		iterator end()   { return m_args.end();   }
	};

	class FieldAccess: public BaseVisitable<FieldAccess>
	{
		std::unique_ptr<Expr> m_expr;
		Identifier m_field;
		Location m_loc;

	public:
		explicit FieldAccess(Expr expr, Identifier field, const Location& loc)
			: m_expr(std::unique_ptr<Expr>(new Expr(std::move(expr))))
			, m_field(std::move(field))
			, m_loc(loc)
		{}

		const Expr& expr() const { return *m_expr; }
		const Identifier& field() const { return m_field; }
		const Location& loc() const { return m_loc; }
	};

	class New: public BaseVisitable<New>
	{
		Type::ObjectType m_type;
		Location m_loc;

	public:
		explicit New(Type::ObjectType type, const Location& loc)
			: m_type(std::move(type))
			, m_loc(loc)
		{}

		const Type::ObjectType& type() const { return m_type; }
		const Location& loc() const { return m_loc; }
	};

	Expr(const Expr&) = delete;
	Expr& operator=(const Expr&) = delete;

	Expr(Expr&& other);
	Expr& operator=(Expr&& other);

	~Expr();

private:
	enum class Tag
	{
		INTEGER_CONST,
		BOOLEAN_CONST,
		NULL_EXPR,
		THIS,
		CAST,
		UNARY,
		BINARY,
		INDEX,
		VARIABLE_EXPR,
		METHOD_CALL,
		MEMBER_METHOD_CALL,
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
		MemberMethodCall m_member_method_call;
		FieldAccess m_field_access;
		New m_new_expr;
	};

	void destroy_variant();
	void construct_variant_from_other(Expr& other);

public:
	Expr(IntegerConst integer_const)
		: m_tag(Tag::INTEGER_CONST)
		, m_integer_const(std::move(integer_const))
	{}

	Expr(BooleanConst boolean_const)
		: m_tag(Tag::BOOLEAN_CONST)
		, m_boolean_const(std::move(boolean_const))
	{}

	Expr(Null null_expr)
		: m_tag(Tag::NULL_EXPR)
		, m_null_expr(std::move(null_expr))
	{}

	Expr(This this_expr)
		: m_tag(Tag::THIS)
		, m_this_expr(std::move(this_expr))
	{}

	Expr(Cast cast)
		: m_tag(Tag::CAST)
		, m_cast(std::move(cast))
	{}

	Expr(Unary unary)
		: m_tag(Tag::UNARY)
		, m_unary(std::move(unary))
	{}

	Expr(Binary binary)
		: m_tag(Tag::BINARY)
		, m_binary(std::move(binary))
	{}

	Expr(IndexExpr index_expr)
		: m_tag(Tag::INDEX)
		, m_index_expr(std::move(index_expr))
	{}

	Expr(VariableExpr variable_expr)
		: m_tag(Tag::VARIABLE_EXPR)
		, m_variable_expr(std::move(variable_expr))
	{}

	Expr(MethodCall method_call)
		: m_tag(Tag::METHOD_CALL)
		, m_method_call(std::move(method_call))
	{}

	Expr(MemberMethodCall member_method_call)
		: m_tag(Tag::MEMBER_METHOD_CALL)
		, m_member_method_call(std::move(member_method_call))
	{}

	Expr(FieldAccess field_access)
		: m_tag(Tag::FIELD_ACCESS)
		, m_field_access(std::move(field_access))
	{}

	Expr(New new_expr)
		: m_tag(Tag::NEW)
		, m_new_expr(std::move(new_expr))
	{}

	const Location& loc() const;

	template<typename T>
	void accept(T& visitor) const
	{
		switch (m_tag) {
		case Tag::INTEGER_CONST:
			visitor.visit(m_integer_const);
			break;

		case Tag::BOOLEAN_CONST:
			visitor.visit(m_boolean_const);
			break;

		case Tag::NULL_EXPR:
			visitor.visit(m_null_expr);
			break;

		case Tag::THIS:
			visitor.visit(m_this_expr);
			break;

		case Tag::CAST:
			visitor.visit(m_cast);
			break;

		case Tag::UNARY:
			visitor.visit(m_unary);
			break;

		case Tag::BINARY:
			visitor.visit(m_binary);
			break;

		case Tag::INDEX:
			visitor.visit(m_index_expr);
			break;

		case Tag::VARIABLE_EXPR:
			visitor.visit(m_variable_expr);
			break;

		case Tag::MEMBER_METHOD_CALL:
			visitor.visit(m_member_method_call);
			break;

		case Tag::METHOD_CALL:
			visitor.visit(m_method_call);
			break;

		case Tag::FIELD_ACCESS:
			visitor.visit(m_field_access);
			break;

		case Tag::NEW:
			visitor.visit(m_new_expr);
			break;
		}
	}
};

class Stmt
{
public:
	class Noop: public BaseVisitable<Noop> {};

	class VariableDeclarations: public BaseVisitable<VariableDeclarations>
	{
		std::vector<VariableDeclaration> m_decls;

	public:
		explicit VariableDeclarations(std::vector<VariableDeclaration> decls)
			: m_decls(std::move(decls))
		{}

		using iterator = decltype(m_decls)::iterator;
		using const_iterator = decltype(m_decls)::const_iterator;

		const_iterator begin() const { return m_decls.begin(); }
		const_iterator end()   const { return m_decls.end();   }

		iterator begin() { return m_decls.begin(); }
		iterator end()   { return m_decls.end();   }
	};

	class PoolDeclarations: public BaseVisitable<PoolDeclarations>
	{
		std::vector<PoolDeclaration> m_decls;

	public:
		explicit PoolDeclarations(std::vector<PoolDeclaration> decls)
			: m_decls(std::move(decls))
		{}

		using iterator = decltype(m_decls)::iterator;
		using const_iterator = decltype(m_decls)::const_iterator;

		const_iterator begin() const { return m_decls.begin(); }
		const_iterator end()   const { return m_decls.end();   }

		iterator begin() { return m_decls.begin(); }
		iterator end()   { return m_decls.end();   }
	};

	class Assignment: public BaseVisitable<Assignment>
	{
		std::unique_ptr<Expr> m_lhs;
		std::unique_ptr<Expr> m_rhs;

	public:
		Assignment(Expr lhs, Expr rhs)
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

	class If: public BaseVisitable<If>
	{
		Expr m_cond;
		std::unique_ptr<Stmt> m_then_branch;
		std::unique_ptr<Stmt> m_else_branch;

	public:
		explicit If(Expr cond, Stmt then_branch, Stmt else_branch)
			: m_cond(std::move(cond))
			, m_then_branch(std::unique_ptr<Stmt>(new Stmt(std::move(then_branch))))
			, m_else_branch(std::unique_ptr<Stmt>(new Stmt(std::move(else_branch))))
		{}

		const Expr& cond() const { return m_cond; }

		const Stmt& then_branch() const { return *m_then_branch; }
		const Stmt& else_branch() const { return *m_else_branch; }
	};

	class While: public BaseVisitable<While>
	{
		Expr m_cond;
		std::unique_ptr<Stmt> m_body;

	public:
		explicit While(Expr cond, Stmt body)
			: m_cond(std::move(cond))
			, m_body(std::unique_ptr<Stmt>(new Stmt(std::move(body))))
		{}

		const Expr& cond() const { return m_cond; }
		const Stmt& body() const { return *m_body; }
	};

	class ForeachRange: public BaseVisitable<ForeachRange>
	{
		Identifier m_var;
		Expr m_range_begin;
		Expr m_range_end;
		std::unique_ptr<Stmt> m_body;

	public:
		explicit ForeachRange(Identifier var, Expr range_begin, Expr range_end, Stmt body)
			: m_var(std::move(var))
			, m_range_begin(std::move(range_begin))
			, m_range_end(std::move(range_end))
			, m_body(std::unique_ptr<Stmt>(new Stmt(std::move(body))))
		{}

		const Identifier& var() const { return m_var; }

		const Expr& range_begin() const { return m_range_begin; }
		const Expr& range_end()   const { return m_range_end;   }

		const Stmt& body() const { return *m_body; }
	};

	class ForeachPool: public BaseVisitable<ForeachPool>
	{
		Identifier m_var;
		Identifier m_pool;
		std::unique_ptr<Stmt> m_body;

	public:
		explicit ForeachPool(Identifier var, Identifier pool, Stmt body)
			: m_var(std::move(var))
			, m_pool(std::move(pool))
			, m_body(std::unique_ptr<Stmt>(new Stmt(std::move(body))))
		{}

		const Identifier& var()  const { return m_var;  }
		const Identifier& pool() const { return m_pool; }

		const Stmt& body() const { return *m_body; }
	};

	class Block: public BaseVisitable<Block>
	{
		std::vector<Stmt> m_stmts;

	public:
		using const_iterator = decltype(m_stmts)::const_iterator;
		using iterator = decltype(m_stmts)::iterator;

		Block() = default;

		explicit Block(std::vector<Stmt> stmts)
			: m_stmts(std::move(stmts))
		{}

		const_iterator begin() const { return m_stmts.begin(); }
		const_iterator end()   const { return m_stmts.end();   }

		iterator begin() { return m_stmts.begin(); }
		iterator end()   { return m_stmts.end();   }

		void add(Stmt stmt) { m_stmts.emplace_back(std::move(stmt)); }
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

	class Break: public BaseVisitable<Break>
	{
		Location m_loc;

	public:
		explicit Break(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class Continue: public BaseVisitable<Continue>
	{
		Location m_loc;

	public:
		explicit Continue(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class Return: public BaseVisitable<Return>
	{
		Expr m_expr;

	public:
		explicit Return(Expr expr)
			: m_expr(std::move(expr))
		{}

		const Expr& expr() const { return m_expr; }
	};

	class ReturnVoid: public BaseVisitable<ReturnVoid>
	{
		Location m_loc;

	public:
		explicit ReturnVoid(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	Stmt(const Stmt&) = delete;
	Stmt& operator=(const Stmt&) = delete;

	Stmt(Stmt&& other);
	Stmt& operator=(Stmt&& other);

	~Stmt();

private:
	enum class Tag
	{
		NOOP,
		VARIABLE_DECLARATIONS,
		POOL_DECLARATIONS,
		ASSIGNMENT,
		OP_ASSIGNMENT,
		IF,
		WHILE,
		FOREACH_RANGE,
		FOREACH_POOL,
		BLOCK,
		EXPR_STMT,
		BREAK,
		CONTINUE,
		RETURN,
		RETURN_VOID,
	};

	Tag m_tag;
	union
	{
		Noop m_noop;
		VariableDeclarations m_variable_declarations;
		PoolDeclarations m_pool_declarations;
		Assignment m_assignment;
		OpAssignment m_op_assignment;
		If m_if;
		While m_while;
		ForeachRange m_foreach_range;
		ForeachPool m_foreach_pool;
		Block m_block;
		ExprStmt m_expr_stmt;
		Break m_break;
		Continue m_continue;
		Return m_return;
		ReturnVoid m_return_void;
	};
	Location m_loc;

	void destroy_variant();
	void construct_variant_from_other(Stmt& other);

public:
	explicit Stmt(Noop noop, const Location& loc)
		: m_tag(Tag::NOOP)
		, m_noop(std::move(noop))
		, m_loc(loc)
	{}

	explicit Stmt(VariableDeclarations variable_declarations, const Location& loc)
		: m_tag(Tag::VARIABLE_DECLARATIONS)
		, m_variable_declarations(std::move(variable_declarations))
		, m_loc(loc)
	{}

	explicit Stmt(PoolDeclarations pool_declarations, const Location& loc)
		: m_tag(Tag::POOL_DECLARATIONS)
		, m_pool_declarations(std::move(pool_declarations))
		, m_loc(loc)
	{}

	explicit Stmt(Assignment assignment, const Location& loc)
		: m_tag(Tag::ASSIGNMENT)
		, m_assignment(std::move(assignment))
		, m_loc(loc)
	{}

	explicit Stmt(OpAssignment op_assignment, const Location& loc)
		: m_tag(Tag::OP_ASSIGNMENT)
		, m_op_assignment(std::move(op_assignment))
		, m_loc(loc)
	{}

	explicit Stmt(If if_stmt, const Location& loc)
		: m_tag(Tag::IF)
		, m_if(std::move(if_stmt))
		, m_loc(loc)
	{}

	explicit Stmt(While while_stmt, const Location& loc)
		: m_tag(Tag::WHILE)
		, m_while(std::move(while_stmt))
		, m_loc(loc)
	{}

	explicit Stmt(ForeachRange foreach_range, const Location& loc)
		: m_tag(Tag::FOREACH_RANGE)
		, m_foreach_range(std::move(foreach_range))
		, m_loc(loc)
	{}

	explicit Stmt(ForeachPool foreach_pool, const Location& loc)
		: m_tag(Tag::FOREACH_POOL)
		, m_foreach_pool(std::move(foreach_pool))
		, m_loc(loc)
	{}

	explicit Stmt(Block block, const Location& loc)
		: m_tag(Tag::BLOCK)
		, m_block(std::move(block))
		, m_loc(loc)
	{}

	explicit Stmt(ExprStmt expr_stmt, const Location& loc)
		: m_tag(Tag::EXPR_STMT)
		, m_expr_stmt(std::move(expr_stmt))
		, m_loc(loc)
	{}

	explicit Stmt(Break break_stmt, const Location& loc)
		: m_tag(Tag::BREAK)
		, m_break(std::move(break_stmt))
		, m_loc(loc)
	{}

	explicit Stmt(Continue continue_stmt, const Location& loc)
		: m_tag(Tag::CONTINUE)
		, m_continue(std::move(continue_stmt))
		, m_loc(loc)
	{}

	explicit Stmt(Return return_stmt, const Location& loc)
		: m_tag(Tag::RETURN)
		, m_return(std::move(return_stmt))
		, m_loc(loc)
	{}

	explicit Stmt(ReturnVoid return_void, const Location& loc)
		: m_tag(Tag::RETURN_VOID)
		, m_return_void(std::move(return_void))
		, m_loc(loc)
	{}

	template<typename T>
	void accept(T& visitor) const
	{
		switch (m_tag) {
		case Tag::NOOP:
			visitor.visit(m_noop);
			break;

		case Tag::VARIABLE_DECLARATIONS:
			visitor.visit(m_variable_declarations);
			break;

		case Tag::POOL_DECLARATIONS:
			visitor.visit(m_pool_declarations);
			break;

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

		case Tag::BLOCK:
			visitor.visit(m_block);
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

		case Tag::RETURN_VOID:
			visitor.visit(m_return_void);
			break;
		}
	}
};

class Field: public BaseVisitable<Field>
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

class MethodParameter: public BaseVisitable<MethodParameter>
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

class Method: public BaseVisitable<Method>
{
	using ReturnType = Optional<Type>;

	Identifier m_name;
	ReturnType m_return_type;
	std::vector<MethodParameter> m_params;
	Stmt::Block m_body;

public:
	using iterator = decltype(m_params)::iterator;
	using const_iterator = decltype(m_params)::const_iterator;

	Method(Identifier name, std::vector<MethodParameter> params, Type return_type, Stmt::Block body)
		: m_name(std::move(name))
		, m_return_type(std::move(return_type))
		, m_params(std::move(params))
		, m_body(std::move(body))
	{}

	Method(Identifier name, std::vector<MethodParameter> params, Stmt::Block body)
		: m_name(std::move(name))
		, m_return_type()
		, m_params(std::move(params))
		, m_body(std::move(body))
	{}

	const Identifier& name() const { return m_name; }

	const Type* type() const { return m_return_type.get(); }

	const_iterator begin() const { return m_params.begin(); }
	const_iterator end()   const { return m_params.end();   }

	iterator begin() { return m_params.begin(); }
	iterator end()   { return m_params.end();   }

	const Stmt::Block& body() const { return m_body; }
};

class Class: public BaseVisitable<Class>
{
	Identifier m_name;

	std::vector<FormalPoolParameter> m_pool_params;
	std::vector<FormalPoolBound> m_pool_param_bounds;

	std::vector<Field> m_fields;
	std::vector<Method> m_methods;

public:
	Class(
			Identifier name,
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

	decltype(m_pool_params)::const_iterator pool_params_begin() const { return m_pool_params.begin(); }
	decltype(m_pool_params)::const_iterator pool_params_end()   const { return m_pool_params.end();   }

	decltype(m_pool_param_bounds)::const_iterator pool_param_bounds_begin() const { return m_pool_param_bounds.begin(); }
	decltype(m_pool_param_bounds)::const_iterator pool_param_bounds_end()   const { return m_pool_param_bounds.end();   }

	decltype(m_fields)::const_iterator fields_begin() const { return m_fields.begin(); }
	decltype(m_fields)::const_iterator fields_end()   const { return m_fields.end();   }

	decltype(m_methods)::const_iterator methods_begin() const { return m_methods.begin(); }
	decltype(m_methods)::const_iterator methods_end()   const { return m_methods.end();   }
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

class ClusterField: public BaseVisitable<ClusterField>
{
	Identifier m_name;

public:
	explicit ClusterField(Identifier name)
		: m_name(std::move(name))
	{}

	const std::string& ident() const { return m_name.ident(); }
	const Location& loc() const { return m_name.loc(); }
};

class Cluster: public BaseVisitable<Cluster>
{
	std::vector<ClusterField> m_fields;
	Location m_loc;

	using iterator = decltype(m_fields)::iterator;
	using const_iterator = decltype(m_fields)::const_iterator;

public:
	Cluster() = default;
	explicit Cluster(std::vector<ClusterField> fields, const Location& loc)
		: m_fields(std::move(fields))
		, m_loc(loc)
	{}

	const_iterator begin() const { return m_fields.begin(); }
	const_iterator end()   const { return m_fields.end();   }

	iterator begin() { return m_fields.begin(); }
	iterator end()   { return m_fields.end();   }

	const Location& loc() const { return m_loc; }
};

class Layout: public BaseVisitable<Layout>
{
	Identifier m_name;
	Identifier m_class;
	std::vector<Cluster> m_clusters;

	Location m_loc;

	using iterator = decltype(m_clusters)::iterator;
	using const_iterator = decltype(m_clusters)::const_iterator;

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

	const_iterator begin() const { return m_clusters.begin(); }
	const_iterator end()   const { return m_clusters.end();   }

	iterator begin() { return m_clusters.begin(); }
	iterator end()   { return m_clusters.end();   }

	const Location& loc() { return m_loc; }
};

class Program: public BaseVisitable<Program>
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

	decltype(m_classes)::const_iterator classes_begin() const { return m_classes.begin(); }
	decltype(m_classes)::const_iterator classes_end()   const { return m_classes.end();   }

	decltype(m_layouts)::const_iterator layouts_begin() const { return m_layouts.begin(); }
	decltype(m_layouts)::const_iterator layouts_end()   const { return m_layouts.end();   }

	void add_class(Class new_class) {
		m_classes.emplace_back(std::move(new_class));
	}

	void add_layout(Layout new_layout) {
		m_layouts.emplace_back(std::move(new_layout));
	}
};

class CstVisitor: public BaseVisitor
	, public Visitor<FormalPoolParameter>
	, public Visitor<PoolParameter::Pool>
	, public Visitor<PoolParameter::None>
	, public Visitor<Type::PrimitiveType>
	, public Visitor<Type::ObjectType>
	, public Visitor<BoundType>
	, public Visitor<LayoutType>
	, public Visitor<FormalPoolBound>
	, public Visitor<VariableDeclaration>
	, public Visitor<PoolDeclaration>
	, public Visitor<Expr::IntegerConst>
	, public Visitor<Expr::BooleanConst>
	, public Visitor<Expr::Null>
	, public Visitor<Expr::This>
	, public Visitor<Expr::Cast>
	, public Visitor<Expr::Binary>
	, public Visitor<Expr::VariableExpr>
	, public Visitor<Expr::Unary>
	, public Visitor<Expr::IndexExpr>
	, public Visitor<Expr::MethodCall>
	, public Visitor<Expr::MemberMethodCall>
	, public Visitor<Expr::FieldAccess>
	, public Visitor<Expr::New>
	, public Visitor<Stmt::Noop>
	, public Visitor<Stmt::VariableDeclarations>
	, public Visitor<Stmt::PoolDeclarations>
	, public Visitor<Stmt::Assignment>
	, public Visitor<Stmt::OpAssignment>
	, public Visitor<Stmt::If>
	, public Visitor<Stmt::While>
	, public Visitor<Stmt::ForeachRange>
	, public Visitor<Stmt::ForeachPool>
	, public Visitor<Stmt::Block>
	, public Visitor<Stmt::ExprStmt>
	, public Visitor<Stmt::Break>
	, public Visitor<Stmt::Continue>
	, public Visitor<Stmt::Return>
	, public Visitor<Stmt::ReturnVoid>
	, public Visitor<Field>
	, public Visitor<MethodParameter>
	, public Visitor<Method>
	, public Visitor<Class>
	, public Visitor<ClusterField>
	, public Visitor<Cluster>
	, public Visitor<Layout>
	, public Visitor<Program>
{
};

class DefaultVisitor: public CstVisitor
{
public:
	template<typename Iter>
	void visit(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			it->accept(*this);
		}
	}

	void visit(const FormalPoolParameter& e)        override;
	void visit(const PoolParameter::Pool& e)        override;
	void visit(const PoolParameter::None& e)        override;
	void visit(const Type::PrimitiveType& e)        override;
	void visit(const Type::ObjectType& e)           override;
	void visit(const BoundType& e)                  override;
	void visit(const LayoutType& e)                 override;
	void visit(const FormalPoolBound& e)            override;
	void visit(const VariableDeclaration& e)        override;
	void visit(const PoolDeclaration& e)            override;
	void visit(const Expr::IntegerConst& e)         override;
	void visit(const Expr::BooleanConst& e)         override;
	void visit(const Expr::Null& e)                 override;
	void visit(const Expr::This& e)                 override;
	void visit(const Expr::Cast& e)                 override;
	void visit(const Expr::Binary& e)               override;
	void visit(const Expr::VariableExpr& e)         override;
	void visit(const Expr::Unary& e)                override;
	void visit(const Expr::IndexExpr& e)            override;
	void visit(const Expr::MethodCall& e)           override;
	void visit(const Expr::MemberMethodCall& e)     override;
	void visit(const Expr::FieldAccess& e)          override;
	void visit(const Expr::New& e)                  override;
	void visit(const Stmt::Noop& e)                 override;
	void visit(const Stmt::VariableDeclarations& e) override;
	void visit(const Stmt::PoolDeclarations& e)     override;
	void visit(const Stmt::Assignment& e)           override;
	void visit(const Stmt::OpAssignment& e)         override;
	void visit(const Stmt::If& e)                   override;
	void visit(const Stmt::While& e)                override;
	void visit(const Stmt::ForeachRange& e)         override;
	void visit(const Stmt::ForeachPool& e)          override;
	void visit(const Stmt::Block& e)                override;
	void visit(const Stmt::ExprStmt& e)             override;
	void visit(const Stmt::Break& e)                override;
	void visit(const Stmt::Continue& e)             override;
	void visit(const Stmt::Return& e)               override;
	void visit(const Stmt::ReturnVoid& e)           override;
	void visit(const Field& e)                      override;
	void visit(const MethodParameter& e)            override;
	void visit(const Method& e)                     override;
	void visit(const Class& e)                      override;
	void visit(const ClusterField& e)               override;
	void visit(const Cluster& e)                    override;
	void visit(const Layout& e)                     override;
	void visit(const Program& e)                    override;
};

} // namespace Cst
