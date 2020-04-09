#include "ast.h"

#include <vector>
#include <utility>

namespace Ast
{

PoolType::PoolType(PoolType&& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	construct_variant_from_other(other);
}

PoolType& PoolType::operator=(PoolType&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;
	construct_variant_from_other(other);

	return *this;
}

PoolType::~PoolType()
{
	destroy_variant();
}

void PoolType::destroy_variant()
{
	switch (m_tag) {
	case Tag::INVALID:
		break;

	case Tag::LAYOUT:
		m_layout.~LayoutType();
		break;

	case Tag::BOUND:
		m_bound.~BoundType();
		break;
	}
}

void PoolType::construct_variant_from_other(PoolType& other)
{
	switch (other.m_tag) {
	case Tag::INVALID:
		break;

	case Tag::LAYOUT:
		new (&m_layout) LayoutType(std::move(other.m_layout));
		break;

	case Tag::BOUND:
		new (&m_bound) BoundType(std::move(other.m_bound));
		break;
	}
}

const Class& PoolType::LayoutType::of_class() const
{
	return m_layout.for_class();
}

PoolParameter::PoolParameter(const PoolParameter& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	switch (m_tag) {
	case Tag::POOL:
		new (&m_pool) PoolRef(other.m_pool);

	case Tag::NONE:
		new (&m_none) None(other.m_none);
	}
}

PoolParameter& PoolParameter::operator=(const PoolParameter& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;

	switch (m_tag) {
	case Tag::POOL:
		new (&m_pool) PoolRef(other.m_pool);

	case Tag::NONE:
		new (&m_none) None(other.m_none);
	}

	return *this;
}

PoolParameter::PoolParameter(PoolParameter&& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	construct_variant_from_other(other);
}

PoolParameter& PoolParameter::operator=(PoolParameter&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;
	construct_variant_from_other(other);

	return *this;
}

PoolParameter::~PoolParameter()
{
	destroy_variant();
}

void PoolParameter::destroy_variant()
{
	switch (m_tag) {
	case Tag::NONE:
		m_none.~None();
		break;

	case Tag::POOL:
		m_pool.~PoolRef();
		break;
	}
}

void PoolParameter::construct_variant_from_other(PoolParameter& other)
{
	switch (other.m_tag) {
	case Tag::NONE:
		new (&m_none) None(std::move(other.m_none));
		break;

	case Tag::POOL:
		new (&m_pool) PoolRef(std::move(other.m_pool));
		break;
	}
}

bool PoolParameter::operator==(const PoolParameter& rhs) const
{
	if (m_tag != rhs.m_tag) {
		return false;
	}

	switch (m_tag) {
	case Tag::NONE:
		return true;

	case Tag::POOL:
		return &m_pool.pool() == &rhs.m_pool.pool();
	}

	// Silence gcc
	return false;
}

bool PoolType::compatible_with_bound(const BoundType& bound) const
{
	switch (m_tag) {
	case Tag::INVALID:
		return false;

	case Tag::LAYOUT:
		return m_layout.compatible_with_bound(bound);

	case Tag::BOUND:
		return m_bound.compatible_with_bound(bound);
	}
}

Type::Type(const Type& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	switch (m_tag) {
	case Tag::INVALID:
		break;

	case Tag::OBJECT:
		new (&m_object_type) ObjectType(other.m_object_type);
		break;

	case Tag::PRIMITIVE:
		new (&m_primitive_type) PrimitiveType(other.m_primitive_type);
		break;

	case Tag::NULLPTR:
		new (&m_null_type) NullType(other.m_null_type);
		break;

	case Tag::VOID:
		new (&m_void_type) VoidType(other.m_void_type);
		break;
	}
}

Type& Type::operator=(const Type& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;

	switch (m_tag) {
	case Tag::INVALID:
		break;

	case Tag::OBJECT:
		new (&m_object_type) ObjectType(other.m_object_type);
		break;

	case Tag::PRIMITIVE:
		new (&m_primitive_type) PrimitiveType(other.m_primitive_type);
		break;

	case Tag::NULLPTR:
		new (&m_null_type) NullType(other.m_null_type);
		break;

	case Tag::VOID:
		new (&m_void_type) VoidType(other.m_void_type);
		break;
	}

	return *this;
}

Type::Type(Type&& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	construct_variant_from_other(other);
}

Type& Type::operator=(Type&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;
	construct_variant_from_other(other);

	return *this;
}

Type::~Type()
{
	destroy_variant();
}

void Type::destroy_variant()
{
	switch (m_tag) {
	case Tag::INVALID:
		break;

	case Tag::PRIMITIVE:
		m_primitive_type.~PrimitiveType();
		break;

	case Tag::NULLPTR:
		m_null_type.~NullType();
		break;

	case Tag::OBJECT:
		m_object_type.~ObjectType();
		break;

	case Tag::VOID:
		m_void_type.~VoidType();
		break;
	}
}

void Type::construct_variant_from_other(Type& other)
{
	switch (other.m_tag) {
	case Tag::INVALID:
		break;

	case Tag::PRIMITIVE:
		new (&m_primitive_type) PrimitiveType(std::move(other.m_primitive_type));
		break;

	case Tag::NULLPTR:
		new (&m_null_type) NullType(std::move(other.m_null_type));
		break;

	case Tag::OBJECT:
		new (&m_object_type) ObjectType(std::move(other.m_object_type));
		break;

	case Tag::VOID:
		new (&m_void_type) VoidType(std::move(other.m_void_type));
		break;
	}
}

const Type::PrimitiveType* Type::as_primitive_type() const
{
	if (m_tag == Tag::PRIMITIVE) {
		return &m_primitive_type;
	}
	return nullptr;
}

const Type::ObjectType* Type::as_object_type() const
{
	if (m_tag == Tag::OBJECT) {
		return &m_object_type;
	}
	return nullptr;
}

const Type::NullType* Type::as_null_type() const
{
	if (m_tag == Tag::OBJECT) {
		return &m_null_type;
	}
	return nullptr;
}

bool Type::ObjectType::operator==(const ObjectType& rhs) const
{
	if (&m_class != &rhs.m_class) {
		return false;
	}

	if (m_params.size() != rhs.m_params.size()) {
		return false;
	}

	return std::equal(m_params.begin(), m_params.end(), rhs.m_params.begin());
}

bool Type::operator==(const Type& rhs) const
{
	if (m_tag != rhs.m_tag) {
		return false;
	}

	switch (m_tag) {
	case Tag::INVALID:
		return true;

	case Tag::PRIMITIVE:
		return m_primitive_type.kind() == rhs.m_primitive_type.kind();

	case Tag::NULLPTR:
		return true;

	case Tag::OBJECT:
		return m_object_type == rhs.m_object_type;

	default:
		return false;
	}

	// Silence gcc
	return false;
}

Expr::Expr(Expr&& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	construct_variant_from_other(other);
}

Expr& Expr::operator=(Expr&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;
	construct_variant_from_other(other);

	return *this;
}

Expr::~Expr()
{
	destroy_variant();
}

void Expr::destroy_variant()
{
	switch (m_tag) {
	case Tag::INVALID:
		break;

	case Tag::INTEGER_CONST:
		m_integer_const.~IntegerConst();
		break;

	case Tag::BOOLEAN_CONST:
		m_boolean_const.~BooleanConst();
		break;

	case Tag::NULL_EXPR:
		m_null_expr.~Null();
		break;

	case Tag::THIS:
		m_this_expr.~This();
		break;

	case Tag::CAST:
		m_cast.~Cast();
		break;

	case Tag::UNARY:
		m_unary.~Unary();
		break;

	case Tag::BINARY:
		m_binary.~Binary();
		break;

	case Tag::INDEX:
		m_index_expr.~IndexExpr();
		break;

	case Tag::VARIABLE:
		m_variable_expr.~VariableExpr();
		break;

	case Tag::METHOD_CALL:
		m_method_call.~MethodCall();
		break;

	case Tag::FIELD_ACCESS:
		m_field_access.~FieldAccess();
		break;

	case Tag::NEW:
		m_new_expr.~New();
		break;
	}
}

void Expr::construct_variant_from_other(Expr& other)
{
	switch (other.m_tag) {
	case Tag::INVALID:
		break;

	case Tag::INTEGER_CONST:
		new (&m_integer_const) IntegerConst(std::move(other.m_integer_const));
		break;

	case Tag::BOOLEAN_CONST:
		new (&m_boolean_const) BooleanConst(std::move(other.m_boolean_const));
		break;

	case Tag::NULL_EXPR:
		new (&m_null_expr) Null(std::move(other.m_null_expr));
		break;

	case Tag::THIS:
		new (&m_this_expr) This(std::move(other.m_this_expr));
		break;

	case Tag::CAST:
		new (&m_cast) Cast(std::move(other.m_cast));
		break;

	case Tag::UNARY:
		new (&m_unary) Unary(std::move(other.m_unary));
		break;

	case Tag::BINARY:
		new (&m_binary) Binary(std::move(other.m_binary));
		break;

	case Tag::INDEX:
		new (&m_index_expr) IndexExpr(std::move(other.m_index_expr));
		break;

	case Tag::VARIABLE:
		new (&m_variable_expr) VariableExpr(std::move(other.m_variable_expr));
		break;

	case Tag::METHOD_CALL:
		new (&m_method_call) MethodCall(std::move(other.m_method_call));
		break;

	case Tag::FIELD_ACCESS:
		new (&m_field_access) FieldAccess(std::move(other.m_field_access));
		break;

	case Tag::NEW:
		new (&m_new_expr) New(std::move(other.m_new_expr));
		break;
	}
}

const Expr::IntegerConst* Expr::as_integer_const() const
{
	if (m_tag == Tag::INTEGER_CONST) {
		return &m_integer_const;
	}
	return nullptr;
}

const Expr::BooleanConst* Expr::as_boolean_const() const
{
	if (m_tag == Tag::BOOLEAN_CONST) {
		return &m_boolean_const;
	}
	return nullptr;
}

const Expr::Null* Expr::as_null_expr() const
{
	if (m_tag == Tag::NULL_EXPR) {
		return &m_null_expr;
	}
	return nullptr;
}

const Expr::This* Expr::as_this_expr() const
{
	if (m_tag == Tag::THIS) {
		return &m_this_expr;
	}
	return nullptr;
}

const Expr::Unary* Expr::as_unary() const
{
	if (m_tag == Tag::UNARY) {
		return &m_unary;
	}
	return nullptr;
}

const Expr::Binary* Expr::as_binary() const
{
	if (m_tag == Tag::BINARY) {
		return &m_binary;
	}
	return nullptr;
}

const Expr::IndexExpr* Expr::as_index_expr() const
{
	if (m_tag == Tag::INDEX) {
		return &m_index_expr;
	}
	return nullptr;
}

const Expr::VariableExpr* Expr::as_variable_expr() const
{
	if (m_tag == Tag::VARIABLE) {
		return &m_variable_expr;
	}
	return nullptr;
}

const Expr::MethodCall* Expr::as_method_call() const
{
	if (m_tag == Tag::METHOD_CALL) {
		return &m_method_call;
	}
	return nullptr;
}

const Expr::FieldAccess* Expr::as_field_access() const
{
	if (m_tag == Tag::FIELD_ACCESS) {
		return &m_field_access;
	}
	return nullptr;
}

const Expr::New* Expr::as_new_expr() const
{
	if (m_tag == Tag::NEW) {
		return &m_new_expr;
	}
	return nullptr;
}

bool Expr::is_lvalue() const
{
	switch (m_tag) {
	case Tag::THIS:
	case Tag::INDEX:
	case Tag::VARIABLE:
	case Tag::FIELD_ACCESS:
	case Tag::NEW:
		return true;

	case Tag::METHOD_CALL: {
		const auto* type = m_method_call.method().return_type();
		return type != nullptr && type->as_object_type() != nullptr;
	}

	default:
		return false;
	}
}

Stmt::Stmt(Stmt&& other)
	: m_tag(other.m_tag)
{
	construct_variant_from_other(other);
}

Stmt& Stmt::operator=(Stmt&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	construct_variant_from_other(other);

	return *this;
}

Stmt::~Stmt()
{
	destroy_variant();
}

void Stmt::destroy_variant()
{
	switch (m_tag) {
	case Tag::ASSIGNMENT:
		m_assignment.~Assignment();
		break;

	case Tag::OP_ASSIGNMENT:
		m_op_assignment.~OpAssignment();
		break;

	case Tag::IF:
		m_if.~If();
		break;

	case Tag::WHILE:
		m_while.~While();
		break;

	case Tag::FOREACH_RANGE:
		m_foreach_range.~ForeachRange();
		break;

	case Tag::FOREACH_POOL:
		m_foreach_pool.~ForeachPool();
		break;

	case Tag::EXPR_STMT:
		m_expr_stmt.~ExprStmt();
		break;

	case Tag::BREAK:
		m_break.~Break();
		break;

	case Tag::CONTINUE:
		m_continue.~Continue();
		break;

	case Tag::RETURN:
		m_return.~Return();
		break;
	}
}

void Stmt::construct_variant_from_other(Stmt& other)
{
	switch (other.m_tag) {
	case Tag::ASSIGNMENT:
		new (&m_assignment) Assignment(std::move(other.m_assignment));
		break;

	case Tag::OP_ASSIGNMENT:
		new (&m_op_assignment) OpAssignment(std::move(other.m_op_assignment));
		break;

	case Tag::IF:
		new (&m_if) If(std::move(other.m_if));
		break;

	case Tag::WHILE:
		new (&m_while) While(std::move(other.m_while));
		break;

	case Tag::FOREACH_RANGE:
		new (&m_foreach_range) ForeachRange(std::move(other.m_foreach_range));
		break;

	case Tag::FOREACH_POOL:
		new (&m_foreach_pool) ForeachPool(std::move(other.m_foreach_pool));
		break;

	case Tag::EXPR_STMT:
		new (&m_expr_stmt) ExprStmt(std::move(other.m_expr_stmt));
		break;

	case Tag::BREAK:
		new (&m_break) Break(std::move(other.m_break));
		break;

	case Tag::CONTINUE:
		new (&m_continue) Continue(std::move(other.m_continue));
		break;

	case Tag::RETURN:
		new (&m_return) Return(std::move(other.m_return));
		break;
	}
}

void DefaultVisitor::visit(const PoolType::BoundType&)
{
}

void DefaultVisitor::visit(const PoolType::LayoutType&)
{
}

void DefaultVisitor::visit(const Pool&)
{
}

void DefaultVisitor::visit(const PoolParameter::PoolRef& e)
{
	e.pool().accept(*this);
}

void DefaultVisitor::visit(const PoolParameter::None&)
{
}

void DefaultVisitor::visit(const Type::PrimitiveType&)
{
}

void DefaultVisitor::visit(const Type::NullType&)
{
}

void DefaultVisitor::visit(const Type::VoidType&)
{
}

void DefaultVisitor::visit(const Type::ObjectType& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Variable& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Expr::IntegerConst&)
{
}

void DefaultVisitor::visit(const Expr::BooleanConst&)
{
}

void DefaultVisitor::visit(const Expr::Null&)
{
}

void DefaultVisitor::visit(const Expr::This& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Expr::Cast& e)
{
	e.expr().accept(*this);
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Expr::Unary& e)
{
	e.expr().accept(*this);
}

void DefaultVisitor::visit(const Expr::Binary& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void DefaultVisitor::visit(const Expr::IndexExpr& e)
{
	e.idx().accept(*this);
}

void DefaultVisitor::visit(const Expr::VariableExpr&)
{
}

void DefaultVisitor::visit(const Expr::MethodCall& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Expr::FieldAccess& e)
{
	e.expr().accept(*this);
}

void DefaultVisitor::visit(const Expr::New& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Field& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Cluster&)
{
}

void DefaultVisitor::visit(const Layout& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Stmt::Assignment& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void DefaultVisitor::visit(const Stmt::OpAssignment& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void DefaultVisitor::visit(const Stmt::If& e)
{
	e.cond().accept(*this);
	visit(e.then_begin(), e.then_end());
	visit(e.else_begin(), e.else_end());
}

void DefaultVisitor::visit(const Stmt::While& e)
{
	e.cond().accept(*this);
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Stmt::ForeachRange& e)
{
	e.range_begin().accept(*this);
	e.range_end().accept(*this);
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Stmt::ForeachPool& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Stmt::ExprStmt& e)
{
	e.expr().accept(*this);
}

void DefaultVisitor::visit(const Stmt::Break&)
{
}

void DefaultVisitor::visit(const Stmt::Continue&)
{
}

void DefaultVisitor::visit(const Stmt::Return& e)
{
	auto* expr = e.expr();
	if (expr != nullptr) {
		expr->accept(*this);
	}
}

void DefaultVisitor::visit(const Method& e)
{
	visit(e.vars_begin(), e.vars_end());
	visit(e.body_begin(), e.body_end());
}

void DefaultVisitor::visit(const Class& e)
{
	visit_ref(e.pools_begin(), e.pools_end());
	visit_ref(e.fields_begin(), e.fields_end());
	visit_ref(e.methods_begin(), e.methods_end());
}

void DefaultVisitor::visit(const Program& e)
{
	visit_ref(e.classes_begin(), e.classes_end());
}

} // namespace Ast
