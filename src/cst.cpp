#include "cst.h"

namespace Cst
{

Type::Type(Type&& other)
	: m_tag(other.m_tag)
{
	construct_variant_from_other(other);
}

Type& Type::operator=(Type&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
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
	case Tag::PRIMITIVE:
		m_primitive_type.~PrimitiveType();
		break;

	case Tag::OBJECT:
		m_object_type.~ObjectType();
		break;
	}
}

void Type::construct_variant_from_other(Type& other)
{
	switch (other.m_tag) {
	case Tag::PRIMITIVE:
		new (&m_primitive_type) PrimitiveType(std::move(other.m_primitive_type));
		break;

	case Tag::OBJECT:
		new (&m_object_type) ObjectType(std::move(other.m_object_type));
		break;
	}
}

Expr::Expr(Expr&& other)
	: m_tag(other.m_tag)
{
	construct_variant_from_other(other);
}

Expr& Expr::operator=(Expr&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
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

	case Tag::UNARY:
		m_unary.~Unary();
		break;

	case Tag::CAST:
		m_cast.~Cast();
		break;

	case Tag::BINARY:
		m_binary.~Binary();
		break;

	case Tag::INDEX:
		m_index_expr.~IndexExpr();
		break;

	case Tag::VARIABLE_EXPR:
		m_variable_expr.~VariableExpr();
		break;

	case Tag::METHOD_CALL:
		m_method_call.~MethodCall();
		break;

	case Tag::MEMBER_METHOD_CALL:
		m_member_method_call.~MemberMethodCall();
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

	case Tag::VARIABLE_EXPR:
		new (&m_variable_expr) VariableExpr(std::move(other.m_variable_expr));
		break;

	case Tag::MEMBER_METHOD_CALL:
		new (&m_member_method_call) MemberMethodCall(std::move(other.m_member_method_call));
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

const Location& Expr::loc() const
{
	switch (m_tag) {
	case Tag::INTEGER_CONST:
		return m_integer_const.loc();
		break;

	case Tag::BOOLEAN_CONST:
		return m_boolean_const.loc();
		break;

	case Tag::NULL_EXPR:
		return m_null_expr.loc();
		break;

	case Tag::THIS:
		return m_this_expr.loc();
		break;

	case Tag::CAST:
		return m_cast.loc();
		break;

	case Tag::UNARY:
		return m_unary.loc();
		break;

	case Tag::BINARY:
		return m_binary.loc();
		break;

	case Tag::INDEX:
		return m_index_expr.loc();
		break;

	case Tag::VARIABLE_EXPR:
		return m_variable_expr.loc();
		break;

	case Tag::METHOD_CALL:
		return m_method_call.loc();
		break;

	case Tag::MEMBER_METHOD_CALL:
		return m_member_method_call.loc();
		break;

	case Tag::FIELD_ACCESS:
		return m_field_access.loc();
		break;

	case Tag::NEW:
		return m_new_expr.loc();
		break;
	}
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
	case Tag::POOL:
		m_pool.~Pool();
		break;

	case Tag::NONE:
		m_none.~None();
		break;
	}
}

void PoolParameter::construct_variant_from_other(PoolParameter& other)
{
	switch (other.m_tag) {
	case Tag::POOL:
		new (&m_pool) Pool(std::move(other.m_pool));
		break;

	case Tag::NONE:
		new (&m_none) None(std::move(other.m_none));
		break;
	}
}

Stmt::Stmt(Stmt&& other)
	: m_tag(other.m_tag)
	, m_loc(other.m_loc)
{
	construct_variant_from_other(other);
}

Stmt& Stmt::operator=(Stmt&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	m_loc = other.m_loc;
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
	case Tag::NOOP:
		m_noop.~Noop();
		break;

	case Tag::VARIABLE_DECLARATIONS:
		m_variable_declarations.~VariableDeclarations();
		break;

	case Tag::POOL_DECLARATIONS:
		m_pool_declarations.~PoolDeclarations();
		break;

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

	case Tag::BLOCK:
		m_block.~Block();
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

	case Tag::RETURN_VOID:
		m_return_void.~ReturnVoid();
		break;
	}
}

void Stmt::construct_variant_from_other(Stmt& other)
{
	switch (other.m_tag) {
	case Tag::NOOP:
		new (&m_noop) Noop(std::move(other.m_noop));
		break;

	case Tag::VARIABLE_DECLARATIONS:
		new (&m_variable_declarations) VariableDeclarations(std::move(other.m_variable_declarations));
		break;

	case Tag::POOL_DECLARATIONS:
		new (&m_pool_declarations) PoolDeclarations(std::move(other.m_pool_declarations));
		break;

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

	case Tag::BLOCK:
		new (&m_block) Block(std::move(other.m_block));
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

	case Tag::RETURN_VOID:
		new (&m_return_void) ReturnVoid(std::move(other.m_return_void));
		break;
	}
}

void DefaultVisitor::visit(const FormalPoolParameter&)
{
}

void DefaultVisitor::visit(const PoolParameter::Pool&)
{
}

void DefaultVisitor::visit(const PoolParameter::None&)
{
}

void DefaultVisitor::visit(const Type::PrimitiveType&)
{
}

void DefaultVisitor::visit(const Type::ObjectType& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const BoundType& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const LayoutType& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const FormalPoolBound& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const VariableDeclaration& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const PoolDeclaration& e)
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

void DefaultVisitor::visit(const Expr::This&)
{
}

void DefaultVisitor::visit(const Expr::Cast& e)
{
	e.expr().accept(*this);
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Expr::Binary& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void DefaultVisitor::visit(const Expr::VariableExpr&)
{
}

void DefaultVisitor::visit(const Expr::Unary& e)
{
	e.expr().accept(*this);
}

void DefaultVisitor::visit(const Expr::IndexExpr& e)
{
	e.pool().accept(*this);
	e.idx().accept(*this);
}

void DefaultVisitor::visit(const Expr::MethodCall& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Expr::MemberMethodCall& e)
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

void DefaultVisitor::visit(const Stmt::Noop&)
{
}

void DefaultVisitor::visit(const Stmt::VariableDeclarations& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Stmt::PoolDeclarations& e)
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
	e.then_branch().accept(*this);
	e.else_branch().accept(*this);
}

void DefaultVisitor::visit(const Stmt::While& e)
{
	e.cond().accept(*this);
	e.body().accept(*this);
}

void DefaultVisitor::visit(const Stmt::ForeachRange& e)
{
	e.range_begin().accept(*this);
	e.range_end().accept(*this);
	e.body().accept(*this);
}

void DefaultVisitor::visit(const Stmt::ForeachPool& e)
{
	e.body().accept(*this);
}

void DefaultVisitor::visit(const Stmt::Block& e)
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
	e.expr().accept(*this);
}

void DefaultVisitor::visit(const Stmt::ReturnVoid&)
{
}

void DefaultVisitor::visit(const Field& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const MethodParameter& e)
{
	e.type().accept(*this);
}

void DefaultVisitor::visit(const Method& e)
{
	visit(e.begin(), e.end());
	e.body().accept(*this);
}

void DefaultVisitor::visit(const Class& e)
{
	visit(e.pool_params_begin(), e.pool_params_end());
	visit(e.pool_param_bounds_begin(), e.pool_param_bounds_end());
	visit(e.fields_begin(), e.fields_end());
	visit(e.methods_begin(), e.methods_end());
}

void DefaultVisitor::visit(const ClusterField&)
{
}

void DefaultVisitor::visit(const Cluster& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Layout& e)
{
	visit(e.begin(), e.end());
}

void DefaultVisitor::visit(const Program& e)
{
	visit(e.classes_begin(), e.classes_end());
	visit(e.layouts_begin(), e.layouts_end());
}

} // namespace Cst
