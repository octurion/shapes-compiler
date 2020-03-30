#include "cst.h"

void Cst::DefaultVisitor::visit(const Cst::Identifier&)
{
}

void Cst::DefaultVisitor::visit(const Cst::NoneParam&)
{
}

void Cst::DefaultVisitor::visit(const Cst::Variable& e)
{
	e.name().accept(*this);
	e.type().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::PrimitiveType&)
{
}

void Cst::DefaultVisitor::visit(const Cst::ClassType& e)
{
	e.class_name().accept(*this);
	visitPtrIter(e.pool_params_begin(), e.pool_params_end());
}

void Cst::DefaultVisitor::visit(const Cst::BoundType& e)
{
	e.class_name().accept(*this);
	visitPtrIter(e.pool_params_begin(), e.pool_params_end());
}

void Cst::DefaultVisitor::visit(const Cst::InvalidType&)
{
}

void Cst::DefaultVisitor::visit(const Cst::IntegerConst&)
{
}

void Cst::DefaultVisitor::visit(const Cst::NullExpr&)
{
}

void Cst::DefaultVisitor::visit(const Cst::ThisExpr&)
{
}

void Cst::DefaultVisitor::visit(const Cst::BinaryExpr& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::UnaryExpr& e)
{
	e.expr().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::IndexExpr& e)
{
	e.expr().accept(*this);
	e.idx().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::IdentifierExpr& e)
{
	e.name().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::MethodCall& e)
{
	e.name().accept(*this);
	visitPtrIter(e.params_begin(), e.params_end());
}

void Cst::DefaultVisitor::visit(const Cst::MemberMethodCall& e)
{
	e.this_expr().accept(*this);
	e.name().accept(*this);
	visitPtrIter(e.params_begin(), e.params_end());
}

void Cst::DefaultVisitor::visit(const Cst::FieldAccess& e)
{
	e.expr().accept(*this);
	e.field().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::NewExpr& e)
{
	e.type().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::InvalidExpr&)
{
}

void Cst::DefaultVisitor::visit(const Cst::NoopStmt&)
{
}

void Cst::DefaultVisitor::visit(const Cst::VariableDeclsStmt& e)
{
	visitIter(e.begin(), e.end());
}

void Cst::DefaultVisitor::visit(const Cst::AssignStmt& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::OpAssignStmt& e)
{
	e.lhs().accept(*this);
	e.rhs().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::IfStmt& e)
{
	e.cond().accept(*this);
	e.then_branch().accept(*this);
	e.else_branch().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::WhileStmt& e)
{
	e.cond().accept(*this);
	e.body().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::ForeachRangeStmt& e)
{
	e.var().accept(*this);
	e.range_begin().accept(*this);
	e.range_end().accept(*this);
	e.body().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::ForeachPoolStmt& e)
{
	e.var().accept(*this);
	e.pool().accept(*this);
	e.body().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::BlockStmt& e)
{
	visitPtrIter(e.begin(), e.end());
}

void Cst::DefaultVisitor::visit(const Cst::ExprStmt& e)
{
	e.expr().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::BreakStmt&)
{
}

void Cst::DefaultVisitor::visit(const Cst::ContinueStmt&)
{
}

void Cst::DefaultVisitor::visit(const Cst::ReturnStmt& e)
{
	e.expr().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::ReturnVoidStmt&)
{
}

void Cst::DefaultVisitor::visit(const Cst::Method& e)
{
	e.name().accept(*this);
	if (e.type() != nullptr) {
		e.type()->accept(*this);
	}
	e.body().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::Class& e)
{
	e.name().accept(*this);
	visitIter(e.pool_params_begin(), e.pool_params_end());
	visitIter(e.pool_param_bounds_begin(), e.pool_param_bounds_end());
	visitIter(e.fields_begin(), e.fields_end());
	visitIter(e.methods_begin(), e.methods_end());
}

void Cst::DefaultVisitor::visit(const Cst::Field& e)
{
	e.name().accept(*this);
	e.type().accept(*this);
}

void Cst::DefaultVisitor::visit(const Cst::Layout& e)
{
	e.name().accept(*this);
	e.for_class().accept(*this);
	visitIter(e.clusters_begin(), e.clusters_end());
}

void Cst::DefaultVisitor::visit(const Cst::Cluster& e)
{
	visitIter(e.fields_begin(), e.fields_end());
}

void Cst::DefaultVisitor::visit(const Cst::Program& e)
{
	visitIter(e.classes_begin(), e.classes_end());
	visitIter(e.layouts_begin(), e.layouts_end());
}
