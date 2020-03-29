#include "cst.h"

class DefaultVisitor: public Cst::Visitor
{
	template<typename Iter>
	void visitIter(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			visit(*it);
		}
	}

	template<typename Iter>
	void visitPtrIter(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			visit(**it);
		}
	}

	void visit(const Cst::Identifier&) override {};

	void visit(const Cst::NoneParam&) override {};

	void visit(const Cst::Variable& e) override
	{
		e.name().accept(*this);
		e.type().accept(*this);
	}

	void visit(const Cst::PrimitiveType&) override {};
	void visit(const Cst::ClassType& e) override
	{
		e.class_name().accept(*this);
		visitIter(e.pool_params_begin(), e.pool_params_end());
	}
	void visit(const Cst::BoundType& e) override
	{
		e.class_name().accept(*this);
		visitIter(e.pool_params_begin(), e.pool_params_end());
	}
	void visit(const Cst::InvalidType&) override {};

	void visit(const Cst::IntegerConst&) override {};
	void visit(const Cst::NullExpr&) override {};
	void visit(const Cst::ThisExpr&) override {};
	void visit(const Cst::BinaryExpr& e) override
	{
		e.lhs().accept(*this);
		e.rhs().accept(*this);
	}
	void visit(const Cst::UnaryExpr& e) override
	{
		e.expr().accept(*this);
	}
	void visit(const Cst::IndexExpr& e) override
	{
		e.expr().accept(*this);
		e.idx().accept(*this);
	}
	void visit(const Cst::IdentifierExpr& e) override
	{
		e.name().accept(*this);
	}
	void visit(const Cst::MethodCall& e) override
	{
		e.name().accept(*this);
		visitPtrIter(e.params_begin(), e.params_end());
	}
	void visit(const Cst::MemberMethodCall& e) override
	{
		e.this_expr().accept(*this);
		e.name().accept(*this);
		visitPtrIter(e.params_begin(), e.params_end());
	}
	void visit(const Cst::FieldAccess& e) override
	{
		e.expr().accept(*this);
		e.field().accept(*this);
	}
	void visit(const Cst::NewExpr& e) override
	{
		e.type().accept(*this);
	}
	void visit(const Cst::InvalidExpr&) override {}

	void visit(const Cst::NoopStmt&) override {}
	void visit(const Cst::VariableDeclsStmt& e) override
	{
		visitIter(e.begin(), e.end());
	}
	void visit(const Cst::AssignStmt& e) override
	{
		e.lhs().accept(*this);
		e.rhs().accept(*this);
	}
	void visit(const Cst::OpAssignStmt& e) override
	{
		e.lhs().accept(*this);
		e.rhs().accept(*this);
	}
	void visit(const Cst::IfStmt& e) override
	{
		e.cond().accept(*this);
		e.then_branch().accept(*this);
		e.else_branch().accept(*this);
	}
	void visit(const Cst::WhileStmt& e) override
	{
		e.cond().accept(*this);
		e.body().accept(*this);
	}
	void visit(const Cst::ForeachRangeStmt& e) override
	{
		e.var().accept(*this);
		e.range_begin().accept(*this);
		e.range_end().accept(*this);
		e.body().accept(*this);
	}
	void visit(const Cst::ForeachPoolStmt& e) override
	{
		e.var().accept(*this);
		e.pool().accept(*this);
		e.body().accept(*this);
	}
	void visit(const Cst::BlockStmt& e) override
	{
		visitPtrIter(e.begin(), e.end());
	}
	void visit(const Cst::ExprStmt& e) override
	{
		e.expr().accept(*this);
	}
	void visit(const Cst::BreakStmt&) override {}
	void visit(const Cst::ContinueStmt&) override {}
	void visit(const Cst::ReturnStmt& e) override
	{
		e.expr().accept(*this);
	}
	void visit(const Cst::ReturnVoidStmt&) override {}

	void visit(const Cst::Method& e) override
	{
		e.name().accept(*this);
		if (e.type() != nullptr) {
			e.type()->accept(*this);
		}
		e.body().accept(*this);
	}
	void visit(const Cst::Class& e) override
	{
		e.name().accept(*this);
		visitIter(e.pool_params_begin(), e.pool_params_end());
		visitIter(e.pool_param_bounds_begin(), e.pool_param_bounds_end());
		visitIter(e.fields_begin(), e.fields_end());
		visitIter(e.methods_begin(), e.methods_end());
	}
	void visit(const Cst::Field& e) override
	{
		e.name().accept(*this);
		e.type().accept(*this);
	}
	void visit(const Cst::Layout& e) override
	{
		e.name().accept(*this);
		e.for_class().accept(*this);
		visitIter(e.clusters_begin(), e.clusters_end());
	}
	void visit(const Cst::Cluster& e) override
	{
		visitIter(e.fields_begin(), e.fields_end());
	}

	void visit(const Cst::Program& e) override
	{
		visitIter(e.classes_begin(), e.classes_end());
		visitIter(e.layouts_begin(), e.layouts_end());
	}
};
