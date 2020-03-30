#pragma once

#include "parse_tree_common.h"

/**
 * Forward declarations for all CST nodes available in the program. By grouping
 * all of them in one place, we can avoid having to put up with circular
 * definition errors and be able to define a visitor for all CST nodes
 * as well as provide an implementation for the `accept()` method on each CST
 * node.
 */
namespace Cst
{

class PoolParameter;
class Expr;
class Stmt;
class Type;

class Identifier;
class NoneParam;

class PrimitiveType;
class ClassType;
class BoundType;
class InvalidType;

class Variable;

class IntegerConst;
class NullExpr;
class ThisExpr;
class BinaryExpr;
class UnaryExpr;
class IndexExpr;
class IdentifierExpr;
class MethodCall;
class MemberMethodCall;
class FieldAccess;
class NewExpr;
class InvalidExpr;

class NoopStmt;
class VariableDeclsStmt;
class AssignStmt;
class OpAssignStmt;
class IfStmt;
class WhileStmt;
class ForeachRangeStmt;
class ForeachPoolStmt;
class BlockStmt;
class ExprStmt;
class BreakStmt;
class ContinueStmt;
class ReturnStmt;
class ReturnVoidStmt;

class Class;
class ClassBody;
class Field;
class Method;

class Layout;
class Cluster;

class Program;

/**
 * Base visitor declaration for all CST nodes.
 */
class Visitor
{
public:
	virtual ~Visitor() {}

	virtual void visit(const Cst::Identifier&)   = 0;
	virtual void visit(const Cst::NoneParam&)    = 0;

	virtual void visit(const Cst::Variable&)     = 0;

	virtual void visit(const Cst::BoundType&)     = 0;
	virtual void visit(const Cst::PrimitiveType&) = 0;
	virtual void visit(const Cst::ClassType&)     = 0;
	virtual void visit(const Cst::InvalidType&)   = 0;

	virtual void visit(const Cst::IntegerConst&)     = 0;
	virtual void visit(const Cst::NullExpr&)         = 0;
	virtual void visit(const Cst::ThisExpr&)         = 0;
	virtual void visit(const Cst::BinaryExpr&)       = 0;
	virtual void visit(const Cst::UnaryExpr&)        = 0;
	virtual void visit(const Cst::IndexExpr&)        = 0;
	virtual void visit(const Cst::IdentifierExpr&)   = 0;
	virtual void visit(const Cst::MethodCall&)       = 0;
	virtual void visit(const Cst::MemberMethodCall&) = 0;
	virtual void visit(const Cst::FieldAccess&)      = 0;
	virtual void visit(const Cst::NewExpr&)          = 0;
	virtual void visit(const Cst::InvalidExpr&)      = 0;

	virtual void visit(const Cst::NoopStmt&)          = 0;
	virtual void visit(const Cst::VariableDeclsStmt&) = 0;
	virtual void visit(const Cst::AssignStmt&)        = 0;
	virtual void visit(const Cst::OpAssignStmt&)      = 0;
	virtual void visit(const Cst::IfStmt&)            = 0;
	virtual void visit(const Cst::WhileStmt&)         = 0;
	virtual void visit(const Cst::ForeachRangeStmt&)  = 0;
	virtual void visit(const Cst::ForeachPoolStmt&)   = 0;
	virtual void visit(const Cst::BlockStmt&)         = 0;
	virtual void visit(const Cst::ExprStmt&)          = 0;
	virtual void visit(const Cst::BreakStmt&)         = 0;
	virtual void visit(const Cst::ContinueStmt&)      = 0;
	virtual void visit(const Cst::ReturnStmt&)        = 0;
	virtual void visit(const Cst::ReturnVoidStmt&)    = 0;

	virtual void visit(const Cst::Method&)  = 0;
	virtual void visit(const Cst::Class&)   = 0;
	virtual void visit(const Cst::Field&)   = 0;
	virtual void visit(const Cst::Layout&)  = 0;
	virtual void visit(const Cst::Cluster&) = 0;

	virtual void visit(const Cst::Program&) = 0;
};

class DefaultVisitor: public Visitor
{
public:
	template<typename Iter>
	void visitIter(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			it->accept(*this);
		}
	}

	template<typename Iter>
	void visitPtrIter(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++) {
			(*it)->accept(*this);
		}
	}

	virtual void visit(const Cst::Identifier&)   override;
	virtual void visit(const Cst::NoneParam&)    override;

	virtual void visit(const Cst::Variable&)     override;

	virtual void visit(const Cst::BoundType&)     override;
	virtual void visit(const Cst::PrimitiveType&) override;
	virtual void visit(const Cst::ClassType&)     override;
	virtual void visit(const Cst::InvalidType&)   override;

	virtual void visit(const Cst::IntegerConst&)     override;
	virtual void visit(const Cst::NullExpr&)         override;
	virtual void visit(const Cst::ThisExpr&)         override;
	virtual void visit(const Cst::BinaryExpr&)       override;
	virtual void visit(const Cst::UnaryExpr&)        override;
	virtual void visit(const Cst::IndexExpr&)        override;
	virtual void visit(const Cst::IdentifierExpr&)   override;
	virtual void visit(const Cst::MethodCall&)       override;
	virtual void visit(const Cst::MemberMethodCall&) override;
	virtual void visit(const Cst::FieldAccess&)      override;
	virtual void visit(const Cst::NewExpr&)          override;
	virtual void visit(const Cst::InvalidExpr&)      override;

	virtual void visit(const Cst::NoopStmt&)          override;
	virtual void visit(const Cst::VariableDeclsStmt&) override;
	virtual void visit(const Cst::AssignStmt&)        override;
	virtual void visit(const Cst::OpAssignStmt&)      override;
	virtual void visit(const Cst::IfStmt&)            override;
	virtual void visit(const Cst::WhileStmt&)         override;
	virtual void visit(const Cst::ForeachRangeStmt&)  override;
	virtual void visit(const Cst::ForeachPoolStmt&)   override;
	virtual void visit(const Cst::BlockStmt&)         override;
	virtual void visit(const Cst::ExprStmt&)          override;
	virtual void visit(const Cst::BreakStmt&)         override;
	virtual void visit(const Cst::ContinueStmt&)      override;
	virtual void visit(const Cst::ReturnStmt&)        override;
	virtual void visit(const Cst::ReturnVoidStmt&)    override;

	virtual void visit(const Cst::Method&)  override;
	virtual void visit(const Cst::Class&)   override;
	virtual void visit(const Cst::Field&)   override;
	virtual void visit(const Cst::Layout&)  override;
	virtual void visit(const Cst::Cluster&) override;

	virtual void visit(const Cst::Program&) override;
};

}
