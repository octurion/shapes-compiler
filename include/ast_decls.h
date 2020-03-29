#pragma once

namespace Ast
{

class Expr;
class Stmt;
class Type;

class PrimitiveType;
class ClassType;
class PoolType;
class BoundType;
class NullType;

class Variable;
class Pool;

class IntegerConst;
class NullExpr;
class ThisExpr;
class BinaryExpr;
class UnaryExpr;
class IndexExpr;
class VariableExpr;
class MethodCall;
class FieldAccess;
class NewExpr;

class AssignStmt;
class OpAssignStmt;
class IfStmt;
class WhileStmt;
class ForeachRangeStmt;
class ForeachPoolStmt;

class ExprStmt;
class BreakStmt;
class ContinueStmt;
class ReturnStmt;

class Class;
class Field;
class Method;

class Layout;
class Cluster;

class Program;

class Visitor
{
public:
	virtual ~Visitor() {}

	virtual void visit(const PrimitiveType&) = 0;
	virtual void visit(const ClassType&)     = 0;
	virtual void visit(const PoolType&)      = 0;
	virtual void visit(const BoundType&)     = 0;
	virtual void visit(const NullType&)      = 0;

	virtual void visit(const Variable&) = 0;
	virtual void visit(const Pool&)     = 0;

	virtual void visit(const IntegerConst&) = 0;
	virtual void visit(const NullExpr&)     = 0;
	virtual void visit(const ThisExpr&)     = 0;
	virtual void visit(const BinaryExpr&)   = 0;
	virtual void visit(const UnaryExpr&)    = 0;
	virtual void visit(const IndexExpr&)    = 0;
	virtual void visit(const VariableExpr&) = 0;
	virtual void visit(const MethodCall&)   = 0;
	virtual void visit(const FieldAccess&)  = 0;
	virtual void visit(const NewExpr&)      = 0;

	virtual void visit(const AssignStmt&)       = 0;
	virtual void visit(const OpAssignStmt&)     = 0;
	virtual void visit(const IfStmt&)           = 0;
	virtual void visit(const WhileStmt&)        = 0;
	virtual void visit(const ForeachRangeStmt&) = 0;
	virtual void visit(const ForeachPoolStmt&)  = 0;

	virtual void visit(const ExprStmt&)     = 0;
	virtual void visit(const BreakStmt&)    = 0;
	virtual void visit(const ContinueStmt&) = 0;
	virtual void visit(const ReturnStmt&)   = 0;

	virtual void visit(const Class&)  = 0;
	virtual void visit(const Field&)  = 0;
	virtual void visit(const Method&) = 0;

	virtual void visit(const Layout&)  = 0;
	virtual void visit(const Cluster&) = 0;

	virtual void visit(const Program&) = 0;
};

}
