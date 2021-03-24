#include "cst.h"

#include <ostream>

namespace Cst
{

static const char* const PRIMITIVE_TYPE_NAMES[] = {
	"bool", // PrimitiveType::BOOL
	"i8",   // PrimitiveType::I8
	"u8",   // PrimitiveType::U8
	"i16",  // PrimitiveType::I16
	"u16",  // PrimitiveType::U16
	"i32",  // PrimitiveType::I32
	"u32",  // PrimitiveType::U32
	"i64",  // PrimitiveType::I64
	"u64",  // PrimitiveType::U64
	"f32",  // PrimitiveType::F32
	"f64",  // PrimitiveType::F64
};

std::ostream& operator<<(std::ostream& os, const Pool& pool) {
	return os << pool.ident();
}
std::ostream& operator<<(std::ostream& os, const None&) {
	return os << "none";
}

std::ostream& print_pool_params(
	std::ostream& os, const std::vector<Pool>& pool_params) {
	if (pool_params.empty()) {
		return os;
	}

	os << "<";
	auto it = pool_params.begin();
	os << *it;

	for (; it != pool_params.end(); it++) {
		os << ", " << *it;
	}

	return os << ">";
}

std::ostream& operator<<(std::ostream& os, const PoolParameter& param) {
	return mpark::visit(
		[&os](const auto& e) -> std::ostream& { return os << e; },
		param);
}

std::ostream& operator<<(std::ostream& os, const PrimitiveType& type)
{
	return os << type.kind();
}

std::ostream& operator<<(std::ostream& os, PrimitiveKind kind)
{
	return os << PRIMITIVE_TYPE_NAMES[(size_t) kind];
}

std::ostream& operator<<(std::ostream& os, const Type& type) {
	return mpark::visit(
		[&os](const auto& e) -> std::ostream& { return os << e; },
		type);
}

std::ostream& print_pool_params(
	std::ostream& os, const std::vector<PoolParameter>& pool_params) {
	if (pool_params.empty()) {
		return os;
	}

	os << "<";
	auto it = pool_params.begin();

	for (; it != pool_params.end(); it++) {
		os << ", " << *it;
	}

	return os << ">";
}

std::ostream& operator<<(std::ostream& os, const ObjectType& type)
{
	os << type.class_name().ident();
	print_pool_params(os, type.params());
	return os;
}

std::ostream& operator<<(std::ostream& os, const LayoutType& type)
{
	os << type.layout_name().ident();
	print_pool_params(os, type.params());
	return os;
}

std::ostream& operator<<(std::ostream& os, const BoundType& type)
{
	os << "[";
	os << type.class_name().ident();
	print_pool_params(os, type.params());
	return os << "]";
}

Assignment::Assignment(Expr lhs, Expr rhs)
	: m_lhs(new Expr(std::move(lhs)))
	, m_rhs(new Expr(std::move(rhs)))
{}

CastExpr::CastExpr(Expr expr, PrimitiveType type, const Location& loc)
	: m_expr(new Expr(std::move(expr)))
	, m_type(std::move(type))
	, m_loc(loc)
{}
const Expr& CastExpr::expr() const { return *m_expr; }

BinaryExpr::BinaryExpr(Expr lhs, BinOp op, Expr rhs, const Location& loc)
	: m_lhs(new Expr(std::move(lhs)))
	, m_rhs(new Expr(std::move(rhs)))
	, m_op(op)
	, m_loc(loc)
{}

const Expr& BinaryExpr::lhs() const { return *m_lhs; }
const Expr& BinaryExpr::rhs() const { return *m_rhs; }

UnaryExpr::UnaryExpr(UnOp op, Expr expr, const Location& loc)
	: m_op(op)
	, m_expr(new Expr(std::move(expr)))
	, m_loc(loc)
{}
const Expr& UnaryExpr::expr() const { return *m_expr; }

MethodCall::MethodCall(
		Identifier name, std::vector<Expr> params, const Location& loc)
	: m_name(std::move(name))
	, m_params(std::move(params))
	, m_loc(loc)
{}

MemberMethodCall::MemberMethodCall(
		Expr this_expr,
		Identifier name,
		std::vector<Expr> args,
		const Location& loc)
	: m_this_expr(new Expr(std::move(this_expr)))
	, m_name(std::move(name))
	, m_args(std::move(args))
	, m_loc(loc)
{}
const Expr& MemberMethodCall::this_expr() const { return *m_this_expr; }

FieldAccess::FieldAccess(Expr expr, Identifier field, const Location& loc)
	: m_expr(new Expr(std::move(expr)))
	, m_field(std::move(field))
	, m_loc(loc)
{}
const Expr& FieldAccess::expr() const { return *m_expr; }

const Location& location(const Expr& expr)
{
	return mpark::visit([](const auto& e) -> const Location& { return e.loc(); }, expr);
}

IfStmt::IfStmt(Expr cond, Stmt then_branch, Stmt else_branch)
	: m_cond(std::move(cond))
	, m_then_branch(new Stmt(std::move(then_branch)))
	, m_else_branch(new Stmt(std::move(else_branch)))
{}
const Stmt& IfStmt::then_branch() const { return *m_then_branch; }
const Stmt& IfStmt::else_branch() const { return *m_else_branch; }

WhileStmt::WhileStmt(Expr cond, Stmt body)
	: m_cond(std::move(cond))
	, m_body(new Stmt(std::move(body)))
{}
const Stmt& WhileStmt::body() const { return *m_body; }

ForeachRange::ForeachRange(
		Identifier var, Expr range_begin, Expr range_end, Stmt body)
	: m_var(std::move(var))
	, m_range_begin(std::move(range_begin))
	, m_range_end(std::move(range_end))
	, m_body(std::unique_ptr<Stmt>(new Stmt(std::move(body))))
{}
const Stmt& ForeachRange::body() const { return *m_body; }

ForeachPool::ForeachPool(Identifier var, Identifier pool, Stmt body)
	: m_var(std::move(var))
	, m_pool(std::move(pool))
	, m_body(std::unique_ptr<Stmt>(new Stmt(std::move(body))))
{}
const Stmt& ForeachPool::body() const { return *m_body; }

Block::Block(std::vector<Stmt> stmts)
	: m_stmts(std::move(stmts))
{}
void Block::add(Stmt stmt) { m_stmts.emplace_back(std::move(stmt)); }

} // namespace Cst
