#include "ast.h"

#include <cinttypes>
#include <ostream>
#include <sstream>
#include <vector>
#include <type_traits>
#include <unordered_map>
#include <utility>

namespace Ast
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

static const char* const UN_OP_NAMES[] = {
	"+", // UnOp::PLUS
	"-", // UnOp::MINUS
	"!", // UnOp::NOT
};

static const char* const BIN_OP_NAMES[] = {
	"+",  // BinOp::PLUS
	"-",  // BinOp::MINUS
	"*",  // BinOp::TIMES
	"/",  // BinOp::DIV
	"&&", // BinOp::LAND
	"||", // BinOp::LOR
	"&",  // BinOp::AND
	"|",  // BinOp::OR
	"^",  // BinOp::XOR
	"<<", // BinOp::SHL
	">>", // BinOp::SHR
	"==", // BinOp::EQ
	"!=", // BinOp::NE
	"<",  // BinOp::LT
	"<=", // BinOp::LE
	">",  // BinOp::GT
	">=", // BinOp::GE
};

template<typename T>
static std::string to_string(const T& value) {
	std::ostringstream os;
	os << value;
	return os.str();
}

static BoundType extract_bound_of_param(
	const Class& clazz, const std::vector<PoolParameter>& params, size_t idx, const Location& loc)
{
	const auto& formal_params = clazz.pools();

	assert_msg(idx < params.size(), "Pool index out of bounds");
	assert_msg(params.size() == formal_params.size(), "Pool parameter size mismatch");

	std::vector<PoolParameter> new_params;

	std::unordered_map<const Pool*, PoolParameter> pool_mapping;
	for (size_t i = 0; i < params.size(); i++) {
		pool_mapping.emplace(&formal_params[i].get(), params[i]);
	}

	const Pool& pool = formal_params[idx];
	const auto& bound = pool.type();
	assert_msg(mpark::holds_alternative<BoundType>(bound),
			   "Formal pool parameter has no bound type");
	const auto* bound_type = mpark::get_if<BoundType>(&bound);

	for (const auto& e: bound_type->params()) {
		const auto* as_pool_ref = mpark::get_if<PoolRef>(&e);
		if (as_pool_ref == nullptr) {
			new_params.emplace_back(None());
		} else {
			new_params.push_back(pool_mapping[&as_pool_ref->pool()]);
		}
	}

	return BoundType(bound_type->of_class(), std::move(new_params), loc);
}

PrimitiveType InvalidExpr::type() const
{
	unreachable("This is a propagated semantic error expression");
}
bool InvalidExpr::is_lvalue() const
{
	unreachable("This is a propagated semantic error expression");
}

bool PoolRef::operator==(const PoolRef& rhs) const {
	return m_pool == rhs.m_pool;
}
bool PoolRef::operator!=(const PoolRef& rhs) const {
	return m_pool != rhs.m_pool;
}
std::ostream& operator<<(std::ostream& os, const PoolRef& pool) {
	return os << pool.pool().name();
}
std::ostream& operator<<(std::ostream& os, const None&) {
	return os << "none";
}

static std::ostream& print_pool_params(
	std::ostream& os, const std::vector<PoolParameter>& pool_params) {
	if (pool_params.empty()) {
		return os;
	}

	os << "<";
	auto it = pool_params.begin();
	mpark::visit([&os](const auto& e) { os << e; }, *it++);

	for (; it != pool_params.end(); it++) {
		os << ", ";
		mpark::visit([&os](const auto& e) { os << e; }, *it);
	}

	return os << ">";
}

bool BoundType::operator==(const BoundType& rhs) const
{
	return m_class == rhs.m_class && m_params == rhs.m_params;
}
bool BoundType::operator!=(const BoundType& rhs) const
{
	return m_class != rhs.m_class || !(m_params == rhs.m_params);
}
bool BoundType::compatible_with_bound(const BoundType& bound) const
{
	return *this == bound;
}
bool BoundType::first_pool_param_is(const Pool& pool) const
{
	assert_msg(!m_params.empty(), "Bound type with no pool parameters");
	const auto* as_pool = mpark::get_if<PoolRef>(&m_params.front());
	return as_pool != nullptr && &as_pool->pool() == &pool;
}
ObjectType BoundType::to_object_type() const
{
	return ObjectType(*m_class, m_params, m_loc);
}
BoundType BoundType::to_bound_type() const
{
	return *this;
}
BoundType BoundType::bound_of_param(size_t idx) const
{
	return extract_bound_of_param(*m_class, m_params, idx, m_loc);
}
std::ostream& operator<<(std::ostream& os, const BoundType& type)
{
	os << "[";
	os << type.of_class().name();
	print_pool_params(os, type.params());
	return os << "]";
}

std::ostream& operator<<(std::ostream& os, const PoolType& type)
{
	mpark::visit([&os](const auto& e) { os << e; }, type);
	return os;
}
bool compatible_with_bound(const PoolType& type, const BoundType& bound)
{
	return mpark::visit([&bound](const auto& e) {
		return e.compatible_with_bound(bound);
	}, type);
}
bool first_pool_param_is(const PoolType& type, const Pool& pool)
{
	return mpark::visit([&pool](const auto& e) {
		return e.first_pool_param_is(pool);
	}, type);
}

struct PoolTypeClass {
	const Class& operator()(const LayoutType& e) {
		return e.for_class();
	}
	const Class& operator()(const BoundType& e) {
		return e.of_class();
	}
	const Class& operator()(const NoneType&) {
		unreachable("Can't pass none to formal pool parameter");
	}
};
const Class& for_class(const PoolType& type) {
	return mpark::visit(PoolTypeClass(), type);
}

struct PoolToObjType {
	ObjectType operator()(const LayoutType& e) {
		return e.to_object_type();
	}
	ObjectType operator()(const BoundType& e) {
		return e.to_object_type();
	}
	ObjectType operator()(const NoneType&) {
		unreachable("Can't pass none to index expression");
	}
};
ObjectType from_pool_type(const PoolType& type) {
	return mpark::visit(PoolToObjType(), type);
}

bool LayoutType::operator==(const LayoutType& rhs) const
{
	return m_layout == rhs.m_layout && m_params == rhs.m_params;
}
bool LayoutType::operator!=(const LayoutType& rhs) const
{
	return m_layout != rhs.m_layout || !(m_params == rhs.m_params);
}
bool LayoutType::compatible_with_bound(const BoundType& bound) const
{
	if (&for_class() != &bound.of_class()) {
		return false;
	}

	return m_params == bound.params();
}
bool LayoutType::first_pool_param_is(const Pool& pool) const
{
	assert_msg(!m_params.empty(), "Layout type with no pool parameters");
	const auto* as_pool = mpark::get_if<PoolRef>(&m_params.front());
	return as_pool != nullptr && &as_pool->pool() == &pool;
}
const Class& LayoutType::for_class() const {
	return m_layout->for_class();
}
ObjectType LayoutType::to_object_type() const
{
	return ObjectType(m_layout->for_class(), m_params, m_loc);
}
BoundType LayoutType::bound_of_param(size_t idx) const
{
	return extract_bound_of_param(m_layout->for_class(), m_params, idx, m_loc);
}

std::ostream& operator<<(std::ostream& os, const LayoutType& type)
{
	os << type.for_class().name();
	return print_pool_params(os, type.params());
}

std::ostream& operator<<(std::ostream& os, PrimitiveType type)
{
	return os << PRIMITIVE_TYPE_NAMES[(size_t) type];
}

std::ostream& operator<<(std::ostream& os, const NullptrType&)
{
	return os << "nullptr";
}
std::ostream& operator<<(std::ostream& os, const VoidType&)
{
	return os << "void";
}
std::ostream& operator<<(std::ostream& os, const ObjectType& type)
{
	os << type.of_class().name();
	return print_pool_params(os, type.params());
}

bool ObjectType::operator==(const ObjectType& rhs) const {
	return m_class == rhs.m_class && m_params == rhs.m_params;
}
bool ObjectType::operator!=(const ObjectType& rhs) const {
	return m_class != rhs.m_class || !(m_params == rhs.m_params);
}
bool ObjectType::compatible_with_bound(const BoundType& type) const
{
	return m_class == &type.of_class() && m_params == type.params();
}
BoundType ObjectType::to_bound_type() const
{
	return BoundType(*m_class, m_params, m_loc);
}
BoundType ObjectType::bound_of_param(size_t idx) const
{
	return extract_bound_of_param(*m_class, m_params, idx, m_loc);
}
ObjectType ObjectType::remap_formal_pool_params(const ObjectType& type_with_formal_pools) const
{
	const auto& new_pools = m_params;
	const auto& old_pools = m_class->pools();

	std::unordered_map<const Pool*, PoolParameter> mapping;
	for (size_t i = 0; i < new_pools.size(); i++) {
		const Pool& old = old_pools[i];
		mapping.emplace(&old, new_pools[i]);
	}

	std::vector<PoolParameter> new_params;
	for (const auto& e: type_with_formal_pools.m_params) {
		const auto* as_pool = mpark::get_if<PoolRef>(&e);
		if (as_pool == nullptr) {
			new_params.emplace_back(None());
			continue;
		}

		const auto it = mapping.find(&as_pool->pool());
		assert_msg(it != mapping.end(), "Spotted non-formal pool parameter");
		new_params.push_back(it->second);
	}

	return ObjectType(*type_with_formal_pools.m_class, std::move(new_params), m_loc);
}
Type ObjectType::remap_formal_pool_params(const Type& type_with_formal_pools) const
{
	const auto* as_obj_type = mpark::get_if<ObjectType>(&type_with_formal_pools);
	if (as_obj_type != nullptr) {
		return remap_formal_pool_params(*as_obj_type);
	}

	return type_with_formal_pools;
}

std::ostream& operator<<(std::ostream& os, const Type& type)
{
	mpark::visit([&os](const auto& e) { os << e; }, type);
	return os;
}

struct AssignableFromFunctor
{
	template<typename T, typename U>
	bool operator()(const T&, const U&) const { return false; }

	bool operator()(const ObjectType& lhs, const ObjectType& rhs) {
		return lhs == rhs;
	}

	bool operator()(const ObjectType&, const NullptrType&) { return true; }

	bool operator()(const PrimitiveType& lhs, const PrimitiveType& rhs) {
		return lhs == rhs;
	}
};

bool assignable_from(const Type& lhs, const Type& rhs)
{
	return mpark::visit(AssignableFromFunctor(), lhs, rhs);
}

Type expr_type(const Expr& expr) {
	return mpark::visit([](const auto& e) -> Type { return e.type(); }, expr);
}
bool is_lvalue(const Expr& expr) {
	return mpark::visit([](const auto& e) { return e.is_lvalue(); }, expr);
}

std::ostream& operator<<(std::ostream& os, UnOp op)
{
	return os << UN_OP_NAMES[(int)op];
}

std::ostream& operator<<(std::ostream& os, BinOp op)
{
	return os << BIN_OP_NAMES[(int)op];
}

CastExpr::CastExpr(Expr expr, PrimitiveType type)
	: m_expr(new Expr(std::move(expr)))
	, m_type(std::move(type))
{}
const Expr& CastExpr::expr() const { return *m_expr; }

BinaryExpr::BinaryExpr(Expr lhs, BinOp op, Expr rhs, PrimitiveType type)
	: m_lhs(new Expr(std::move(lhs)))
	, m_rhs(new Expr(std::move(rhs)))
	, m_op(op)
	, m_type(type)
{}
const Expr& BinaryExpr::lhs() const { return *m_lhs; }
const Expr& BinaryExpr::rhs() const { return *m_rhs; }

UnaryExpr::UnaryExpr(UnOp op, Expr expr)
	: m_expr(new Expr(std::move(expr)))
	, m_op(op)
{}
const Expr& UnaryExpr::expr() const { return *m_expr; }
Type UnaryExpr::type() const { return expr_type(*m_expr); }

PoolIndexExpr::PoolIndexExpr(const Pool& pool, Expr index, const Location& loc)
	: m_pool(pool)
	, m_index(new Expr(std::move(index)))
	, m_loc(loc)
{
}
const Expr& PoolIndexExpr::index() const { return *m_index; }
Type PoolIndexExpr::type() const { return from_pool_type(m_pool.type()); }

MethodCall::MethodCall(const Method& method, Expr this_expr, std::vector<Expr> args, Type type)
	: m_method(method)
	, m_this_expr(new Expr(std::move(this_expr)))
	, m_args(std::move(args))
	, m_type(std::move(type))
{}
const Expr& MethodCall::this_expr() const { return *m_this_expr; }
const Type& MethodCall::type() const { return m_type; }

FieldAccess::FieldAccess(Expr expr, const Field& field, Type type)
	: m_expr(new Expr(std::move(expr)))
	, m_field(field)
	, m_type(std::move(type))
{}
const Expr& FieldAccess::expr() const { return *m_expr; }

Assignment::Assignment(Expr lhs, Expr rhs)
	: m_lhs(std::unique_ptr<Expr>(new Expr(std::move(lhs))))
	, m_rhs(std::unique_ptr<Expr>(new Expr(std::move(rhs))))
{}

const Expr& Assignment::lhs() const { return *m_lhs; }
const Expr& Assignment::rhs() const { return *m_rhs; }

OpAssignment::OpAssignment(Expr lhs, BinOp op, Expr rhs)
	: m_lhs(std::move(lhs))
	, m_rhs(std::move(rhs))
	, m_op(op)
{}

If::If(Expr cond, std::vector<Stmt> then_branch, std::vector<Stmt> else_branch)
	: m_cond(std::move(cond))
	, m_then_branch(std::move(then_branch))
	, m_else_branch(std::move(else_branch))
{}

While::While(Expr cond, std::vector<Stmt> body)
	: m_cond(std::move(cond))
	, m_body(std::move(body))
{}

ForeachRange::ForeachRange(const Variable& var,
			 Expr range_begin,
			 Expr range_end,
			 std::vector<Stmt> body)
	: m_var(var)
	, m_range_begin(std::move(range_begin))
	, m_range_end(std::move(range_end))
	, m_body(std::move(body))
{}

ForeachPool::ForeachPool(const Variable& var, const Pool& pool, std::vector<Stmt> body)
	: m_var(var)
	, m_pool(pool)
	, m_body(std::move(body))
{}

void Layout::build_field_map()
{
	for (size_t i = 0; i < m_clusters.size(); i++) {
		const auto& fields = m_clusters[i].fields();
		for (size_t j = 0; j < fields.size(); j++) {
			FieldPos pos;
			pos.cluster_idx = i;
			pos.pos = j;

			m_field_map[fields[j]] = pos;
		}
	}
}

const Pool* Class::find_pool(const std::string& name) const
{
	auto it = m_pool_map.find(name);
	if (it == m_pool_map.end()) {
		return nullptr;
	}
	return &it->second;
}

Pool* Class::find_pool(const std::string& name)
{
	auto it = m_pool_map.find(name);
	if (it == m_pool_map.end()) {
		return nullptr;
	}
	return &it->second;
}

const Field* Class::find_field(const std::string& name) const
{
	auto it = m_field_map.find(name);
	if (it == m_field_map.end()) {
		return nullptr;
	}
	return &it->second;
}

Field* Class::find_field(const std::string& name)
{
	auto it = m_field_map.find(name);
	if (it == m_field_map.end()) {
		return nullptr;
	}
	return &it->second;
}

const Method* Class::find_method(const std::string& name) const
{
	auto it = m_method_map.find(name);
	if (it == m_method_map.end()) {
		return nullptr;
	}
	return &it->second;
}

Method* Class::find_method(const std::string& name)
{
	auto it = m_method_map.find(name);
	if (it == m_method_map.end()) {
		return nullptr;
	}
	return &it->second;
}

std::pair<Pool*, bool> Class::add_pool(std::string name, const Location& loc)
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

std::pair<Field*, bool> Class::add_field(std::string name, const Location& loc)
{
	auto name_copy = name;
	auto res = m_field_map.emplace(
		std::move(name_copy),
		Field(std::move(name), loc)
		);
	if (res.second) {
		m_field_indices[&res.first->second] = m_fields.size();
		m_fields.emplace_back(res.first->second);
	}
	return std::make_pair(&res.first->second, res.second);
}

std::pair<Method*, bool> Class::add_method(std::string name, const Location& loc)
{
	auto name_copy = name;
	auto res = m_method_map.emplace(
		std::move(name_copy), Method(std::move(name), loc));
	if (res.second) {
		m_methods.emplace_back(res.first->second);
	}
	return std::make_pair(&res.first->second, res.second);
}

size_t Class::index_of(const Field& field) const
{
	const auto it = m_field_indices.find(&field);
	if (it == m_field_indices.end()) {
		return -1;
	}

	return it->second;
}

const Class* Program::find_class(const std::string& name) const
{
	auto it = m_class_map.find(name);
	if (it == m_class_map.end()) {
		return nullptr;
	}
	return &it->second;
}

const Layout* Program::find_layout(const std::string& name) const
{
	auto it = m_layout_map.find(name);
	if (it == m_layout_map.end()) {
		return nullptr;
	}
	return &it->second;
}

Class* Program::find_class(const std::string& name)
{
	auto it = m_class_map.find(name);
	if (it == m_class_map.end()) {
		return nullptr;
	}
	return &it->second;
}

std::pair<Class*, bool> Program::add_class(std::string name, const Location& loc)
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

std::pair<Layout*, bool> Program::add_layout(
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

ObjectType Class::this_object_type() const {
	assert_msg(!m_pools.empty(), "Class has no pool parameters");

	std::vector<PoolParameter> params;
	for (const Pool& e: m_pools) {
		params.emplace_back(PoolRef(e));
	}

	const Pool& first = m_pools.front();
	return ObjectType(*this, std::move(params), first.loc());
}

BoundType Class::this_bound_type() const {
	assert_msg(!m_pools.empty(), "Class has no pool parameters");

	std::vector<PoolParameter> params;
	for (const Pool& e: m_pools) {
		params.emplace_back(PoolRef(e));
	}

	const Pool& first = m_pools.front();
	return BoundType(*this, std::move(params), first.loc());
}

class AstDebugger
{
	int indentation_level = 0;

	void indent() { indentation_level++; }

	void dedent() { indentation_level--; }

	void emit_indentation()
	{
		fprintf(stderr, "%*c", indentation_level * 2, ' ');
	}

public:
	void operator()(const Field& field)
	{
		emit_indentation();
		fprintf(stderr, "Field: %s (type: %s)\n",
			field.name().c_str(),
			to_string(field.type()).c_str());
	}

	void operator()(const Pool& pool)
	{
		emit_indentation();
		if (pool.has_type()) {
			fprintf(stderr, "Pool: %s (type: %s)\n",
				pool.name().c_str(), to_string(pool.type()).c_str());
		} else {
			fprintf(stderr, "Pool: %s (no type)\n", pool.name().c_str());
		}
	}

	void operator()(const InvalidExpr&)
	{
		emit_indentation();
		fprintf(stderr, "Invalid expression\n");
	}

	void operator()(const IntegerConst& expr)
	{
		emit_indentation();
		fprintf(stderr, "Integer constant: %" PRIu64 " (type: %s)\n",
				expr.value(), to_string(expr.type()).c_str());
	}

	void operator()(const DoubleConst& expr)
	{
		emit_indentation();
		fprintf(stderr, "Double constant: %.f\n", expr.value());
	}

	void operator()(const NullExpr&)
	{
		emit_indentation();
		fprintf(stderr, "Null expression\n");
	}

	void operator()(const ThisExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "This expression (type: %s)\n",
			to_string(expr.type()).c_str());
	}

	void operator()(const BooleanConst& expr)
	{
		emit_indentation();
		fprintf(stderr, "Boolean constant: %s\n",
			expr.value() ? "true" : "false");
	}

	void operator()(const CastExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "Cast expression into type: %s\n",
			to_string(expr.type()).c_str());

		emit_indentation();
		fprintf(stderr, "Expression:\n");

		indent();
		mpark::visit(*this, expr.expr());
		dedent();
	}

	void operator()(const UnaryExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "Unary expression (op: %s)\n",
			to_string(expr.type()).c_str());

		emit_indentation();
		fprintf(stderr, "Expression:\n");

		indent();
		mpark::visit(*this, expr.expr());
		dedent();
	}

	void operator()(const BinaryExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "Binary expression (op: %s)\n",
			to_string(expr.op()).c_str());

		emit_indentation();
		fprintf(stderr, "Lhs:\n");

		indent();
		mpark::visit(*this, expr.lhs());
		dedent();

		emit_indentation();
		fprintf(stderr, "Rhs:\n");

		indent();
		mpark::visit(*this, expr.rhs());
		dedent();
	}

	void operator()(const VariableExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "Variable expression (variable: %s)\n",
			expr.var().name().c_str());
	}

	void operator()(const PoolIndexExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "Pool indexing\n");

		emit_indentation();
		fprintf(stderr, "Pool: %s\n", expr.pool().name().c_str());

		emit_indentation();
		fprintf(stderr, "Index:\n");

		indent();
		mpark::visit(*this, expr.index());
		dedent();
	}

	void operator()(const MethodCall& expr)
	{
		emit_indentation();
		fprintf(stderr, "Method call: %s\n",
			expr.method().name().c_str());

		emit_indentation();
		fprintf(stderr, "This expression:\n");

		indent();
		mpark::visit(*this, expr.this_expr());
		dedent();

		emit_indentation();
		fprintf(stderr, "Arguments:\n");

		indent();
		for (const auto& e: expr.args()) {
			mpark::visit(*this, e);
		}
		dedent();
	}

	void operator()(const FieldAccess& expr)
	{
		emit_indentation();
		fprintf(stderr, "Field Access: %s\n",
			expr.field().name().c_str());

		emit_indentation();
		fprintf(stderr, "Inner expression:\n");

		indent();
		mpark::visit(*this, expr.expr());
		dedent();
	}

	void operator()(const NewExpr& expr)
	{
		emit_indentation();
		fprintf(stderr, "New expression (type: %s)\n",
			to_string(expr.type()).c_str());
	}

	void operator()(const Assignment& stmt)
	{
		emit_indentation();
		fprintf(stderr, "Assignment:\n");

		emit_indentation();
		fprintf(stderr, "Lhs:\n");
		indent();
		mpark::visit(*this, stmt.lhs());
		dedent();

		emit_indentation();
		fprintf(stderr, "Rhs:\n");
		indent();
		mpark::visit(*this, stmt.rhs());
		dedent();
	}

	void operator()(const OpAssignment& stmt)
	{
		emit_indentation();
		fprintf(stderr, "Operation assignment:\n");

		emit_indentation();
		fprintf(stderr, "Lhs:\n");
		indent();
		mpark::visit(*this, stmt.lhs());
		dedent();

		emit_indentation();
		fprintf(stderr, "Op: %s\n", to_string(stmt.op()).c_str());

		emit_indentation();
		fprintf(stderr, "Rhs:\n");
		indent();
		mpark::visit(*this, stmt.rhs());
		dedent();
	}

	void operator()(const If& stmt)
	{
		emit_indentation();
		fprintf(stderr, "If stmt. Cond:\n");

		indent();
		mpark::visit(*this, stmt.cond());
		dedent();

		emit_indentation();
		fprintf(stderr, "Then branch:\n");

		indent();
		for (const auto& e: stmt.then_stmts()) {
			mpark::visit(*this, e);
		}
		dedent();

		emit_indentation();
		fprintf(stderr, "Else branch:\n");

		indent();
		for (const auto& e: stmt.else_stmts()) {
			mpark::visit(*this, e);
		}
		dedent();
	}

	void operator()(const While& stmt)
	{
		emit_indentation();
		fprintf(stderr, "While stmt. Cond:\n");

		indent();
		mpark::visit(*this, stmt.cond());
		dedent();

		emit_indentation();
		fprintf(stderr, "Body:\n");

		indent();
		for (const auto& e: stmt.body()) {
			mpark::visit(*this, e);
		}
		dedent();
	}

	void operator()(const ForeachRange& stmt)
	{
		emit_indentation();
		fprintf(stderr, "Foreach range stmt. Variable: %s (type: %s)\n",
			stmt.var().name().c_str(), to_string(stmt.var().type()).c_str());

		emit_indentation();
		fprintf(stderr, "Begin:\n");
		indent();
		mpark::visit(*this, stmt.range_begin());
		dedent();

		emit_indentation();
		fprintf(stderr, "End:\n");
		indent();
		mpark::visit(*this, stmt.range_end());
		dedent();

		emit_indentation();
		fprintf(stderr, "Body:\n");

		indent();
		for (const auto& e: stmt.body()) {
			mpark::visit(*this, e);
		}
		dedent();
	}

	void operator()(const ForeachPool& stmt)
	{
		emit_indentation();
		fprintf(stderr, "Foreach pool stmt. Variable: %s (type: %s), Pool %s\n",
			stmt.var().name().c_str(),
			to_string(stmt.var().type()).c_str(),
			stmt.pool().name().c_str());

		emit_indentation();
		fprintf(stderr, "Body:\n");

		indent();
		for (const auto& e: stmt.body()) {
			mpark::visit(*this, e);
		}
		dedent();
	}

	void operator()(const ExprStmt& stmt)
	{
		emit_indentation();
		fprintf(stderr, "Expression stmt\n");

		indent();
		mpark::visit(*this, stmt.expr());
		dedent();
	}

	void operator()(const Break&)
	{
		emit_indentation();
		fprintf(stderr, "Break stmt\n");
	}

	void operator()(const Continue&)
	{
		emit_indentation();
		fprintf(stderr, "Continue stmt\n");
	}

	void operator()(const Return& stmt)
	{
		if (stmt.expr() != nullptr) {
			emit_indentation();
			fprintf(stderr, "Return stmt. Expr:\n");

			indent();
			mpark::visit(*this, *stmt.expr());
			dedent();
		} else {
			emit_indentation();
			fprintf(stderr, "Return stmt.\n");
		}
	}

	void operator()(const Method& method)
	{
		emit_indentation();
		fprintf(stderr, "Method: %s\n", method.name().c_str());

		emit_indentation();
		fprintf(stderr, "Parameters:\n");

		indent();
		for (const auto& e: method.params()) {
			emit_indentation();
			fprintf(stderr, "%s: %s\n", e.name().c_str(), to_string(e.type()).c_str());
		}
		dedent();

		emit_indentation();
		fprintf(stderr, "Return type: %s\n",
			to_string(method.return_type()).c_str());

		emit_indentation();
		fprintf(stderr, "Body:\n");

		indent();
		for (const auto& e: method.body()) {
			mpark::visit(*this, e);
		}
		dedent();
	}

	void operator()(const Class& clazz)
	{
		emit_indentation();
		fprintf(stderr, "Class: %s\n",
				clazz.name().c_str());

		indent();
		for (const auto& e: clazz.pools()) {
			(*this)(e);
		}

		for (const auto& e: clazz.fields()) {
			(*this)(e);
		}

		for (const auto& e: clazz.methods()) {
			(*this)(e);
		}

		dedent();
	}

	void operator()(const Cluster& cluster)
	{
		emit_indentation();
		fprintf(stderr, "Cluster:\n");

		indent();
		for (const auto& e: cluster.fields()) {
			(*this)(*e);
		}
		dedent();
	}

	void operator()(const Layout& layout)
	{
		emit_indentation();
		fprintf(stderr, "Layout: %s (class: %s)\n",
				layout.name().c_str(),
				layout.for_class().name().c_str());

		indent();
		for (const auto& e: layout.clusters()) {
			(*this)(e);
		}
		dedent();
	}

	void operator()(const Program& ast)
	{
		for (const Class& e: ast.ordered_classes()) {
			(*this)(e);
		}
		for (const Layout& e: ast.ordered_layouts()) {
			(*this)(e);
		}
	}
};

void debug_ast(const Program& ast)
{
	AstDebugger debugger;
	debugger(ast);
}

} // namespace Ast
