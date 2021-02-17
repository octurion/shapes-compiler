#include "ast.h"

#include <ostream>
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

bool PoolRef::operator==(const PoolRef& rhs) const {
	return this == &rhs;
}
bool PoolRef::operator!=(const PoolRef& rhs) const {
	return this != &rhs;
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
	mpark::visit([&os](const auto& e) { os << e; }, *it);

	for (; it != pool_params.end(); it++) {
		os << ", ";
		mpark::visit([&os](const auto& e) { os << e; }, *it);
	}

	return os << ">";
}

bool BoundType::operator==(const BoundType& rhs) const
{
	return &m_class == &rhs.m_class && m_params == rhs.m_params;
}
bool BoundType::operator!=(const BoundType& rhs) const
{
	return &m_class != &rhs.m_class || !(m_params == rhs.m_params);
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
	return &m_layout == &rhs.m_layout && m_params == rhs.m_params;
}
bool LayoutType::operator!=(const LayoutType& rhs) const
{
	return &m_layout != &rhs.m_layout || !(m_params == rhs.m_params);
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
	return os << "nullptr_t";
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
	return &m_class == &rhs.m_class && m_params == rhs.m_params;
}
bool ObjectType::operator!=(const ObjectType& rhs) const {
	return &m_class != &rhs.m_class || !(m_params == rhs.m_params);
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
ObjectType ObjectType::remap_formal_pool_params(const ObjectType& type_with_new_pools) const
{
	std::unordered_map<const Pool*, PoolParameter> mapping;
	const auto& new_pools = type_with_new_pools.params();
	const auto& old_pools = type_with_new_pools.of_class().pools();

	for (size_t i = 0; i < new_pools.size(); i++) {
		const Pool& old = old_pools[i];
		mapping.emplace(&old, new_pools[i]);
	}

	std::vector<PoolParameter> new_params;
	for (const auto& e: m_params) {
		const auto* as_pool = mpark::get_if<PoolRef>(&e);
		if (as_pool == nullptr) {
			new_params.emplace_back(None());
			continue;
		}

		const auto it = mapping.find(&as_pool->pool());
		assert_msg(it != mapping.end(), "Spotted non-formal pool parameter");
		new_params.push_back(it->second);
	}

	return ObjectType(*m_class, std::move(new_params), m_loc);
}
Type ObjectType::remap_formal_pool_params(const Type& type_with_new_pools) const
{
	const auto* as_obj_type = mpark::get_if<ObjectType>(&type_with_new_pools);
	if (as_obj_type != nullptr) {
		return remap_formal_pool_params(*as_obj_type);
	}

	return type_with_new_pools;
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

IndexExpr::IndexExpr(const Pool& pool, Expr idx)
	: m_pool(pool)
	, m_idx(new Expr(std::move(idx)))
{}
const Expr& IndexExpr::idx() const { return *m_idx; }

ObjectType IndexExpr::type() const {
	return from_pool_type(m_pool.type());
}

MethodCall::MethodCall(const Method& method, Expr this_expr, std::vector<Expr> args, Type type)
	: m_method(method)
	, m_this_expr(new Expr(std::move(this_expr)))
	, m_args(std::move(args))
	, m_type(std::move(type))
{}
const Expr& MethodCall::this_expr() const { return *m_this_expr; }
bool MethodCall::is_lvalue() const {
	return mpark::holds_alternative<ObjectType>(m_type);
}
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

			m_field_map[fields[i]] = pos;
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

} // namespace Ast
