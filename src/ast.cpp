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
	case Tag::LAYOUT:
		new (&m_layout) LayoutType(std::move(other.m_layout));
		break;

	case Tag::BOUND:
		new (&m_bound) BoundType(std::move(other.m_bound));
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
	case Tag::PRIMITIVE:
		m_primitive_type.~PrimitiveType();
		break;

	case Tag::NULLPTR:
		m_null_type.~NullType();
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

	case Tag::NULLPTR:
		new (&m_null_type) NullType(std::move(other.m_null_type));
		break;

	case Tag::OBJECT:
		new (&m_object_type) ObjectType(std::move(other.m_object_type));
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
	case Tag::PRIMITIVE:
		return m_primitive_type.kind() == rhs.m_primitive_type.kind();

	case Tag::NULLPTR:
		return true;
		break;

	case Tag::OBJECT:
		return m_object_type == rhs.m_object_type;
		break;
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

}

#if 0

template<typename Iter>
void print_pool_params(std::ostringstream& os, Iter begin, Iter end)
{
	auto it = begin;
	if (it != end) {
		if (*it == nullptr) {
			os << "none";
		}
		else {
			os << (*it)->name();
		}
		it++;
	}
	for (; it != end; it++) {
		os << ", ";
		if (*it == nullptr) {
			os << "none";
		}
		else {
			os << (*it)->name();
		}
	}
}

std::string Ast::PrimitiveType::to_string() const
{
	switch (m_kind) {
	case Ast::PrimitiveType::Kind::BOOL:
		return "bool";
	case Ast::PrimitiveType::Kind::U8:
		return "u8";
	case Ast::PrimitiveType::Kind::U16:
		return "u16";
	case Ast::PrimitiveType::Kind::U32:
		return "u32";
	case Ast::PrimitiveType::Kind::U64:
		return "u64";
	case Ast::PrimitiveType::Kind::I8:
		return "i8";
	case Ast::PrimitiveType::Kind::I16:
		return "i16";
	case Ast::PrimitiveType::Kind::I32:
		return "i32";
	case Ast::PrimitiveType::Kind::I64:
		return "i64";
	case Ast::PrimitiveType::Kind::F32:
		return "f32";
	case Ast::PrimitiveType::Kind::F64:
		return "f64";
	}

	// Not reachable
	return std::string();
}

std::string Ast::ClassType::to_string() const
{
	std::ostringstream os;
	os << m_klass->name() << "<";
	print_pool_params(os, pool_params_begin(), pool_params_end());
	os << ">";

	return os.str();
}

std::string Ast::PoolType::to_string() const
{
	std::ostringstream os;
	os << m_layout->klass().name() << "<";
	print_pool_params(os, pool_params_begin(), pool_params_end());
	os << ">";

	return os.str();
}

std::string Ast::BoundType::to_string() const
{
	std::ostringstream os;
	os << m_klass->name() << "<";
	print_pool_params(os, pool_params_begin(), pool_params_end());
	os << ">";

	return os.str();
}

bool Ast::PoolType::operator==(const Ast::PoolType& oth) const
{
	if (&m_layout->klass() != &oth.m_layout->klass()) {
		return false;
	}

	if (m_pool_params.size() != oth.m_pool_params.size()) {
		return false;
	}

	return std::equal(pool_params_begin(), pool_params_end(),
					  oth.pool_params_begin());
}

bool Ast::BoundType::operator==(const Ast::BoundType& oth) const
{
	if (m_klass != oth.m_klass) {
		return false;
	}

	if (m_pool_params.size() != oth.m_pool_params.size()) {
		return false;
	}

	return std::equal(pool_params_begin(), pool_params_end(),
					  oth.pool_params_begin());
}

bool Ast::ClassType::operator==(const Ast::ClassType& oth) const
{
	if (m_klass != oth.m_klass) {
		return false;
	}

	if (m_pool_params.size() != oth.m_pool_params.size()) {
		return false;
	}

	return std::equal(pool_params_begin(), pool_params_end(),
					  oth.pool_params_begin());
}

template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
	return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

class SymbolTable
{
	struct SymbolTableEntry
	{
		std::vector<std::string> m_vars_declared;
		std::vector<std::string> m_pools_declared;
	};

	std::unordered_map<std::string, const Ast::Pool*> m_pools;
	std::unordered_map<std::string, std::vector<const Ast::Variable*>> m_vars;

	std::vector<SymbolTableEntry> m_entries;

public:
	void push_scope()
	{
		m_entries.emplace_back();
	}

	void pop_scope()
	{
		auto& top = m_entries.back();
		for (auto& e: top.m_pools_declared) {
			m_pools.erase(e);
		}

		for (auto& e: top.m_vars_declared) {
			m_vars[e].pop_back();
		}

		m_entries.pop_back();
	}

	bool add_pool(const std::string& name, const Ast::Pool* pool)
	{
		auto it = m_pools.emplace(name, pool);
		if (!it.second) {
			return false;
		}

		m_entries.back().m_pools_declared.push_back(name);
		return true;
	}

	void add_variable(const std::string& name, const Ast::Variable* var)
	{
		m_vars[name].emplace_back(var);
		m_entries.back().m_vars_declared.push_back(name);
	}

	const Ast::Pool* find_pool(const std::string& name) const
	{
		auto it = m_pools.find(name);
		if (it == m_pools.end()) {
			return nullptr;
		}

		return it->second;
	}

	const Ast::Variable* find_variable(const std::string& name) const
	{
		auto it = m_vars.find(name);
		if (it == m_vars.end()) {
			return nullptr;
		}

		return it->second.back();
	}
};

class TypeCollector: public Cst::DefaultVisitor
{
	const Ast::Program* m_ast;
	Ast::SemanticErrorList* m_errors;

	bool m_limited;
	Ast::TypeKind m_kind;

	bool m_success = false;
	std::unique_ptr<Ast::Type> m_type;

	const Ast::Class* m_class = nullptr;
	const SymbolTable* m_table;
	std::vector<const Ast::Pool*> m_pools;

	Ast::PrimitiveType::Kind to_kind(Cst::PrimitiveType::Kind kind)
	{
		switch (kind) {
		case Cst::PrimitiveType::Kind::BOOL:
			return Ast::PrimitiveType::Kind::BOOL;
		case Cst::PrimitiveType::Kind::U8:
			return Ast::PrimitiveType::Kind::U8;
		case Cst::PrimitiveType::Kind::U16:
			return Ast::PrimitiveType::Kind::U16;
		case Cst::PrimitiveType::Kind::U32:
			return Ast::PrimitiveType::Kind::U32;
		case Cst::PrimitiveType::Kind::U64:
			return Ast::PrimitiveType::Kind::U64;
		case Cst::PrimitiveType::Kind::I8:
			return Ast::PrimitiveType::Kind::I8;
		case Cst::PrimitiveType::Kind::I16:
			return Ast::PrimitiveType::Kind::I16;
		case Cst::PrimitiveType::Kind::I32:
			return Ast::PrimitiveType::Kind::I32;
		case Cst::PrimitiveType::Kind::I64:
			return Ast::PrimitiveType::Kind::I64;
		case Cst::PrimitiveType::Kind::F32:
			return Ast::PrimitiveType::Kind::F32;
		case Cst::PrimitiveType::Kind::F64:
			return Ast::PrimitiveType::Kind::F64;
		}

		// Dead code, but it silences gcc
		return Ast::PrimitiveType::Kind::U8;
	}

	template<typename Iter>
	bool construct_bound(const Cst::Identifier& name, Iter begin, Iter end)
	{
		if (m_class == nullptr) {
			m_errors->add(make_unique<Ast::MissingDefinition>(
					name.ident(),
					Ast::ErrorKind::CLASS,
					name.loc()));
			return false;
		}

		m_pools.clear();
		visitPtrIter(begin, end);

		if (m_pools.size() != m_class->num_pools()) {
			m_errors->add(make_unique<Ast::PoolParameterCountMismatch>(
					m_class->num_pools(),
					m_pools.size(),
					name.loc()));
			return false;
		}

		return true;
	}

public:
	TypeCollector(const Ast::Program* ast, Ast::SemanticErrorList* errors, const SymbolTable* table)
		: m_ast(ast)
		, m_errors(errors)
		, m_limited(false)
		, m_kind(Ast::TypeKind::PRIMITIVE)
		, m_table(table)
	{}

	TypeCollector(const Ast::Program* ast,
				  Ast::SemanticErrorList* errors,
				  const SymbolTable* table,
				  Ast::TypeKind kind)
		: m_ast(ast)
		, m_errors(errors)
		, m_limited(true)
		, m_kind(kind)
		, m_table(table)
	{}

	bool success() const { return m_success; }
	std::unique_ptr<Ast::Type> grab_type() { return std::move(m_type); }

	void visit(const Cst::PrimitiveType& type) override
	{
		if (m_limited && m_kind != Ast::TypeKind::PRIMITIVE)
		{
			m_errors->add(make_unique<Ast::UnexpectedTypeKind>(
					Ast::TypeKind::PRIMITIVE, Location())); // TODO: Line information
			return;
		}

		m_type = make_unique<Ast::PrimitiveType>(to_kind(type.kind()));
		m_success = true;
	}

	void visit(const Cst::ClassType& type) override
	{
		if (m_limited && m_kind == Ast::TypeKind::POOL) {
			auto* layout = m_ast->find_layout(type.class_name().ident());
			if (layout == nullptr) {
				m_errors->add(make_unique<Ast::UnexpectedTypeKind>(
						Ast::TypeKind::POOL, Location())); // TODO: Line information
				return;
			}
			m_class = &layout->klass();

			if (!construct_bound(type.class_name(), type.pool_params_begin(), type.pool_params_end())) {
				return;
			}
			m_type = make_unique<Ast::PoolType>(layout, std::move(m_pools));
			m_success = true;

			return;
		}

		if (m_limited && m_kind != Ast::TypeKind::CLASS) {
			m_errors->add(make_unique<Ast::UnexpectedTypeKind>(
					Ast::TypeKind::CLASS, Location())); // TODO: Line information
			return;
		}

		m_class = m_ast->find_class(type.class_name().ident());
		if (!construct_bound(type.class_name(), type.pool_params_begin(), type.pool_params_end())) {
			return;
		}
		m_type = make_unique<Ast::ClassType>(m_class, std::move(m_pools));
		m_success = true;
	}

	void visit(const Cst::BoundType& type) override
	{
		if (!m_limited || m_kind != Ast::TypeKind::BOUND)
		{
			m_errors->add(make_unique<Ast::UnexpectedTypeKind>(
					Ast::TypeKind::BOUND, Location())); // TODO: Line information
			return;
		}

		m_class = m_ast->find_class(type.class_name().ident());
		if (!construct_bound(type.class_name(), type.pool_params_begin(), type.pool_params_end())) {
			return;
		}

		m_type = make_unique<Ast::BoundType>(m_class, std::move(m_pools));
		m_success = true;
	}

	void visit(const Cst::Identifier& pool_param) override
	{
		auto* pool = m_table->find_pool(pool_param.ident());
		if (pool == nullptr) {
			m_errors->add(make_unique<Ast::MissingDefinition>(
					pool_param.ident(),
					Ast::ErrorKind::POOL,
					pool_param.loc()));
			m_pools.emplace_back(nullptr);
		}
		m_pools.emplace_back(pool);
	}

	void visit(const Cst::NoneParam&) override
	{
		m_pools.emplace_back(nullptr);
	}
};

struct ClassAndPoolsCollector: public Cst::DefaultVisitor
{
	std::unordered_map<std::string, Ast::Class> class_map;
	std::vector<const Ast::Class*> classes;

	std::unordered_map<std::string, Ast::Pool> pool_map;
	std::vector<const Ast::Pool*> pools;

	Ast::SemanticErrorList* errors;

	explicit ClassAndPoolsCollector(Ast::SemanticErrorList* errors)
		: errors(errors)
	{
		assert(errors != nullptr);
	}

	void visit(const Cst::Class& klass) override
	{
		const auto& name = klass.name();
		auto it = class_map.find(name.ident());
		if (it != class_map.end()) {
			errors->add(make_unique<Ast::DuplicateDefinition>(
					name.ident(),
					Ast::ErrorKind::CLASS,
					name.loc(),
					it->second.loc()));
			return;
		}

		if (klass.pool_params_begin() == klass.pool_params_end()) {
			errors->add(make_unique<Ast::NoPoolParameters>(name.loc()));
			return;
		}

		Ast::Class new_class(name.ident(), name.loc());
		for (auto param_it = klass.pool_params_begin(); param_it != klass.pool_params_end(); param_it++) {
			auto& pool_param = *param_it;
			auto it = pool_map.find(pool_param.ident());
			if (it != pool_map.end()) {
				errors->add(make_unique<Ast::DuplicateDefinition>(
						pool_param.ident(),
						Ast::ErrorKind::CLASS,
						pool_param.loc(),
						it->second.loc()));
				return;
			}

			auto new_it = pool_map.emplace(
				pool_param.ident(), Ast::Pool(pool_param.ident(), pool_param.loc()));
			pools.emplace_back(&new_it.first->second);
		}

		new_class.set_pools(std::move(pool_map), std::move(pools));
		pool_map.clear();
		pools.clear();

		auto new_it = class_map.emplace(name.ident(), std::move(new_class));
		classes.emplace_back(&new_it.first->second);
	}
};

struct ClassMembersBoundsCollector: public Cst::DefaultVisitor
{
	Ast::Program* ast;
	Ast::SemanticErrorList* errors;

	Ast::Class* curr_class = nullptr;
	SymbolTable* table = nullptr;

	std::unordered_map<std::string, Ast::Field> class_field_map;
	std::vector<const Ast::Field*> class_fields;

	std::unordered_map<std::string, Ast::Method> method_map;
	std::vector<const Ast::Method*> methods;

	explicit ClassMembersBoundsCollector(Ast::Program* ast, Ast::SemanticErrorList* errors)
		: ast(ast)
		, errors(errors)
	{}

	void visit(const Cst::Class& klass) override
	{
		curr_class = ast->find_class(klass.name().ident());

		SymbolTable table;
		table.push_scope();

		for (auto it = curr_class->pools_begin(); it != curr_class->pools_end(); it++) {
			table.add_pool((*it)->name(), *it);
		}

		visitIter(klass.pool_param_bounds_begin(), klass.pool_param_bounds_end());
		bool must_abort = false;
		for (auto it = curr_class->pools_begin(); it != curr_class->pools_end(); it++) {
			auto& pool = **it;
			if (!pool.type_init()) {
				errors->add(make_unique<Ast::MissingDefinition>(
						pool.name(),
						Ast::ErrorKind::TYPE,
						pool.loc()));
			}
			must_abort = true;
		}
		if (must_abort) {
			return;
		}

		auto& first_param = *curr_class->pools_begin();
		auto& first_param_type = static_cast<const Ast::BoundType&>(first_param->type());

		if (&first_param_type.klass() != curr_class) {
			errors->add(make_unique<Ast::ClassTypeMismatch>(
					first_param_type.klass().name(), curr_class->name(), first_param->loc()));
			return;
		}

		auto num_params = std::distance(curr_class->pools_begin(), curr_class->pools_end());
		auto num_formal = std::distance(first_param_type.pool_params_begin(), first_param_type.pool_params_end());
		if (num_params != num_formal) {
			errors->add(make_unique<Ast::PoolParameterCountMismatch>(
					num_formal, num_params, first_param->loc()));
			return;
		}

		auto mismatch = std::mismatch(
			curr_class->pools_begin(), curr_class->pools_end(),
			first_param_type.pool_params_begin());

		if (mismatch.first != curr_class->pools_end()) {
			errors->add(make_unique<Ast::PoolParameterMismatch>(
				(*mismatch.second)->name(), (*mismatch.first)->name(),
				(*mismatch.first)->loc()));
			return;
		}

		class_fields.clear();
		class_field_map.clear();
		visitIter(klass.fields_begin(), klass.fields_end());

		curr_class->set_fields(std::move(class_field_map), std::move(class_fields));

		methods.clear();
		method_map.clear();
		visitIter(klass.methods_begin(), klass.methods_end());
		curr_class->set_methods(std::move(method_map), std::move(methods));

		table.pop_scope();
	}

	void visit(const Cst::Variable& bound) override
	{
		const auto& name = bound.name();
		auto* pool = curr_class->find_pool(name.ident());
		if (pool == nullptr) {
			errors->add(make_unique<Ast::MissingDefinition>(
					name.ident(),
					Ast::ErrorKind::POOL,
					name.loc()));
			return;
		}

		if (pool->type_init()) {
			errors->add(make_unique<Ast::DuplicateDefinition>(
					name.ident(),
					Ast::ErrorKind::TYPE,
					name.loc(),
					pool->loc()));
			return;
		}

		TypeCollector type_collector(ast, errors, table, Ast::TypeKind::BOUND);
		bound.type().accept(type_collector);
		if (!type_collector.success()) {
			return;
		}

		auto type = type_collector.grab_type();
		auto* bound_cast = static_cast<const Ast::BoundType*>(type.get());

		if (pool != *bound_cast->pool_params_begin()) {
			errors->add(make_unique<Ast::FirstPoolParameterMismatch>(name.loc()));
			return;
		}

		for (auto it = bound_cast->pool_params_begin(); it != bound_cast->pool_params_end(); it++) {
			if (*it == nullptr) {
				errors->add(make_unique<Ast::ClassPoolParameterNoNone>(name.ident(), name.loc()));
				continue;
			}
		}

		pool->set_type(std::move(type));
	}

	void visit(const Cst::Field& field) override
	{
		auto name = field.name();
		auto existing_it = class_field_map.find(name.ident());
		if (existing_it != class_field_map.end()) {
			errors->add(make_unique<Ast::DuplicateDefinition>(
					name.ident(),
					Ast::ErrorKind::FIELD,
					name.loc(),
					existing_it->second.loc()));
			return;
		}

		TypeCollector type_collector(ast, errors, table);
		field.type().accept(type_collector);
		if (!type_collector.success()) {
			return;
		}

		auto it = class_field_map.emplace(
			name.ident(),
			Ast::Field(name.ident(), type_collector.grab_type(), name.loc()));
		class_fields.emplace_back(&it.first->second);
	}

	void visit(const Cst::Method& method) override
	{
		const auto& name = method.name();
		auto existing_it = method_map.find(name.ident());
		if (existing_it != method_map.end()) {
			errors->add(make_unique<Ast::DuplicateDefinition>(
					name.ident(),
					Ast::ErrorKind::METHOD,
					name.loc(),
					existing_it->second.loc()));
			return;
		}

		std::unique_ptr<Ast::Type> type;
		if (method.type() != nullptr) {
			TypeCollector type_collector(ast, errors, table);
			method.type()->accept(type_collector);
			type = type_collector.grab_type();
		}

		Ast::Method new_method(name.ident(), std::move(type), name.loc());
		std::unordered_map<std::string, Location> var_names;
		for (auto it = method.params_begin(); it != method.params_end(); it++) {
			const auto& var = *it;
			const auto& name = var.name();
			auto existing_it = var_names.find(name.ident());
			if (existing_it != var_names.end()) {
				errors->add(make_unique<Ast::DuplicateDefinition>(
						name.ident(),
						Ast::ErrorKind::VARIABLE,
						name.loc(),
						existing_it->second));
				continue;
			}

			TypeCollector type_collector(ast, errors, table);
			var.type().accept(type_collector);

			auto type = type_collector.success()
				? type_collector.grab_type()
				: std::unique_ptr<Ast::Type>();

			new_method.add_parameter(
				Ast::Variable(name.ident(), std::move(type), name.loc())
			);
			var_names.emplace(name.ident(), name.loc());
		}

		auto it = method_map.emplace(name.ident(), std::move(new_method));
		methods.push_back(&it.first->second);
	}
};

struct LayoutsCollector: public Cst::DefaultVisitor
{
	Ast::Program* ast;
	Ast::SemanticErrorList* errors;

	const Ast::Class* curr_class = nullptr;

	std::unordered_map<std::string, Ast::Layout> layout_map;
	std::vector<const Ast::Layout*> layouts;

	std::vector<Ast::Cluster> clusters;
	std::unordered_set<const Ast::Field*> field_set;

	LayoutsCollector(Ast::Program* ast, Ast::SemanticErrorList* errors)
		: ast(ast)
		, errors(errors)
	{}

	void visit(const Cst::Layout& layout) override
	{
		const auto& name = layout.name();
		const auto* existing_layout = ast->find_layout(name.ident());
		if (existing_layout != nullptr) {
			errors->add(make_unique<Ast::DuplicateDefinition>(
					name.ident(), Ast::ErrorKind::LAYOUT, name.loc(), existing_layout->loc()));
			return;
		}

		const auto* clashing_class = ast->find_class(name.ident());
		if (clashing_class != nullptr) {
			errors->add(make_unique<Ast::ClassLayoutNameClash>(
					name.ident(), clashing_class->loc(), name.loc()));
			return;
		}

		const auto& class_name = layout.for_class();
		const auto* klass = ast->find_class(class_name.ident());
		if (klass == nullptr) {
			errors->add(make_unique<Ast::MissingDefinition>(
					class_name.ident(), Ast::ErrorKind::CLASS, class_name.loc()));
			return;
		}

		curr_class = klass;
		clusters.clear();
		field_set.clear();
		visitIter(layout.clusters_begin(), layout.clusters_end());

		for (auto it = klass->fields_begin(); it != klass->fields_end(); it++) {
			if (field_set.find(*it) == field_set.end()) {
				errors->add(make_unique<Ast::MissingFieldInLayout>(
						(*it)->name(), (*it)->loc(), layout.name().loc()));
				return;
			}
		}

		auto it = layout_map.emplace(
			name.ident(),
			Ast::Layout(name.ident(), klass, std::move(clusters), name.loc()));

		layouts.emplace_back(&it.first->second);
	}

	void visit(const Cst::Cluster& cluster) override
	{
		std::vector<const Ast::Field*> fields;
		for (auto it = cluster.fields_begin(); it != cluster.fields_end(); it++) {
			const auto& name = *it;
			const auto* field = curr_class->find_field(name.ident());
			if (field == nullptr) {
				errors->add(make_unique<Ast::MissingDefinition>(
						name.ident(), Ast::ErrorKind::FIELD, name.loc()));
				continue;
			}

			bool already_added = field_set.insert(field).second;
			if (!already_added) {
				errors->add(make_unique<Ast::DuplicateFieldInLayout>(
						name.ident(), name.loc()));
				continue;
			}

			fields.emplace_back(field);
		}
		clusters.emplace_back(std::move(fields));
	}
};

bool validate_type(const Ast::Type& type, const Location& loc, Ast::SemanticErrorList* errors)
{
	const Ast::Class* klass;
	std::vector<const Ast::Pool*>::const_iterator begin;
	std::vector<const Ast::Pool*>::const_iterator end;

	switch (type.kind()) {
	case Ast::Type::Kind::PRIMITIVE:
	case Ast::Type::Kind::NULLPTR:
		return true;

	case Ast::Type::Kind::CLASS: {
		const auto& class_type = static_cast<const Ast::ClassType&>(type);
		klass = &class_type.klass();
		begin = class_type.pool_params_begin();
		end = class_type.pool_params_end();
		break;
	}

	case Ast::Type::Kind::BOUND: {
		const auto& bound_type = static_cast<const Ast::BoundType&>(type);
		klass = &bound_type.klass();
		begin = bound_type.pool_params_begin();
		end = bound_type.pool_params_end();
		break;
	}

	case Ast::Type::Kind::POOL: {
		const auto& pool_type = static_cast<const Ast::PoolType&>(type);
		klass = &pool_type.layout().klass();
		begin = pool_type.pool_params_begin();
		end = pool_type.pool_params_end();
		break;
	}
	}

	auto formal_begin = klass->pools_begin();
	auto formal_end = klass->pools_end();

	auto num_params = std::distance(begin, end);
	auto num_formal_params = std::distance(formal_begin, formal_end);

	if (num_params != num_formal_params) {
		errors->add(make_unique<Ast::PoolParameterCountMismatch>(
				num_formal_params, num_params, loc));
		return false;
	}

	std::unordered_map<const Ast::Pool*, const Ast::Pool*> map;
	for (auto formal_it = formal_begin, pool_it = begin;
			formal_it != formal_end; formal_it++, pool_it++) {
		map.emplace(*formal_it, *pool_it);
	}

	bool success = true;
	for (auto formal_it = formal_begin, pool_it = begin;
			formal_it != formal_end; formal_it++, pool_it++) {
		const auto* pool_pool = *pool_it;
		const auto* formal_pool = *formal_it;
		if (pool_pool == nullptr) {
			continue;
		}

		const Ast::Class* pool_class;
		std::vector<const Ast::Pool*>::const_iterator pool_begin;
		std::vector<const Ast::Pool*>::const_iterator pool_end;

		assert(pool_pool->type().kind() == Ast::Type::Kind::POOL
				|| pool_pool->type().kind() == Ast::Type::Kind::BOUND);

		if (pool_pool->type().kind() == Ast::Type::Kind::POOL) {
			const auto& pool_type = static_cast<const Ast::PoolType&>(pool_pool->type());
			pool_class = &pool_type.layout().klass();
			pool_begin = pool_type.pool_params_begin();
			pool_end = pool_type.pool_params_end();
		} else {
			const auto& bound_type = static_cast<const Ast::BoundType&>(pool_pool->type());
			pool_class = &bound_type.klass();
			pool_begin = bound_type.pool_params_begin();
			pool_end = bound_type.pool_params_end();
		}

		const Ast::Class* formal_class;
		std::vector<const Ast::Pool*>::const_iterator formal_begin;
		std::vector<const Ast::Pool*>::const_iterator formal_end;

		if (formal_pool->type().kind() == Ast::Type::Kind::POOL) {
			const auto& pool_type = static_cast<const Ast::PoolType&>(formal_pool->type());
			formal_class = &pool_type.layout().klass();
			formal_begin = pool_type.pool_params_begin();
			formal_end = pool_type.pool_params_end();
		} else {
			const auto& bound_type = static_cast<const Ast::BoundType&>(formal_pool->type());
			formal_class = &bound_type.klass();
			formal_begin = bound_type.pool_params_begin();
			formal_end = bound_type.pool_params_end();
		}

		if (formal_class != pool_class) {
			errors->add(make_unique<Ast::ClassTypeMismatch>(
				formal_class->name(), pool_class->name(), loc));
			success = false;
			continue;
		}

		for (auto pool_it = pool_begin, formal_it = formal_begin;
				pool_it != pool_end; pool_it++, formal_it++) {
			if (map[*formal_it] != *pool_it) {
				std::string got_name = (map[*formal_it] != nullptr)
					?  map[*formal_it]->name()
					: "none";
				errors->add(make_unique<Ast::PoolParameterMismatch>(
					(*pool_it)->name(), std::move(got_name), loc));
				success = false;
				continue;
			}
		}
	}

	return success;
}

class TypeAndMethodBodyVisitor: public Cst::DefaultVisitor
{
	struct BlockStack
	{
		std::vector<std::vector<std::unique_ptr<Ast::Stmt>>> blocks;

		void push_block()
		{
			blocks.emplace_back();
		}

		std::vector<std::unique_ptr<Ast::Stmt>> pop_block()
		{
			auto block = std::move(blocks.back());
			blocks.pop_back();

			return block;
		}

		void add_stmt(std::unique_ptr<Ast::Stmt> stmt)
		{
			blocks.back().emplace_back(std::move(stmt));
		}
	};

	Ast::Program* prog;
	Ast::SemanticErrorList* errors;

	Ast::Class* curr_class = nullptr;
	Ast::Method* curr_method = nullptr;

	SymbolTable* table = nullptr;

	BlockStack block_stack;
	size_t loop_nesting = 0;

	std::unique_ptr<Ast::Expr> result_expr;
	std::vector<Ast::Stmt> result_block;

public:
	explicit TypeAndMethodBodyVisitor(Ast::Program* prog, Ast::SemanticErrorList* errors)
		: prog(prog)
		, errors(errors)
	{}

	void visit(const Cst::Class& klass) override
	{
		SymbolTable symtab;
		table = &symtab;

		table->push_scope();

		curr_class = prog->find_class(klass.name().ident());
		for (auto it = curr_class->pools_begin(); it != curr_class->pools_end(); it++) {
			validate_type((*it)->type(), (*it)->loc(), errors);
			table->add_pool((*it)->name(), *it);
		}

		for (auto it = curr_class->fields_begin(); it != curr_class->fields_end(); it++) {
			validate_type((*it)->type(), (*it)->loc(), errors);
		}

		visitIter(klass.methods_begin(), klass.methods_end());
	}

	void visit(const Cst::Method& method) override
	{
		curr_method = curr_class->find_method(method.name().ident());
		table->push_scope();

		if (curr_method->return_type() != nullptr) {
			validate_type(*curr_method->return_type(), curr_method->loc(), errors);
		}

		for (auto param_it = curr_method->params_begin(); param_it != curr_method->params_end(); param_it++) {
			validate_type(param_it->type(), param_it->loc(), errors);
			table->add_variable(param_it->name(), &*param_it);
		}

		if (!errors->has_errors()) {
			method.body().accept(*this);
		}

		table->pop_scope();
	}

	void visit(const Cst::VariableDeclsStmt& stmt) override
	{
	}

	void visit(const Cst::AssignStmt& stmt) override
	{
	}

	void visit(const Cst::OpAssignStmt& stmt) override
	{
	}

	void visit(const Cst::IfStmt& stmt) override
	{
		stmt.cond().accept(*this);
		auto cond = std::move(result_expr);
		if (cond == nullptr) {
			cond = make_unique<Ast::BooleanConst>(true);
		}

		Ast::PrimitiveType bool_type(Ast::PrimitiveType::Kind::BOOL);
		if (cond->type() != bool_type) {
			errors->add(make_unique<Ast::UnexpectedType>(
					bool_type.to_string(), cond->type().to_string(), cond->loc()));
			cond = make_unique<Ast::BooleanConst>(true);
		}

		block_stack.push_block();
		stmt.then_branch().accept(*this);
		auto then_branch = block_stack.pop_block();

		block_stack.push_block();
		stmt.else_branch().accept(*this);
		auto else_branch = block_stack.pop_block();

		block_stack.add_stmt(make_unique<Ast::IfStmt>(
				std::move(cond),
				std::move(then_branch),
				std::move(else_branch)));
	}

	void visit(const Cst::WhileStmt& stmt) override
	{
		stmt.cond().accept(*this);
		auto cond = std::move(result_expr);
		if (cond == nullptr) {
			cond = make_unique<Ast::BooleanConst>(true);
		}

		Ast::PrimitiveType bool_type(Ast::PrimitiveType::Kind::BOOL);
		if (cond->type() != bool_type) {
			errors->add(make_unique<Ast::UnexpectedType>(
					bool_type.to_string(), cond->type().to_string(), cond->loc()));
			cond = make_unique<Ast::BooleanConst>(true);
		}

		block_stack.push_block();

		loop_nesting++;
		stmt.body().accept(*this);
		loop_nesting--;

		auto body = block_stack.pop_block();

		block_stack.add_stmt(make_unique<Ast::WhileStmt>(
				std::move(cond),
				std::move(body)));
	}

	void visit(const Cst::ForeachRangeStmt& stmt) override
	{
	}

	void visit(const Cst::ForeachPoolStmt& stmt) override
	{
	}

	void visit(const Cst::BlockStmt& stmt) override
	{
		table->push_scope();
		visitPtrIter(stmt.begin(), stmt.end());
		table->pop_scope();
	}

	void visit(const Cst::ExprStmt& stmt) override
	{
		stmt.expr().accept(*this);
		if (result_expr == nullptr) {
			return;
		}

		block_stack.add_stmt(make_unique<Ast::ExprStmt>(std::move(result_expr)));
	}

	void visit(const Cst::BreakStmt& stmt) override
	{
		if (loop_nesting == 0) {
			errors->add(make_unique<Ast::NotInsideLoop>(stmt.loc()));
			return;
		}
		block_stack.add_stmt(make_unique<Ast::BreakStmt>());
	}

	void visit(const Cst::ContinueStmt& stmt) override
	{
		if (loop_nesting == 0) {
			errors->add(make_unique<Ast::NotInsideLoop>(stmt.loc()));
			return;
		}
		block_stack.add_stmt(make_unique<Ast::ContinueStmt>());
	}

	void visit(const Cst::ReturnStmt& stmt) override
	{
		stmt.expr().accept(*this);
		if (result_expr == nullptr) {
			return;
		}

		if (curr_method->return_type() == nullptr) {
			errors->add(make_unique<Ast::NoReturnType>(
					curr_method->name(), Location()));
			return;
		}

		if (*curr_method->return_type() != result_expr->type()) {
			errors->add(make_unique<Ast::UnexpectedType>(
					*curr_method->return_type(), result_expr->type(), Location()));
			return;
		}
		block_stack.add_stmt(make_unique<Ast::ReturnStmt>(std::move(result_expr)));
	}

	void visit(const Cst::ReturnVoidStmt& stmt) override
	{
		if (curr_method->return_type() == nullptr) {
			errors->add(make_unique<Ast::ExpectedReturnType>(
					curr_method->name(), Location()));
			return;
		}
		block_stack.add_stmt(make_unique<Ast::ReturnStmt>());
	}

	void visit(const Cst::IntegerConst& expr) override
	{
	}

	void visit(const Cst::BooleanConst& expr) override
	{
		result_expr = make_unique<Ast::BooleanConst>(expr.value(), expr.loc());
	}

	void visit(const Cst::NullExpr& expr) override
	{
		result_expr = make_unique<Ast::NullExpr>(expr.loc());
	}

	void visit(const Cst::ThisExpr& expr) override
	{
		std::vector<const Ast::Pool*> params(curr_class->pools_begin(), curr_class->pools_end());
		Ast::ClassType type(curr_class, std::move(params));
		result_expr = make_unique<Ast::ThisExpr>(std::move(type), expr.loc());
	}

	void visit(const Cst::BinaryExpr& expr) override
	{
	}

	void visit(const Cst::UnaryExpr& expr) override
	{
	}

	void visit(const Cst::IndexExpr& expr) override
	{
	}

	void visit(const Cst::IdentifierExpr& expr) override
	{
	}

	void visit(const Cst::MethodCall& expr) override
	{
	}

	void visit(const Cst::MemberMethodCall& expr) override
	{
	}

	void visit(const Cst::FieldAccess& expr) override
	{
	}

	void visit(const Cst::NewExpr& expr) override
	{
	}
};

void Ast::run_semantic_analysis(const Cst::Program& cst, Ast::SemanticErrorList* errors, Ast::Program* ast)
{
	Ast::Program program;

	ClassAndPoolsCollector class_pools_collector(errors);
	cst.accept(class_pools_collector);
	if (errors->has_errors()) {
		return;
	}

	program.set_classes(
		std::move(class_pools_collector.class_map),
		std::move(class_pools_collector.classes));

	ClassMembersBoundsCollector members_bounds_collector(&program, errors);
	cst.accept(members_bounds_collector);
	if (errors->has_errors()) {
		return;
	}

	LayoutsCollector layouts_collector(&program, errors);
	cst.accept(layouts_collector);
	if (errors->has_errors()) {
		return;
	}

	program.set_layouts(
		std::move(layouts_collector.layout_map),
		std::move(layouts_collector.layouts));

	TypeAndMethodBodyVisitor body_visitor(&program, errors);
	cst.accept(body_visitor);
	if (errors->has_errors()) {
		return;
	}

	*ast = std::move(program);
}

#endif
