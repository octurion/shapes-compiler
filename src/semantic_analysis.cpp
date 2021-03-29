#include "semantic_analysis.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <sstream>
#include <unordered_set>

namespace Ast {

struct IntegerLiteralInfo
{
	const char* suffix;
	PrimitiveType type;
	uint64_t max_value;
};

static const IntegerLiteralInfo INT_LITERAL_INFO[] = {
	{"i8", PrimitiveType::I8, INT8_MAX},
	{"u8", PrimitiveType::U8, UINT8_MAX},
	{"i16", PrimitiveType::I16, INT16_MAX},
	{"u16", PrimitiveType::U16, UINT16_MAX},
	{"i32", PrimitiveType::I32, INT32_MAX},
	{"u32", PrimitiveType::U32, UINT32_MAX},
	{"i64", PrimitiveType::I64, INT64_MAX},
	{"u64", PrimitiveType::U64, UINT64_MAX},
	{"", PrimitiveType::I32, INT32_MAX},
};

static PrimitiveType get_primitive_type(Cst::PrimitiveType type)
{
	switch (type.kind()) {
	case Cst::PrimitiveKind::BOOL:
		return PrimitiveType::BOOL;
	case Cst::PrimitiveKind::U8:
		return PrimitiveType::U8;
	case Cst::PrimitiveKind::U16:
		return PrimitiveType::U16;
	case Cst::PrimitiveKind::U32:
		return PrimitiveType::U32;
	case Cst::PrimitiveKind::U64:
		return PrimitiveType::U64;
	case Cst::PrimitiveKind::I8:
		return PrimitiveType::I8;
	case Cst::PrimitiveKind::I16:
		return PrimitiveType::I16;
	case Cst::PrimitiveKind::I32:
		return PrimitiveType::I32;
	case Cst::PrimitiveKind::I64:
		return PrimitiveType::I64;
	case Cst::PrimitiveKind::F32:
		return PrimitiveType::F32;
	case Cst::PrimitiveKind::F64:
		return PrimitiveType::F64;
	}

	unreachable("Unknown primitive type?");
}

static BinOp to_ast_binop(Cst::BinOp op) {
	switch (op) {
	case Cst::BinOp::PLUS:
		return BinOp::PLUS;
	case Cst::BinOp::MINUS:
		return BinOp::MINUS;
	case Cst::BinOp::TIMES:
		return BinOp::TIMES;
	case Cst::BinOp::DIV:
		return BinOp::DIV;
	case Cst::BinOp::LAND:
		return BinOp::LAND;
	case Cst::BinOp::LOR:
		return BinOp::LOR;
	case Cst::BinOp::AND:
		return BinOp::AND;
	case Cst::BinOp::OR:
		return BinOp::OR;
	case Cst::BinOp::XOR:
		return BinOp::XOR;
	case Cst::BinOp::SHL:
		return BinOp::SHL;
	case Cst::BinOp::SHR:
		return BinOp::SHR;
	case Cst::BinOp::EQ:
		return BinOp::EQ;
	case Cst::BinOp::NE:
		return BinOp::NE;
	case Cst::BinOp::LT:
		return BinOp::LT;
	case Cst::BinOp::LE:
		return BinOp::LE;
	case Cst::BinOp::GT:
		return BinOp::GT;
	case Cst::BinOp::GE:
		return BinOp::GE;
	}

	unreachable("Unknown binary operator?");
}

static UnOp to_ast_unop(Cst::UnOp op) {
	switch (op) {
	case Cst::UnOp::PLUS:
		return UnOp::PLUS;
	case Cst::UnOp::MINUS:
		return UnOp::MINUS;
	case Cst::UnOp::NOT:
		return UnOp::NOT;
	}

	unreachable("Unknown unary operator?");
}

template<typename T>
static std::string to_string(const T& value) {
	std::ostringstream os;
	os << value;
	return os.str();
}

static bool invalid(const Expr& expr)
{
	return mpark::holds_alternative<InvalidExpr>(expr);
}

static void collect_classes_fields_pool_params(
	const Cst::Program& cst, Program& ast, SemanticErrorList& errors)
{
	for (const auto& clazz: cst.classes()) {
		const auto& name = clazz.name();
		auto res = ast.add_class(name.ident(), name.loc());

		auto* new_class = res.first;
		bool inserted = res.second;
		if (!inserted) {
			errors.add<DuplicateDefinition>(
				name.ident(),
				ErrorKind::CLASS,
				name.loc(),
				new_class->loc()
			);
			continue;
		}

		for (const auto& pool_param: clazz.pool_params()) {
			auto res = new_class->add_pool(
				pool_param.ident(), pool_param.loc());
			auto* new_param = res.first;
			bool inserted = res.second;

			if (!inserted) {
				errors.add<DuplicateDefinition>(
					pool_param.ident(),
					ErrorKind::POOL,
					pool_param.loc(),
					new_param->loc());
			}
		}
		if (new_class->num_pools() == 0) {
			errors.add<NoPoolParameters>(
				clazz.name().ident(), clazz.name().loc());
		}

		for (const auto& field: clazz.fields()) {
			const auto& name = field.name();
			auto res = new_class->add_field(name.ident(), name.loc());
			auto* new_field = res.first;
			bool inserted = res.second;

			if (!inserted) {
				errors.add<DuplicateDefinition>(
					name.ident(),
					ErrorKind::FIELD,
					name.loc(),
					new_field->loc());
			}
		}
	}
}

static void collect_layouts(
	const Cst::Program& cst, Program& ast, SemanticErrorList& errors)
{
	for (const auto& layout: cst.layouts()) {
		const auto& name = layout.name();
		const auto* same_name_class = ast.find_class(name.ident());
		if (same_name_class != nullptr) {
			errors.add<LayoutNameClash>(
				name.ident(),
				name.loc(),
				same_name_class->loc());
			continue;
		}

		const auto& for_class_name = layout.for_class();
		auto* for_class = ast.find_class(for_class_name.ident());
		if (for_class == nullptr) {
			errors.add<MissingDefinition>(
				for_class_name.ident(),
				ErrorKind::CLASS,
				for_class_name.loc());
			continue;
		}

		auto res = ast.add_layout(name.ident(), *for_class, name.loc());
		auto* new_layout = res.first;
		bool inserted = res.second;
		if (!inserted) {
			errors.add<DuplicateDefinition>(
				name.ident(),
				ErrorKind::LAYOUT,
				name.loc(),
				new_layout->loc());
			continue;
		}

		std::unordered_map<const Field*, const Location&> fields_added;
		for (const auto& cluster: layout.clusters()) {
			if (cluster.fields().empty()) {
				errors.add<EmptyCluster>(cluster.loc());
				continue;
			}

			auto& new_cluster = new_layout->add_cluster(cluster.loc());
			for (const auto& field: cluster.fields()) {
				const auto* class_field = for_class->find_field(field.ident());
				if (class_field == nullptr) {
					errors.add<MissingDefinition>(
						field.ident(), ErrorKind::FIELD, field.loc());
					continue;
				}

				auto it = fields_added.find(class_field);
				if (it != fields_added.end()) {
					errors.add<DuplicateDefinition>(
						field.ident(),
						ErrorKind::LAYOUT,
						field.loc(),
						it->second);
					continue;
				}

				new_cluster.add_field(*class_field);
				fields_added.emplace(class_field, field.loc());
			}
		}

		bool fields_missing = false;
		for (const Field& e: for_class->fields())
		{
			if (fields_added.find(&e) == fields_added.end()) {
				errors.add<LayoutMissingField>(layout.name().ident(), e.name(), e.loc());
				fields_missing = true;
			}
		}
		if (fields_missing) {
			continue;
		}

		new_layout->build_field_map();
		for_class->add_layout(*new_layout);
	}
}

class ScopeStack
{
	struct Entry
	{
		std::unordered_map<std::string, const Variable*> m_vars;
		std::unordered_map<std::string, Pool*> m_pools;
	};

	std::vector<Entry> m_entries;

	Class* m_class = nullptr;

public:
	ScopeStack(Class& class_scope)
		: m_class(&class_scope)
	{
		push_scope();
		for (Pool& pool: class_scope.pools()) {
			add_pool(pool.name(), pool);
		}
	}

	ScopeStack(Class& class_scope, Method& method_scope)
		: m_class(&class_scope)
	{
		push_scope();
		for (Pool& pool: class_scope.pools()) {
			add_pool(pool.name(), pool);
		}

		for (const Variable& var: method_scope.params()) {
			add_variable(var.name(), var);
		}
	}

	void push_scope()
	{
		m_entries.emplace_back();
	}

	void pop_scope()
	{
		assert_msg(!m_entries.empty(), "No entry scope");
		m_entries.pop_back();
	}

	bool add_pool(const std::string& name, Pool& pool)
	{
		if (find_pool(name) != nullptr) {
			return false;
		}

		assert_msg(!m_entries.empty(), "No entry scope");
		m_entries.back().m_pools.emplace(name, &pool);

		return true;
	}

	void add_variable(const std::string& name, const Variable& var)
	{
		assert_msg(!m_entries.empty(), "No entry scope");
		auto res = m_entries.back().m_vars.emplace(name, &var);
		if (!res.second) {
			res.first->second = &var;
		}
	}

	const Pool* find_pool(const std::string& name) const
	{
		for (auto it = m_entries.rbegin(); it != m_entries.rend(); it++) {
			auto pool_it = it->m_pools.find(name);
			if (pool_it != it->m_pools.end()) {
				return pool_it->second;
			}
		}

		return nullptr;
	}

	Pool* find_pool(const std::string& name)
	{
		for (auto it = m_entries.rbegin(); it != m_entries.rend(); it++) {
			auto pool_it = it->m_pools.find(name);
			if (pool_it != it->m_pools.end()) {
				return pool_it->second;
			}
		}

		return nullptr;
	}

	const Variable* find_variable(const std::string& name) const
	{
		for (auto it = m_entries.rbegin(); it != m_entries.rend(); it++) {
			auto var_it = it->m_vars.find(name);
			if (var_it != it->m_vars.end()) {
				return var_it->second;
			}
		}

		return nullptr;
	}

	const Field* find_field(const std::string& name) const {
		return m_class->find_field(name);
	}
};

class TypeConstructor {
	const Program& m_ast;
	const ScopeStack& m_stack;
	SemanticErrorList& m_errors;

	std::vector<PoolParameter> extract_params(
		const std::vector<Cst::Pool>& params)
	{
		std::vector<PoolParameter> ast_params;
		for (const auto& e: params) {
			const auto* found_pool = m_stack.find_pool(e.ident());
			if (found_pool == nullptr) {
				m_errors.add<MissingDefinition>(
					e.ident(), ErrorKind::POOL, e.loc());
				return std::vector<PoolParameter>();
			}
			ast_params.emplace_back(PoolRef(*found_pool));
		}

		return ast_params;
	}

	std::vector<PoolParameter> extract_params(
		const std::vector<Cst::PoolParameter>& params)
	{
		std::vector<PoolParameter> ast_params;
		for (const auto& e: params) {
			const auto* none = mpark::get_if<Cst::None>(&e);
			if (none != nullptr) {
				ast_params.emplace_back(None());
				continue;
			}

			const auto* pool = mpark::get_if<Cst::Pool>(&e);
			if (pool != nullptr) {
				const auto* found_pool = m_stack.find_pool(pool->ident());
				if (found_pool == nullptr) {
					m_errors.add<MissingDefinition>(
						pool->ident(), ErrorKind::POOL, pool->loc());
					return std::vector<PoolParameter>();
				}
				ast_params.emplace_back(PoolRef(*found_pool));
			}
		}

		return ast_params;
	}

public:
	TypeConstructor(const Program& ast, const ScopeStack& stack, SemanticErrorList& errors)
		: m_ast(ast)
		, m_stack(stack)
		, m_errors(errors)
	{}

	mpark::variant<mpark::monostate, ObjectType, PrimitiveType>
	operator()(const Cst::ObjectType& type) {
		const auto* of_class = m_ast.find_class(type.class_name().ident());
		if (of_class == nullptr) {
			m_errors.add<MissingDefinition>(
				type.class_name().ident(),
				ErrorKind::CLASS,
				type.class_name().loc());
			return mpark::monostate();
		}

		if (type.params().empty()) {
			m_errors.add<NoPoolParameters>(to_string(type), type.loc());
			return mpark::monostate();
		}

		auto params = extract_params(type.params());
		if (params.empty()) {
			return mpark::monostate();
		}

		return ObjectType(*of_class, std::move(params), type.loc());
	}

	mpark::variant<mpark::monostate, ObjectType, PrimitiveType>
	operator()(const Cst::PrimitiveType& type) {
		return get_primitive_type(type);
	}

	mpark::variant<mpark::monostate, LayoutType, BoundType>
	operator()(const Cst::BoundType& type) {
		const auto* of_class = m_ast.find_class(type.class_name().ident());
		if (of_class == nullptr) {
			m_errors.add<MissingDefinition>(
				type.class_name().ident(),
				ErrorKind::CLASS,
				type.class_name().loc());
			return mpark::monostate();
		}

		if (type.params().empty()) {
			m_errors.add<NoPoolParameters>(to_string(type), type.loc());
			return mpark::monostate();
		}

		auto params = extract_params(type.params());
		if (params.empty()) {
			return mpark::monostate();
		}

		return BoundType(*of_class, std::move(params), type.loc());
	}

	mpark::variant<mpark::monostate, LayoutType, BoundType>
	operator()(const Cst::LayoutType& type) {
		const auto* layout = m_ast.find_layout(type.layout_name().ident());
		if (layout == nullptr) {
			m_errors.add<MissingDefinition>(
				type.layout_name().ident(),
				ErrorKind::LAYOUT,
				type.layout_name().loc());
			return mpark::monostate();
		}

		if (type.params().empty()) {
			m_errors.add<NoPoolParameters>(to_string(type), type.loc());
			return mpark::monostate();
		}

		auto params = extract_params(type.params());
		if (params.empty()) {
			return mpark::monostate();
		}

		return LayoutType(*layout, std::move(params), type.loc());
	}
};

struct PoolTypeExtractor {
	PoolType operator()(BoundType type) { return PoolType(std::move(type)); }
	PoolType operator()(LayoutType type) { return PoolType(std::move(type)); }
	PoolType operator()(mpark::monostate) {
		unreachable("Please check if no pool type was extracted");
	}
};
static PoolType get_type(mpark::variant<mpark::monostate, LayoutType, BoundType> type) {
	return mpark::visit(PoolTypeExtractor(), std::move(type));
}

struct TypeExtractor {
	Type operator()(ObjectType type) { return Type(std::move(type)); }
	Type operator()(PrimitiveType type) { return Type(type); }
	Type operator()(mpark::monostate) {
		unreachable("Please check if no object type was extracted");
	}
};
static Type get_type(mpark::variant<mpark::monostate, ObjectType, PrimitiveType> type) {
	return mpark::visit(TypeExtractor(), std::move(type));
}

static void collect_pool_field_method_types(
	const Cst::Program& cst, Program& ast, SemanticErrorList& errors)
{
	for (const auto& cst_class: cst.classes()) {
		auto* clazz = ast.find_class(cst_class.name().ident());
		assert_msg(clazz != nullptr, "Class not added?");

		ScopeStack scope(*clazz);
		TypeConstructor type_ctor(ast, scope, errors);

		std::unordered_map<const Pool*, Location> defined_pool_types;
		for (const auto& cst_bound: cst_class.pool_param_bounds()) {
			auto* bound = clazz->find_pool(cst_bound.pool().ident());
			if (bound == nullptr) {
				errors.add<MissingDefinition>(
					cst_bound.pool().ident(),
					ErrorKind::POOL_BOUND,
					cst_bound.pool().loc());
				continue;
			}

			if (bound->has_type()) {
				errors.add<DuplicateDefinition>(
					cst_bound.pool().ident(),
					ErrorKind::LAYOUT,
					cst_bound.pool().loc(),
					defined_pool_types[bound]);
				continue;
			}

			auto maybe_bound_type = type_ctor(cst_bound.type());
			if (mpark::holds_alternative<mpark::monostate>(maybe_bound_type)) {
				continue;
			}
			bound->set_type(get_type(maybe_bound_type));
			defined_pool_types[bound] = cst_bound.pool().loc();
		}

		for (const auto& cst_field: cst_class.fields()) {
			auto* field = clazz->find_field(cst_field.name().ident());
			assert_msg(field != nullptr, "Field should always exist");

			auto maybe_type = mpark::visit(type_ctor, cst_field.type());
			if (mpark::holds_alternative<mpark::monostate>(maybe_type)) {
				continue;
			}
			field->set_type(get_type(maybe_type));
		}

		for (const auto& cst_method: cst_class.methods()) {
			const auto& method_name = cst_method.name();
			auto res = clazz->add_method(method_name.ident(), method_name.loc());
			auto* new_method = res.first;
			bool inserted = res.second;

			if (!inserted) {
				errors.add<DuplicateDefinition>(
					method_name.ident(),
					ErrorKind::METHOD,
					method_name.loc(),
					new_method->loc());
				continue;
			}

			const auto* cst_return_type = cst_method.type();
			if (cst_return_type != nullptr) {
				auto maybe_type = mpark::visit(type_ctor, *cst_return_type);
				if (!mpark::holds_alternative<mpark::monostate>(maybe_type)) {
					new_method->set_return_type(get_type(maybe_type));
				}
			} else {
				new_method->set_return_type(VoidType());
			}

			std::unordered_map<std::string, Location> param_locs;
			std::deque<Variable> params;
			for (const auto& e: cst_method.params()) {
				auto maybe_type = mpark::visit(type_ctor, e.type());
				if (mpark::holds_alternative<mpark::monostate>(maybe_type)) {
					continue;
				}
				auto type = get_type(std::move(maybe_type));

				auto res = param_locs.emplace(e.name().ident(), e.name().loc());
				auto it = res.first;
				bool inserted = res.second;

				if (!inserted) {
					errors.add<DuplicateDefinition>(
						e.name().ident(),
						ErrorKind::VARIABLE,
						e.name().loc(),
						it->second);
					continue;
				}
				params.emplace_back(e.name().ident(), std::move(type), e.name().loc());
			}

			new_method->set_params(std::move(params));
		}
	}
}

class TypeValidator
{
	SemanticErrorList& m_errors;

	template<typename T>
	bool visit(const T& type) {
		bool success = true;
		const auto& params = type.params();
		for (size_t i = 0; i < params.size(); i++) {
			auto given_bound = mpark::visit(*this, params[i]);
			auto new_bound = type.bound_of_param(i);
			if (!compatible_with_bound(given_bound, new_bound)) {
				m_errors.add<IncompatibleBound>(
					type.loc(), to_string(given_bound), to_string(new_bound));
				success = false;
			}
		}

		return success;
	}

public:
	explicit TypeValidator(SemanticErrorList& errors)
		: m_errors(errors)
	{}

	bool operator()(const NoneType&) {
		unreachable("Must not validate a None type");
	}

	bool operator()(const ObjectType& type) {
		return visit(type);
	}

	bool operator()(const PrimitiveType&) {
		return true;
	}

	bool operator()(const NullptrType&) {
		return true;
	}

	bool operator()(const VoidType&) {
		return true;
	}

	bool operator()(const BoundType& type) {
		return visit(type);
	}

	bool operator()(const LayoutType& type) {
		return visit(type);
	}

	PoolType operator()(const None&) {
		return NoneType();
	}

	PoolType operator()(const PoolRef& e) {
		return e.pool().type();
	}
};

static bool validate_type(const Type& type, SemanticErrorList& errors) {
	return mpark::visit(TypeValidator(errors), type);
}

static bool validate_type(const PoolType& type, SemanticErrorList& errors) {
	return mpark::visit(TypeValidator(errors), type);
}

static void validate_top_level_types(const Program& ast, SemanticErrorList& errors) {
	bool pool_types_valid = true;
	for (const Class& clazz: ast.ordered_classes()) {
		for (const Pool& pool: clazz.pools()) {
			if (!first_pool_param_is(pool.type(), pool)) {
				pool_types_valid = false;
				continue;
			}
			if (!validate_type(pool.type(), errors)) {
				pool_types_valid = false;
			}
		}

		const Pool& first_pool = clazz.pools().front();
		if (!compatible_with_bound(first_pool.type(), clazz.this_bound_type())) {
			pool_types_valid = false;
		}
	}
	if (!pool_types_valid) {
		return;
	}

	for (const auto& clazz: ast.ordered_classes()) {
		for (const auto& field: clazz.get().fields()) {
			validate_type(field.get().type(), errors);
		}
		for (const auto& method: clazz.get().methods()) {
			validate_type(method.get().return_type(), errors);
			for (const auto& param: method.get().params()) {
				validate_type(param.type(), errors);
			}
		}
	}
}

class BlockStack
{
	std::vector<std::vector<Stmt>> blocks;

public:
	void push_block()
	{
		blocks.emplace_back();
	}

	std::vector<Stmt> pop_block()
	{
		auto block = std::move(blocks.back());
		blocks.pop_back();

		return block;
	}

	template<typename StmtType, typename... Args>
	void add(Args&&... args)
	{
		blocks.back().emplace_back(StmtType(std::forward<Args>(args)...));
	}

	template<typename Iter>
	void add(Iter begin, Iter end)
	{
		for (auto it = begin; it != end; it++)
		{
			blocks.back().emplace_back(std::move(*it));
		}
	}
};

class MethodBodyCollector
{
	Program& m_ast;

	Class& m_class;
	Method& m_method;

	SemanticErrorList& m_errors;

	ScopeStack m_scopes;
	BlockStack m_blocks;

	unsigned m_loop_nesting_count = 0;

public:
	MethodBodyCollector(Program& ast, Class& clazz, Method& method, SemanticErrorList& errors)
		: m_ast(ast)
		, m_class(clazz)
		, m_method(method)
		, m_errors(errors)
		, m_scopes(m_class, m_method)
	{
		for (const auto& var: method.vars()) {
			m_scopes.add_variable(var.name(), var);
		}
		m_blocks.push_block();
	}

	void operator()(const Cst::Block& block) {
		m_scopes.push_scope();
		for (const auto& e: block.stmts()) {
			mpark::visit(*this, e);
		}
		m_scopes.pop_scope();
	}

	void operator()(const Cst::Noop&) {}

	void operator()(const Cst::VariableDeclarations& decls)
	{
		for (const auto& e: decls.vars()) {
			TypeConstructor type_ctor(m_ast, m_scopes, m_errors);
			auto maybe_type = mpark::visit(type_ctor, e.type());
			if (mpark::holds_alternative<mpark::monostate>(maybe_type)) {
				continue;
			}
			auto type = get_type(std::move(maybe_type));
			if (!validate_type(type, m_errors)) {
				continue;
			}
			const auto& new_var = m_method.add_variable(
				e.name().ident(), std::move(type), e.name().loc());
			m_scopes.add_variable(e.name().ident(), new_var);
		}
	}

	void operator()(const Cst::PoolDeclarations& decls)
	{
		std::vector<Pool*> pools;

		bool typechecks = true;
		// Set up a new scope so that we can tear it down in case of an error
		m_scopes.push_scope();
		// Pass 1: Get all pool names
		for (const auto& e: decls.pools()) {
			auto& new_pool = m_method.add_pool(e.name().ident(), e.name().loc());
			pools.push_back(&new_pool);
			if (!m_scopes.add_pool(new_pool.name(), new_pool)) {
				const auto* existing_pool = m_scopes.find_pool(new_pool.name());

				m_errors.add<DuplicateDefinition>(
					e.name().ident(),
					ErrorKind::POOL,
					e.name().loc(),
					existing_pool->loc());
				typechecks = false;
				continue;
			}
		}
		if (!typechecks) {
			m_scopes.pop_scope();
			return;
		}

		// Pass 2: Get all pool types
		TypeConstructor type_ctor(m_ast, m_scopes, m_errors);
		for (size_t i = 0; i < pools.size(); i++) {
			auto* pool = pools[i];
			auto maybe_type = type_ctor(decls.pools()[i].type());
			if (mpark::holds_alternative<mpark::monostate>(maybe_type)) {
				typechecks = false;
				continue;
			}
			auto type = get_type(maybe_type);
			if (!first_pool_param_is(type, *pool)) {
				typechecks = false;
				continue;
			}
			pool->set_type(std::move(type));
		}
		if (!typechecks) {
			m_scopes.pop_scope();
			return;
		}

		// Pass 3: Validate the types of pool parameters
		for (const auto& e: pools) {
			if (!validate_type(e->type(), m_errors)) {
				typechecks = false;
			}
		}

		m_scopes.pop_scope();
		if (!typechecks) {
			return;
		}

		for (auto e: pools) {
			m_scopes.add_pool(e->name(), *e);
		}
	}

	void operator()(const Cst::Assignment& e) {
		auto lhs = mpark::visit(*this, e.lhs());
		auto lhs_type = expr_type(lhs);

		auto rhs = mpark::visit(*this, e.rhs());
		auto rhs_type = expr_type(rhs);

		if (!is_lvalue(lhs)) {
			m_errors.add<NotLvalue>(location(e.lhs()));
			return;
		}

		if (!assignable_from(lhs_type, rhs_type)) {
			m_errors.add<NonAssignableType>(
				location(e.rhs()), to_string(rhs_type), to_string(lhs_type));
		}

		m_blocks.add<Assignment>(std::move(lhs), std::move(rhs));
	}

	void operator()(const Cst::OpAssignment& e) {
		auto lhs = mpark::visit(*this, e.lhs());
		auto rhs = mpark::visit(*this, e.rhs());

		if (invalid(lhs) || invalid(rhs)) {
			return;
		}

		auto op = to_ast_binop(e.op());

		if (!is_lvalue(lhs)) {
			m_errors.add<NotLvalue>(location(e.lhs()));
			return;
		}

		auto lhs_type = expr_type(lhs);
		auto rhs_type = expr_type(rhs);

		switch (op) {
		case BinOp::PLUS:
		case BinOp::MINUS:
		case BinOp::TIMES:
		case BinOp::DIV: {
			const auto* as_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (as_primitive == nullptr) {
				m_errors.add<ExpectedPrimitiveType>(
					location(e.lhs()), to_string(lhs_type));
				return;
			}
			if (lhs_type != rhs_type) {
				m_errors.add<IncorrectType>(
					location(e.rhs()),
					to_string(lhs_type),
					to_string(rhs_type));
				return;
			}

			m_blocks.add<OpAssignment>(std::move(lhs), op, std::move(rhs));
			break;
		}
		case BinOp::AND:
		case BinOp::OR:
		case BinOp::XOR: {
			const auto* as_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (as_primitive == nullptr || is_floating_point(*as_primitive)) {
				m_errors.add<ExpectedIntegerType>(
					location(e.lhs()), to_string(lhs_type));
				return;
			}
			if (lhs_type != rhs_type) {
				m_errors.add<IncorrectType>(
					location(e.rhs()),
					to_string(lhs_type),
					to_string(rhs_type));
				return;
			}

			m_blocks.add<OpAssignment>(std::move(lhs), op, std::move(rhs));
			break;
		}
		case BinOp::SHL:
		case BinOp::SHR: {
			const auto* lhs_as_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			const auto* rhs_as_primitive = mpark::get_if<PrimitiveType>(&rhs_type);
			if (lhs_as_primitive == nullptr || !is_integer(*lhs_as_primitive)) {
				m_errors.add<ExpectedIntegerType>(
					location(e.lhs()), to_string(lhs_type));
				return;
			}
			if (rhs_as_primitive == nullptr || !is_integer(*rhs_as_primitive)) {
				m_errors.add<ExpectedIntegerType>(
					location(e.rhs()), to_string(rhs_type));
				return;
			}

			m_blocks.add<OpAssignment>(std::move(lhs), op, std::move(rhs));
			break;
		}
		default:
			unreachable("Missing operator case");
		}
	}

	void operator()(const Cst::IfStmt& e) {
		auto cond = mpark::visit(*this, e.cond());
		if (invalid(cond)) {
			cond = BooleanConst(true);
		}

		auto type = expr_type(cond);
		if (type != Type(PrimitiveType::BOOL)) {
			m_errors.add<IncorrectType>(
				location(e.cond()),
				to_string(PrimitiveType::BOOL),
				to_string(type));
		}

		m_scopes.push_scope();
		m_blocks.push_block();

		mpark::visit(*this, e.then_branch());

		auto then_branch = m_blocks.pop_block();
		m_scopes.pop_scope();

		m_scopes.push_scope();
		m_blocks.push_block();

		mpark::visit(*this, e.else_branch());

		auto else_branch = m_blocks.pop_block();
		m_scopes.pop_scope();

		m_blocks.add<If>(
			std::move(cond),
			std::move(then_branch),
			std::move(else_branch));
	}

	void operator()(const Cst::WhileStmt& e) {
		auto cond = mpark::visit(*this, e.cond());
		if (invalid(cond)) {
			cond = BooleanConst(true);
		}

		auto type = expr_type(cond);
		if (type != Type(PrimitiveType::BOOL)) {
			m_errors.add<IncorrectType>(
				location(e.cond()),
				to_string(PrimitiveType::BOOL),
				to_string(type));
		}

		m_scopes.push_scope();
		m_blocks.push_block();

		m_loop_nesting_count++;

		mpark::visit(*this, e.body());

		m_loop_nesting_count--;

		auto body = m_blocks.pop_block();
		m_scopes.pop_scope();

		m_blocks.add<While>(std::move(cond), std::move(body));
	}

	void operator()(const Cst::ForeachRange& e) {
		auto begin = mpark::visit(*this, e.range_begin());
		auto end = mpark::visit(*this, e.range_end());

		if (invalid(begin) || invalid(end)) {
			return;
		}

		auto begin_type = expr_type(begin);
		auto end_type = expr_type(end);

		const auto* begin_as_primitive = mpark::get_if<PrimitiveType>(&begin_type);
		const auto* end_as_primitive = mpark::get_if<PrimitiveType>(&end_type);

		if (begin_as_primitive == nullptr || !is_integer(*begin_as_primitive)) {
			m_errors.add<ExpectedIntegerType>(
				location(e.range_begin()), to_string(begin_type));
			return;
		}
		if (end_as_primitive == nullptr || !is_integer(*end_as_primitive)) {
			m_errors.add<ExpectedIntegerType>(
				location(e.range_end()), to_string(end_type));
			return;
		}
		if (*begin_as_primitive != *end_as_primitive) {
			m_errors.add<IncorrectType>(
				location(e.range_end()), to_string(begin_type), to_string(end_type));
		}

		const auto& var = e.var();
		const auto& new_var = m_method.add_variable(
			var.ident(), *begin_as_primitive, var.loc());
		m_scopes.add_variable(var.ident(), new_var);

		m_scopes.push_scope();
		m_blocks.push_block();

		m_loop_nesting_count++;

		mpark::visit(*this, e.body());

		m_loop_nesting_count--;

		auto body = m_blocks.pop_block();
		m_scopes.pop_scope();

		m_blocks.add<ForeachRange>(new_var, std::move(begin), std::move(end), std::move(body));
	}

	void operator()(const Cst::ForeachPool& e) {
		const auto* pool = m_scopes.find_pool(e.pool().ident());
		if (pool == nullptr) {
			m_errors.add<MissingDefinition>(
				e.pool().ident(), ErrorKind::POOL, e.pool().loc());
			return;
		}

		auto type = from_pool_type(pool->type());
		const auto& var = e.var();
		const auto& new_var = m_method.add_variable(
			var.ident(), std::move(type), var.loc());
		m_scopes.add_variable(var.ident(), new_var);

		m_scopes.push_scope();
		m_blocks.push_block();

		m_loop_nesting_count++;

		mpark::visit(*this, e.body());

		m_loop_nesting_count--;

		auto body = m_blocks.pop_block();
		m_scopes.pop_scope();

		m_blocks.add<ForeachPool>(new_var, *pool, std::move(body));
	}

	void operator()(const Cst::ExprStmt& e) {
		auto expr = mpark::visit(*this, e.expr());
		if (invalid(expr)) {
			return;
		}
		m_blocks.add<ExprStmt>(std::move(expr));
	}

	void operator()(const Cst::Break& e) {
		if (m_loop_nesting_count == 0) {
			m_errors.add<NotInsideLoop>(e.loc());
			return;
		}

		m_blocks.add<Break>();
	}

	void operator()(const Cst::Continue& e) {
		if (m_loop_nesting_count == 0) {
			m_errors.add<NotInsideLoop>(e.loc());
			return;
		}

		m_blocks.add<Break>();
	}

	void operator()(const Cst::Return& e) {
		const auto& ret_type = m_method.return_type();
		if (mpark::holds_alternative<VoidType>(ret_type)) {
			m_errors.add<ReturnWithExpression>(location(e.expr()));
		}

		auto expr = mpark::visit(*this, e.expr());
		if (invalid(expr)) {
			return;
		}

		auto type = expr_type(expr);
		if (!assignable_from(ret_type, type)) {
			m_errors.add<NonAssignableType>(
				location(e.expr()), to_string(type), to_string(ret_type));
		}

		m_blocks.add<Return>(std::move(expr));
	}

	void operator()(const Cst::ReturnVoid& e) {
		if (!mpark::holds_alternative<VoidType>(m_method.return_type())) {
			m_errors.add<ReturnWithoutExpression>(e.loc());
		}

		m_blocks.add<Return>();
	}

	Expr operator()(const Cst::IntegerConst& e) {
		unsigned long long value;
		size_t idx;

		const auto& str = e.value();
		try {
			value = std::stoull(str, &idx);
		} catch (...) {
			m_errors.add<IntegerOutOfBounds>(e.loc());
			return InvalidExpr();
		}

		auto* suffix_begin = &str[idx];
		auto it = std::find_if(
			std::begin(INT_LITERAL_INFO), std::end(INT_LITERAL_INFO),
			[=](const IntegerLiteralInfo& info) {
				return strcmp(suffix_begin, info.suffix) == 0;
			});
		assert_msg(it != std::end(INT_LITERAL_INFO), "Missing integer literal suffix?");

		if (value > it->max_value) {
			m_errors.add<IntegerOutOfBounds>(e.loc());
			return InvalidExpr();
		}

		return IntegerConst(value, it->type);
	}

	Expr operator()(const Cst::DoubleConst& e) {
		double value;
		size_t idx;

		const auto& str = e.value();
		try {
			value = std::stod(e.value(), &idx);
		} catch (...) {
			m_errors.add<DoubleOutOfBounds>(e.loc());
			return InvalidExpr();
		}

		if (!std::isfinite(value)) {
			m_errors.add<DoubleOutOfBounds>(e.loc());
			return InvalidExpr();
		}

		auto* suffix_begin = &str[idx];
		if (strcmp(suffix_begin, "f32") == 0) {
			float actual_value = value;

			if (!std::isfinite(actual_value)) {
				m_errors.add<DoubleOutOfBounds>(e.loc());
				return InvalidExpr();
			}
			return DoubleConst(value, PrimitiveType::F32);
		}

		return DoubleConst(value, PrimitiveType::F64);
	}

	Expr operator()(const Cst::BooleanConst& e)
	{
		return BooleanConst(e.value());
	}

	Expr operator()(const Cst::NullExpr&)
	{
		return NullExpr();
	}

	Expr operator()(const Cst::ThisExpr& e)
	{
		std::vector<PoolParameter> params;
		for (const Pool& pool: m_class.pools()) {
			params.emplace_back(PoolRef(pool));
		}

		return ThisExpr(ObjectType(m_class, std::move(params), e.loc()));
	}

	Expr operator()(const Cst::CastExpr& e)
	{
		TypeConstructor type_ctor(m_ast, m_scopes, m_errors);
		auto expr = mpark::visit(*this, e.expr());
		if (invalid(expr)) {
			return InvalidExpr();
		}

		auto type = expr_type(expr);
		if (!mpark::holds_alternative<PrimitiveType>(type)) {
			m_errors.add<ExpectedPrimitiveType>(location(e.expr()), to_string(type));
			return InvalidExpr();
		}

		return CastExpr(std::move(expr), get_primitive_type(e.type()));
	}

	Expr operator()(const Cst::UnaryExpr& e)
	{
		auto expr = mpark::visit(*this, e.expr());
		if (invalid(expr)) {
			return InvalidExpr();
		}

		auto type = expr_type(expr);

		if (!mpark::holds_alternative<PrimitiveType>(type)) {
			m_errors.add<ExpectedPrimitiveType>(location(e.expr()), to_string(type));
			return InvalidExpr();
		}

		auto op = to_ast_unop(e.op());
		const auto* primitive_type = mpark::get_if<PrimitiveType>(&type);
		if (is_bitwise_operator(op) && is_floating_point(*primitive_type)) {
			m_errors.add<ExpectedIntegerType>(location(e.expr()), to_string(type));
			return InvalidExpr();
		}

		return UnaryExpr(op, std::move(expr));
	}

	Expr operator()(const Cst::BinaryExpr& e)
	{
		auto lhs = mpark::visit(*this, e.lhs());
		auto rhs = mpark::visit(*this, e.rhs());

		if (invalid(lhs) || invalid(rhs)) {
			return InvalidExpr();
		}

		auto lhs_type = expr_type(lhs);
		auto rhs_type = expr_type(rhs);

		auto op = to_ast_binop(e.op());

		switch (op) {
		case BinOp::PLUS:
		case BinOp::MINUS:
		case BinOp::TIMES:
		case BinOp::DIV: {
			const auto* lhs_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (lhs_primitive == nullptr) {
				m_errors.add<ExpectedPrimitiveType>(
					location(e.lhs()), to_string(lhs_type));
				return InvalidExpr();
			}
			if (lhs_type != rhs_type) {
				m_errors.add<IncorrectType>(
					location(e.rhs()), to_string(lhs_type), to_string(rhs_type));
				return InvalidExpr();
			}
			return BinaryExpr(std::move(lhs), op, std::move(rhs), *lhs_primitive);
		}

		case BinOp::AND:
		case BinOp::OR:
		case BinOp::XOR: {
			const auto* lhs_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (lhs_primitive == nullptr || is_floating_point(*lhs_primitive)) {
				m_errors.add<ExpectedIntegerType>(
					location(e.lhs()), to_string(lhs_type));
				return InvalidExpr();
			}
			if (lhs_type != rhs_type) {
				m_errors.add<IncorrectType>(
					location(e.rhs()), to_string(lhs_type), to_string(rhs_type));
				return InvalidExpr();
			}
			return BinaryExpr(std::move(lhs), op, std::move(rhs), *lhs_primitive);
		}

		case BinOp::SHL:
		case BinOp::SHR: {
			const auto* lhs_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (lhs_primitive == nullptr || !is_integer(*lhs_primitive)) {
				m_errors.add<ExpectedIntegerType>(
					location(e.lhs()), to_string(lhs_type));
				return InvalidExpr();
			}
			const auto* rhs_primitive = mpark::get_if<PrimitiveType>(&rhs_type);
			if (rhs_primitive == nullptr || !is_integer(*rhs_primitive)) {
				m_errors.add<ExpectedIntegerType>(
					location(e.rhs()), to_string(rhs_type));
				return InvalidExpr();
			}
			return BinaryExpr(std::move(lhs), op, std::move(rhs), *lhs_primitive);
		}

		case BinOp::LAND:
		case BinOp::LOR: {
			const auto* lhs_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (lhs_primitive == nullptr || *lhs_primitive != PrimitiveType::BOOL) {
				m_errors.add<ExpectedBooleanType>(
					location(e.lhs()), to_string(lhs_type));
				return InvalidExpr();
			}

			const auto* rhs_primitive = mpark::get_if<PrimitiveType>(&rhs_type);
			if (rhs_primitive == nullptr || *rhs_primitive != PrimitiveType::BOOL) {
				m_errors.add<ExpectedBooleanType>(
					location(e.rhs()), to_string(rhs_type));
				return InvalidExpr();
			}
			return BinaryExpr(std::move(lhs), op, std::move(rhs), PrimitiveType::BOOL);
		}

		case BinOp::EQ:
		case BinOp::NE: {
			if (mpark::holds_alternative<PrimitiveType>(lhs_type) && lhs_type != rhs_type) {
				m_errors.add<IncorrectType>(
					location(e.rhs()), to_string(lhs_type), to_string(rhs_type));
				return InvalidExpr();
			}
			bool lhs_is_null = mpark::holds_alternative<NullptrType>(lhs_type);
			bool rhs_is_null = mpark::holds_alternative<NullptrType>(rhs_type);

			if (lhs_is_null && rhs_is_null) {
				return BooleanConst(op == BinOp::EQ);
			}
			if (!lhs_is_null && !rhs_is_null) {
				if (lhs_type != rhs_type) {
					m_errors.add<IncorrectType>(
						location(e.rhs()), to_string(lhs_type), to_string(rhs_type));
					return InvalidExpr();
				}
			}

			return BinaryExpr(std::move(lhs), op, std::move(rhs), PrimitiveType::BOOL);
		}

		case BinOp::GT:
		case BinOp::GE:
		case BinOp::LT:
		case BinOp::LE: {
			const auto* lhs_primitive = mpark::get_if<PrimitiveType>(&lhs_type);
			if (lhs_primitive == nullptr) {
				m_errors.add<ExpectedPrimitiveType>(
					location(e.lhs()), to_string(lhs_type));
				return InvalidExpr();
			}
			if (lhs_type != rhs_type) {
				m_errors.add<IncorrectType>(
					location(e.rhs()), to_string(lhs_type), to_string(rhs_type));
				return InvalidExpr();
			}
			return BinaryExpr(std::move(lhs), op, std::move(rhs), PrimitiveType::BOOL);
		}
		}

		unreachable("Forgot to handle operand");
	}

	Expr operator()(const Cst::VariableExpr& e)
	{
		const auto* var = m_scopes.find_variable(e.name().ident());
		if (var != nullptr) {
			return VariableExpr(*var, e.loc());
		}

		const auto* field = m_scopes.find_field(e.name().ident());
		if (field != nullptr) {
			return FieldAccess(
				ThisExpr(m_class.this_object_type()),
				*field,
				field->type());
		}

		m_errors.add<MissingDefinition>(
			e.name().ident(), ErrorKind::VARIABLE, e.name().loc());
		return InvalidExpr();
	}

	Expr operator()(const Cst::MethodCall& e)
	{
		const auto* method = m_class.find_method(e.name().ident());
		if (method == nullptr) {
			m_errors.add<MissingDefinition>(
				e.name().ident(), ErrorKind::METHOD, e.name().loc());
			return InvalidExpr();
		}
		const auto& method_params = method->params();

		std::vector<Expr> args;
		for (const auto& cst_arg: e.params()) {
			auto arg = mpark::visit(*this, cst_arg);
			if (invalid(arg)) {
				continue;
			}
			args.emplace_back(std::move(arg));
		}
		if (args.size() != method_params.size()) {
			m_errors.add<IncorrectArgsNumber>(
				e.loc(), method_params.size(), args.size());
			return InvalidExpr();
		}

		bool type_mismatch = false;
		for (size_t i = 0; i < args.size(); i++) {
			if (!assignable_from(method_params[i].type(), expr_type(args[i]))) {
				m_errors.add<NonAssignableType>(
					location(e.params()[i]),
					to_string(expr_type(args[i])),
					to_string(method_params[i].type()));
				type_mismatch = true;
			}
		}

		if (type_mismatch) {
			return InvalidExpr();
		}

		return MethodCall(
			*method,
			ThisExpr(m_class.this_object_type()),
			std::move(args),
			method->return_type());
	}

	Expr operator()(const Cst::MemberMethodCall& e)
	{
		auto this_expr = mpark::visit(*this, e.this_expr());
		if (invalid(this_expr)) {
			return InvalidExpr();
		}

		auto this_type = expr_type(this_expr);
		const auto* obj_type = mpark::get_if<ObjectType>(&this_type);
		if (obj_type == nullptr) {
			m_errors.add<ExpectedObjectType>(
				location(e.this_expr()),
				to_string(this_type));
			return InvalidExpr();
		}

		const auto& clazz = obj_type->of_class();
		const auto* method = clazz.find_method(e.name().ident());

		if (method == nullptr) {
			m_errors.add<MissingDefinition>(
				e.name().ident(), ErrorKind::METHOD, e.name().loc());
			return InvalidExpr();
		}
		const auto& method_params = method->params();

		std::vector<Expr> args;
		for (const auto& cst_arg: e.args()) {
			auto arg = mpark::visit(*this, cst_arg);
			if (invalid(arg)) {
				continue;
			}
			args.emplace_back(std::move(arg));
		}
		if (args.size() != method_params.size()) {
			m_errors.add<IncorrectArgsNumber>(
				e.loc(), method_params.size(), args.size());
			return InvalidExpr();
		}

		bool type_mismatch = false;
		for (size_t i = 0; i < args.size(); i++) {
			auto remapped_type =
				obj_type->remap_formal_pool_params(method_params[i].type());
			if (!assignable_from(remapped_type, expr_type(args[i]))) {
				m_errors.add<NonAssignableType>(
					location(e.args()[i]),
					to_string(expr_type(args[i])),
					to_string(remapped_type));
				type_mismatch = true;
			}
		}

		if (type_mismatch) {
			return InvalidExpr();
		}

		return MethodCall(
			*method,
			std::move(this_expr),
			std::move(args),
			obj_type->remap_formal_pool_params(method->return_type()));
	}

	Expr operator()(const Cst::FieldAccess& e)
	{
		auto expr = mpark::visit(*this, e.expr());
		if (invalid(expr)) {
			return InvalidExpr();
		}

		auto type = expr_type(expr);
		const auto* as_obj_type = mpark::get_if<ObjectType>(&type);
		if (as_obj_type == nullptr) {
			m_errors.add<ExpectedObjectType>(location(e.expr()), to_string(type));
			return InvalidExpr();
		}

		const auto* field = as_obj_type->of_class().find_field(e.field().ident());
		if (field == nullptr) {
			m_errors.add<MissingDefinition>(
				e.field().ident(), ErrorKind::FIELD, e.field().loc());
			return InvalidExpr();
		}

		Type new_type;
		const auto& field_type = field->type();
		const auto* as_field_obj_type = mpark::get_if<ObjectType>(&field_type);
		if (as_field_obj_type != nullptr) {
			new_type = as_obj_type->remap_formal_pool_params(*as_field_obj_type);
		} else {
			new_type = field_type;
		}

		return FieldAccess(
			std::move(expr), *field, std::move(new_type));
	}

	Expr operator()(const Cst::NewExpr& e)
	{
		TypeConstructor type_ctor(m_ast, m_scopes, m_errors);
		auto maybe_type = type_ctor(e.type());
		auto* type = mpark::get_if<ObjectType>(&maybe_type);
		if (type == nullptr) {
			return InvalidExpr();
		}

		return NewExpr(std::move(*type));
	}

	std::vector<Stmt> get_body() { return m_blocks.pop_block(); }
};

static void collect_method_bodies(
	const Cst::Program& cst, Program& ast, SemanticErrorList& errors)
{
	for (const auto& c: cst.classes()) {
		auto* clazz = ast.find_class(c.name().ident());
		assert_msg(clazz != nullptr, "Class cannot be null");

		for (const auto& m: c.methods()) {
			auto* method = clazz->find_method(m.name().ident());
			assert_msg(method != nullptr, "Method cannot be null");

			MethodBodyCollector collector(ast, *clazz, *method, errors);
			collector(m.body());
			method->set_body(collector.get_body());
		}
	}
}

class VarsInitFunctor
{
	std::unordered_set<const Variable*> m_set_vars;
	std::reference_wrapper<SemanticErrorList> m_errors;

public:
	VarsInitFunctor(SemanticErrorList& errors)
		: m_errors(errors)
	{}

	void add_param(const Variable* var)
	{
		m_set_vars.insert(var);
	}

	void operator()(const InvalidExpr&)
	{
		unreachable("AST should not have any invalid expressions at this point");
	}
	void operator()(const IntegerConst&) {}
	void operator()(const DoubleConst&) {}
	void operator()(const BooleanConst&) {}
	void operator()(const NullExpr&) {}
	void operator()(const ThisExpr&) {}
	void operator()(const NewExpr&) {}

	void operator()(const CastExpr& e)
	{
		mpark::visit(*this, e.expr());
	}

	void operator()(const UnaryExpr& e)
	{
		mpark::visit(*this, e.expr());
	}

	void operator()(const BinaryExpr& e)
	{
		mpark::visit(*this, e.lhs());
		mpark::visit(*this, e.rhs());
	}

	void operator()(const MethodCall& e)
	{
		mpark::visit(*this, e.this_expr());
		for (const auto& arg: e.args()) {
			mpark::visit(*this, arg);
		}
	}

	void operator()(const FieldAccess& e)
	{
		mpark::visit(*this, e.expr());
	}

	void operator()(const VariableExpr& e)
	{
		if (m_set_vars.find(&e.var()) != m_set_vars.end()) {
			return;
		}

		m_errors.get().add<VarMaybeUninitialized>(e.var().name(), e.loc());
	}

	void operator()(const Assignment& e) {
		mpark::visit(*this, e.rhs());

		const auto* as_var = mpark::get_if<VariableExpr>(&e.lhs());
		if (as_var != nullptr) {
			m_set_vars.insert(&as_var->var());
		} else {
			mpark::visit(*this, e.lhs());
		}
	}

	void operator()(const OpAssignment& e) {
		mpark::visit(*this, e.lhs());
		mpark::visit(*this, e.rhs());
	}

	void operator()(const If& e) {
		mpark::visit(*this, e.cond());

		auto lhs_functor = *this;
		for (const auto& stmt: e.then_stmts()) {
			mpark::visit(lhs_functor, stmt);
		}

		auto rhs_functor = *this;
		for (const auto& stmt: e.else_stmts()) {
			mpark::visit(rhs_functor, stmt);
		}

		bool cmp = lhs_functor.m_set_vars.size() < rhs_functor.m_set_vars.size();
		const auto& smallest = cmp ? lhs_functor : rhs_functor;
		const auto& largest = cmp ? rhs_functor : lhs_functor;

		for (const auto& e: smallest.m_set_vars) {
			if (largest.m_set_vars.find(e) != largest.m_set_vars.end()) {
				m_set_vars.insert(e);
			}
		}
	}

	void operator()(const While& e) {
		mpark::visit(*this, e.cond());

		auto functor = *this;
		for (const auto& stmt: e.body()) {
			mpark::visit(functor, stmt);
		}
	}

	void operator()(const ForeachRange& e) {
		mpark::visit(*this, e.range_begin());
		mpark::visit(*this, e.range_end());

		auto functor = *this;
		functor.m_set_vars.insert(&e.var());
		for (const auto& stmt: e.body()) {
			mpark::visit(functor, stmt);
		}
	}

	void operator()(const ForeachPool& e) {
		auto functor = *this;
		functor.m_set_vars.insert(&e.var());
		for (const auto& stmt: e.body()) {
			mpark::visit(functor, stmt);
		}
	}

	void operator()(const ExprStmt& e) {
		mpark::visit(*this, e.expr());
	}

	void operator()(const Break&) {}
	void operator()(const Continue&) {}

	void operator()(const Return& e) {
		if (e.expr() != nullptr) {
			mpark::visit(*this, *e.expr());
		}
	}
};

static void ensure_all_vars_init(const Program& ast, SemanticErrorList& errors)
{
	for (const Class& c: ast.ordered_classes()) {
		for (const Method& m: c.methods()) {
			VarsInitFunctor functor(errors);
			for (const auto& param: m.params()) {
				functor.add_param(&param);
			}

			for (const auto& stmt: m.body()) {
				mpark::visit(functor, stmt);
			}
		}
	}
}

static bool ensure_all_paths_return_impl(const std::vector<Stmt>& stmts)
{
	for (auto it = stmts.rbegin(); it != stmts.rend(); it++) {
		const auto& e = *it;

		if (mpark::holds_alternative<Return>(e)) {
			return true;
		}

		const auto* as_if_stmt = mpark::get_if<If>(&e);
		if (as_if_stmt == nullptr) {
			continue;
		}

		auto lhs = ensure_all_paths_return_impl(as_if_stmt->then_stmts());
		auto rhs = ensure_all_paths_return_impl(as_if_stmt->else_stmts());

		if (lhs && rhs) {
			return true;
		}
	}

	return false;
}

static void ensure_all_paths_return(Program& ast, SemanticErrorList& errors)
{
	for (const Class& c: ast.ordered_classes()) {
		for (const Method& m: c.methods()) {
			if (mpark::holds_alternative<VoidType>(m.return_type())) {
				continue;
			}

			if (!ensure_all_paths_return_impl(m.body())) {
				errors.add<NotAllPathsReturn>(m.name(), m.loc());
			}
		}
	}
}

void run_semantic_analysis(const Cst::Program& cst, Program& dest_ast, SemanticErrorList& errors)
{
	Program ast;
	collect_classes_fields_pool_params(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	collect_layouts(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	collect_pool_field_method_types(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	validate_top_level_types(ast, errors);
	if (errors.has_errors()) {
		return;
	}

	collect_method_bodies(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	ensure_all_vars_init(ast, errors);
	ensure_all_paths_return(ast, errors);
	if (errors.has_errors()) {
		return;
	}

	dest_ast = std::move(ast);
}

} // namespace Ast
