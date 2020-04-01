#include "cst.h"
#include "ast.h"
#include "ast_errors.h"

#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <utility>

template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
	return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

class TypeCollector: public Cst::DefaultVisitor
{
	const Ast::Program* m_ast;
	Ast::SemanticErrorList* m_errors;

	bool m_limited;
	Ast::TypeKind m_kind;

	bool m_success = false;
	std::unique_ptr<Ast::Type> m_type;

	const Ast::Class* m_class = nullptr;
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
			m_errors->add(make_unique<Ast::PoolParametersMismatch>(
					m_class->num_pools(),
					m_pools.size(),
					name.loc()));
			return false;
		}

		return true;
	}

public:
	TypeCollector(const Ast::Program* ast, Ast::SemanticErrorList* errors)
		: m_ast(ast)
		, m_errors(errors)
		, m_limited(false)
		, m_kind(Ast::TypeKind::PRIMITIVE)
	{}

	TypeCollector(const Ast::Program* ast,
				  Ast::SemanticErrorList* errors,
				  Ast::TypeKind kind)
		: m_ast(ast)
		, m_errors(errors)
		, m_limited(true)
		, m_kind(kind)
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
		auto* pool = m_class->find_pool(pool_param.ident());
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

		visitIter(klass.pool_param_bounds_begin(), klass.pool_param_bounds_end());
		for (auto it = curr_class->pools_begin(); it != curr_class->pools_end(); it++) {
			auto& pool = **it;
			if (!pool.type_init()) {
				errors->add(make_unique<Ast::MissingDefinition>(
						pool.name(),
						Ast::ErrorKind::TYPE,
						pool.loc()));
			}
		}

		class_fields.clear();
		class_field_map.clear();
		visitIter(klass.fields_begin(), klass.fields_end());

		curr_class->set_fields(std::move(class_field_map), std::move(class_fields));

		methods.clear();
		method_map.clear();
		visitIter(klass.methods_begin(), klass.methods_end());
		curr_class->set_methods(std::move(method_map), std::move(methods));
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

		TypeCollector type_collector(ast, errors, Ast::TypeKind::BOUND);
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

		TypeCollector type_collector(ast, errors);
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
			TypeCollector type_collector(ast, errors);
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

			TypeCollector type_collector(ast, errors);
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

	*ast = std::move(program);
}
