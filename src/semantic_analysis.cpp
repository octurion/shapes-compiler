#include "semantic_analysis.h"

#include <cstdint>
#include <sstream>

using Ast::SemanticError;

Ast::SemanticError::SemanticError(SemanticError&& other)
	: m_tag(other.m_tag)
{
	construct_variant_from_other(other);
}

Ast::SemanticError& Ast::SemanticError::operator=(SemanticError&& other)
{
	destroy_variant();

	m_tag = other.m_tag;
	construct_variant_from_other(other);

	return *this;
}

Ast::SemanticError::~SemanticError()
{
	destroy_variant();
}

void Ast::SemanticError::destroy_variant()
{
	switch (m_tag) {
	case Tag::DUPLICATE_DEFINITION:
		m_duplicate_definition.~DuplicateDefinition();
		break;

	case Tag::MISSING_DEFINITION:
		m_missing_definition.~MissingDefinition();
		break;

	case Tag::MISSING_BOUND:
		m_missing_bound.~MissingBound();
		break;

	case Tag::LAYOUT_MISSING_FIELD:
		m_layout_missing_field.~LayoutMissingField();
		break;

	case Tag::LAYOUT_DUPLICATE_FIELD:
		m_layout_duplicate_field.~LayoutDuplicateField();
		break;

	case Tag::NO_POOL_PARAMETERS:
		m_no_pool_parameters.~NoPoolParameters();
		break;

	case Tag::EMPTY_CLUSTER:
		m_empty_cluster.~EmptyCluster();
		break;

	case Tag::NOT_INSIDE_LOOP:
		m_not_inside_loop.~NotInsideLoop();
		break;

	case Tag::INTEGER_OUT_OF_BOUNDS:
		m_integer_out_of_bounds.~IntegerOutOfBounds();
		break;

	case Tag::INCORRECT_FIRST_POOL_PARAMETER:
		m_incorrect_first_pool_parameter.~IncorrectFirstPoolParameter();
		break;

	case Tag::INCORRECT_TYPE:
		m_incorrect_type.~IncorrectType();
		break;

	case Tag::INCOMPATIBLE_BOUND:
		m_incompatible_bound.~IncompatibleBound();
		break;

	case Tag::INCORRECT_POOLS_NUMBER:
		m_incorrect_pools_number.~IncorrectPoolsNumber();
		break;

	case Tag::EXPECTED_OBJECT_TYPE:
		m_expected_object_type.~ExpectedObjectType();
		break;

	case Tag::EXPECTED_PRIMITIVE_TYPE:
		m_expected_primitive_type.~ExpectedPrimitiveType();
		break;

	case Tag::EXPECTED_BOOLEAN_TYPE:
		m_expected_integer_type.~ExpectedIntegerType();
		break;

	case Tag::EXPECTED_INTEGER_TYPE:
		m_expected_integer_type.~ExpectedIntegerType();
		break;

	case Tag::EXPECTED_NUMERIC_TYPE:
		m_expected_numeric_type.~ExpectedNumericType();
		break;

	case Tag::RETURN_WITH_EXPRESSION:
		m_return_with_expression.~ReturnWithExpression();
		break;

	case Tag::RETURN_WITHOUT_EXPRESSION:
		m_return_without_expression.~ReturnWithoutExpression();
		break;

	case Tag::NON_ASSIGNABLE_TYPE:
		m_non_assignable_type.~NonAssignableType();
		break;

	case Tag::NOT_LVALUE:
		m_not_lvalue.~NotLvalue();
		break;

	case Tag::INCORRECT_ARGS_NUMBER:
		m_incorrect_args_number.~IncorrectArgsNumber();
		break;
	}
}

void Ast::SemanticError::construct_variant_from_other(Ast::SemanticError& other)
{
	switch (other.m_tag) {
	case Tag::DUPLICATE_DEFINITION:
		new (&m_duplicate_definition) DuplicateDefinition(std::move(other.m_duplicate_definition));
		break;

	case Tag::MISSING_DEFINITION:
		new (&m_missing_definition) MissingDefinition(std::move(other.m_missing_definition));
		break;

	case Tag::MISSING_BOUND:
		new (&m_missing_bound) MissingBound(std::move(other.m_missing_bound));
		break;

	case Tag::LAYOUT_MISSING_FIELD:
		new (&m_layout_missing_field) LayoutMissingField(std::move(other.m_layout_missing_field));
		break;

	case Tag::LAYOUT_DUPLICATE_FIELD:
		new (&m_layout_duplicate_field) LayoutDuplicateField(std::move(other.m_layout_duplicate_field));
		break;

	case Tag::NO_POOL_PARAMETERS:
		new (&m_no_pool_parameters) NoPoolParameters(std::move(other.m_no_pool_parameters));
		break;

	case Tag::EMPTY_CLUSTER:
		new (&m_empty_cluster) EmptyCluster(std::move(other.m_empty_cluster));
		break;

	case Tag::NOT_INSIDE_LOOP:
		new (&m_not_inside_loop) NotInsideLoop(std::move(other.m_not_inside_loop));
		break;

	case Tag::INTEGER_OUT_OF_BOUNDS:
		new (&m_integer_out_of_bounds) IntegerOutOfBounds(std::move(other.m_integer_out_of_bounds));
		break;

	case Tag::INCORRECT_FIRST_POOL_PARAMETER:
		new (&m_incorrect_first_pool_parameter) IncorrectFirstPoolParameter(std::move(other.m_incorrect_first_pool_parameter));
		break;

	case Tag::INCORRECT_TYPE:
		new (&m_incorrect_type) IncorrectType(std::move(other.m_incorrect_type));
		break;

	case Tag::INCOMPATIBLE_BOUND:
		new (&m_incompatible_bound) IncompatibleBound(std::move(other.m_incompatible_bound));
		break;

	case Tag::INCORRECT_POOLS_NUMBER:
		new (&m_incorrect_pools_number) IncorrectPoolsNumber(std::move(other.m_incorrect_pools_number));
		break;

	case Tag::EXPECTED_OBJECT_TYPE:
		new (&m_expected_object_type) ExpectedObjectType(std::move(other.m_expected_object_type));
		break;

	case Tag::EXPECTED_PRIMITIVE_TYPE:
		new (&m_expected_primitive_type) ExpectedPrimitiveType(std::move(other.m_expected_primitive_type));
		break;

	case Tag::EXPECTED_BOOLEAN_TYPE:
		new (&m_expected_boolean_type) ExpectedBooleanType(std::move(other.m_expected_boolean_type));
		break;

	case Tag::EXPECTED_INTEGER_TYPE:
		new (&m_expected_integer_type) ExpectedIntegerType(std::move(other.m_expected_integer_type));
		break;

	case Tag::EXPECTED_NUMERIC_TYPE:
		new (&m_expected_numeric_type) ExpectedNumericType(std::move(other.m_expected_numeric_type));
		break;

	case Tag::RETURN_WITH_EXPRESSION:
		new (&m_return_with_expression) ReturnWithExpression(std::move(other.m_return_with_expression));
		break;

	case Tag::RETURN_WITHOUT_EXPRESSION:
		new (&m_return_without_expression) ReturnWithoutExpression(std::move(other.m_return_without_expression));
		break;

	case Tag::NON_ASSIGNABLE_TYPE:
		new (&m_non_assignable_type) NonAssignableType(std::move(other.m_non_assignable_type));
		break;

	case Tag::NOT_LVALUE:
		new (&m_not_lvalue) NotLvalue(std::move(other.m_not_lvalue));
		break;

	case Tag::INCORRECT_ARGS_NUMBER:
		new (&m_incorrect_args_number) IncorrectArgsNumber(std::move(other.m_incorrect_args_number));
		break;
	}
}

class TypePrinter final: public Ast::DefaultVisitor
{
	std::ostringstream m_os;

	TypePrinter() = default;

public:
	template<typename Iter>
	void print_args(Iter begin, Iter end)
	{
		auto it = begin;
		if (it != end) {
			it->accept(*this);
			it++;
		}

		for (; it != end; it++) {
			m_os << ", ";
			it->accept(*this);
		}
	}

	void visit(const Ast::Type::PrimitiveType& e) override
	{
		switch (e.kind()) {
		case Ast::Type::PrimitiveKind::BOOL:
			m_os << "bool";
			break;

		case Ast::Type::PrimitiveKind::U8:
			m_os << "u8";
			break;

		case Ast::Type::PrimitiveKind::U16:
			m_os << "u16";
			break;

		case Ast::Type::PrimitiveKind::U32:
			m_os << "u32";
			break;

		case Ast::Type::PrimitiveKind::U64:
			m_os << "u64";
			break;

		case Ast::Type::PrimitiveKind::I8:
			m_os << "i8";
			break;

		case Ast::Type::PrimitiveKind::I16:
			m_os << "i16";
			break;

		case Ast::Type::PrimitiveKind::I32:
			m_os << "i32";
			break;

		case Ast::Type::PrimitiveKind::I64:
			m_os << "i64";
			break;

		case Ast::Type::PrimitiveKind::F32:
			m_os << "f32";
			break;

		case Ast::Type::PrimitiveKind::F64:
			m_os << "f64";
			break;
		}
	}

	void visit(const Ast::Type::NullType&) override
	{
		m_os << "nullptr";
	}

	void visit(const Ast::Type::ObjectType& e) override
	{
		m_os << e.of_class().name() << "<";
		print_args(e.begin(), e.end());
		m_os << ">";
	}

	void visit(const Ast::PoolType::BoundType& e) override
	{
		m_os << "[" << e.of_class().name() << "<";
		print_args(e.begin(), e.end());
		m_os << ">]";
	}

	void visit(const Ast::PoolType::LayoutType& e) override
	{
		m_os << e.of_class().name() << "<";
		print_args(e.begin(), e.end());
		m_os << ">";
	}

	void visit(const Ast::PoolParameter::PoolRef& e) override
	{
		m_os << e.pool().name();
	}

	void visit(const Ast::PoolParameter::None&) override
	{
		m_os << "none";
	}

	void visit(const Ast::Type::VoidType&) override
	{
		m_os << "void";
	}

	static std::string to_string(const Ast::Type& type)
	{
		TypePrinter printer;
		type.accept(printer);
		return printer.m_os.str();
	}

	static std::string to_string(const Ast::Type::PrimitiveType& type)
	{
		TypePrinter printer;
		type.accept(printer);
		return printer.m_os.str();
	}

	static std::string to_string(const Ast::PoolType& type)
	{
		TypePrinter printer;
		type.accept(printer);
		return printer.m_os.str();
	}

	static std::string to_string(const Ast::PoolType::BoundType& type)
	{
		TypePrinter printer;
		type.accept(printer);
		return printer.m_os.str();
	}
};

Ast::Type::PrimitiveKind to_kind(Cst::Type::PrimitiveKind kind)
{
	switch (kind) {
	case Cst::Type::PrimitiveKind::BOOL:
		return Ast::Type::PrimitiveKind::BOOL;

	case Cst::Type::PrimitiveKind::U8:
		return Ast::Type::PrimitiveKind::U8;

	case Cst::Type::PrimitiveKind::U16:
		return Ast::Type::PrimitiveKind::U16;

	case Cst::Type::PrimitiveKind::U32:
		return Ast::Type::PrimitiveKind::U32;

	case Cst::Type::PrimitiveKind::U64:
		return Ast::Type::PrimitiveKind::U64;

	case Cst::Type::PrimitiveKind::I8:
		return Ast::Type::PrimitiveKind::I8;

	case Cst::Type::PrimitiveKind::I16:
		return Ast::Type::PrimitiveKind::I16;

	case Cst::Type::PrimitiveKind::I32:
		return Ast::Type::PrimitiveKind::I32;

	case Cst::Type::PrimitiveKind::I64:
		return Ast::Type::PrimitiveKind::I64;

	case Cst::Type::PrimitiveKind::F32:
		return Ast::Type::PrimitiveKind::F32;

	case Cst::Type::PrimitiveKind::F64:
		return Ast::Type::PrimitiveKind::F64;
	}

	// Dead code, but it silences gcc
	return Ast::Type::PrimitiveKind::U8;
}

Ast::Expr::BinOp to_ast_binop(Cst::BinOp op) {
	switch (op) {
	case Cst::BinOp::PLUS:
		return Ast::Expr::BinOp::PLUS;
	case Cst::BinOp::MINUS:
		return Ast::Expr::BinOp::MINUS;
	case Cst::BinOp::TIMES:
		return Ast::Expr::BinOp::TIMES;
	case Cst::BinOp::DIV:
		return Ast::Expr::BinOp::DIV;
	case Cst::BinOp::LAND:
		return Ast::Expr::BinOp::LAND;
	case Cst::BinOp::LOR:
		return Ast::Expr::BinOp::LOR;
	case Cst::BinOp::AND:
		return Ast::Expr::BinOp::AND;
	case Cst::BinOp::OR:
		return Ast::Expr::BinOp::OR;
	case Cst::BinOp::XOR:
		return Ast::Expr::BinOp::XOR;
	case Cst::BinOp::SHL:
		return Ast::Expr::BinOp::SHL;
	case Cst::BinOp::SHR:
		return Ast::Expr::BinOp::SHR;
	case Cst::BinOp::EQ:
		return Ast::Expr::BinOp::EQ;
	case Cst::BinOp::NE:
		return Ast::Expr::BinOp::NE;
	case Cst::BinOp::LT:
		return Ast::Expr::BinOp::LT;
	case Cst::BinOp::LE:
		return Ast::Expr::BinOp::LE;
	case Cst::BinOp::GT:
		return Ast::Expr::BinOp::GT;
	case Cst::BinOp::GE:
		return Ast::Expr::BinOp::GE;
	}
}

class ClassLayoutMembersCollector final: public Cst::DefaultVisitor
{
	Ast::Program& m_ast;
	Ast::SemanticErrorList& m_errors;

	Ast::Class* m_curr_class = nullptr;

	Ast::Layout* m_curr_layout = nullptr;
	Ast::Cluster* m_curr_cluster = nullptr;

	Ast::PoolType m_res_pool_type;

	std::vector<Ast::PoolParameter> m_pool_params;

	std::unordered_map<std::string, const Location&> m_used_fields;

public:
	using Cst::DefaultVisitor::visit;

	explicit ClassLayoutMembersCollector(Ast::Program& ast, Ast::SemanticErrorList& errors)
		: m_ast(ast)
		, m_errors(errors)
	{}

	void visit(const Cst::Class& e) override
	{
		const auto& name = e.name();
		auto res = m_ast.add_class(name.ident(), name.loc());

		if (!res.second) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				name.ident(),
				Ast::ErrorKind::CLASS,
				name.loc(),
				res.first->loc()
			);
			return;
		}

		m_curr_class = res.first;

		Cst::DefaultVisitor::visit(e);

		m_curr_class = nullptr;

		for (auto it = res.first->pools_begin(); it != res.first->pools_end(); it++) {
			const auto& pool = it->get();
			if (!pool.type().valid()) {
				m_errors.add<SemanticError::MissingBound>(
					pool.name(), pool.loc()
				);
			}
		}
	}

	void visit(const Cst::FormalPoolParameter& e) override
	{
		auto res = m_curr_class->add_pool(e.ident(), e.loc());
		if (!res.second) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				e.ident(),
				Ast::ErrorKind::POOL,
				e.loc(),
				res.first->loc()
			);
			return;
		}
	}

	void visit(const Cst::FormalPoolBound& e) override
	{
		m_pool_params.clear();
		e.type().accept(*this);
		auto* pool = m_curr_class->find_pool(e.pool().ident());
		if (pool == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.pool().ident(),
				Ast::ErrorKind::POOL,
				e.pool().loc()
			);
			return;
		}

		if (pool->type().valid()) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				e.pool().ident(),
				Ast::ErrorKind::POOL_BOUND,
				e.type().loc(),
				pool->type().loc()
			);
			return;
		}

		pool->set_type(std::move(m_res_pool_type));
	}

	void visit(const Cst::PoolParameter::Pool& e) override
	{
		auto* pool = m_curr_class->find_pool(e.ident());
		if (pool == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.ident(),
				Ast::ErrorKind::POOL,
				e.loc()
			);
			return;
		}

		m_pool_params.emplace_back(
			Ast::PoolParameter::PoolRef(*pool), e.loc()
		);
	}

	void visit(const Cst::BoundType& e) override
	{
		m_pool_params.clear();

		const auto& name = e.class_name();
		auto* for_class = m_ast.find_class(name.ident());
		if (for_class == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				name.ident(),
				Ast::ErrorKind::CLASS,
				name.loc()
			);
			return;
		}

		visit(e.begin(), e.end());
		m_res_pool_type = Ast::PoolType(
			Ast::PoolType::BoundType(*for_class, std::move(m_pool_params), e.loc()),
			e.loc()
		);
	}


	void visit(const Cst::Field& e) override
	{
		const auto& name = e.name();
		auto res = m_curr_class->add_field(name.ident(), name.loc());
		if (!res.second) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				name.ident(),
				Ast::ErrorKind::FIELD,
				name.loc(),
				res.first->loc()
			);
			return;
		}

		if (m_curr_class->pools_begin() == m_curr_class->pools_end()) {
			m_errors.add<SemanticError::NoPoolParameters>(
				m_curr_class->name(),
				m_curr_class->loc()
			);
			return;
		}
	}

	void visit(const Cst::Method& e) override
	{
		const auto& name = e.name();
		auto res = m_curr_class->add_method(name.ident(), name.loc());
		if (!res.second) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				name.ident(),
				Ast::ErrorKind::METHOD,
				name.loc(),
				res.first->loc()
			);
			return;
		}
	}

	void visit(const Cst::Layout& e) override
	{
		m_curr_layout = nullptr;
		m_curr_cluster = nullptr;
		m_used_fields.clear();

		const auto& class_name = e.for_class();
		const auto* for_class = m_ast.find_class(class_name.ident());
		if (for_class == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				class_name.ident(),
				Ast::ErrorKind::CLASS,
				class_name.loc()
			);
			return;
		}

		const auto& name = e.name();
		auto res = m_ast.add_layout(name.ident(), *for_class, name.loc());
		if (!res.second) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				name.ident(),
				Ast::ErrorKind::LAYOUT,
				name.loc(),
				res.first->loc()
			);
			return;
		}

		m_curr_layout = res.first;

		visit(e.begin(), e.end());

		for (auto it = for_class->fields_begin(); it != for_class->fields_end(); it++) {
			const auto& field = it->get();
			if (m_used_fields.find(field.name()) == m_used_fields.end()) {
				m_errors.add<SemanticError::LayoutMissingField>(
					m_curr_layout->name(),
					field.name(),
					m_curr_layout->loc()
				);
			}
		}
	}

	void visit(const Cst::Cluster& e) override
	{
		m_curr_cluster = &m_curr_layout->add_cluster(e.loc());

		visit(e.begin(), e.end());

		if (m_curr_cluster->begin() == m_curr_cluster->end()) {
			m_errors.add<SemanticError::EmptyCluster>(e.loc());
		}
	}

	void visit(const Cst::ClusterField& e) override
	{
		auto it = m_curr_layout->for_class().find_field(e.ident());
		if (it == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.ident(),
				Ast::ErrorKind::FIELD,
				e.loc()
			);
			return;
		}

		auto res = m_used_fields.emplace(e.ident(), e.loc());
		if (!res.second) {
			m_errors.add<SemanticError::LayoutDuplicateField>(
				m_curr_layout->name(),
				m_curr_layout->loc(),
				e.ident(),
				e.loc(),
				res.first->second
			);
			return;
		}

		m_curr_cluster->add_field(*it);
	}

	void visit(const Cst::Program& e) override
	{
		// Default visitor implementation classes first, so that we collect the
		// classes and class fields before we traverse the layouts
		Cst::DefaultVisitor::visit(e);
	}

	static void collect(const Cst::Program& cst, Ast::Program& ast, Ast::SemanticErrorList& errors)
	{
		ClassLayoutMembersCollector collector(ast, errors);
		cst.accept(collector);
	}
};

class SymbolTable
{
	struct SymbolTableEntry
	{
		std::vector<std::string> m_vars_declared;
		std::vector<std::string> m_pools_declared;
	};

	std::unordered_map<std::string, std::reference_wrapper<Ast::Pool>> m_pools;
	std::unordered_map<std::string, std::vector<std::reference_wrapper<const Ast::Variable>>> m_vars;

	const Ast::Class* m_class = nullptr;
	std::vector<SymbolTableEntry> m_entries;

public:
	void set_class(const Ast::Class& new_class)
	{
		m_class = &new_class;

		m_pools.clear();
		m_vars.clear();
	}

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

	bool add_pool(const std::string& name, Ast::Pool& pool)
	{
		auto it = m_pools.emplace(name, pool);
		if (!it.second) {
			return false;
		}

		m_entries.back().m_pools_declared.push_back(name);
		return true;
	}

	void add_variable(const std::string& name, const Ast::Variable& var)
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

		return &it->second.get();
	}

	Ast::Pool* find_pool(const std::string& name)
	{
		auto it = m_pools.find(name);
		if (it == m_pools.end()) {
			return nullptr;
		}

		return &it->second.get();
	}

	const Ast::Variable* find_variable(const std::string& name) const
	{
		auto it = m_vars.find(name);
		if (it == m_vars.end()) {
			return nullptr;
		}

		return &it->second.back().get();
	}
};

class PoolMapper: public Ast::DefaultVisitor
{
	std::unordered_map<const Ast::Pool*, const Ast::PoolParameter*> m_pool_map;

	PoolMapper() = default;

public:
	using Ast::DefaultVisitor::visit;

	void visit(const Ast::Type::ObjectType& e) override
	{
		auto pool_it = e.of_class().pools_begin();
		for (auto it = e.begin(); it != e.end(); it++, pool_it++) {
			m_pool_map[&pool_it->get()] = &*it;
		}
	}

	void visit(const Ast::PoolType::LayoutType& e) override
	{
		auto pool_it = e.of_class().pools_begin();
		for (auto it = e.begin(); it != e.end(); it++, pool_it++) {
			m_pool_map[&pool_it->get()] = &*it;
		}
	}

	void visit(const Ast::PoolType::BoundType& e) override
	{
		auto pool_it = e.of_class().pools_begin();
		for (auto it = e.begin(); it != e.end(); it++, pool_it++) {
			m_pool_map[&pool_it->get()] = &*it;
		}
	}

	static std::unordered_map<const Ast::Pool*, const Ast::PoolParameter*>
	make_pool_map(const Ast::Type& type)
	{
		PoolMapper mapper;
		type.accept(mapper);

		return std::move(mapper.m_pool_map);
	}

	static std::unordered_map<const Ast::Pool*, const Ast::PoolParameter*>
	make_pool_map(const Ast::PoolType& type)
	{
		PoolMapper mapper;
		type.accept(mapper);

		return std::move(mapper.m_pool_map);
	}
};

class TypeValidator: public Ast::DefaultVisitor
{
	Ast::SemanticErrorList& m_errors;

	std::unordered_map<const Ast::Pool*, const Ast::PoolParameter*> m_pool_map;

	const Ast::Class* m_curr_class = nullptr;
	Ast::Class::pools_const_iterator m_class_pool_iter;

	bool m_valid = true;

public:
	explicit TypeValidator(Ast::SemanticErrorList& errors)
		: m_errors(errors)
	{}

	using Ast::DefaultVisitor::visit;

	void visit(const Ast::PoolType::BoundType& e) override
	{
		m_curr_class = &e.of_class();
		if (e.num_params() != m_curr_class->num_pools()) {
			m_errors.add<Ast::SemanticError::IncorrectPoolsNumber>(
				e.loc(), m_curr_class->num_pools(), e.num_params()
			);
			m_valid = false;
			return;
		}

		m_class_pool_iter = m_curr_class->pools_begin();
		visit(e.begin(), e.end());
	}

	void visit(const Ast::PoolType::LayoutType& e) override
	{
		m_curr_class = &e.of_class();
		if (e.num_params() != m_curr_class->num_pools()) {
			m_errors.add<Ast::SemanticError::IncorrectPoolsNumber>(
				e.loc(), m_curr_class->num_pools(), e.num_params()
			);
			m_valid = false;
			return;
		}

		m_class_pool_iter = m_curr_class->pools_begin();
		visit(e.begin(), e.end());
	}

	void visit(const Ast::Type::ObjectType& e) override
	{
		m_curr_class = &e.of_class();
		if (e.num_params() != m_curr_class->num_pools()) {
			m_errors.add<Ast::SemanticError::IncorrectPoolsNumber>(
				e.loc(), m_curr_class->num_pools(), e.num_params()
			);
			m_valid = false;
			return;
		}

		m_class_pool_iter = m_curr_class->pools_begin();
		visit(e.begin(), e.end());
	}

	void visit(const Ast::PoolParameter::PoolRef& e) override
	{
		const auto& pool = m_class_pool_iter->get();
		const auto* bound_type = pool.type().as_bound_type();
		assert(bound_type != nullptr);

		std::vector<Ast::PoolParameter> params;

		for (auto it = bound_type->begin(); it != bound_type->end(); it++) {
			auto* substituted_pool = it->as_pool();
			const auto* substitute = m_pool_map[&substituted_pool->pool()];

			if (substitute != nullptr) {
				params.emplace_back(
					Ast::PoolParameter::PoolRef(*substitute->as_pool()),
					substituted_pool->pool().loc()
				);
			} else {
				params.emplace_back(
					Ast::PoolParameter::None(),
					Location()
				);
			}
		}

		Ast::PoolType::BoundType substituted_bound(
			*m_curr_class, std::move(params), Location()
		);

		if (!e.pool().type().compatible_with_bound(substituted_bound)) {
			m_errors.add<Ast::SemanticError::IncompatibleBound>(
				e.pool().loc(),
				TypePrinter::to_string(e.pool().type()),
				TypePrinter::to_string(substituted_bound)
			);
			m_valid = false;
		}

		m_class_pool_iter++;
	}

	void visit(const Ast::PoolParameter::None&) override
	{
		m_class_pool_iter++;
	}

	static bool validate(const Ast::Type& type, Ast::SemanticErrorList& errors)
	{
		TypeValidator validator(errors);
		validator.m_pool_map = PoolMapper::make_pool_map(type);
		type.accept(validator);

		return validator.m_valid;
	}

	static bool validate(const Ast::PoolType& type, Ast::SemanticErrorList& errors)
	{
		TypeValidator validator(errors);
		validator.m_pool_map = PoolMapper::make_pool_map(type);
		type.accept(validator);

		return validator.m_valid;
	}
};

class MemberTypesCollector: public Cst::DefaultVisitor
{
	Ast::Program& m_ast;
	Ast::SemanticErrorList& m_errors;

	SymbolTable m_symtab;
	Ast::Class* m_curr_class = nullptr;
	bool m_pool_param_errors = false;

	Ast::Method* m_curr_method = nullptr;

	Ast::Type m_res_type;
	Ast::PoolType m_res_pool_type;

	std::vector<Ast::PoolParameter> m_pool_params;

	std::unordered_map<std::string, Location> m_method_params;

	explicit MemberTypesCollector(Ast::Program& ast, Ast::SemanticErrorList& errors)
		: m_ast(ast)
		, m_errors(errors)
	{}

public:
	using Cst::DefaultVisitor::visit;

	void visit(const Cst::FormalPoolParameter& e) override
	{
		const auto* pool = m_curr_class->find_pool(e.ident());
		const auto* bound_type = pool->type().as_bound_type();
		assert(bound_type != nullptr);

		const auto* first_pool_param = bound_type->begin()->as_pool();
		assert(first_pool_param != nullptr);

		if (pool != &first_pool_param->pool()) {
			m_errors.add<Ast::SemanticError::IncorrectFirstPoolParameter>(
				bound_type->begin()->loc(),
				pool->name(),
				first_pool_param->pool().name()
			);
			m_pool_param_errors = true;
			return;
		}

		if (!TypeValidator::validate(pool->type(), m_errors)) {
			m_pool_param_errors = true;
		}
	}

	void visit(const Cst::PoolParameter::Pool& e) override
	{
		auto* pool = m_symtab.find_pool(e.ident());
		if (pool == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.ident(),
				Ast::ErrorKind::POOL,
				e.loc()
			);
			m_pool_params.emplace_back(
				Ast::PoolParameter(Ast::PoolParameter::None(), e.loc())
			);
			return;
		}

		m_pool_params.emplace_back(
			Ast::PoolParameter(Ast::PoolParameter::PoolRef(*pool), e.loc())
		);
	}

	void visit(const Cst::PoolParameter::None& e) override
	{
		m_pool_params.emplace_back(
			Ast::PoolParameter(Ast::PoolParameter::None(), e.loc())
		);
	}

	void visit(const Cst::Type::PrimitiveType& e) override
	{
		m_res_type = Ast::Type(
			Ast::Type::PrimitiveType(to_kind(e.kind())), e.loc()
		);
	}

	void visit(const Cst::Type::ObjectType& e) override
	{
		const auto& name = e.class_name();
		auto* for_class = m_ast.find_class(name.ident());
		if (for_class == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				name.ident(),
				Ast::ErrorKind::CLASS,
				name.loc()
			);
			return;
		}

		visit(e.begin(), e.end());
		Ast::Type type(
			Ast::Type::ObjectType(*for_class, std::move(m_pool_params), e.loc()),
			e.loc()
		);

		if (!TypeValidator::validate(type, m_errors)) {
			m_res_type = Ast::Type();
			return;
		}

		m_res_type = std::move(type);
	}

	void visit(const Cst::Field& e) override
	{
		auto* field = m_curr_class->find_field(e.name().ident());
		e.type().accept(*this);
		field->set_type(std::move(m_res_type));
	}

	void visit(const Cst::Method& e) override
	{
		m_curr_method = m_curr_class->find_method(e.name().ident());
		const auto* return_type = e.type();
		if (return_type != nullptr) {
			return_type->accept(*this);
			auto type = std::move(m_res_type);

			m_curr_method->set_return_type(std::move(type));
		}

		visit(e.begin(), e.end());
	}

	void visit(const Cst::MethodParameter& e) override
	{
		e.type().accept(*this);
		auto type = std::move(m_res_type);
		if (!type.valid()) {
			return;
		}

		auto& name = e.name();
		auto res = m_method_params.emplace(name.ident(), name.loc());
		if (!res.second) {
			m_errors.add<SemanticError::DuplicateDefinition>(
				name.ident(),
				Ast::ErrorKind::VARIABLE,
				name.loc(),
				res.first->second
			);
			return;
		}

		m_curr_method->add_parameter(name.ident(), std::move(type), name.loc());
	}

	void visit(const Cst::Class& e) override
	{
		m_curr_class = m_ast.find_class(e.name().ident());
		m_pool_param_errors = false;
		m_symtab.set_class(*m_curr_class);

		m_symtab.push_scope();
		for (auto it = m_curr_class->pools_begin(); it != m_curr_class->pools_end(); it++) {
			auto& ref = it->get();
			m_symtab.add_pool(ref.name(), ref);
		}

		std::vector<Ast::PoolParameter> first_bound_params;
		for (auto it = m_curr_class->pools_begin(); it != m_curr_class->pools_end(); it++) {
			first_bound_params.emplace_back(
				Ast::PoolParameter::PoolRef(*it), it->get().loc()
			);
		}
		Ast::PoolType::BoundType first_expected(
			*m_curr_class,
			std::move(first_bound_params),
			Location());

		const auto& first_actual = *m_curr_class->pools_begin()->get().type().as_bound_type();
		if (first_expected != first_actual) {
			m_errors.add<Ast::SemanticError::IncorrectType>(
				m_curr_class->pools_begin()->get().type().loc(),
				TypePrinter::to_string(first_expected),
				TypePrinter::to_string(first_actual)
			);
		}

		// If we get pool parameter errors, then we'd expect to get a lot
		// of typechecking errors in general, so bail out
		visit(e.pool_params_begin(), e.pool_params_end());
		if (!m_pool_param_errors) {
			visit(e.fields_begin(), e.fields_end());
			visit(e.methods_begin(), e.methods_end());
		}

		m_curr_class = nullptr;
	}

	static void collect(const Cst::Program& cst, Ast::Program& ast, Ast::SemanticErrorList& errors)
	{
		MemberTypesCollector collector(ast, errors);
		cst.accept(collector);
	}
};

class BlockStack
{
	std::vector<std::vector<Ast::Stmt>> blocks;

public:
	void push_block()
	{
		blocks.emplace_back();
	}

	std::vector<Ast::Stmt> pop_block()
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

class PoolTypeToObjectType: public Ast::DefaultVisitor
{
	const Ast::Class* m_class = nullptr;
	Location m_loc;
	std::vector<Ast::PoolParameter> m_params;

public:
	using Ast::DefaultVisitor::visit;

	void visit(const Ast::PoolType::BoundType& e) override
	{
		m_class = &e.of_class();
		m_loc = e.loc();
		visit(e.begin(), e.end());
	}

	void visit(const Ast::PoolType::LayoutType& e) override
	{
		m_class = &e.of_class();
		m_loc = e.loc();
		visit(e.begin(), e.end());
	}

	void visit(const Ast::PoolParameter::PoolRef& e) override
	{
		m_params.emplace_back(e, Location());
	}

	void visit(const Ast::PoolParameter::None& e) override
	{
		m_params.emplace_back(e, Location());
	}

	static Ast::Type convert(const Ast::PoolType& type)
	{
		PoolTypeToObjectType converter;
		type.accept(converter);

		return Ast::Type(
			Ast::Type::ObjectType(*converter.m_class, std::move(converter.m_params), converter.m_loc),
			converter.m_loc
		);
	}
};

Ast::Type substitute_parameters(const Ast::Type::ObjectType& src, const Ast::Type& dest)
{
	const auto& for_class = src.of_class();
	auto it = for_class.pools_begin();

	std::unordered_map<const Ast::Pool*, const Ast::PoolParameter*> map;
	for (const auto& e: src) {
		map[&it->get()] = &e;
		it++;
	}

	const auto* object_type = dest.as_object_type();
	if (object_type == nullptr) {
		return dest;
	}

	std::vector<Ast::PoolParameter> params;
	for (auto e: *object_type) {
		if (e.as_pool() == nullptr) {
			params.emplace_back(*e.as_none(), e.loc());
		} else {
			params.emplace_back(*e.as_pool(), e.loc());
		}
	}

	return Ast::Type(
		Ast::Type::ObjectType(for_class, std::move(params), dest.loc()),
		dest.loc()
	);
}

class MethodBodiesCollector final: public Cst::DefaultVisitor
{
	Ast::Program& m_ast;
	Ast::SemanticErrorList& m_errors;

	BlockStack m_blocks;
	SymbolTable m_symtab;

	Ast::Expr m_res_expr;
	Ast::Type m_res_type;
	Ast::PoolType m_res_pool_type;

	Ast::Class* m_curr_class = nullptr;
	Ast::Method* m_curr_method = nullptr;

	std::vector<Ast::PoolParameter> m_pool_params;

	unsigned loop_nesting_count = 0;

	explicit MethodBodiesCollector(Ast::Program& ast, Ast::SemanticErrorList& errors)
		: m_ast(ast)
		, m_errors(errors)
	{}

	Ast::Type::ObjectType make_this_type()
	{
		std::vector<Ast::PoolParameter> params;
		for (auto it = m_curr_class->pools_begin(); it != m_curr_class->pools_end(); it++) {
			const auto& ref = it->get();
			params.emplace_back(Ast::PoolParameter::PoolRef(ref), ref.loc());
		}

		return Ast::Type::ObjectType(*m_curr_class, std::move(params), Location());
	}

public:
	using Cst::DefaultVisitor::visit;

	void visit(const Cst::PoolParameter::Pool& e) override
	{
		auto* pool = m_symtab.find_pool(e.ident());
		if (pool == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.ident(),
				Ast::ErrorKind::POOL,
				e.loc()
			);
			m_pool_params.emplace_back(
				Ast::PoolParameter(Ast::PoolParameter::None(), e.loc())
			);
			return;
		}

		m_pool_params.emplace_back(
			Ast::PoolParameter(Ast::PoolParameter::PoolRef(*pool), e.loc())
		);
	}

	void visit(const Cst::PoolParameter::None& e) override
	{
		m_pool_params.emplace_back(
			Ast::PoolParameter(Ast::PoolParameter::None(), e.loc())
		);
	}

	void visit(const Cst::Type::PrimitiveType& e) override
	{
		m_res_type = Ast::Type(
			Ast::Type::PrimitiveType(to_kind(e.kind())), e.loc()
		);
	}

	void visit(const Cst::Type::ObjectType& e) override
	{
		const auto& name = e.class_name();
		auto* for_class = m_ast.find_class(name.ident());
		if (for_class == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				name.ident(),
				Ast::ErrorKind::CLASS,
				name.loc()
			);
			return;
		}

		visit(e.begin(), e.end());
		Ast::Type type(
			Ast::Type::ObjectType(*for_class, std::move(m_pool_params), e.loc()),
			e.loc()
		);

		if (!TypeValidator::validate(type, m_errors)) {
			m_res_type = Ast::Type();
		} else {
			m_res_type = std::move(type);
		}
	}

	void visit(const Cst::LayoutType& e) override
	{
		const auto& name = e.layout_name();
		auto* layout = m_ast.find_layout(name.ident());
		if (layout == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				name.ident(),
				Ast::ErrorKind::LAYOUT,
				name.loc()
			);
			return;
		}

		visit(e.begin(), e.end());
		Ast::PoolType type(
			Ast::PoolType::LayoutType(*layout, std::move(m_pool_params), e.loc()),
			e.loc()
		);

		if (!TypeValidator::validate(type, m_errors)) {
			m_res_pool_type = Ast::PoolType();
		} else {
			m_res_pool_type = std::move(type);
		}
	}

	void visit(const Cst::VariableDeclaration& e) override
	{
		e.type().accept(*this);
		auto type = std::move(m_res_type);
		if (!type.valid()) {
			return;
		}

		const auto& name = e.name();
		const auto& var = m_curr_method->add_variable(
			name.ident(), std::move(type), name.loc()
		);

		m_symtab.add_variable(name.ident(), var);
	}

	void visit(const Cst::Stmt::PoolDeclarations& e) override
	{
		for (auto& pool: e) {
			const auto& name = pool.name();
			auto& pool_ref = m_curr_method->add_pool(name.ident(), name.loc());
			if (!m_symtab.add_pool(name.ident(), pool_ref)) {
				m_errors.add<SemanticError::DuplicateDefinition>(
					name.ident(),
					Ast::ErrorKind::POOL,
					name.loc(),
					m_symtab.find_pool(name.ident())->loc()
				);
				continue;
			}
		}

		Cst::DefaultVisitor::visit(e);
	}

	void visit(const Cst::PoolDeclaration& e) override
	{
		e.type().accept(*this);
		auto type = std::move(m_res_pool_type);
		if (!type.valid()) {
			return;
		}

		const auto& name = e.name();
		auto* pool = m_symtab.find_pool(name.ident());

		pool->set_type(std::move(type));
	}

	void visit(const Cst::Expr::IntegerConst& e) override
	{
		unsigned long long value = 0;

		try {
			value = std::stoull(e.value());
		} catch (...) {
			m_errors.add<SemanticError::IntegerOutOfBounds>(e.loc());
			m_res_expr = Ast::Expr(
				Ast::Expr::IntegerConst(0),
				Ast::Type(Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::U64), e.loc()),
				e.loc()
			);
			return;
		}

		if (value > UINT64_MAX) {
			m_errors.add<SemanticError::IntegerOutOfBounds>(e.loc());
			m_res_expr = Ast::Expr(
				Ast::Expr::IntegerConst(0),
				Ast::Type(Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::U64), e.loc()),
				e.loc()
			);
			return;
		}

		m_res_expr = Ast::Expr(
			Ast::Expr::IntegerConst(value),
			Ast::Type(Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::U64), e.loc()),
			e.loc()
		);
	}

	void visit(const Cst::Expr::BooleanConst& e) override
	{
		m_res_expr = Ast::Expr(
			Ast::Expr::BooleanConst(e.value()),
			Ast::Type(Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL), e.loc()),
			e.loc()
		);
	}

	void visit(const Cst::Expr::Null& e) override
	{
		m_res_expr = Ast::Expr(
			Ast::Expr::Null(),
			Ast::Type(Ast::Type::NullType(), e.loc()),
			e.loc()
		);
	}

	void visit(const Cst::Expr::This& e) override
	{
		m_res_expr = Ast::Expr(
			Ast::Expr::This(make_this_type()),
			Ast::Type(make_this_type(), e.loc()),
			e.loc()
		);
	}

	void visit(const Cst::Expr::Cast& e) override
	{
		e.expr().accept(*this);
		e.type().accept(*this);
		auto expr = std::move(m_res_expr);
		m_res_expr = Ast::Expr();

		if (!expr.valid()) {
			return;
		}

		const auto* type = expr.type().as_primitive_type();
		if (type == nullptr) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				e.loc(),
				TypePrinter::to_string(expr.type())
			);
			return;
		}

		m_res_expr = Ast::Expr(
			Ast::Expr::Cast(std::move(expr), *type),
			Ast::Type(*type, e.loc()),
			e.loc()
		);
	}

	void visit(const Cst::Expr::Binary& e) override
	{
		e.lhs().accept(*this);
		auto lhs = std::move(m_res_expr);

		e.rhs().accept(*this);
		auto rhs = std::move(m_res_expr);

		m_res_expr = Ast::Expr();
		if (!lhs.valid() || !rhs.valid()) {
			return;
		}

		auto* lhs_primitive_type = lhs.type().as_primitive_type();
		if (lhs_primitive_type == nullptr) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				e.lhs().loc(), TypePrinter::to_string(lhs.type())
			);
			return;
		}

		auto* rhs_primitive_type = rhs.type().as_primitive_type();
		if (rhs_primitive_type == nullptr) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				e.rhs().loc(), TypePrinter::to_string(rhs.type())
			);
			return;
		}

		auto op = to_ast_binop(e.op());

		auto type = lhs.type();

		switch (op) {
		case Ast::Expr::BinOp::PLUS:
		case Ast::Expr::BinOp::MINUS:
		case Ast::Expr::BinOp::TIMES:
		case Ast::Expr::BinOp::DIV:
		case Ast::Expr::BinOp::LT:
		case Ast::Expr::BinOp::LE:
		case Ast::Expr::BinOp::GT:
		case Ast::Expr::BinOp::GE: {
			if (lhs.type() != rhs.type()) {
				m_errors.add<Ast::SemanticError::IncorrectType>(
					rhs.loc(),
					TypePrinter::to_string(lhs.type()),
					TypePrinter::to_string(rhs.type())
				);
				return;
			}

			if (!lhs_primitive_type->is_integer() && !lhs_primitive_type->is_floating_point()) {
				m_errors.add<SemanticError::ExpectedNumericType>(
					e.loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}
			break;
		}

		case Ast::Expr::BinOp::LAND:
		case Ast::Expr::BinOp::LOR: {
			if (lhs.type() != rhs.type()) {
				m_errors.add<Ast::SemanticError::IncorrectType>(
					rhs.loc(),
					TypePrinter::to_string(lhs.type()),
					TypePrinter::to_string(rhs.type())
				);
				return;
			}

			if (!lhs_primitive_type->is_boolean()) {
				m_errors.add<SemanticError::ExpectedBooleanType>(
					e.loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}
			break;
		}
		case Ast::Expr::BinOp::AND:
		case Ast::Expr::BinOp::OR:
		case Ast::Expr::BinOp::XOR: {
			if (lhs.type() != rhs.type()) {
				m_errors.add<Ast::SemanticError::IncorrectType>(
					rhs.loc(),
					TypePrinter::to_string(lhs.type()),
					TypePrinter::to_string(rhs.type())
				);
				return;
			}

			if (!lhs_primitive_type->is_integer()) {
				m_errors.add<SemanticError::ExpectedIntegerType>(
					e.loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}
			break;
		}

		case Ast::Expr::BinOp::EQ:
		case Ast::Expr::BinOp::NE: {
			type = Ast::Type(
				Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL), e.loc()
			);
			auto lhs_is_null = lhs.type().as_null_type() != nullptr;
			auto rhs_is_null = rhs.type().as_null_type() != nullptr;

			auto lhs_is_object = lhs.type().as_object_type() != nullptr;
			auto rhs_is_object = rhs.type().as_object_type() != nullptr;

			if (lhs_is_null || rhs_is_null || lhs_is_object || rhs_is_object) {
				if (lhs_is_null || rhs_is_null) {
					// Allow all kinds of comparisons with the null pointer constant
					break;
				}
			}

			if (lhs.type() != rhs.type()) {
				m_errors.add<Ast::SemanticError::IncorrectType>(
					rhs.loc(),
					TypePrinter::to_string(lhs.type()),
					TypePrinter::to_string(rhs.type())
				);
				return;
			}
			break;
		}

		case Ast::Expr::BinOp::SHL:
		case Ast::Expr::BinOp::SHR: {
			if (!lhs_primitive_type->is_integer()) {
				m_errors.add<SemanticError::ExpectedIntegerType>(
					lhs.loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}

			if (!rhs_primitive_type->is_integer()) {
				m_errors.add<SemanticError::ExpectedIntegerType>(
					rhs.loc(), TypePrinter::to_string(rhs.type())
				);
				return;
			}
			break;
		}

		}

		m_res_expr = Ast::Expr(
			Ast::Expr::Binary(std::move(lhs), op, std::move(rhs)),
			std::move(type),
			e.loc()
		);
	}

	void visit(const Cst::Expr::VariableExpr& e) override
	{
		const auto& name = e.name();
		auto* var = m_symtab.find_variable(name.ident());
		if (var != nullptr) {
			m_res_expr = Ast::Expr(Ast::Expr::VariableExpr(*var), var->type(), e.loc());
			return;
		}

		const auto* field = m_curr_class->find_field(name.ident());
		if (field != nullptr) {
			Cst::Expr::This made_up_this_expr(e.loc());
			made_up_this_expr.accept(*this);
			m_res_expr = Ast::Expr(
				Ast::Expr::FieldAccess(std::move(m_res_expr), *field),
				field->type(),
				e.loc()
			);
			return;
		}

		m_errors.add<SemanticError::MissingDefinition>(
			name.ident(),
			Ast::ErrorKind::VARIABLE,
			name.loc()
		);
		m_res_expr = Ast::Expr();
	}

	void visit(const Cst::Expr::Unary& e) override
	{
		e.expr().accept(*this);
		auto expr = std::move(m_res_expr);
		m_res_expr = Ast::Expr();
		if (!expr.valid()) {
			return;
		}

		const auto* primitive_type = expr.type().as_primitive_type();
		if (primitive_type == nullptr) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				e.loc(), TypePrinter::to_string(expr.type())
			);
			return;
		}

		if ((e.op() == Cst::UnOp::PLUS || e.op() == Cst::UnOp::MINUS)
				&& primitive_type->is_boolean()) {
			m_errors.add<Ast::SemanticError::IncorrectType>(
				m_curr_class->pools_begin()->get().type().loc(),
				TypePrinter::to_string(*primitive_type),
				TypePrinter::to_string(
					Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL)
				)
			);
			return;
		}

		Ast::Expr::UnOp op;
		switch (e.op()) {
		case Cst::UnOp::PLUS:
			op = Ast::Expr::UnOp::PLUS;
			break;

		case Cst::UnOp::MINUS:
			op = Ast::Expr::UnOp::MINUS;
			break;

		case Cst::UnOp::NOT:
			op = Ast::Expr::UnOp::NOT;
			break;
		}

		m_res_expr = Ast::Expr(
			Ast::Expr::Unary(op, std::move(expr)),
			expr.type(),
			e.loc()
		);
	}

	void visit(const Cst::Expr::IndexExpr& e) override
	{
		e.idx().accept(*this);
		auto expr = std::move(m_res_expr);
		m_res_expr = Ast::Expr();
		if (!expr.valid()) {
			return;
		}

		const auto* primitive_type = expr.type().as_primitive_type();
		if (primitive_type == nullptr || !primitive_type->is_integer()) {
			m_errors.add<Ast::SemanticError::ExpectedIntegerType>(
				expr.loc(), TypePrinter::to_string(expr.type())
			);
			return;
		}

		const auto& pool_name = e.pool();
		const auto* pool = m_symtab.find_pool(pool_name.ident());
		if (pool == nullptr) {
			m_errors.add<Ast::SemanticError::MissingDefinition>(
				pool_name.ident(), Ast::ErrorKind::POOL, expr.loc()
			);
			return;
		}

		auto type = PoolTypeToObjectType::convert(pool->type());

		m_res_expr = Ast::Expr(
			Ast::Expr::IndexExpr(*pool, std::move(expr)),
			std::move(type),
			e.loc()
		);
	}

	void visit(const Cst::Expr::MethodCall& e) override
	{
		const auto& name = e.name();
		const auto* method = m_curr_class->find_method(name.ident());

		if (method == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.name().ident(),
				Ast::ErrorKind::METHOD,
				e.name().loc()
			);
			m_res_expr = Ast::Expr();
			return;
		}

		if (method->num_params() != e.num_args()) {
			m_errors.add<SemanticError::IncorrectArgsNumber>(
				e.name().loc(),
				method->num_params(),
				e.num_args()
			);
			m_res_expr = Ast::Expr();
			return;
		}

		std::vector<Ast::Expr> args;

		auto param_it = method->params_begin();
		auto expr_it = e.begin();

		for (; param_it != method->params_end(); param_it++, expr_it++) {
			expr_it->accept(*this);
			auto expr = std::move(m_res_expr);
			if (!expr.valid()) {
				m_res_expr = Ast::Expr();
				return;
			}

			if (!expr.type().assignable_to(param_it->type())) {
				m_errors.add<Ast::SemanticError::NonAssignableType>(
					expr.loc(),
					TypePrinter::to_string(expr.type()),
					TypePrinter::to_string(param_it->type())
				);
				m_res_expr = Ast::Expr();
				return;
			}

			args.emplace_back(std::move(expr));
		}

		auto type = method->return_type() != nullptr
			? *method->return_type()
			: Ast::Type();

		Ast::Expr this_expr(
			Ast::Expr::This(make_this_type()),
			Ast::Type(make_this_type(), e.loc()),
			e.loc()
		);

		m_res_expr = Ast::Expr(
			Ast::Expr::MethodCall(*method, std::move(this_expr), std::move(args)),
			std::move(type),
			e.loc()
		);
	}

	void visit(const Cst::Expr::MemberMethodCall& e) override
	{
		e.this_expr().accept(*this);
		auto expr = std::move(m_res_expr);
		m_res_expr = Ast::Expr();
		if (!m_res_expr.valid()) {
			return;
		}

		auto* object_type = expr.type().as_object_type();
		if (object_type == nullptr) {
			m_errors.add<SemanticError::ExpectedObjectType>(
				expr.loc(),
				TypePrinter::to_string(expr.type())
			);
			return;
		}

		const auto& name = e.method_name();
		const auto* method = object_type->of_class().find_method(name.ident());
		if (method == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				name.ident(),
				Ast::ErrorKind::METHOD,
				name.loc()
			);
			return;
		}

		if (method->num_params() != e.num_args()) {
			m_errors.add<SemanticError::IncorrectArgsNumber>(
				name.loc(),
				method->num_params(),
				e.num_args()
			);
			m_res_expr = Ast::Expr();
			return;
		}

		std::vector<Ast::Expr> args;
		for (auto& arg: e) {
			arg.accept(*this);
		}

		auto return_type = (method->return_type() == nullptr)
			? Ast::Type(Ast::Type::VoidType())
			: substitute_parameters(*object_type, *method->return_type());

		m_res_expr = Ast::Expr(
			Ast::Expr::MethodCall(*method, std::move(expr), std::move(args)),
			return_type,
			e.loc()
		);
	}

	void visit(const Cst::Expr::FieldAccess& e) override
	{
		e.expr().accept(*this);
		auto expr = std::move(m_res_expr);
		m_res_expr = Ast::Expr();
		if (!m_res_expr.valid()) {
			return;
		}

		auto* object_type = expr.type().as_object_type();
		if (object_type == nullptr) {
			m_errors.add<SemanticError::ExpectedObjectType>(
				expr.loc(),
				TypePrinter::to_string(expr.type())
			);
			return;
		}

		const auto& name = e.field();
		const auto* field = object_type->of_class().find_field(name.ident());
		if (field == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				name.ident(),
				Ast::ErrorKind::FIELD,
				name.loc()
			);
			return;
		}

		m_res_expr = Ast::Expr(
			Ast::Expr::FieldAccess(std::move(expr), *field),
			substitute_parameters(*object_type, field->type()),
			e.loc()
		);
	}

	void visit(const Cst::Expr::New& e) override
	{
		e.type().accept(*this);
		auto type = std::move(m_res_type);
		m_res_type = Ast::Type();

		if (!type.valid()) {
			return;
		}

		m_res_expr = Ast::Expr(
			Ast::Expr::New(*type.as_object_type()),
			Ast::Type(*type.as_object_type(), e.type().loc()),
			e.loc()
		);
	}

	void visit(const Cst::Stmt::Assignment& e) override
	{
		e.lhs().accept(*this);
		auto lhs = std::move(m_res_expr);

		e.rhs().accept(*this);
		auto rhs = std::move(m_res_expr);

		m_res_expr = Ast::Expr();

		if (!lhs.valid() || !rhs.valid()) {
			return;
		}

		if (!lhs.is_lvalue()) {
			m_errors.add<Ast::SemanticError::NotLvalue>(lhs.loc());
			return;
		}

		if (!rhs.type().assignable_to(lhs.type())) {
			m_errors.add<Ast::SemanticError::NonAssignableType>(
				rhs.loc(),
				TypePrinter::to_string(rhs.type()),
				TypePrinter::to_string(lhs.type())
			);
			return;
		}

		m_blocks.add<Ast::Stmt::Assignment>(
			std::move(lhs), std::move(rhs)
		);
	}

	void visit(const Cst::Stmt::OpAssignment& e) override
	{
		e.lhs().accept(*this);
		auto lhs = std::move(m_res_expr);

		e.rhs().accept(*this);
		auto rhs = std::move(m_res_expr);

		m_res_expr = Ast::Expr();

		if (!lhs.valid() || !rhs.valid()) {
			return;
		}

		if (!lhs.is_lvalue()) {
			m_errors.add<Ast::SemanticError::NotLvalue>(lhs.loc());
			return;
		}

		const auto* lhs_primitive_type = lhs.type().as_primitive_type();
		if (lhs_primitive_type == nullptr) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				lhs.loc(), TypePrinter::to_string(lhs.type())
			);
			return;
		}

		const auto* rhs_primitive_type = rhs.type().as_primitive_type();
		if (rhs_primitive_type == nullptr) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				rhs.loc(), TypePrinter::to_string(rhs.type())
			);
			return;
		}

		auto op = to_ast_binop(e.op());

		switch (op) {
		case Ast::Expr::BinOp::PLUS:
		case Ast::Expr::BinOp::MINUS:
		case Ast::Expr::BinOp::TIMES:
		case Ast::Expr::BinOp::DIV: {
			if (lhs.type() != rhs.type()) {
				m_errors.add<Ast::SemanticError::IncorrectType>(
					rhs.loc(),
					TypePrinter::to_string(lhs.type()),
					TypePrinter::to_string(rhs.type())
				);
				return;
			}

			if (!lhs_primitive_type->is_integer() && !lhs_primitive_type->is_floating_point()) {
				m_errors.add<SemanticError::ExpectedNumericType>(
					e.lhs().loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}
			break;
		}

		case Ast::Expr::BinOp::AND:
		case Ast::Expr::BinOp::OR:
		case Ast::Expr::BinOp::XOR: {
			if (lhs.type() != rhs.type()) {
				m_errors.add<Ast::SemanticError::IncorrectType>(
					rhs.loc(),
					TypePrinter::to_string(lhs.type()),
					TypePrinter::to_string(rhs.type())
				);
				return;
			}

			if (!lhs_primitive_type->is_integer()) {
				m_errors.add<SemanticError::ExpectedIntegerType>(
					e.lhs().loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}
			break;
		}

		case Ast::Expr::BinOp::SHL:
		case Ast::Expr::BinOp::SHR: {
			if (!lhs_primitive_type->is_integer()) {
				m_errors.add<SemanticError::ExpectedIntegerType>(
					lhs.loc(), TypePrinter::to_string(lhs.type())
				);
				return;
			}

			if (!rhs_primitive_type->is_integer()) {
				m_errors.add<SemanticError::ExpectedIntegerType>(
					rhs.loc(), TypePrinter::to_string(rhs.type())
				);
				return;
			}
			break;

		default:
			// Never occurs
			assert(false);
		}

		}

		m_blocks.add<Ast::Stmt::OpAssignment>(
			std::move(lhs), op, std::move(rhs)
		);
	}

	void visit(const Cst::Stmt::If& e) override
	{
		e.cond().accept(*this);
		auto cond = std::move(m_res_expr);
		m_res_expr = Ast::Expr();
		if (!cond.valid()) {
			cond = Ast::Expr(
				Ast::Expr::BooleanConst(false),
				Ast::Type(
					Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL),
					e.cond().loc()
				),
				e.cond().loc()
			);
		}

		const auto* primitive_type = cond.type().as_primitive_type();
		if (primitive_type == nullptr || primitive_type->is_boolean()) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				cond.loc(), TypePrinter::to_string(cond.type())
			);
			cond = Ast::Expr(
				Ast::Expr::BooleanConst(false),
				Ast::Type(
					Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL),
					e.cond().loc()
				),
				e.cond().loc()
			);
		}

		m_symtab.push_scope();
		m_blocks.push_block();
		e.then_branch().accept(*this);
		auto then_branch = m_blocks.pop_block();
		m_symtab.pop_scope();

		m_symtab.push_scope();
		m_blocks.push_block();
		e.else_branch().accept(*this);
		auto else_branch = m_blocks.pop_block();
		m_symtab.pop_scope();

		m_blocks.add<Ast::Stmt::If>(
			std::move(cond), std::move(then_branch), std::move(else_branch)
		);
	}

	void visit(const Cst::Stmt::While& e) override
	{
		e.cond().accept(*this);
		auto cond = std::move(m_res_expr);
		m_res_expr = Ast::Expr();
		if (!cond.valid()) {
			cond = Ast::Expr(
				Ast::Expr::BooleanConst(false),
				Ast::Type(
					Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL),
					e.cond().loc()
				),
				e.cond().loc()
			);
		}

		const auto* primitive_type = cond.type().as_primitive_type();
		if (primitive_type == nullptr || primitive_type->is_boolean()) {
			m_errors.add<Ast::SemanticError::ExpectedPrimitiveType>(
				cond.loc(), TypePrinter::to_string(cond.type())
			);
			cond = Ast::Expr(
				Ast::Expr::BooleanConst(false),
				Ast::Type(
					Ast::Type::PrimitiveType(Ast::Type::PrimitiveKind::BOOL),
					e.cond().loc()
				),
				e.cond().loc()
			);
		}

		m_symtab.push_scope();
		m_blocks.push_block();
		e.body().accept(*this);
		auto body = m_blocks.pop_block();
		m_symtab.pop_scope();

		m_blocks.add<Ast::Stmt::While>(std::move(cond), std::move(body));
	}

	void visit(const Cst::Stmt::ForeachRange& e) override
	{
		e.range_begin().accept(*this);
		auto range_begin = std::move(m_res_expr);

		e.range_end().accept(*this);
		auto range_end = std::move(m_res_expr);

		if (!range_begin.valid() || !range_end.valid()) {
			return;
		}

		const auto* primitive_type = range_begin.type().as_primitive_type();
		if (primitive_type == nullptr || !primitive_type->is_integer()) {
			m_errors.add<Ast::SemanticError::ExpectedIntegerType>(
				range_begin.loc(), TypePrinter::to_string(range_begin.type())
			);
			return;
		}

		if (range_begin.type() != range_end.type()) {
			m_errors.add<Ast::SemanticError::IncorrectType>(
				range_end.loc(),
				TypePrinter::to_string(range_begin.type()),
				TypePrinter::to_string(range_end.type())
			);
			return;
		}

		m_symtab.push_scope();
		m_blocks.push_block();

		auto& var = m_curr_method->add_variable(
			e.var().ident(), range_begin.type(), e.var().loc()
		);
		m_symtab.add_variable(e.var().ident(), var);

		e.body().accept(*this);

		auto stmts = m_blocks.pop_block();
		m_symtab.pop_scope();

		m_blocks.add<Ast::Stmt::ForeachRange>(
			var, std::move(range_begin), std::move(range_end), std::move(stmts)
		);
	}

	void visit(const Cst::Stmt::ForeachPool& e) override
	{
		const auto* pool = m_symtab.find_pool(e.pool().ident());
		if (pool == nullptr) {
			m_errors.add<SemanticError::MissingDefinition>(
				e.pool().ident(),
				Ast::ErrorKind::POOL,
				e.pool().loc()
			);
			return;
		}

		m_symtab.push_scope();
		m_blocks.push_block();

		auto& var = m_curr_method->add_variable(
			e.var().ident(),
			PoolTypeToObjectType::convert(pool->type()),
			e.var().loc()
		);
		m_symtab.add_variable(e.var().ident(), var);

		e.body().accept(*this);

		auto stmts = m_blocks.pop_block();
		m_symtab.pop_scope();

		m_blocks.add<Ast::Stmt::ForeachPool>(
			var, *pool, std::move(stmts)
		);
	}

	void visit(const Cst::Stmt::Block& e) override
	{
		m_symtab.push_scope();
		m_blocks.push_block();

		visit(e.begin(), e.end());

		auto stmts = m_blocks.pop_block();
		m_blocks.add(stmts.begin(), stmts.end());
		m_symtab.pop_scope();
	}

	void visit(const Cst::Stmt::ExprStmt& e) override
	{
		e.expr().accept(*this);
		auto expr = std::move(m_res_expr);
		if (!expr.valid()) {
			return;
		}

		m_blocks.add<Ast::Stmt::ExprStmt>(std::move(expr));
	}

	void visit(const Cst::Stmt::Break& e) override
	{
		if (loop_nesting_count == 0) {
			m_errors.add<SemanticError::NotInsideLoop>(e.loc());
			return;
		}

		m_blocks.add<Ast::Stmt::Break>();
	}

	void visit(const Cst::Stmt::Continue& e) override
	{
		if (loop_nesting_count == 0) {
			m_errors.add<SemanticError::NotInsideLoop>(e.loc());
			return;
		}

		m_blocks.add<Ast::Stmt::Continue>();
	}

	void visit(const Cst::Stmt::Return& e) override
	{
		e.expr().accept(*this);
		auto expr = std::move(m_res_expr);
		if (!m_res_expr.valid()) {
			return;
		}

		if (m_curr_method->return_type() == nullptr) {
			m_errors.add<Ast::SemanticError::ReturnWithExpression>(e.expr().loc());
			return;
		}

		if (!expr.type().assignable_to(*m_curr_method->return_type())) {
			m_errors.add<Ast::SemanticError::NonAssignableType>(
				e.expr().loc(),
				TypePrinter::to_string(expr.type()),
				TypePrinter::to_string(*m_curr_method->return_type())
			);
			return;
		}
	}

	void visit(const Cst::Stmt::ReturnVoid& e) override
	{
		if (m_curr_method->return_type() != nullptr) {
			m_errors.add<Ast::SemanticError::ReturnWithoutExpression>(e.loc());
		}

		m_blocks.add<Ast::Stmt::Return>();
	}

	void visit(const Cst::Method& e) override
	{
		m_curr_method = m_curr_class->find_method(e.name().ident());
		for (auto it = m_curr_method->params_begin(); it != m_curr_method->params_end(); it++) {
			m_symtab.add_variable(it->name(), *it);
		}

		m_symtab.push_scope();
		m_blocks.push_block();

		Cst::DefaultVisitor::visit(e);
		m_curr_method->set_body(m_blocks.pop_block());
		m_symtab.pop_scope();

		m_curr_class = nullptr;
	}

	void visit(const Cst::Class& e) override
	{
		m_curr_class = m_ast.find_class(e.name().ident());
		m_symtab.set_class(*m_curr_class);

		m_symtab.push_scope();
		for (auto it = m_curr_class->pools_begin(); it != m_curr_class->pools_end(); it++) {
			auto& ref = it->get();
			m_symtab.add_pool(ref.name(), ref);
		}

		Cst::DefaultVisitor::visit(e);

		m_curr_class = nullptr;
	}

	static void collect(const Cst::Program& cst, Ast::Program& ast, Ast::SemanticErrorList& errors)
	{
		MethodBodiesCollector collector(ast, errors);
		cst.accept(collector);
	}
};

void Ast::run_semantic_analysis(const Cst::Program& cst, Ast::Program& ast, Ast::SemanticErrorList& errors)
{
	Ast::Program program;

	ClassLayoutMembersCollector::collect(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	MemberTypesCollector::collect(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	MethodBodiesCollector::collect(cst, ast, errors);
	if (errors.has_errors()) {
		return;
	}

	ast = std::move(program);
}
