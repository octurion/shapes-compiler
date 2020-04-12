#pragma once

#include "parse_tree_common.h"

#include "ast.h"
#include "cst.h"

#include <utility>

namespace Ast
{

enum class ErrorKind { CLASS, FIELD, POOL, POOL_BOUND, VARIABLE, TYPE, LAYOUT, METHOD };

class SemanticError
{
public:
	class DuplicateDefinition: public BaseVisitable<DuplicateDefinition>
	{
		std::string m_name;
		ErrorKind m_kind;
		Location m_loc;
		Location m_existing_loc;

	public:
		explicit DuplicateDefinition(
				std::string name,
				ErrorKind kind,
				const Location& loc,
				const Location& existing_loc)
			: m_name(std::move(name))
			, m_kind(kind)
			, m_loc(loc)
			, m_existing_loc(existing_loc)
		{}

		const std::string& name() const { return m_name; }
		ErrorKind kind() const { return m_kind; }
		const Location& loc() const { return m_loc; }
		const Location& existing_loc() const { return m_existing_loc; }
	};

	class MissingDefinition: public BaseVisitable<MissingDefinition>
	{
		std::string m_name;
		ErrorKind m_kind;
		Location m_loc;

	public:
		explicit MissingDefinition(
				std::string name,
				ErrorKind kind,
				const Location& loc)
			: m_name(std::move(name))
			, m_kind(kind)
			, m_loc(loc)
		{}

		const std::string& name() const { return m_name; }
		ErrorKind kind() const { return m_kind; }
		const Location& loc() const { return m_loc; }
	};

	class MissingBound: public BaseVisitable<MissingBound>
	{
		std::string m_pool_name;
		Location m_loc;

	public:
		explicit MissingBound(std::string pool_name, const Location& loc)
			: m_pool_name(pool_name)
			, m_loc(loc)
		{}

		const std::string& pool_name() const { return m_pool_name; }
		const Location& loc() const { return m_loc; }
	};

	class LayoutMissingField: public BaseVisitable<LayoutMissingField>
	{
		std::string m_layout_name;
		std::string m_field_name;
		Location m_layout_loc;

	public:
		explicit LayoutMissingField(
				std::string layout_name,
				std::string field_name,
				const Location& layout_loc)
			: m_layout_name(std::move(layout_name))
			, m_field_name(std::move(field_name))
			, m_layout_loc(layout_loc)
		{}

		const std::string& layout_name() const { return m_layout_name; }
		const std::string& field_name() const { return m_field_name; }
		const Location& layout_loc() const { return m_layout_loc; }
	};

	class LayoutDuplicateField: public BaseVisitable<LayoutDuplicateField>
	{
		std::string m_layout_name;
		Location m_layout_loc;

		std::string m_field_name;
		Location m_field_loc;

		Location m_existing_field_loc;

	public:
		explicit LayoutDuplicateField(
				std::string layout_name,
				const Location& layout_loc,
				std::string field_name,
				const Location& field_loc,
				const Location& existing_field_loc)
			: m_layout_name(std::move(layout_name))
			, m_layout_loc(layout_loc)
			, m_field_name(std::move(field_name))
			, m_field_loc(field_loc)
			, m_existing_field_loc(existing_field_loc)
		{}

		const std::string& layout_name() const { return m_layout_name; }
		const Location& layout_loc() const { return m_layout_loc; }

		const std::string& field_name() const { return m_field_name; }
		const Location& field_loc() const { return m_field_loc; }

		const Location& existing_field_loc() const { return m_existing_field_loc; }
	};

	class NoPoolParameters: public BaseVisitable<NoPoolParameters>
	{
		std::string m_name;
		Location m_loc;

	public:
		explicit NoPoolParameters(std::string name, const Location& loc)
			: m_name(name)
			, m_loc(loc)
		{}

		const std::string& name() const { return m_name; }
		const Location& loc() const { return m_loc; }
	};

	class EmptyCluster: public BaseVisitable<EmptyCluster>
	{
		Location m_loc;

	public:
		explicit EmptyCluster(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class NotInsideLoop: public BaseVisitable<NotInsideLoop>
	{
		Location m_loc;

	public:
		explicit NotInsideLoop(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class IntegerOutOfBounds: public BaseVisitable<IntegerOutOfBounds>
	{
		Location m_loc;

	public:
		explicit IntegerOutOfBounds(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class IncorrectFirstPoolParameter: public BaseVisitable<IncorrectFirstPoolParameter>
	{
		Location m_loc;

		std::string m_expected;
		std::string m_got;

	public:
		explicit IncorrectFirstPoolParameter(
				const Location& loc,
				std::string expected,
				std::string got)
			: m_loc(loc)
			, m_expected(std::move(expected))
			, m_got(std::move(got))
		{}

		const Location& loc() const { return m_loc; }
		const std::string& expected() const { return m_expected; }
		const std::string& got() const { return m_got; }
	};

	class IncorrectType: public BaseVisitable<IncorrectType>
	{
		Location m_loc;

		std::string m_expected_type;
		std::string m_got_type;

	public:
		explicit IncorrectType(
				const Location& loc,
				std::string expected_type,
				std::string got_type)
			: m_loc(loc)
			, m_expected_type(std::move(expected_type))
			, m_got_type(std::move(got_type))
		{}

		const Location& loc() const { return m_loc; }
		const std::string& expected_type() const { return m_expected_type; }
		const std::string& got_type() const { return m_got_type; }
	};

	class IncompatibleBound: public BaseVisitable<IncompatibleBound>
	{
		Location m_loc;

		std::string m_type;
		std::string m_bound;

	public:
		explicit IncompatibleBound(const Location& loc, std::string type, std::string bound)
			: m_loc(loc)
			, m_type(std::move(type))
			, m_bound(std::move(bound))
		{}

		const Location& loc() const { return m_loc; }
		const std::string& type() const { return m_type; }
		const std::string& bound() const { return m_bound; }
	};

	class IncorrectPoolsNumber: public BaseVisitable<IncorrectPoolsNumber>
	{
		Location m_loc;

		size_t m_num_expected;
		size_t m_num_got;

	public:
		explicit IncorrectPoolsNumber(const Location& loc,
									  size_t num_expected,
									  size_t num_got)
			: m_loc(loc)
			, m_num_expected(num_expected)
			, m_num_got(num_got)
		{}

		const Location& loc() const { return m_loc; }
		size_t num_expected() const { return m_num_expected; }
		size_t num_got() const { return m_num_got; }
	};

	class ExpectedPrimitiveType: public BaseVisitable<ExpectedPrimitiveType>
	{
		Location m_loc;

		std::string m_type_got;

	public:
		explicit ExpectedPrimitiveType(const Location& loc, std::string type_got)
			: m_loc(loc)
			, m_type_got(type_got)
		{}

		const Location& loc() const { return m_loc; }
		const std::string& type_got() const { return m_type_got; }
	};

	class ExpectedObjectType: public BaseVisitable<ExpectedObjectType>
	{
		Location m_loc;

		std::string m_type_got;

	public:
		explicit ExpectedObjectType(const Location& loc, std::string type_got)
			: m_loc(loc)
			, m_type_got(type_got)
		{}

		const Location& loc() const { return m_loc; }
		const std::string& type_got() const { return m_type_got; }
	};

	class ExpectedBooleanType: public BaseVisitable<ExpectedBooleanType>
	{
		Location m_loc;

		std::string m_type_got;

	public:
		explicit ExpectedBooleanType(const Location& loc, std::string type_got)
			: m_loc(loc)
			, m_type_got(type_got)
		{}

		const Location& loc() const { return m_loc; }
		const std::string& type_got() const { return m_type_got; }
	};

	class ExpectedIntegerType: public BaseVisitable<ExpectedIntegerType>
	{
		Location m_loc;

		std::string m_type_got;

	public:
		explicit ExpectedIntegerType(const Location& loc, std::string type_got)
			: m_loc(loc)
			, m_type_got(type_got)
		{}

		const Location& loc() const { return m_loc; }
		const std::string& type_got() const { return m_type_got; }
	};

	class ReturnWithExpression: public BaseVisitable<ReturnWithExpression>
	{
		Location m_loc;

	public:
		explicit ReturnWithExpression(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class ReturnWithoutExpression: public BaseVisitable<ReturnWithoutExpression>
	{
		Location m_loc;

	public:
		explicit ReturnWithoutExpression(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class NonAssignableType: public BaseVisitable<NonAssignableType>
	{
		Location m_loc;

		std::string m_assigned_from;
		std::string m_assigned_to;

	public:
		explicit NonAssignableType(
				const Location& loc,
				std::string assigned_from,
				std::string assigned_to)
			: m_loc(loc)
			, m_assigned_from(std::move(assigned_from))
			, m_assigned_to(std::move(assigned_to))
		{}

		const Location& loc() const { return m_loc; }
		const std::string& assigned_from() const { return m_assigned_from; }
		const std::string& assigned_to() const { return m_assigned_to; }
	};

	class NotLvalue: public BaseVisitable<NotLvalue>
	{
		Location m_loc;

	public:
		explicit NotLvalue(const Location& loc)
			: m_loc(loc)
		{}

		const Location& loc() const { return m_loc; }
	};

	class ExpectedNumericType: public BaseVisitable<ExpectedNumericType>
	{
		Location m_loc;

		std::string m_type_got;

	public:
		explicit ExpectedNumericType(const Location& loc, std::string type_got)
			: m_loc(loc)
			, m_type_got(type_got)
		{}

		const Location& loc() const { return m_loc; }
		const std::string& type_got() const { return m_type_got; }
	};

	class IncorrectArgsNumber: public BaseVisitable<IncorrectArgsNumber>
	{
		Location m_loc;

		size_t m_num_expected;
		size_t m_num_got;

	public:
		explicit IncorrectArgsNumber(const Location& loc,
									 size_t num_expected,
									 size_t num_got)
			: m_loc(loc)
			, m_num_expected(num_expected)
			, m_num_got(num_got)
		{}

		const Location& loc() const { return m_loc; }
		size_t num_expected() const { return m_num_expected; }
		size_t num_got() const { return m_num_got; }
	};

	SemanticError(const SemanticError&) = delete;
	SemanticError& operator=(const SemanticError&) = delete;

	SemanticError(SemanticError&&);
	SemanticError& operator=(SemanticError&&);

	~SemanticError();

private:
	enum class Tag
	{
		DUPLICATE_DEFINITION,
		MISSING_DEFINITION,
		MISSING_BOUND,
		LAYOUT_MISSING_FIELD,
		LAYOUT_DUPLICATE_FIELD,
		NO_POOL_PARAMETERS,
		EMPTY_CLUSTER,
		NOT_INSIDE_LOOP,
		INTEGER_OUT_OF_BOUNDS,
		INCORRECT_FIRST_POOL_PARAMETER,
		INCORRECT_TYPE,
		INCOMPATIBLE_BOUND,
		INCORRECT_POOLS_NUMBER,
		EXPECTED_OBJECT_TYPE,
		EXPECTED_PRIMITIVE_TYPE,
		EXPECTED_BOOLEAN_TYPE,
		EXPECTED_INTEGER_TYPE,
		EXPECTED_NUMERIC_TYPE,
		RETURN_WITH_EXPRESSION,
		RETURN_WITHOUT_EXPRESSION,
		NON_ASSIGNABLE_TYPE,
		NOT_LVALUE,
		INCORRECT_ARGS_NUMBER,
	};

	Tag m_tag;
	union
	{
		DuplicateDefinition m_duplicate_definition;
		MissingDefinition m_missing_definition;
		MissingBound m_missing_bound;
		LayoutMissingField m_layout_missing_field;
		LayoutDuplicateField m_layout_duplicate_field;
		NoPoolParameters m_no_pool_parameters;
		EmptyCluster m_empty_cluster;
		NotInsideLoop m_not_inside_loop;
		IntegerOutOfBounds m_integer_out_of_bounds;
		IncorrectFirstPoolParameter m_incorrect_first_pool_parameter;
		IncorrectType m_incorrect_type;
		IncompatibleBound m_incompatible_bound;
		IncorrectPoolsNumber m_incorrect_pools_number;
		ExpectedObjectType m_expected_object_type;
		ExpectedPrimitiveType m_expected_primitive_type;
		ExpectedBooleanType m_expected_boolean_type;
		ExpectedIntegerType m_expected_integer_type;
		ExpectedNumericType m_expected_numeric_type;
		ReturnWithExpression m_return_with_expression;
		ReturnWithoutExpression m_return_without_expression;
		NonAssignableType m_non_assignable_type;
		NotLvalue m_not_lvalue;
		IncorrectArgsNumber m_incorrect_args_number;
	};

	void destroy_variant();
	void construct_variant_from_other(SemanticError& other);

public:
	SemanticError(DuplicateDefinition duplicate_definition)
		: m_tag(Tag::DUPLICATE_DEFINITION)
		, m_duplicate_definition(std::move(duplicate_definition))
	{}

	SemanticError(MissingDefinition missing_definition)
		: m_tag(Tag::MISSING_DEFINITION)
		, m_missing_definition(std::move(missing_definition))
	{}

	SemanticError(MissingBound missing_bound)
		: m_tag(Tag::MISSING_BOUND)
		, m_missing_bound(std::move(missing_bound))
	{}

	SemanticError(LayoutMissingField layout_missing_field)
		: m_tag(Tag::LAYOUT_MISSING_FIELD)
		, m_layout_missing_field(std::move(layout_missing_field))
	{}

	SemanticError(LayoutDuplicateField layout_duplicate_field)
		: m_tag(Tag::LAYOUT_DUPLICATE_FIELD)
		, m_layout_duplicate_field(std::move(layout_duplicate_field))
	{}

	SemanticError(NoPoolParameters no_pool_parameters)
		: m_tag(Tag::NO_POOL_PARAMETERS)
		, m_no_pool_parameters(std::move(no_pool_parameters))
	{}

	SemanticError(EmptyCluster empty_cluster)
		: m_tag(Tag::EMPTY_CLUSTER)
		, m_empty_cluster(std::move(empty_cluster))
	{}

	SemanticError(NotInsideLoop not_inside_loop)
		: m_tag(Tag::NOT_INSIDE_LOOP)
		, m_not_inside_loop(std::move(not_inside_loop))
	{}

	SemanticError(IntegerOutOfBounds integer_out_of_bounds)
		: m_tag(Tag::INTEGER_OUT_OF_BOUNDS)
		, m_integer_out_of_bounds(std::move(integer_out_of_bounds))
	{}

	SemanticError(IncorrectFirstPoolParameter incorrect_first_pool_parameter)
		: m_tag(Tag::INCORRECT_FIRST_POOL_PARAMETER)
		, m_incorrect_first_pool_parameter(std::move(incorrect_first_pool_parameter))
	{}

	SemanticError(IncorrectType incorrect_type)
		: m_tag(Tag::INCORRECT_TYPE)
		, m_incorrect_type(std::move(incorrect_type))
	{}

	SemanticError(IncompatibleBound incompatible_bound)
		: m_tag(Tag::INCOMPATIBLE_BOUND)
		, m_incompatible_bound(std::move(incompatible_bound))
	{}

	SemanticError(IncorrectPoolsNumber incorrect_pools_number)
		: m_tag(Tag::INCORRECT_POOLS_NUMBER)
		, m_incorrect_pools_number(std::move(incorrect_pools_number))
	{}

	SemanticError(ExpectedObjectType expected_object_type)
		: m_tag(Tag::EXPECTED_OBJECT_TYPE)
		, m_expected_object_type(std::move(expected_object_type))
	{}

	SemanticError(ExpectedPrimitiveType expected_primitive_type)
		: m_tag(Tag::EXPECTED_PRIMITIVE_TYPE)
		, m_expected_primitive_type(std::move(expected_primitive_type))
	{}

	SemanticError(ExpectedNumericType expected_numeric_type)
		: m_tag(Tag::EXPECTED_NUMERIC_TYPE)
		, m_expected_numeric_type(std::move(expected_numeric_type))
	{}

	SemanticError(ExpectedBooleanType expected_boolean_type)
		: m_tag(Tag::EXPECTED_BOOLEAN_TYPE)
		, m_expected_boolean_type(std::move(expected_boolean_type))
	{}

	SemanticError(ExpectedIntegerType expected_integer_type)
		: m_tag(Tag::EXPECTED_INTEGER_TYPE)
		, m_expected_integer_type(std::move(expected_integer_type))
	{}

	SemanticError(NonAssignableType non_assignable_type)
		: m_tag(Tag::NON_ASSIGNABLE_TYPE)
		, m_non_assignable_type(std::move(non_assignable_type))
	{}

	SemanticError(ReturnWithExpression return_with_expression)
		: m_tag(Tag::RETURN_WITH_EXPRESSION)
		, m_return_with_expression(std::move(return_with_expression))
	{}

	SemanticError(ReturnWithoutExpression return_without_expression)
		: m_tag(Tag::RETURN_WITHOUT_EXPRESSION)
		, m_return_without_expression(std::move(return_without_expression))
	{}

	SemanticError(NotLvalue not_lvalue)
		: m_tag(Tag::NOT_LVALUE)
		, m_not_lvalue(std::move(not_lvalue))
	{}

	SemanticError(IncorrectArgsNumber incorrect_args_number)
		: m_tag(Tag::INCORRECT_ARGS_NUMBER)
		, m_incorrect_args_number(std::move(incorrect_args_number))
	{}

	template<typename T>
	void accept(T& visitor) const
	{
		switch (m_tag) {
		case Tag::DUPLICATE_DEFINITION:
			visitor.visit(m_duplicate_definition);
			break;

		case Tag::MISSING_DEFINITION:
			visitor.visit(m_missing_definition);
			break;

		case Tag::MISSING_BOUND:
			visitor.visit(m_missing_bound);
			break;

		case Tag::LAYOUT_MISSING_FIELD:
			visitor.visit(m_layout_missing_field);
			break;

		case Tag::LAYOUT_DUPLICATE_FIELD:
			visitor.visit(m_layout_duplicate_field);
			break;

		case Tag::NO_POOL_PARAMETERS:
			visitor.visit(m_no_pool_parameters);
			break;

		case Tag::EMPTY_CLUSTER:
			visitor.visit(m_empty_cluster);
			break;

		case Tag::NOT_INSIDE_LOOP:
			visitor.visit(m_not_inside_loop);
			break;

		case Tag::INTEGER_OUT_OF_BOUNDS:
			visitor.visit(m_integer_out_of_bounds);
			break;

		case Tag::INCORRECT_FIRST_POOL_PARAMETER:
			visitor.visit(m_incorrect_first_pool_parameter);
			break;

		case Tag::INCORRECT_TYPE:
			visitor.visit(m_incorrect_type);
			break;

		case Tag::INCOMPATIBLE_BOUND:
			visitor.visit(m_incompatible_bound);
			break;

		case Tag::INCORRECT_POOLS_NUMBER:
			visitor.visit(m_incorrect_pools_number);
			break;

		case Tag::EXPECTED_OBJECT_TYPE:
			visitor.visit(m_expected_object_type);
			break;

		case Tag::EXPECTED_PRIMITIVE_TYPE:
			visitor.visit(m_expected_primitive_type);
			break;

		case Tag::EXPECTED_BOOLEAN_TYPE:
			visitor.visit(m_expected_boolean_type);
			break;

		case Tag::EXPECTED_INTEGER_TYPE:
			visitor.visit(m_expected_integer_type);
			break;

		case Tag::EXPECTED_NUMERIC_TYPE:
			visitor.visit(m_expected_numeric_type);
			break;

		case Tag::NON_ASSIGNABLE_TYPE:
			visitor.visit(m_non_assignable_type);
			break;

		case Tag::RETURN_WITH_EXPRESSION:
			visitor.visit(m_return_with_expression);
			break;

		case Tag::RETURN_WITHOUT_EXPRESSION:
			visitor.visit(m_return_without_expression);
			break;

		case Tag::NOT_LVALUE:
			visitor.visit(m_not_lvalue);
			break;

		case Tag::INCORRECT_ARGS_NUMBER:
			visitor.visit(m_incorrect_args_number);
			break;
		}
	}
};

class SemanticErrorList
{
	std::vector<SemanticError> m_errors;

public:
	using iterator = decltype(m_errors)::iterator;
	using const_iterator = decltype(m_errors)::const_iterator;

	const_iterator begin() const { return m_errors.begin(); }
	const_iterator end()   const { return m_errors.end();   }

	iterator begin() { return m_errors.begin(); }
	iterator end()   { return m_errors.end();   }

	bool has_errors() const
	{
		return !m_errors.empty();
	}

	template<typename Error, typename... Args>
	void add(Args&&... args)
	{
		m_errors.emplace_back(Error(std::forward<Args>(args)...));
	}
};

class SemanticErrorVisitor: public BaseVisitor
	, public Visitor<SemanticError::DuplicateDefinition>
	, public Visitor<SemanticError::MissingDefinition>
	, public Visitor<SemanticError::MissingBound>
	, public Visitor<SemanticError::LayoutMissingField>
	, public Visitor<SemanticError::LayoutDuplicateField>
	, public Visitor<SemanticError::NoPoolParameters>
	, public Visitor<SemanticError::EmptyCluster>
	, public Visitor<SemanticError::NotInsideLoop>
	, public Visitor<SemanticError::IntegerOutOfBounds>
	, public Visitor<SemanticError::IncorrectFirstPoolParameter>
	, public Visitor<SemanticError::IncorrectType>
	, public Visitor<SemanticError::IncompatibleBound>
	, public Visitor<SemanticError::IncorrectPoolsNumber>
	, public Visitor<SemanticError::ExpectedObjectType>
	, public Visitor<SemanticError::ExpectedBooleanType>
	, public Visitor<SemanticError::ExpectedPrimitiveType>
	, public Visitor<SemanticError::ExpectedIntegerType>
	, public Visitor<SemanticError::ExpectedNumericType>
	, public Visitor<SemanticError::NonAssignableType>
	, public Visitor<SemanticError::ReturnWithExpression>
	, public Visitor<SemanticError::ReturnWithoutExpression>
	, public Visitor<SemanticError::NotLvalue>
	, public Visitor<SemanticError::IncorrectArgsNumber>
{
};

extern void run_semantic_analysis(const Cst::Program& cst, Ast::Program& ast, SemanticErrorList& errors);

} // namespace Ast
