#pragma once

#include "common.h"

#include "ast.h"
#include "cst.h"

#include "variant.h"

#include <utility>

namespace Ast
{

enum class ErrorKind
{
	CLASS, FIELD, POOL, POOL_BOUND, VARIABLE, TYPE, LAYOUT, METHOD
};

class DuplicateDefinition
{
	std::string m_name;
	ErrorKind m_kind;
	Location m_loc;
	Location m_existing_loc;

public:
	DuplicateDefinition(
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

class MissingDefinition
{
	std::string m_name;
	ErrorKind m_kind;
	Location m_loc;

public:
	MissingDefinition(std::string name, ErrorKind kind, const Location& loc)
		: m_name(std::move(name))
		, m_kind(kind)
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	ErrorKind kind() const { return m_kind; }
	const Location& loc() const { return m_loc; }
};

class MissingBound
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

class LayoutMissingField
{
	std::string m_layout_name;
	std::string m_field_name;

	Location m_layout_loc;
	Location m_field_loc;

public:
	LayoutMissingField(
			std::string layout_name,
			std::string field_name,
			const Location& layout_loc,
			const Location& field_loc)
		: m_layout_name(std::move(layout_name))
		, m_field_name(std::move(field_name))
		, m_layout_loc(layout_loc)
		, m_field_loc(field_loc)
	{}

	const std::string& layout_name() const { return m_layout_name; }
	const std::string& field_name() const { return m_field_name; }

	const Location& layout_loc() const { return m_layout_loc; }
	const Location& field_loc() const { return m_field_loc; }
};

class LayoutNameClash
{
	std::string m_layout_name;
	Location m_layout_loc;
	Location m_class_loc;

public:
	LayoutNameClash(
			std::string layout_name,
			const Location& layout_loc,
			const Location& class_loc)
		: m_layout_name(std::move(layout_name))
		, m_layout_loc(layout_loc)
		, m_class_loc(class_loc)
	{}

	const std::string& layout_name() const { return m_layout_name; }
	const Location& layout_loc() const { return m_layout_loc; }
	const Location& class_loc() const { return m_class_loc; }
};

class NoPoolParameters
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

class LayoutDuplicateField
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

class EmptyCluster
{
	Location m_loc;

public:
	explicit EmptyCluster(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class NotInsideLoop
{
	Location m_loc;

public:
	explicit NotInsideLoop(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class IntegerOutOfBounds
{
	Location m_loc;

public:
	explicit IntegerOutOfBounds(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class DoubleOutOfBounds
{
	Location m_loc;

public:
	explicit DoubleOutOfBounds(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class IncorrectFirstPoolParameter
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

class IncorrectType
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

class IncompatibleBound
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

class IncorrectPoolsNumber
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

class ExpectedPrimitiveType
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

class ExpectedObjectType
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

class ExpectedBooleanType
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

class ExpectedIntegerType
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

class ReturnWithExpression
{
	Location m_loc;

public:
	explicit ReturnWithExpression(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class ReturnWithoutExpression
{
	Location m_loc;

public:
	explicit ReturnWithoutExpression(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class NonAssignableType
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

class NotLvalue
{
	Location m_loc;

public:
	explicit NotLvalue(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }
};

class ExpectedNumericType
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

class IncorrectArgsNumber
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

class NotAllPathsReturn
{
	std::string m_name;
	Location m_loc;

public:
	NotAllPathsReturn(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Location& loc() const { return m_loc; }
};

class VarMaybeUninitialized
{
	std::string m_name;
	Location m_loc;

public:
	VarMaybeUninitialized(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Location& loc() const { return m_loc; }
};

using SemanticError = mpark::variant<
	DuplicateDefinition,
	MissingDefinition,
	MissingBound,
	LayoutMissingField,
	LayoutNameClash,
	LayoutDuplicateField,
	NoPoolParameters,
	EmptyCluster,
	NotInsideLoop,
	IntegerOutOfBounds,
	DoubleOutOfBounds,
	IncorrectFirstPoolParameter,
	IncorrectType,
	IncompatibleBound,
	IncorrectPoolsNumber,
	ExpectedObjectType,
	ExpectedBooleanType,
	ExpectedPrimitiveType,
	ExpectedIntegerType,
	ExpectedNumericType,
	NonAssignableType,
	ReturnWithExpression,
	ReturnWithoutExpression,
	NotLvalue,
	IncorrectArgsNumber,
	NotAllPathsReturn,
	VarMaybeUninitialized>;

class SemanticErrorList
{
	std::vector<SemanticError> m_errors;

public:
	const std::vector<SemanticError>& errors() const { return m_errors; }

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

extern void run_semantic_analysis(
	const Cst::Program& cst,
	Ast::Program& ast,
	SemanticErrorList& errors);

} // namespace Ast
