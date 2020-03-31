#pragma once

#include "parse_tree_common.h"

#include <string>
#include <utility>

namespace Ast
{
#define DEFINE_VISITOR_DISPATCH \
	void accept(SemanticErrorVisitor& v) const override { v.visit(*this); }

enum class ErrorKind { CLASS, FIELD, POOL, VARIABLE, TYPE, LAYOUT, METHOD };

enum class TypeKind { CLASS, POOL, BOUND, PRIMITIVE };

class NotLvalue;
class MissingDefinition;
class DuplicateDefinition;
class UnexpectedTypeKind;
class MissingBound;
class NoPoolParameters;
class PoolParametersMismatch;
class FirstPoolParameterMismatch;
class ClassLayoutNameClash;
class MissingFieldInLayout;
class DuplicateFieldInLayout;

class SemanticErrorList;

class SemanticErrorVisitor
{
public:
	virtual ~SemanticErrorVisitor() {}

	virtual void visit(const NotLvalue&)                  = 0;
	virtual void visit(const MissingDefinition&)          = 0;
	virtual void visit(const DuplicateDefinition&)        = 0;
	virtual void visit(const UnexpectedTypeKind&)         = 0;
	virtual void visit(const MissingBound&)               = 0;
	virtual void visit(const NoPoolParameters&)           = 0;
	virtual void visit(const PoolParametersMismatch&)     = 0;
	virtual void visit(const FirstPoolParameterMismatch&) = 0;
	virtual void visit(const ClassLayoutNameClash&)       = 0;
	virtual void visit(const MissingFieldInLayout&)       = 0;
	virtual void visit(const DuplicateFieldInLayout&)     = 0;
};

class SemanticError
{
public:
	virtual void accept(SemanticErrorVisitor& v) const = 0;
};

class NotLvalue: public SemanticError
{
	Location m_loc;

public:
	explicit NotLvalue(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class MissingDefinition: public SemanticError
{
public:

private:
	std::string m_name;
	ErrorKind m_kind;
	Location m_loc;

public:
	explicit MissingDefinition(std::string name, ErrorKind kind, const Location& loc)
		: m_name(std::move(name))
		, m_kind(kind)
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	ErrorKind kind()          const { return m_kind; }
	const Location& loc()     const { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class DuplicateDefinition: public SemanticError
{
	std::string m_name;
	ErrorKind m_kind;
	Location m_loc;
	Location m_orig_loc;

public:
	explicit DuplicateDefinition(std::string name,
								 ErrorKind kind,
								 const Location& loc,
								 const Location& orig_loc)
		: m_name(std::move(name))
		, m_kind(kind)
		, m_loc(loc)
		, m_orig_loc(orig_loc)
	{}

	const std::string name()   const { return m_name;     }
	ErrorKind kind()           const { return m_kind;     }
	const Location& loc()      const { return m_loc;      }
	const Location& orig_loc() const { return m_orig_loc; }

	DEFINE_VISITOR_DISPATCH
};

class UnexpectedTypeKind: public SemanticError
{
	TypeKind m_kind;
	Location m_loc;

public:
	explicit UnexpectedTypeKind(TypeKind kind, const Location& loc)
		: m_kind(kind)
		, m_loc(loc)
	{}

	TypeKind kind()       const { return m_kind; }
	const Location& loc() const { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class MissingBound: public SemanticError
{
	std::string m_name;
	Location m_loc;

public:
	explicit MissingBound(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Location& loc()     const { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class NoPoolParameters: public SemanticError
{
	Location m_loc;

public:
	explicit NoPoolParameters(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class PoolParametersMismatch: public SemanticError
{
	size_t m_expected;
	size_t m_got;
	Location m_loc;

public:
	explicit PoolParametersMismatch(size_t expected, size_t got, const Location& loc)
		: m_expected(expected)
		, m_got(got)
		, m_loc(loc)
	{}

	size_t expected() const { return m_expected; }
	size_t got()      const { return m_got;      }

	const Location& loc()    const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class FirstPoolParameterMismatch: public SemanticError
{
	Location m_loc;

public:
	explicit FirstPoolParameterMismatch(const Location& loc)
		: m_loc(loc)
	{}

	const Location& loc() const { return m_loc; }

	DEFINE_VISITOR_DISPATCH
};

class ClassLayoutNameClash: public SemanticError
{
	std::string m_name;
	Location m_class_loc;
	Location m_layout_loc;

public:
	explicit ClassLayoutNameClash(std::string name,
								  const Location& class_loc,
								  const Location& layout_loc)
		: m_name(std::move(name))
		, m_class_loc(class_loc)
		, m_layout_loc(layout_loc)
	{}

	const std::string& name()    const { return m_name;       }
	const Location& class_loc()  const { return m_class_loc;  }
	const Location& layout_loc() const { return m_layout_loc; }

	DEFINE_VISITOR_DISPATCH
};

class DuplicateFieldInLayout: public SemanticError
{
	std::string m_name;
	Location m_loc;

public:
	explicit DuplicateFieldInLayout(std::string name, const Location& loc)
		: m_name(std::move(name))
		, m_loc(loc)
	{}

	const std::string& name() const { return m_name; }
	const Location& loc()     const { return m_loc;  }

	DEFINE_VISITOR_DISPATCH
};

class MissingFieldInLayout: public SemanticError
{
	std::string m_name;
	Location m_loc;
	Location m_layout_loc;

public:
	explicit MissingFieldInLayout(std::string name,
								  const Location& loc,
								  const Location& layout_loc)
		: m_name(std::move(name))
		, m_loc(loc)
		, m_layout_loc(layout_loc)
	{}

	const std::string& name()    const { return m_name;       }
	const Location& loc()        const { return m_loc;        }
	const Location& layout_loc() const { return m_layout_loc; }

	DEFINE_VISITOR_DISPATCH
};

class SemanticErrorList
{
	std::vector<std::unique_ptr<SemanticError>> m_errors;

public:
	SemanticErrorList() = default;

	decltype(m_errors)::const_iterator const begin() { return m_errors.cbegin(); }
	decltype(m_errors)::const_iterator const end()   { return m_errors.cend();   }

	bool has_errors() const
	{
		return !m_errors.empty();
	}

	void add(std::unique_ptr<SemanticError> error)
	{
		m_errors.emplace_back(std::move(error));
	}
};

#undef DEFINE_VISITOR_DISPATCH
}
