#pragma once

template <class Visitable>
class BaseVisitable
{
public:
	template <typename T>
	void accept(T& visitor) const
	{
		visitor.visit(static_cast<const Visitable&>(*this));
	}
};

class BaseVisitor
{
public:
	virtual ~BaseVisitor() {}
};

template <class T>
class Visitor
{
public:
	virtual void visit(const T&) = 0;
};

struct Location {
	int first_line   = 1;
	int first_column = 1;

	int last_line   = 1;
	int last_column = 1;

	Location() = default;

	Location(const Location&) = default;
	Location& operator=(const Location&) = default;

	Location(Location&&) = default;
	Location& operator=(Location&&) = default;
};
