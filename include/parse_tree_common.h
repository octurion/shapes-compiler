#pragma once

#include <utility>

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

template<typename T>
class Optional
{
	bool m_present;
	union {
		T m_item;
	};

public:
	Optional()
		: m_present(false)
	{}

	explicit Optional(T item)
		: m_present(true)
		, m_item(std::move(item))
	{}

	Optional(const Optional&) = delete;
	Optional& operator=(const Optional&) = delete;

	Optional(Optional&& other)
		: m_present(other.m_present)
	{
		if (other.m_present) {
			new (&m_item) T(std::move(other.m_item));
		}
	}

	Optional& operator=(Optional&& other)
	{
		if (m_present) {
			m_item.~T();
		}

		m_present = other.m_present;
		if (other.m_present) {
			new (&m_item) T(std::move(other.m_item));
		}

		return *this;
	}

	Optional& operator=(T item)
	{
		if (m_present) {
			m_item.~T();
		}

		m_present = true;
		new (&m_item) T(std::move(item));

		return *this;
	}

	~Optional()
	{
		if (m_present) {
			m_item.~T();
		}
	}

	bool present() const { return m_present; }

	const T* get() const { return m_present ? &m_item : nullptr; }

	template<typename U>
	void accept(U& visitor) const
	{
		if (m_present) {
			visitor.visit(m_item);
		}
	}
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
