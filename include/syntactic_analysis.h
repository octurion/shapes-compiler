#pragma once

#include "parse_tree_common.h"

namespace Cst
{

class SyntaxError
{
	Location m_loc;
	std::string m_msg;

public:
	SyntaxError(const Location& loc, std::string msg)
		: m_loc(std::move(loc))
		, m_msg(std::move(msg))
	{}

	const Location&    loc() const { return m_loc; }
	const std::string& msg() const { return m_msg; }
};

class SyntaxErrorList
{
	std::vector<SyntaxError> m_list;

public:
	decltype(m_list)::const_iterator begin() const { return m_list.begin(); }
	decltype(m_list)::const_iterator end()   const { return m_list.end();   }

	decltype(m_list)::iterator begin() { return m_list.begin(); }
	decltype(m_list)::iterator end()   { return m_list.end();   }

	void add(Location loc, std::string msg)
	{
		m_list.emplace_back(std::move(loc), std::move(msg));
	}

	bool has_errors() const
	{
		return !m_list.empty();
	}
};

}
