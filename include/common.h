#pragma once

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

#define assert_msg(expr, msg) \
	(void)((expr) || (assert_msg_func(#expr, msg, __FILE__, __LINE__), 0))

#define unreachable(msg) assert_unreachable_func(msg, __FILE__, __LINE__)

[[noreturn]] void assert_msg_func(const char* cond,
				const char* msg, const char* file, int line);

[[noreturn]] void assert_unreachable_func(
	const char* msg, const char* file, int line);
