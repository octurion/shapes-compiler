#include "common.h"

#include <cstdio>
#include <cstdlib>

[[noreturn]] void assert_msg_func(const char* cond,
				const char* msg, const char* file, int line)
{
	fprintf(stderr, "Assertion `%s` at %s:%d failed: %s\n",
			cond, file, line, msg);
	abort();
}

[[noreturn]] void assert_unreachable_func(
	const char* msg, const char* file, int line)
{
	fprintf(stderr, "Unreachable statement at %s:%d: %s\n", file, line, msg);
	abort();
}
