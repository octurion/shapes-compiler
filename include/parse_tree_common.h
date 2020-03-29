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
