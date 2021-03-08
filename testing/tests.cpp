#include "gtest/gtest.h"
#include "gmock/gmock.h"

#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "syntactic_analysis.h"

#include "ast.h"
#include "semantic_analysis.h"

#include <dirent.h>

#include <cstdio>
#include <string>

using namespace testing;

class SyntaxFail: public TestWithParam<std::string> {};
class SemanticFail: public TestWithParam<std::string> {};
class SemanticPass: public TestWithParam<std::string> {};

TEST_P(SyntaxFail, SyntaxFail) {
	const auto& path = GetParam();

	FILE* in = fopen(path.c_str(), "r");
	ASSERT_THAT(in, NotNull());

	Cst::Program cst;
	Cst::SyntaxErrorList syntax_errors;

	yyscan_t scanner;
	yylex_init(&scanner);
	yyset_in(in, scanner);

	yyparse(scanner, &cst, &syntax_errors);

	yylex_destroy(scanner);

	ASSERT_THAT(syntax_errors.has_errors(), IsTrue());
}

TEST_P(SemanticPass, SemanticPass) {
	const auto& path = GetParam();

	FILE* in = fopen(path.c_str(), "r");
	ASSERT_THAT(in, NotNull());

	Cst::Program cst;
	Cst::SyntaxErrorList syntax_errors;

	yyscan_t scanner;
	yylex_init(&scanner);
	yyset_in(in, scanner);

	yyparse(scanner, &cst, &syntax_errors);

	yylex_destroy(scanner);

	ASSERT_THAT(syntax_errors.has_errors(), IsFalse());

	Ast::Program ast;
	Ast::SemanticErrorList errors;

	Ast::run_semantic_analysis(cst, ast, errors);

	EXPECT_THAT(errors.has_errors(), IsFalse());

	fclose(in);
}

TEST_P(SemanticFail, SemanticFail) {
	const auto& path = GetParam();

	FILE* in = fopen(path.c_str(), "r");
	ASSERT_THAT(in, NotNull());

	Cst::Program cst;
	Cst::SyntaxErrorList syntax_errors;

	yyscan_t scanner;
	yylex_init(&scanner);
	yyset_in(in, scanner);

	yyparse(scanner, &cst, &syntax_errors);

	yylex_destroy(scanner);

	ASSERT_THAT(syntax_errors.has_errors(), IsFalse());

	Ast::Program ast;
	Ast::SemanticErrorList errors;

	Ast::run_semantic_analysis(cst, ast, errors);

	EXPECT_THAT(errors.has_errors(), IsTrue());

	fclose(in);
}

std::vector<std::string> files_in_path(const char* path)
{
	std::string root = path;
	root += "/";

	std::vector<std::string> retval;
	auto* dir = opendir(path);

	struct dirent* dp;
	for (;;) {
		dp = readdir(dir);
		if (dp == nullptr) {
			break;
		}

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0) {
			continue;
		}

		retval.push_back(root + dp->d_name);
	}
	return retval;
};

INSTANTIATE_TEST_SUITE_P(
	Parser,
	SyntaxFail,
	ValuesIn(files_in_path("../testcases/parse_error")));

INSTANTIATE_TEST_SUITE_P(
	Parser,
	SemanticPass,
	ValuesIn(files_in_path("../testcases/valid")));

INSTANTIATE_TEST_SUITE_P(
	Parser,
	SemanticFail,
	ValuesIn(files_in_path("../testcases/semantic_error")));
