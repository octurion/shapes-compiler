#include "gtest/gtest.h"
#include "gmock/gmock.h"

#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "syntactic_analysis.h"

#include "ast.h"
#include "semantic_analysis.h"

#include "ir.h"

#include <llvm/ExecutionEngine/GenericValue.h>

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
	if (root.empty() || root.back() != '/') {
		root += '/';
	}

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

class ExecutionTest: public Test
{
protected:
	Ast::Program m_ast;
	Ir::Codegen m_codegen;
	Ir::CodegenInterpreter m_codegen_interpreter;

public:
	void SetUp() override
	{
		// TODO: Make a global test environment for this?
		Ir::init_llvm();

		const auto* path = "../testcases/execution/test_binary.shp";

		FILE* in = fopen(path, "r");

		Cst::Program cst;
		Cst::SyntaxErrorList syntax_errors;

		yyscan_t scanner;
		yylex_init(&scanner);
		yyset_in(in, scanner);

		yyparse(scanner, &cst, &syntax_errors);

		yylex_destroy(scanner);

		fclose(in);

		Ast::SemanticErrorList errors;

		Ast::run_semantic_analysis(cst, m_ast, errors);

		m_codegen.ir(m_ast);

		m_codegen_interpreter.init(m_codegen);
	}
};

TEST_F(ExecutionTest, SimpleReturn) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* method = clazz->find_method("identity");

	auto* func = m_codegen_interpreter.find_method(
		Ir::ClassSpecialization(*clazz, {nullptr}), *method);

	uint64_t param_value = 100;

	llvm::GenericValue this_param(nullptr);

	llvm::GenericValue param;
	param.IntVal = llvm::APInt(32, param_value);

	auto retval = m_codegen_interpreter.run_function(func, {this_param, param});
	EXPECT_THAT(retval.IntVal, Eq(param.IntVal));
}

TEST_F(ExecutionTest, ForeachLoop) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* method = clazz->find_method("foreach_loop");

	auto* func = m_codegen_interpreter.find_method(
		Ir::ClassSpecialization(*clazz, {nullptr}), *method);

	uint64_t begin = 20;
	uint64_t end = 30;

	llvm::GenericValue this_param(nullptr);

	llvm::GenericValue param_begin;
	llvm::GenericValue param_end;

	param_begin.IntVal = llvm::APInt(32, begin);
	param_end.IntVal = llvm::APInt(32, end);

	auto retval = m_codegen_interpreter.run_function(
		func, {this_param, param_begin, param_end});
	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 245)));
}

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
