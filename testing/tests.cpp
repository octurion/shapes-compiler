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

namespace llvm { // Ugly hack to get Googletest to print an APInt
std::ostream& operator<<(std::ostream& os, const APInt& value)
{
	return os << value.toString(10, true);
}
} // namespace llvm

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
}

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
		ASSERT_THAT(syntax_errors.has_errors(), IsFalse());

		yylex_destroy(scanner);

		fclose(in);

		Ast::SemanticErrorList errors;

		Ast::run_semantic_analysis(cst, m_ast, errors);
		ASSERT_THAT(errors.has_errors(), IsFalse());

		m_codegen.ir(m_ast);

		m_codegen_interpreter.init(m_codegen);
	}
};

TEST_F(ExecutionTest, SimpleReturn) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* method = clazz->find_method("identity");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	auto* func = m_codegen_interpreter.find_method(spec, *method);

	uint64_t param_value = 100;

	llvm::GenericValue param;
	param.IntVal = llvm::APInt(32, param_value);

	auto retval = m_codegen_interpreter.run_function(func, {this_param, param});
	EXPECT_THAT(retval.IntVal, Eq(param.IntVal));
}

TEST_F(ExecutionTest, ForeachLoop) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* method = clazz->find_method("foreach_loop");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	auto* func = m_codegen_interpreter.find_method(spec, *method);

	uint64_t begin = 20;
	uint64_t end = 30;

	llvm::GenericValue param_begin;
	param_begin.IntVal = llvm::APInt(32, begin);

	llvm::GenericValue param_end;
	param_end.IntVal = llvm::APInt(32, end);

	auto retval = m_codegen_interpreter.run_function(
		func, {this_param, param_begin, param_end});
	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 245)));
}

TEST_F(ExecutionTest, ForeachLoopUnsigned) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* method = clazz->find_method("foreach_loop_unsigned");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	auto* func = m_codegen_interpreter.find_method(spec, *method);

	uint64_t begin = 20;
	uint64_t end = 30;

	llvm::GenericValue param_begin;
	param_begin.IntVal = llvm::APInt(32, begin);

	llvm::GenericValue param_end;
	param_end.IntVal = llvm::APInt(32, end);

	auto retval = m_codegen_interpreter.run_function(
		func, {this_param, param_begin, param_end});
	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 245)));
}

TEST_F(ExecutionTest, GetterSetter) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* getter = clazz->find_method("getter");
	const auto* setter = clazz->find_method("setter");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	auto* getter_func = m_codegen_interpreter.find_method(spec, *getter);
	auto* setter_func = m_codegen_interpreter.find_method(spec, *setter);

	auto getter_retval1 = m_codegen_interpreter.run_function(
		getter_func, {this_param});
	EXPECT_THAT(getter_retval1.IntVal, Eq(llvm::APInt(32, 0)));

	llvm::GenericValue setval;
	setval.IntVal = llvm::APInt(32, 200);

	m_codegen_interpreter.run_function(setter_func, {this_param, setval});

	auto getter_retval2 = m_codegen_interpreter.run_function(
		getter_func, {this_param});

	EXPECT_THAT(getter_retval2.IntVal, Eq(setval.IntVal));
}

TEST_F(ExecutionTest, MutualGetterSetter) {
	const auto* clazz = m_ast.find_class("A");

	const auto* make_twin = clazz->find_method("make_twin");
	const auto* getter = clazz->find_method("twin_getter");

	const auto* layout = m_ast.find_layout("LB");

	Ir::ClassSpecialization spec(*clazz, {nullptr, layout});
	Ir::ClassSpecialization twin_spec(layout->for_class(), {layout, nullptr});

	struct DummyPool {
		uintptr_t size;
		uintptr_t capacity;
		void* cluster0;
		uint32_t* cluster1;
	};
	DummyPool dummy_pool;

	auto* pool_ctor = m_codegen_interpreter.pool_constructor(twin_spec);
	llvm::GenericValue pool_ptr(&dummy_pool);

	m_codegen_interpreter.run_function(pool_ctor, {pool_ptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto this_param = m_codegen_interpreter.run_function(ctor, {pool_ptr});

	auto* getter_func = m_codegen_interpreter.find_method(spec, *getter);
	auto* make_twin_func = m_codegen_interpreter.find_method(spec, *make_twin);

	llvm::GenericValue setval;
	setval.IntVal = llvm::APInt(32, 200);

	m_codegen_interpreter.run_function(make_twin_func, {this_param, setval, pool_ptr});

	auto getter_retval = m_codegen_interpreter.run_function(
		getter_func, {this_param, pool_ptr});

	EXPECT_THAT(getter_retval.IntVal, Eq(setval.IntVal));
	EXPECT_THAT(dummy_pool.cluster1[0], Eq(200));
}

TEST_F(ExecutionTest, PoolConstruction) {
	const auto* clazz = m_ast.find_class("A");
	const auto* method = clazz->find_method("test_pools");

	Ir::ClassSpecialization spec(*clazz, {nullptr, nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto* llvm_method = m_codegen_interpreter.find_method(spec, *method);

	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	auto retval = m_codegen_interpreter.run_function(llvm_method, {this_param});

	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 5)));
}

TEST_F(ExecutionTest, RaphsonNewton) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* method = clazz->find_method("raphson_newton_sqrt");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto* llvm_method = m_codegen_interpreter.find_method(spec, *method);

	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	llvm::GenericValue param;
	param.FloatVal = 2.0f;

	auto retval = m_codegen_interpreter.run_function(llvm_method, {this_param, param});

	constexpr float EXPECTED = 1.41421356237f;
	EXPECT_THAT(retval.FloatVal, NanSensitiveFloatNear(EXPECTED, 1e-8));
}

TEST_F(ExecutionTest, Factorial) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* factorial_method = clazz->find_method("factorial");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto* factorial = m_codegen_interpreter.find_method(spec, *factorial_method);

	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	llvm::GenericValue param;
	param.IntVal = llvm::APInt(32, 10);

	auto retval = m_codegen_interpreter.run_function(factorial, {this_param, param});

	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 3'628'800)));
}

TEST_F(ExecutionTest, Casts) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* test_casts_method = clazz->find_method("test_casts");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto* test_casts = m_codegen_interpreter.find_method(spec, *test_casts_method);

	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	llvm::GenericValue param;
	param.IntVal = llvm::APInt(32, 10);

	auto retval = m_codegen_interpreter.run_function(test_casts, {this_param});

	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 14)));
}

TEST_F(ExecutionTest, Exprs) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* test_exprs_method = clazz->find_method("test_exprs");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto* test_exprs = m_codegen_interpreter.find_method(spec, *test_exprs_method);

	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	llvm::GenericValue param;
	param.IntVal = llvm::APInt(32, 10);

	auto retval = m_codegen_interpreter.run_function(test_exprs, {this_param});

	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 14)));
}

TEST_F(ExecutionTest, OpAssignment) {
	const auto* clazz = m_ast.find_class("Main");
	const auto* test_op_assign_method = clazz->find_method("test_op_assign");

	Ir::ClassSpecialization spec(*clazz, {nullptr});

	auto* ctor = m_codegen_interpreter.constructor(spec);
	auto* test_op_assign = m_codegen_interpreter.find_method(spec, *test_op_assign_method);

	auto this_param = m_codegen_interpreter.run_function(ctor, {});

	llvm::GenericValue param;
	param.IntVal = llvm::APInt(32, 10);

	auto retval = m_codegen_interpreter.run_function(test_op_assign, {this_param});

	EXPECT_THAT(retval.IntVal, Eq(llvm::APInt(32, 11)));
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

INSTANTIATE_TEST_SUITE_P(
	CaseStudies,
	SemanticPass,
	ValuesIn(files_in_path("../testcases/case_studies")));
