#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "syntactic_analysis.h"

#include "ast.h"
#include "semantic_analysis.h"

#include "ir.h"

#include <cerrno>
#include <cstdio>
#include <cstdlib>

struct Line
{
	size_t begin;
	size_t end;

	Line(size_t begin, size_t end)
		: begin(begin)
		, end(end)
	{}
};

static void print_loc(const Location& loc)
{
	fprintf(stderr, "|Line %d| ", loc.first_line);
}

static void print_error(
	const std::string& file_contents,
	const std::vector<Line>& lines,
	const Location& loc)
{
	constexpr int TAB_SIZE = 4;

	if (loc.first_column == 0) {
		return;
	}

	int spaces_to_print = 0;
	int carets_to_print = 0;

	int chars_printed = 0;

	const auto& line_info = lines[loc.first_line - 1];

	const auto* it = &file_contents[line_info.begin];
	const auto* line_end = &file_contents[line_info.end];

	for (int i = 1; it != line_end; i++, it++) {
		int chars;
		if (*it == '\t') {
			chars = TAB_SIZE - (chars_printed % TAB_SIZE);
			for (int k = 0; k < chars; k++) {
				fputc(' ', stderr);
			}
		} else {
			fputc(*it, stderr);
			chars = 1;
		}

		if (i < loc.first_column) {
			spaces_to_print += chars;
		} else if (loc.first_column <= i && i <= loc.last_column) {
			carets_to_print += chars;
		}
	}

	fputc('\n', stderr);
	for (int i = 0; i < spaces_to_print; i++) {
		fputc(' ', stderr);
	}
	for (int i = 0; i < carets_to_print; i++) {
		fputc('^', stderr);
	}
	fputc('\n', stderr);
}

static const char* kind_str(Ast::ErrorKind kind)
{
	switch (kind) {
	case Ast::ErrorKind::CLASS:
		return "Class";

	case Ast::ErrorKind::FIELD:
		return "Field";

	case Ast::ErrorKind::POOL:
		return "Pool";

	case Ast::ErrorKind::POOL_BOUND:
		return "Pool bound";

	case Ast::ErrorKind::VARIABLE:
		return "Variable";

	case Ast::ErrorKind::TYPE:
		return "Type";

	case Ast::ErrorKind::LAYOUT:
		return "Layout";

	case Ast::ErrorKind::METHOD:
		return "Method";

	default:
		unreachable("Did you introduce an additional error type?");
	}
}

struct SemanticErrorPrinter
{
	const std::string& file_contents;
	const std::vector<Line>& lines;

	SemanticErrorPrinter(
			const std::string& file_contents,
			const std::vector<Line>& lines)
		: file_contents(file_contents)
		, lines(lines)
	{
	}

	void print_line(const Location& loc)
	{
		print_error(file_contents, lines, loc);
	}

	void operator()(const Ast::DuplicateDefinition& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "%s '%s' is already defined.\n",
			kind_str(e.kind()), e.name().c_str());
		print_line(e.loc());

		print_loc(e.existing_loc());
		fprintf(stderr, "Existing definition is here:\n");
		print_line(e.existing_loc());
	}

	void operator()(const Ast::MissingDefinition& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "%s '%s' has not been defined.\n",
			kind_str(e.kind()), e.name().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::MissingBound& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "No bound has been defined for pool '%s'.\n",
			e.pool_name().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::LayoutMissingField& e)
	{
		print_loc(e.layout_loc());
		fprintf(stderr, "Missing field '%s' in layout '%s'.\n",
			e.field_name().c_str(), e.layout_name().c_str());
		print_line(e.layout_loc());
	}

	void operator()(const Ast::LayoutNameClash& e)
	{
		print_loc(e.layout_loc());
		fprintf(stderr, "Layout '%s' has the same name as another class.\n",
			e.layout_name().c_str());
		print_line(e.layout_loc());

		print_loc(e.class_loc());
		fprintf(stderr, "Existing class defined here:\n");
		print_line(e.class_loc());
	}

	void operator()(const Ast::LayoutDuplicateField& e)
	{
		print_loc(e.field_loc());
		fprintf(stderr, "Field '%s' in layout '%s' already exists.\n",
			e.field_name().c_str(), e.layout_name().c_str());
		print_line(e.field_loc());

		print_loc(e.existing_field_loc());
		fprintf(stderr, "Existing field defined here:\n");
		print_line(e.existing_field_loc());
	}

	void operator()(const Ast::NoPoolParameters& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "No pool parameters defined for class '%s'.\n",
			e.name().c_str());
	}

	void operator()(const Ast::EmptyCluster& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Cluster is empty.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::NotInsideLoop& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Statement is not inside loop.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::IntegerOutOfBounds& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Integer constant is too large.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::DoubleOutOfBounds& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Floating point constant is too large.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::IncorrectFirstPoolParameter& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected pool parameter '%s', but got '%s'.\n",
			e.expected().c_str(), e.got().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::IncorrectType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected type '%s', but got '%s'.\n",
			e.expected_type().c_str(), e.got_type().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::IncompatibleBound& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Type '%s' is not compatible with bound '%s'.\n",
			e.type().c_str(), e.bound().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::IncorrectPoolsNumber& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected %zu pool parameters, but got %zu.\n",
			e.num_expected(), e.num_got());
		print_line(e.loc());
	}

	void operator()(const Ast::ExpectedObjectType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected an object type, but got '%s'.\n",
			e.type_got().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::ExpectedPrimitiveType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected a primitive type, but got '%s'.\n",
			e.type_got().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::ExpectedBooleanType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected a boolean type, but got '%s'.\n",
			e.type_got().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::ExpectedIntegerType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected an integer type, but got '%s'.\n",
			e.type_got().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::ExpectedNumericType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected an integer or floating-point type, but got '%s'.\n",
			e.type_got().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::ReturnWithExpression& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Return statement must not have an expression.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::ReturnWithoutExpression& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Return statement has no expression.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::NonAssignableType& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Cannot assign expression of type '%s' into type '%s'.\n",
			e.assigned_from().c_str(), e.assigned_to().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::NotLvalue& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expression is not an lvalue.\n");
		print_line(e.loc());
	}

	void operator()(const Ast::IncorrectArgsNumber& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Expected %zu method arguments, but got %zu.\n",
			e.num_expected(), e.num_got());
		print_line(e.loc());
	}

	void operator()(const Ast::NotAllPathsReturn& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Not all paths return a value in method '%s'.\n",
			e.name().c_str());
		print_line(e.loc());
	}

	void operator()(const Ast::VarMaybeUninitialized& e)
	{
		print_loc(e.loc());
		fprintf(stderr, "Variable '%s' may be used uninitialized here.\n",
			e.name().c_str());
		print_line(e.loc());
	}
};

int main(int argc, char** argv)
{
	if (argc < 2) {
		fprintf(stderr, "You must specify an input file\n");
		return EXIT_FAILURE;
	}

	FILE* in = fopen(argv[1], "r");
	if (in == NULL) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}
	fseek(in, 0, SEEK_END);
	long fsize = ftell(in);
	fseek(in, 0, SEEK_SET);  /* same as rewind(f); */

	std::string file_contents(fsize, '\0');
	fread(&file_contents[0], 1, fsize, in);

	fclose(in);

	std::vector<Line> lines;

	size_t pos = 0;
	size_t prev = 0;
	while ((pos = file_contents.find('\n', prev)) != std::string::npos) {
		lines.emplace_back(prev, pos);
		prev = pos + 1;
	}
	lines.emplace_back(prev, lines.size());

	Cst::Program cst;
	Cst::SyntaxErrorList syntax_errors;

	yyscan_t scanner;
	yylex_init(&scanner);
	auto lex_buffer = yy_scan_bytes(
		file_contents.data(), file_contents.size(), scanner);

	yyparse(scanner, &cst, &syntax_errors);

	yy_delete_buffer(lex_buffer, scanner);
	yylex_destroy(scanner);

	if (syntax_errors.has_errors()) {
		for (auto it = syntax_errors.begin(); it != syntax_errors.end(); it++) {
			const auto& e = *it;
			const auto& loc = e.loc();

			fprintf(stderr, "|Line %d| %s\n", loc.first_line, e.msg().c_str());
			print_error(file_contents, lines, loc);
			fprintf(stderr, "\n");
		}

		return EXIT_FAILURE;
	}

	Ast::Program ast;
	Ast::SemanticErrorList errors;

	Ast::run_semantic_analysis(cst, ast, errors);

	if (errors.has_errors()) {
		SemanticErrorPrinter printer(file_contents, lines);
		for (const auto& e: errors.errors()) {
			mpark::visit(printer, e);
			fprintf(stderr, "\n");
		}

		return EXIT_FAILURE;
	}

	Ir::init_llvm();
	Ir::Codegen codegen;
	codegen.ir(ast);
	codegen.emit("shapes.ll", "shapes.o");

	return EXIT_SUCCESS;
}
