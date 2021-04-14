#include "parser.tab.h"
#include "lexer.yy.h"

#include "cst.h"
#include "syntactic_analysis.h"

#include "ast.h"
#include "semantic_analysis.h"

#include "ir.h"

#include <cerrno>
#include <cstdarg>
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

	void print_error_line(const Location& loc) const
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

	void print_error(const Location& loc, const char* fmt, ...) const
#if defined(__GNUC__) || defined(__clang__)
		__attribute__((format(printf, 3, 4)))
#endif
	{
		va_list args;
		va_start(args, fmt);

		fprintf(stderr, "|Line %d| ", loc.first_line);
		vfprintf(stderr, fmt, args);
		print_error_line(loc);
		fputc('\n', stderr);

		va_end(args);
	}

	void print_syntax_error(const Location& loc, const char* msg) const
	{
		print_error(loc, "%s\n", msg);
	}

	void operator()(const Ast::DuplicateDefinition& e) const
	{
		print_error(e.loc(), "%s '%s' is already defined.\n",
			kind_str(e.kind()), e.name().c_str());
		print_error(e.existing_loc(), "Existing definition is here:\n");
	}

	void operator()(const Ast::MissingDefinition& e) const
	{
		print_error(e.loc(), "%s '%s' has not been defined.\n",
			kind_str(e.kind()), e.name().c_str());
	}

	void operator()(const Ast::MissingBound& e) const
	{
		print_error(e.loc(), "No bound has been defined for pool '%s'.\n",
			e.pool_name().c_str());
	}

	void operator()(const Ast::LayoutMissingField& e) const
	{
		print_error(e.layout_loc(), "Missing field '%s' in layout '%s'.\n",
			e.field_name().c_str(), e.layout_name().c_str());
		print_error(e.field_loc(), "Field is defined here:\n");
	}

	void operator()(const Ast::LayoutNameClash& e) const
	{
		print_error(e.layout_loc(), "Layout '%s' has the same name as another class.\n",
			e.layout_name().c_str());
		print_error(e.class_loc(), "Existing class is defined here:\n");
	}

	void operator()(const Ast::LayoutDuplicateField& e) const
	{
		print_error(e.field_loc(), "Field '%s' in layout '%s' already exists.\n",
			e.field_name().c_str(), e.layout_name().c_str());
		print_error(e.existing_field_loc(), "Existing field is present here:\n");
	}

	void operator()(const Ast::NoPoolParameters& e) const
	{
		print_error(e.loc(), "No pool parameters defined for class '%s'.\n",
			e.name().c_str());
	}

	void operator()(const Ast::EmptyCluster& e) const
	{
		print_error(e.loc(), "Cluster is empty.\n");
	}

	void operator()(const Ast::NotInsideLoop& e) const
	{
		print_error(e.loc(), "Statement is not inside loop.\n");
	}

	void operator()(const Ast::IntegerOutOfBounds& e) const
	{
		print_error(e.loc(), "Integer constant is too large.\n");
	}

	void operator()(const Ast::DoubleOutOfBounds& e) const
	{
		print_error(e.loc(), "Floating point constant is too large.\n");
	}

	void operator()(const Ast::IncorrectFirstPoolParameter& e) const
	{
		print_error(e.loc(), "Expected pool parameter '%s', but got '%s'.\n",
			e.expected().c_str(), e.got().c_str());
	}

	void operator()(const Ast::IncorrectType& e) const
	{
		print_error(e.loc(), "Expected type '%s', but got '%s'.\n",
			e.expected_type().c_str(), e.got_type().c_str());
	}

	void operator()(const Ast::IncompatibleBound& e) const
	{
		print_error(e.loc(), "Type '%s' is not compatible with bound '%s'.\n",
			e.type().c_str(), e.bound().c_str());
	}

	void operator()(const Ast::IncorrectPoolsNumber& e) const
	{
		print_error(e.loc(), "Expected %zu pool parameters, but got %zu.\n",
			e.num_expected(), e.num_got());
	}

	void operator()(const Ast::ExpectedObjectType& e) const
	{
		print_error(e.loc(), "Expected an object type, but got '%s'.\n",
			e.type_got().c_str());
	}

	void operator()(const Ast::ExpectedPrimitiveType& e) const
	{
		print_error(e.loc(), "Expected a primitive type, but got '%s'.\n",
			e.type_got().c_str());
	}

	void operator()(const Ast::ExpectedBooleanType& e) const
	{
		print_error(e.loc(), "Expected a boolean type, but got '%s'.\n",
			e.type_got().c_str());
	}

	void operator()(const Ast::ExpectedIntegerType& e) const
	{
		print_error(e.loc(), "Expected an integer type, but got '%s'.\n",
			e.type_got().c_str());
	}

	void operator()(const Ast::ExpectedNumericType& e) const
	{
		print_error(e.loc(), "Expected an integer or floating-point type, but got '%s'.\n",
			e.type_got().c_str());
	}

	void operator()(const Ast::ReturnWithExpression& e) const
	{
		print_error(e.loc(), "Return statement must not have an expression.\n");
	}

	void operator()(const Ast::ReturnWithoutExpression& e) const
	{
		print_error(e.loc(), "Return statement has no expression.\n");
	}

	void operator()(const Ast::NonAssignableType& e) const
	{
		print_error(e.loc(), "Cannot assign expression of type '%s' into type '%s'.\n",
			e.assigned_from().c_str(), e.assigned_to().c_str());
	}

	void operator()(const Ast::NotLvalue& e) const
	{
		print_error(e.loc(), "Expression is not an lvalue.\n");
	}

	void operator()(const Ast::IncorrectArgsNumber& e) const
	{
		print_error(e.loc(), "Expected %zu method arguments, but got %zu.\n",
			e.num_expected(), e.num_got());
	}

	void operator()(const Ast::NotAllPathsReturn& e) const
	{
		print_error(e.loc(), "Not all paths return a value in method '%s'.\n",
			e.name().c_str());
	}

	void operator()(const Ast::VarMaybeUninitialized& e) const
	{
		print_error(e.loc(), "Variable '%s' may be used uninitialized here.\n",
			e.name().c_str());
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
	fseek(in, 0, SEEK_SET);

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

	SemanticErrorPrinter printer(file_contents, lines);

	if (syntax_errors.has_errors()) {
		for (auto it = syntax_errors.begin(); it != syntax_errors.end(); it++) {
			const auto& e = *it;
			const auto& loc = e.loc();

			printer.print_syntax_error(loc, e.msg().c_str());
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
		}

		return EXIT_FAILURE;
	}

	Ir::init_llvm();
	Ir::Codegen codegen;
	codegen.ir(ast);
	codegen.emit("shapes.ll", "shapes.o");
	codegen.emit_header("shapes.h");

	return EXIT_SUCCESS;
}
