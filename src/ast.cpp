#include "ast.h"

struct SimpleVisitor: public Visitor
{
	virtual void visit(BinaryExpr& e) override {
		e.lhs->accept(*this);
		e.rhs->accept(*this);
	}
	virtual void visit(UnaryExpr& e) override {
		e.expr->accept(*this);
	}
	virtual void visit(FieldExpr& e) override {
		e.expr->accept(*this);
	}
	virtual void visit(AssignStmt& stmt) override {
		stmt.lhs->accept(*this);
		stmt.rhs->accept(*this);
	}
	virtual void visit(OpAssignStmt& stmt) override {
		stmt.lhs->accept(*this);
		stmt.rhs->accept(*this);
	}
	virtual void visit(IfStmt& stmt) override {
		stmt.then_branch->accept(*this);
		stmt.else_branch->accept(*this);
	}
	virtual void visit(WhileStmt& stmt) override {
		stmt.cond->accept(*this);
		stmt.body->accept(*this);
	}
	virtual void visit(ForeachRangeStmt& stmt) override {
		stmt.begin->accept(*this);
		stmt.end->accept(*this);
		stmt.body->accept(*this);
	}
	virtual void visit(ForeachPoolStmt& stmt) override {
		stmt.body->accept(*this);
	}
	virtual void visit(BlockStmt& stmt) override {
		for (auto& e: stmt.stmts) {
			e->accept(*this);
		}
	}
	virtual void visit(ExprStmt& stmt) override {
		stmt.expr->accept(*this);
	}
	virtual void visit(ReturnStmt& stmt) override {
		stmt.expr->accept(*this);
	}
};

struct Scope
{
	struct Scope* previous_scope = nullptr;
	std::unordered_map<std::string, VariableDecl*> locals_mappings;
	std::unordered_map<std::string, VariableDecl*> pool_mappings;
};

struct ClassCheckVisitor: public Visitor
{
	bool passed = true;
	ErrorList& errors;

	Ast* the_ast = nullptr;
	Class* curr_class = nullptr;

	Scope* curr_scope = nullptr;

	virtual void visit(Ast& ast) override {
		the_ast = &ast;
		for (auto& e: ast.classes) {
			auto it = ast.ast_classes.emplace(e.name.ident, &e);
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate class definition", e.name.loc);
				passed = false;
				continue;
			}

			e.accept(*this);
		}

		for (auto& e: ast.layouts) {
			e.accept(*this);
		}
		the_ast = nullptr;
	}

	virtual void visit(Class& clazz) override {
		curr_class = &clazz;
		if (clazz.pool_parameters.empty()) {
			errors.semantic_errors.emplace_back("Classes with no pool parameters are not yet supported", clazz.name.loc);
			passed = false;
			return;
		}

		auto& pool_params = clazz.ast_pool_parameters;
		for (auto& e: clazz.pool_parameters) {
			auto it = pool_params.emplace(e.ident, VariableDecl(e));
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate pool parameter definition", e.loc);
				passed = false;
			}
		}
		std::unordered_set<VariableDecl*> used;
		for (auto& e: clazz.pool_bounds) {
			auto it = pool_params.find(e.name.ident);
			if (it == pool_params.end()) {
				errors.semantic_errors.emplace_back("No such pool parameter found", e.name.loc);
				passed = false;
			}
			auto used_it = used.insert(&it->second);
			if (!used_it.second) {
				errors.semantic_errors.emplace_back("Pool parameter bound declared more than once", e.name.loc);
				passed = false;
			}
		}

		auto& fields = clazz.ast_fields;
		for (auto& e: clazz.fields) {
			auto it = fields.emplace(e.name.ident, &e);
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate field definition", e.name.loc);
				passed = false;
			}
		}

		auto& methods = clazz.ast_methods;
		for (auto& e: clazz.methods) {
			auto it = methods.emplace(e.name.ident, &e);
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate method definition", e.name.loc);
				passed = false;
			}

			e.accept(*this);
		}
		curr_class = nullptr;
	}

	virtual void visit(Method& method) override {
		Scope top_scope;
		curr_scope = &top_scope;

		for (auto& e: method.arguments) {
			// TODO
		}

		curr_scope = nullptr;
	}

	virtual void visit(Layout& layout) override {
		auto& name = layout.name.ident;
		auto& loc = layout.name.loc;
		auto class_clash_it = the_ast->ast_classes.find(name);
		if (class_clash_it != the_ast->ast_classes.end()) {
			errors.semantic_errors.emplace_back("Layout has same name with class", loc);
			passed = false;
		}

		auto it = the_ast->ast_layouts.emplace(name, &layout);
		if (!it.second) {
			errors.semantic_errors.emplace_back("Duplicate layout definition", loc);
			passed = false;
			return;
		}

		auto class_it = the_ast->ast_classes.find(layout.class_name.ident);
		if (class_it == the_ast->ast_classes.end()) {
			errors.semantic_errors.emplace_back("No such class exists", layout.class_name.loc);
			passed = false;
			return;
		}
		layout.clazz = class_it->second;

		std::unordered_set<VariableDecl*> used_fields;
		auto& fields = layout.clazz->ast_fields;

		if (layout.clusters.empty()) {
			errors.semantic_errors.emplace_back("Layout has no clusters", layout.name.loc);
			passed = false;
			return;
		}

		for (auto& c: layout.clusters) {
			if (c.fields.empty()) {
				errors.semantic_errors.emplace_back("Layout has an empty cluster", layout.name.loc);
				passed = false;
			}

			for (auto& f: c.fields) {
				auto field_it = fields.find(f.ident);
				if (field_it == fields.end()) {
					errors.semantic_errors.emplace_back("Respective class has no such field", f.loc);
					passed = false;
					continue;
				}

				c.field_refs.push_back(field_it->second);
				if (!used_fields.insert(field_it->second).second) {
					errors.semantic_errors.emplace_back("Field defined more than once in cluster", f.loc);
					passed = false;
					continue;
				}
			}
		}

		if (used_fields.size() != fields.size()) {
			errors.semantic_errors.emplace_back("Not all fields have been used", layout.name.loc);
			passed = false;
		}
	}

	ClassCheckVisitor(ErrorList& errors)
		: errors(errors)
	{}
};

void run_semantic_analysis(Ast& ast, ErrorList& errors)
{
	ClassCheckVisitor class_visitor(errors);
	class_visitor.visit(ast);
	if (!class_visitor.passed) {
		return;
	}
}
