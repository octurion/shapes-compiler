#include "ast.h"

struct SimpleVisitor: public Visitor
{
	virtual void visit(Identifier&) override { }
	virtual void visit(Number&)     override { }
	virtual void visit(VariableDecl& decl) override
	{
		decl.type->accept(*this);
	};

	virtual void visit(InvalidType&)   override { }
	virtual void visit(VoidType&)      override { };
	virtual void visit(PrimitiveType&) override { };
	virtual void visit(TmpClassType&)  override { };
	virtual void visit(ClassType&)     override { };
	virtual void visit(LayoutType&)    override { };
	virtual void visit(BoundType&)     override { };

	virtual void visit(InvalidExpr&) override { };
	virtual void visit(ThisExpr&)    override { };
	virtual void visit(NullExpr&)    override { };

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
	virtual void visit(IdentifierExpr&) override { };
	virtual void visit(IntConst&)       override { };
	virtual void visit(NewExpr&)        override { };

	virtual void visit(NoopStmt&) override { };
	virtual void visit(VariableDeclsStmt& stmt) override
	{
		for (auto& e: stmt.decls) {
			e.accept(*this);
		}
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
	virtual void visit(BreakStmt&)      override { }
	virtual void visit(ContinueStmt&)   override { }
	virtual void visit(ReturnVoidStmt&) override { }
	virtual void visit(ReturnStmt& stmt) override {
		stmt.expr->accept(*this);
	}

	virtual void visit(Method& method) override {
		for (auto& e: method.ast_arguments) {
			e->accept(*this);
		}
		for (auto& e: method.statements) {
			e->accept(*this);
		}
	}
	virtual void visit(Class& clazz) override {
		for (auto& e: clazz.ast_pool_parameters) {
			e.second.accept(*this);
		}
		for (auto& e: clazz.ast_fields) {
			e.second->accept(*this);
		}
		for (auto& e: clazz.ast_methods) {
			e.second->accept(*this);
		}
	}
	virtual void visit(Field&) override { }
	virtual void visit(Layout& layout) override
	{
		for (auto& e: layout.clusters) {
			e.accept(*this);
		}
	}
	virtual void visit(Cluster&) override { }

	virtual void visit(Ast& ast) override
	{
		for (auto e: ast.ast_classes) {
			e.second->accept(*this);
		}
		for (auto e: ast.ast_layouts) {
			e.second->accept(*this);
		}
	}
};

struct ClassCheckVisitor: public SimpleVisitor
{
	ErrorList& errors;

	Ast* the_ast = nullptr;
	Class* curr_class = nullptr;

	virtual void visit(Ast& ast) override {
		the_ast = &ast;
		for (auto& e: ast.classes) {
			auto it = ast.ast_classes.emplace(e.name.ident, &e);
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate class definition", e.name.loc);
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
			return;
		}

		auto& pool_params = clazz.ast_pool_parameters;
		for (auto& e: clazz.pool_parameters) {
			auto it = pool_params.emplace(e.ident, VariableDecl(e));
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate pool parameter definition", e.loc);
			}
		}
		std::unordered_set<VariableDecl*> used;
		for (auto& e: clazz.pool_bounds) {
			auto it = pool_params.find(e.name.ident);
			if (it == pool_params.end()) {
				errors.semantic_errors.emplace_back("No such pool parameter found", e.name.loc);
				continue;
			}
			auto used_it = used.insert(&it->second);
			if (!used_it.second) {
				errors.semantic_errors.emplace_back("Pool parameter bound declared more than once", e.name.loc);
			}
		}

		auto& fields = clazz.ast_fields;
		for (auto& e: clazz.fields) {
			auto it = fields.emplace(e.name.ident, &e);
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate field definition", e.name.loc);
			}
		}

		auto& methods = clazz.ast_methods;
		for (auto& e: clazz.methods) {
			auto it = methods.emplace(e.name.ident, &e);
			if (!it.second) {
				errors.semantic_errors.emplace_back("Duplicate method definition", e.name.loc);
			}

			e.accept(*this);
		}
		curr_class = nullptr;
	}

	virtual void visit(Layout& layout) override {
		auto& name = layout.name.ident;
		auto& loc = layout.name.loc;
		auto class_clash_it = the_ast->ast_classes.find(name);
		if (class_clash_it != the_ast->ast_classes.end()) {
			errors.semantic_errors.emplace_back("Layout has same name with class", loc);
		}

		auto it = the_ast->ast_layouts.emplace(name, &layout);
		if (!it.second) {
			errors.semantic_errors.emplace_back("Duplicate layout definition", loc);
			return;
		}

		auto class_it = the_ast->ast_classes.find(layout.class_name.ident);
		if (class_it == the_ast->ast_classes.end()) {
			errors.semantic_errors.emplace_back("No such class exists", layout.class_name.loc);
			return;
		}
		layout.clazz = class_it->second;

		std::unordered_set<VariableDecl*> used_fields;
		auto& fields = layout.clazz->ast_fields;

		if (layout.clusters.empty()) {
			errors.semantic_errors.emplace_back("Layout has no clusters", layout.name.loc);
			return;
		}

		for (auto& c: layout.clusters) {
			if (c.fields.empty()) {
				errors.semantic_errors.emplace_back("Layout has an empty cluster", layout.name.loc);
			}

			for (auto& f: c.fields) {
				auto field_it = fields.find(f.ident);
				if (field_it == fields.end()) {
					errors.semantic_errors.emplace_back("Respective class has no such field", f.loc);
					continue;
				}

				c.field_refs.push_back(field_it->second);
				if (!used_fields.insert(field_it->second).second) {
					errors.semantic_errors.emplace_back("Field defined more than once in cluster", f.loc);
					continue;
				}
			}
		}

		if (used_fields.size() != fields.size()) {
			errors.semantic_errors.emplace_back("Not all fields have been used", layout.name.loc);
		}
	}

	ClassCheckVisitor(ErrorList& errors)
		: errors(errors)
	{}
};

struct LocalsScope
{
	struct LocalScope* prev = nullptr;
	std::unordered_map<std::string, VariableDecl*> mappings;
};

struct PoolsScope {
	struct PoolsScope* prev = nullptr;
	std::unordered_map<std::string, VariableDecl*> mappings;
};

struct VariableCheckVisitor: public SimpleVisitor
{
	ErrorList& errors;

	Ast* the_ast = nullptr;
	Class* curr_class = nullptr;

	LocalsScope* curr_locals = nullptr;
	PoolsScope* curr_pools = nullptr;

	virtual void visit(Class& clazz) override
	{
		for (auto& e: clazz.pool_bounds) {
			auto param = clazz.ast_pool_parameters.find(e.name.ident);
			if (param == clazz.ast_pool_parameters.end()) {
				errors.semantic_errors.emplace_back("No pool parameter with this name exists", e.name.loc);
				continue;
			}
			auto* type = static_cast<TmpClassType*>(e.type.get());

			if (type->kind != TmpClassType::Kind::BOUND) {
				errors.semantic_errors.emplace_back("Not a pool bound", type->name.loc);
				continue;
			}

			std::unique_ptr<BoundType> proper_type(new BoundType);

			auto it = the_ast->ast_classes.find(type->name.ident);
			if (it == the_ast->ast_classes.end()) {
				errors.semantic_errors.emplace_back("No class with this name exists", type->name.loc);
				continue;
			}
			proper_type->clazz = it->second;

			for (auto& param: type->pool_parameters) {
				auto it = clazz.ast_pool_parameters.find(param.ident);
				if (it == clazz.ast_pool_parameters.end()) {
					errors.semantic_errors.emplace_back("No pool parameter with this name exists", param.loc);
					continue;
				}

				proper_type->pool_parameters.push_back(&it->second);
			}
			param->second.type = std::move(proper_type);
		}

		for (auto& e: clazz.ast_fields) {
			auto* type = static_cast<TmpClassType*>(e.second->type.get());
			if (type->kind != TmpClassType::Kind::CLASS_LAYOUT) {
				errors.semantic_errors.emplace_back("Not a pool bound", type->name.loc);
				continue;
			}

			std::unique_ptr<ClassType> proper_type(new ClassType);

			auto it = the_ast->ast_classes.find(type->name.ident);
			if (it == the_ast->ast_classes.end()) {
				errors.semantic_errors.emplace_back("No class with this name exists", type->name.loc);
				continue;
			}
			proper_type->clazz = it->second;

			for (auto& param: type->pool_parameters) {
				auto it = clazz.ast_pool_parameters.find(param.ident);
				if (it == clazz.ast_pool_parameters.end()) {
					errors.semantic_errors.emplace_back("No pool parameter with this name exists", param.loc);
					continue;
				}

				proper_type->pool_parameters.push_back(&it->second);
			}
			e.second->type = std::move(proper_type);
		}

		for (auto& e: clazz.ast_methods) {
			e.second->accept(*this);
		}
	}

	virtual void visit(Method& method) override
	{

	}

	virtual void visit(Ast& ast) override
	{
		the_ast = &ast;
		SimpleVisitor::visit(ast);
	}

	VariableCheckVisitor(ErrorList& errors)
		: errors(errors)
	{ }
};

void run_semantic_analysis(Ast& ast, ErrorList& errors)
{
	ClassCheckVisitor class_visitor(errors);
	class_visitor.visit(ast);
	if (!errors.semantic_errors.empty()) {
		return;
	}

	VariableCheckVisitor vars_visitor(errors);
	vars_visitor.visit(ast);
	if (!errors.semantic_errors.empty()) {
		return;
	}
}
