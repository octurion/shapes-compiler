#pragma once

#include "ast.h"

#include <memory>

namespace Ir {

void init_llvm();

class Codegen
{
private:
	class Impl;

	std::unique_ptr<Codegen::Impl> m_impl;

public:
	Codegen();
	~Codegen();

	bool ir(const Ast::Program& ast);
};


class ClassSpecialization
{
	const Ast::Class* m_class = nullptr;
	std::vector<const Ast::Layout*> m_pool_param_types;

	ClassSpecialization specialize(
		const Ast::Class& clazz,
		const std::vector<Ast::PoolParameter>& params) const;

public:
	ClassSpecialization() = default;

	ClassSpecialization(
			const Ast::Class& clazz,
			std::vector<const Ast::Layout*> pool_param_types)
		: m_class(&clazz)
		, m_pool_param_types(std::move(pool_param_types))
	{}

	const Ast::Class& clazz() const {
		assert_msg(m_class != nullptr, "Invalid class specialization");
		return *m_class;
	}

	const std::vector<const Ast::Layout*>& pool_param_types() const {
		return m_pool_param_types;
	}

	const Ast::Layout* first_pool_param_spec() const {
		assert_msg(!m_pool_param_types.empty(), "Specialization has no pool parameters");
		return m_pool_param_types.front();
	}

	bool is_pooled_type() const {
		assert_msg(!m_pool_param_types.empty(), "Specialization has no pool parameters");
		return m_pool_param_types.front() != nullptr;
	}

	bool operator==(const ClassSpecialization& rhs) const {
		return m_class == rhs.m_class
			&& m_pool_param_types == rhs.m_pool_param_types;
	}
	bool operator!=(const ClassSpecialization& rhs) const {
		return m_class != rhs.m_class
			|| !(m_pool_param_types == rhs.m_pool_param_types);
	}

	ClassSpecialization specialize_type(const Ast::LayoutType& type) const
	{
		return specialize(type.for_class(), type.params());
	}

	ClassSpecialization specialize_type(const Ast::BoundType& type) const
	{
		return specialize(type.of_class(), type.params());
	}

	ClassSpecialization specialize_type(const Ast::ObjectType& type) const
	{
		return specialize(type.of_class(), type.params());
	}
};
std::ostream& operator<<(
	std::ostream& os, const ClassSpecialization& specialization);

std::string create_method_name(
	const ClassSpecialization& specialization, const Ast::Method& method);
std::string create_pool_ctor_name(const ClassSpecialization& specialization);
std::string create_pool_dtor_name(const ClassSpecialization& specialization);

} // namespace Ir

namespace std {
template <>
struct hash<Ir::ClassSpecialization> {
	size_t operator()(const Ir::ClassSpecialization& specialization) const;
};
}
