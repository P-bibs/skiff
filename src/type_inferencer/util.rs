use crate::ast::{AstNode, Identifier, Program, Type};

/// Ensures that any function declarations or data declarations that have
/// missing type annotations get converted to `Any`s
pub fn add_any_to_declarations(program: Program) -> Program {
    program
        .into_iter()
        .map(|mut expr| match expr.node {
            AstNode::FunctionNode(function_name, param_list, return_type, body) => {
                // Convert any parameters that don't have a type to any
                let param_list = param_list
                    .into_iter()
                    .map(|param| Identifier::new(param.id, Type::none_to_any(param.type_decl)))
                    .collect();

                // Convert any return types that don't have a type to any
                expr.node = AstNode::FunctionNode(
                    function_name,
                    param_list,
                    Type::none_to_any(return_type),
                    body,
                );
                expr
            }
            AstNode::DataDeclarationNode(data_name, data_variants) => {
                // Convert any variant members that don't have a type to any
                let data_variants = data_variants
                    .into_iter()
                    .map(|(variant_name, variant_members)| {
                        (
                            variant_name,
                            variant_members
                                .into_iter()
                                .map(|member| {
                                    Identifier::new(member.id, Type::none_to_any(member.type_decl))
                                })
                                .collect(),
                        )
                    })
                    .collect();
                expr.node = AstNode::DataDeclarationNode(data_name, data_variants);
                expr
            }
            _ => expr,
        })
        .collect()
}
