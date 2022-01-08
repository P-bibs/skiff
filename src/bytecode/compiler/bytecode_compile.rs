use std::{collections::HashMap, error, fmt, ops::Range, vec};


#[derive(PartialEq, Debug, Clone, Hash)]
pub struct BytecodeError(pub String, pub Range<usize>);
impl fmt::Display for BytecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl error::Error for BytecodeError {}



/// Interpret a Skiff program, possibly returning a runtime error
pub fn interpret(program: &Program) -> Result<Vec<Val>, BytecodeError> {
    // Find every data declaration in the program and add functions declarations
    // to the AST. Add a constructor function for each variant of each data declaration
    let data_funcs_ast = find_data_declarations(program)?;

    // Find all top level function declarations and put them into a map
    let funcs = find_functions(program)?;
    let data_funcs = find_functions(&data_funcs_ast)?;
    let funcs = funcs.into_iter().chain(data_funcs).collect();

    // Initialize state to keep track of top level definitions and values
    let mut env = HashMap::new();
    let mut vals = vec![];

    // Loop through each expression/declaration in the program and evaluate it.
    // The result is either value or a new binding in the environment.
    for expr in program {
        match interpret_top_level(expr, env.clone(), &funcs)? {
            ValOrEnv::V(val) => vals.push(val),
            ValOrEnv::E(new_env) => env = new_env,
        }
    }

    Ok(vals)
}


/// Interprets a Skiff expression to produce either a value or an error
fn interpret_expr(expr: &Ast, context: InterpretContext) -> Result<Val, BytecodeError> {
    let InterpretContext {
        env,
        func_table,
        stack,
    } = context;


    let mut instructions = vec![];

    match &expr.node {
        AstNode::NumberNode(n) => instructions.push(Instruction::NUM(n)),
        AstNode::BoolNode(v) => instructions.push(Instruction::BOOL(v)),
        // Variable nodes are looked up in the environment and then in the function table
        AstNode::VarNode(id) => instructions.push(Instruction::LOOKUP(id)),
        // Add the let binding to the environment and then interpret the body
        AstNode::LetNode(id, binding, body) => interpret_expr(
            body,
            context.new_env(&env.update(id.id.clone(), interpret_expr(binding, context)?)),
        ),
        AstNode::LetNodeTopLevel(_, _) => {
            throw_interp_error!("Found LetNodeTopLevel instead of LetNode in expression".to_string())
        }
        AstNode::BinOpNode(op, e1, e2) => {
            interpret_binop(*op, e1, e2, expr.src_loc.clone(), context)
        }
        AstNode::LambdaNode(params, body) => Err()
        AstNode::FunCallNode(fun, args) => {
            let instructions = vec![];
            instructions.push(Instruction::Canary);
            for arg in args {
                let instrs = interpret_expr(arg, context)?;
                instructions.extend(instrs);
            }
            instructions.extend(interpret_expr(fun, context)?);

            instructions.push(Instruction::CALL());

            return instructions
        }
        AstNode::IfNode(conditions_and_bodies, alternate) => {
            // Loop through conditions in order and see if any match
            for (condition, body) in conditions_and_bodies {
                match interpret_expr(condition, context)? {
                    Val::Bool(true) => return interpret_expr(body, context),
                    Val::Bool(false) => continue,
                    _ => throw_interp_error!("Conditional expression with non-boolean condition"),
                }
            }
            // If no conditions match, then evaluate the alternate
            return interpret_expr(alternate, context);
        }
        AstNode::FunctionNode(_, _, _, _) => throw_interp_error!("Function node not at top level"),
        AstNode::DataDeclarationNode(_, _) => {
            throw_interp_error!("Found DataDeclarationNode instead of LetNode in expression")
        }
        AstNode::DataLiteralNode(discriminant, fields) => {
            // Create a data value with the proper discriminant
            let mut values = vec![];
            for expr in fields {
                values.push(interpret_expr(expr, context)?);
            }
            return Ok(Val::Data(discriminant.clone(), values));
        }
        AstNode::MatchNode(expression_to_match, branches) => {
            // Loop through each pattern and see if it matches. If it does, then evaluate
            // the body with the bindings from the pattern
            for (pattern, expr) in branches {
                let val = interpret_expr(expression_to_match, context)?;
                if let Some(match_env) = match_pattern_with_value(pattern, &val) {
                    return interpret_expr(expr, context.new_env(&env.clone().union(match_env)));
                }
            }
            throw_interp_error!("No branch of match expression matched value")
        }
    }
}