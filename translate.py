import sys
from parse import SymbolTable, generate_AST
import ply.yacc as yacc
from lexer import lexer, tokens


def generate_python_file(input_filename, translated_code, variables):
    # Genera el nombre del archivo de salida
    output_filename = f"{input_filename.replace('.imperat', '')}_output.py"

    # Definiciones iniciales requeridas para lambda cálculo
    preamble = """Z = lambda g: (lambda x: g(lambda v: x(x)(v)))(lambda x: g(lambda v: x(x)(v)))
true = lambda x: lambda y: x
false = lambda x: lambda y: y
nil = lambda x: true
cons = lambda x: lambda y: lambda f: f(x)(y)
head = lambda p: p(true)
tail = lambda p: p(false)
apply = Z(lambda g: lambda f: lambda x: f if x == nil else (g(f(head(x)))(tail(x))))
lift_do = lambda exp: lambda f: lambda g: lambda x: g(f(x)) if (exp(x)) else x
do = lambda exp: lambda f: Z(lift_do(exp)(f))

"""

    # Construye el estado inicial con cons(0) para cada variable
    result_stmnt = "nil"
    for i in range(len(variables)):
        result_stmnt = f"cons(0)({result_stmnt})"

    # Construye la declaración de impresión para mostrar el estado final
    print_vals = ""
    print_lambdas = ""
    for i, var_name in enumerate(variables):
        print_vals += f"'{var_name}': x{i}, "
        print_lambdas += f"lambda x{i}:"

    print_vals = print_vals.rstrip(", ")
    formatted_code = translated_code

    # Escribe el archivo de salida
    with open(output_filename, "w", encoding="utf-8") as f:
        f.write(preamble)
        f.write(f"program = {formatted_code}\n\n")
        f.write(f"result = program({result_stmnt})\n")
        f.write(f"print(apply({print_lambdas}{{{print_vals}}})(result))\n")

    return output_filename


def traduce_expression(node):
    """Traduce una expresión dentro de una instrucción a lambda cálculo"""
    if isinstance(node, tuple):
        tag = node[0]

        if tag == "id":
            return node[1]
        elif tag == "num":
            return node[1]
        elif tag == "string":
            string_value = node[1]
            if string_value.startswith('"') and string_value.endswith('"'):
                string_value = string_value[1:-1]
            return repr(string_value)
        elif tag == "true":
            return "true"
        elif tag == "false":
            return "false"
        elif tag == "Concat":
            left = traduce_expression(node[1])
            right = traduce_expression(node[2])
            return f"str({left}) + str({right})"
        elif tag == "binop":
            op_map = {
                "+": "Plus",
                "-": "Minus",
                "*": "Mult",
                "and": "And",
                "or": "Or",
                "==": "Equal",
                "<>": "NotEqual",
                "<": "Less",
                ">": "Greater",
                "<=": "Leq",
                ">=": "Geq",
                ",": "Comma",
                ":": "TwoPoints",
            }
            op = op_map.get(node[1], node[1])

            def get_tuple_length(n):
                if isinstance(n, tuple) and n[0] == "binop" and n[1] == ",":
                    return get_tuple_length(n[2]) + get_tuple_length(n[3])
                else:
                    return 1

            if op == "Comma":
                length = get_tuple_length(node)
                current = node
                while (
                    isinstance(current, tuple)
                    and current[0] == "binop"
                    and current[1] == ","
                ):
                    current = current[3]
                    length -= 1

                def print_operands(n, op_indent):
                    if isinstance(n, tuple) and n[0] == "binop" and n[1] == ",":
                        print_operands(n[2], op_indent)
                        print_operands(n[3], op_indent - 1)

                print_operands(node[2], 0)
                print_operands(node[3], 0)
                return None
            elif op == "TwoPoints":
                return None
            else:
                # Para operaciones booleanas, usa equivalentes en lambda cálculo
                if node[1] == "and":
                    return f"(lambda x: lambda y: x(y)(false))({traduce_expression(node[2])})({traduce_expression(node[3])})"
                elif node[1] == "or":
                    return f"(lambda x: lambda y: x(true)(y))({traduce_expression(node[2])})({traduce_expression(node[3])})"
                elif node[1] == "==":
                    return f"({traduce_expression(node[2])} == {traduce_expression(node[3])})"
                elif node[1] == "<>":
                    return f"({traduce_expression(node[2])} != {traduce_expression(node[3])})"
                elif node[1] == "<":
                    return f"({traduce_expression(node[2])} < {traduce_expression(node[3])})"
                elif node[1] == "<=":
                    return f"({traduce_expression(node[2])} <= {traduce_expression(node[3])})"
                elif node[1] == ">":
                    return f"({traduce_expression(node[2])} > {traduce_expression(node[3])})"
                elif node[1] == ">=":
                    return f"({traduce_expression(node[2])} >= {traduce_expression(node[3])})"
                else:
                    return f"{traduce_expression(node[2])} {node[1]} {traduce_expression(node[3])}"

        elif tag == "uminus":
            return f"-{traduce_expression(node[1])}"
        elif tag == "not":
            return f"(lambda x: x(false)(true))({traduce_expression(node[1])})"
        elif tag == "app":
            return None
        elif tag == "call":
            return None
        else:
            return None
    else:
        return node


def traduce_condition(node, lambda_state):
    """Traduce una condición booleana usando apply para extraer variables del estado"""
    # Construye las variables lambda para evaluación de condición
    lambda_parts = []
    for var in lambda_state:
        lambda_parts.append(f"lambda {var}")
    lambda_vars = ":".join(lambda_parts)

    # Traduce la expresión de condición
    condition_expr = traduce_expression(node)

    return f"apply({lambda_vars}: {condition_expr})"


def traduce_to_lambda(node, lambda_state=[], current_lambda=""):
    """
    Recibe el AST y genera las instrucciones correspondientes para lambda-cálculo en python
    """

    if isinstance(node, tuple):
        tag = node[0]

        if tag == "Block":
            # Extrae las variables declaradas del bloque
            for var in node[1].symbols.keys():
                lambda_state.append(var)

            return (
                traduce_to_lambda(node[2][1:], lambda_state, ""),
                lambda_state,
            )

        elif tag == "Asig":
            # Maneja asignaciones de variables
            changed_var = node[1][0]
            new_val = traduce_expression(node[2][0])

            # Construye lambda con orden correcto de variables
            lambda_parts = []
            for var in lambda_state:
                lambda_parts.append(f"lambda {var}")
            lambda_vars = ":".join(lambda_parts)

            # Construye estado con asignación en orden correcto
            state_parts = []
            for var in lambda_state:
                if var == changed_var:
                    state_parts.append(new_val)
                else:
                    state_parts.append(var)

            # Construye estructura cons (necesita reverse para orden correcto)
            state_string = "nil"
            for part in reversed(state_parts):
                state_string = f"cons({part})({state_string})"

            new_state = f"apply({lambda_vars}: {state_string})"

            return new_state

        elif tag == "Print":
            # Maneja instrucciones de impresión
            expr = node[1][0]

            # Traduce la expresión a imprimir
            print_expr = traduce_print_expression(expr, lambda_state)

            # El print no cambia el estado, solo ejecuta la impresión
            print_stmt = f"lambda x: (print({print_expr}) or x)"

            return print_stmt

        elif tag == "If":
            # Maneja declaraciones condicionales con guardas
            guards = node[1]

            def build_if_lambda(guards, lambda_state):
                if not guards:
                    return "x"

                guard = guards[0]
                if guard[0] == "Guard":
                    cond = guard[1][0]
                    instrs = guard[2]

                    # Traduce la condición
                    cond_code = traduce_condition(cond, lambda_state)

                    # Traduce las instrucciones
                    if isinstance(instrs, list) and len(instrs) == 1:
                        instr_expr = traduce_to_lambda(instrs[0], lambda_state.copy())
                    else:
                        instr_expr = traduce_to_lambda(instrs, lambda_state.copy())

                    # Maneja guardas restantes
                    else_expr = build_if_lambda(guards[1:], lambda_state)

                    # Construye expresión condicional
                    return f"({instr_expr}(x) if {cond_code}(x) else {else_expr})"

            result = build_if_lambda(guards, lambda_state)

            return f"lambda x: {result}"

    if isinstance(node, list):
        if not node:
            return "lambda x: x"
        if len(node) == 1:
            return traduce_to_lambda(node[0], lambda_state, "")
        else:
            # Construye composición de instrucciones
            instructions = []
            for instr in node:
                result = traduce_to_lambda(instr, lambda_state.copy())
                instructions.append(result)

            # Construye composición con una sola lambda usando parámetro 'x'
            if len(instructions) == 1:
                return instructions[0]
            elif len(instructions) == 2:
                first = instructions[0]
                second = instructions[1]

                if first.startswith("apply(") and second.startswith("lambda"):
                    return f"lambda x: ({second})({first}(x))"
                elif first.startswith("lambda") and second.startswith("lambda"):
                    return f"lambda x: ({second})({first}(x))"
                else:
                    return f"lambda x: ({second})({first}(x))"
            else:
                # Para más de 2, construye composición paso a paso
                result = (
                    f"{instructions[0]}(x)"
                    if instructions[0].startswith("lambda")
                    else f"({instructions[0]})(x)"
                )
                for i in range(1, len(instructions)):
                    if instructions[i].startswith("lambda"):
                        result = f"({instructions[i]})({result})"
                    else:
                        result = f"({instructions[i]})({result})"
                return f"lambda x: {result}"


def traduce_print_expression(node, lambda_state):
    """Traduce una expresión para print, manejando concatenación de strings"""

    if isinstance(node, tuple):
        tag = node[0]

        if tag == "string":
            string_value = node[1]
            if string_value.startswith('"') and string_value.endswith('"'):
                string_value = string_value[1:-1]
            return repr(string_value)

        elif tag == "Concat":
            left = traduce_print_expression(node[1], lambda_state)
            right = traduce_print_expression(node[2], lambda_state)
            return f"str({left}) + str({right})"

        elif tag == "binop" and node[1] == "+":
            left = traduce_print_expression(node[2], lambda_state)
            right = traduce_print_expression(node[3], lambda_state)
            return f"str({left}) + str({right})"

        elif tag == "id":
            var_name = node[1]

            try:
                var_index = lambda_state.index(var_name)
                # Construye apply para extraer la variable
                lambda_parts = []
                for var in lambda_state:
                    lambda_parts.append(f"lambda {var}")
                lambda_vars = ":".join(lambda_parts)

                return f"apply({lambda_vars}: {var_name})(x)"
            except ValueError:
                raise Exception(f"Variable '{var_name}' no declarada")

        elif tag == "num":
            return str(node[1])

        else:
            if tag == "true":
                return "True"
            elif tag == "false":
                return "False"
            else:
                expr = traduce_expression(node)
                if expr is not None:
                    return str(expr)
                else:
                    raise Exception(
                        f"No se puede traducir la expresión print con tag '{tag}'"
                    )

    else:
        return str(node)


def main():
    if len(sys.argv) != 2:
        print("Uso: python translate.py <archivo.imperat>")
        sys.exit(1)

    filename = sys.argv[1]

    if not filename.endswith(".imperat"):
        print("Error: El archivo debe tener extensión .imperat")
        sys.exit(1)

    try:
        with open(filename, "r", encoding="utf-8") as file:
            data = file.read()
            result, decorated = generate_AST(data)

            code, variables = traduce_to_lambda(decorated)
            output_file = generate_python_file(filename, code, variables)
            print(f"Archivo generado: {output_file}")

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)


if __name__ == "__main__":
    main()
