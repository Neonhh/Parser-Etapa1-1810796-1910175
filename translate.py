import sys
from parse import SymbolTable, generate_AST
import ply.yacc as yacc
from lexer import lexer, tokens

def generate_python_file(input_filename, translated_code, variables):
    # Genera el nombre del archivo de salida
    output_filename = f"{input_filename.replace('.imperat', '')}_output.py"

    # Definiciones iniciales requeridas
    preamble = """# Definiciones básicas de lambda cálculo
Z = lambda g: (lambda x: g(lambda v: x(x)(v)))(lambda x: g(lambda v: x(x)(v)))
true = lambda x: lambda y: x
false = lambda x: lambda y: y
nil = lambda x: true
cons = lambda x: lambda y: lambda f: f(x)(y)
head = lambda p: p(true)
tail = lambda p: p(false)
apply = Z(lambda g: lambda f: lambda x: f if x == nil else (g(f(head(x)))(tail(x))))
lift_do = lambda exp: lambda f: lambda g: lambda x: g(f(x)) if (exp(x)) else x
do = lambda exp: lambda f: Z(lift_do(exp)(f))

# Código traducido
"""

    result_stmnt = 'nil'
    print_vals = ''
    print_lambdas = ''
    for i in range(len(variables)):
        result_stmnt = f'cons(0)({result_stmnt})'
        print_vals = f"'x{i}' : x{i}, {print_vals}"
        print_lambdas = f"{print_lambdas}lambda x{i}:"


    result_stmnt = f'result = program({result_stmnt})'
    print_stmnt = f'print(apply({print_lambdas}'+"{"+print_vals+"})(result))"



    # Escribe el archivo
    with open(output_filename, "w", encoding="utf-8") as f:
        f.write(preamble)
        f.write(f"program = {translated_code}\n")
        f.write(result_stmnt+"\n")
        f.write(print_stmnt+"\n")

    return output_filename


def traduce_expression(node):
    """Traduce una expresion dentro de una instruccion a lambda calculo"""
    if isinstance(node, tuple):
        tag = node[0]
        # Si el nodo ya viene decorado con tipo
        if tag == "id":
            # Imprime el tipo si está en la posición 2 (aunque haya más elementos)
            return node[1]
        elif tag == "num":
            return node[1]
        elif tag == "string":
            print("String: ")
        elif tag == "true":
            print("iteral: true | type: bool")
        elif tag == "false":
            print("iteral: false | type: bool")
        elif tag == "Concat":
            print("Concat | type: String")

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
                # Primero imprimir todos los operadores Comma anidados
                current = node
                while (
                    isinstance(current, tuple)
                    and current[0] == "binop"
                    and current[1] == ","
                ):
                    # print(
                    #     f"{'-' * current_indent}Comma | type: function with length={length}"
                    # )
                    print(f"{'-'}Comma | type: function with length={length}")
                    current = current[3]  # Avanzar al siguiente operando izquierdo
                    # current_indent += 1
                    length -= 1

                # Luego imprimir los operandos hoja
                def print_operands(n, op_indent):
                    if isinstance(n, tuple) and n[0] == "binop" and n[1] == ",":
                        print_operands(n[2], op_indent)
                        print_operands(n[3], op_indent - 1)
                    # else:
                    # print_expr_decorated(n, op_indent)

                # print_operands(node[2], current_indent)
                # print_operands(node[3], current_indent)
                print_operands(node[2], 0)
                print_operands(node[3], 0)
            elif op == "TwoPoints":
                print("TwoPoints")
                # print_expr_decorated(node[2], indent + 1)
                # print_expr_decorated(node[3], indent + 1)
            else:
                return f"{traduce_expression(node[2])} {node[1]} {traduce_expression(node[3])}"

                # print_expr_decorated(node[2], indent + 1)
                # print_expr_decorated(node[3], indent + 1)
        elif tag == "uminus":
            return f"-{traduce_expression(node[1])}"
            # print_expr_decorated(node[1], indent + 1)
        elif tag == "not":
            # print(f"{prefix}Not | type: bool")
            print("Not | type: bool")
            # print_expr_decorated(node[1], indent + 1)
        elif tag == "app":
            # print(f"{prefix}ReadFunction | type: int")
            print("ReadFunction | type: int")
            # print_expr_decorated(node[1], indent + 1)
            # print_expr_decorated(node[2], indent + 1)
        elif tag == "call":
            # Imprime el tipo real de la función
            typ = node[3] if len(node) > 3 else "int"
            # print(f"{prefix}WriteFunction | type: {typ}")
            print(f"WriteFunction | type: {typ}")
            # print_expr_decorated(node[1], indent + 1)
            # print_expr_decorated(node[2], indent + 1)
        else:
            # Cualquier otro nodo
            print(f"{tag}")
            # for child in node[1:]:
            # print_expr_decorated(child, indent + 1)
    # elif isinstance(node, list):
    # for n in node:
    # print_expr_decorated(n, indent)
    else:
        print(f"{node}")


def traduce_to_lambda(node, lambda_state=[], current_lambda=""):
    """
    Recibe el AST y genera las instrucciones correspondientes para lambda-calculo en python
    """

    if isinstance(node, tuple):
        tag = node[0]

        if tag == "Block":
            for var in node[1].symbols.keys():
                lambda_state.append(var)
            
            return traduce_to_lambda(node[2][1:],lambda_state,''), lambda_state #El primer elemento tiene el tag declare y no lo necesitamos
        
        elif tag=="Asig":
            changed_var = node[1][0]

            new_val = traduce_expression(node[2][0])

            new_state = ""
            for var in lambda_state:
                new_state = f"lambda {var}:{new_state}"

            state_string = "nil"
            for var in lambda_state:
                if var == changed_var:
                    var = new_val

                state_string = f"cons({var}) ({state_string})"

            new_state = f"apply({new_state} {state_string})"

            print(new_state)
            return new_state

    if isinstance(node, list):
        if not node:
            return
        if len(node) == 1:
            return traduce_to_lambda(node[0], lambda_state, "")

        else:

            def process_nested_sequencing(nodes, lambda_state, current_seq=""):
                """Procesa una lista de nodos y los organiza en secuencias anidadas."""
                if not nodes:
                    return
                if len(nodes) == 1:
                    # Si solo hay un nodo, procesarlo directamente
                    return f"{current_seq}({traduce_to_lambda(nodes[0], lambda_state)} (x1)) "
                elif len(nodes) == 2:
                    return f"{current_seq}(lambda x1: {traduce_to_lambda(nodes[1], lambda_state)} ({traduce_to_lambda(nodes[0], lambda_state)} (x1))) "
                else:
                    # Procesar el resto de los nodos recursivamente
                    return process_nested_sequencing(
                        nodes[:-2],
                        lambda_state,
                        current_seq=f"{current_seq}(lambda x1: {traduce_to_lambda(nodes[-1], lambda_state)} (lambda x1: {traduce_to_lambda(nodes[-2], lambda_state)} (x1))) ",
                    )

            seq = process_nested_sequencing(node, lambda_state)
            seq = f"{seq}"
            print(seq)
            return seq


def main():
    if len(sys.argv) != 2:
        print("Uso: python translate.py <archivo.imperat>")
        sys.exit(1)

    filename = sys.argv[1]

    # Chequear que el archivo tenga extensión .imperat
    if not filename.endswith(".imperat"):
        print("Error: El archivo debe tener extensión .imperat")
        sys.exit(1)

    try:
        # Lee el contenido del archivo
        with open(filename, "r", encoding="utf-8") as file:
            data = file.read()
            result, decorated = generate_AST(data)

            print(decorated)
            print()

            code, variables = traduce_to_lambda(decorated)
            print(f"Codigo es {code}")
            generate_python_file(filename, code, variables)

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)


if __name__ == "__main__":
    main()
