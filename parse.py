# --------------------------------------------------------------
# Etapa 2: Análisis Sintáctico con Árbol Sintáctico
# Proyecto: Traductor de Lenguaje Imperativo al Lenguaje del Lamba Cálculo
# Estudiantes: Néstor Herrera (18-10786) y Luis Isea (19-10175)
# --------------------------------------------------------------

import ply.yacc as yacc
from lexer import lexer, tokens
import sys


# Precedencia de operadores (ajusta según tu gramática)
precedence = (
    ("left", "TkOr"),
    ("left", "TkAnd"),
    ("left", "TkEqual", "TkNEqual"),
    ("left", "TkLess", "TkLeq", "TkGreater", "TkGeq"),
    ("left", "TkPlus", "TkMinus"),
    ("left", "TkMult"),
    ("right", "TkNot"),
    ("right", "UMINUS"),
)


# Símbolo inicial
def p_S(p):
    "S : B"
    p[0] = p[1]


def p_B(p):
    "B : TkOBlock opt_stmt_list TkCBlock"
    #print("Parsed block:", p[2])
    p[0] = ("block", p[2])


def p_opt_stmt_list(p):
    """opt_stmt_list : stmt_list
    |"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = []


def p_stmt_list(p):
    """stmt_list : statement
    | statement TkSemicolon stmt_list"""
    if len(p) == 2:
        #print("Parsed single statement:", p[1])
        p[0] = [p[1]]
    else:
        #print("Parsed statement list with semicolon:", p[1], p[3])
        p[0] = [p[1]] + p[3]


def p_statement(p):
    """statement : declaration_stmt
    | assignment_stmt
    | print_stmt
    | skip_stmt
    | if_stmt
    | while_stmt"""
    #print("Parsed statement:", p[1])
    p[0] = p[1]


def p_declaration_stmt(p):
    """declaration_stmt : TkInt declare_id_list
    | TkBool declare_id_list
    | TkFunction TkOBracket TkSoForth TkNum TkCBracket declare_id_list"""
    if p[1] == "int" or p[1] == "bool":
        p[0] = ("declare", p[1], p[2])
    else:
        p[0] = ("declare_func", p[4], p[6])


def p_declare_id_list(p):
    """declare_id_list : TkId
    | TkId TkComma declare_id_list"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]


def p_assignment_stmt(p):
    "assignment_stmt : TkId TkAsig E"
    p[0] = ("assign", p[1], p[3])


def p_print_stmt(p):
    "print_stmt : TkPrint E"
    p[0] = ("print", p[2])


def p_skip_stmt(p):
    "skip_stmt : TkSkip"
    p[0] = ("skip",)


def p_while_stmt(p):
    "while_stmt : TkWhile E TkArrow body_sequencing TkEnd"
    p[0] = ("while", p[2], p[4])


def p_if_stmt(p):
    "if_stmt : TkIf if_guards_list TkFi"
    p[0] = ("if", p[2])


def p_if_guards_list(p):
    """if_guards_list : if_guard_clause
    | if_guard_clause if_guards_list"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]


def p_if_guard_clause(p):
    """if_guard_clause : E TkArrow body_sequencing
    | TkGuard E TkArrow body_sequencing"""
    if len(p) == 4:
        p[0] = ("guard", p[1], p[3])
    else:
        p[0] = ("guard", p[2], p[4])


def p_body_sequencing(p):
    """body_sequencing : body_stmt_item
    | body_stmt_item TkSemicolon body_sequencing"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]


def p_body_stmt_item(p):
    """body_stmt_item : assignment_stmt
    | print_stmt
    | skip_stmt
    | if_stmt
    | while_stmt
    | B"""
    p[0] = p[1]


# Expresiones
def p_E_binop(p):
    """E : E TkPlus E
    | E TkMinus E
    | E TkMult E
    | E TkAnd E
    | E TkOr E
    | E TkEqual E
    | E TkNEqual E
    | E TkLess E
    | E TkGreater E
    | E TkLeq E
    | E TkGeq E
    | E TkComma E
    | E TkTwoPoints E"""
    p[0] = ("binop", p[2], p[1], p[3])


def p_E_uminus(p):
    "E : TkMinus E %prec UMINUS"
    p[0] = ("uminus", p[2])


def p_E_not(p):
    "E : TkNot E"
    p[0] = ("not", p[2])


def p_E_atom(p):
    "E : atom"
    p[0] = p[1]


def p_atom_app(p):
    "atom : atom TkApp simple_atom"
    p[0] = ("app", p[1], p[3])


def p_atom_simple(p):
    "atom : simple_atom"
    p[0] = p[1]


def p_simple_atom_id(p):
    "simple_atom : TkId"
    p[0] = ("id", p[1])


def p_simple_atom_num(p):
    "simple_atom : TkNum"
    p[0] = ("num", p[1])


def p_simple_atom_true(p):
    "simple_atom : TkTrue"
    p[0] = ("true",)


def p_simple_atom_false(p):
    "simple_atom : TkFalse"
    p[0] = ("false",)


def p_simple_atom_string(p):
    "simple_atom : TkString"
    p[0] = ("string", p[1])


def p_simple_atom_paren(p):
    "simple_atom : TkOpenPar E TkClosePar"
    p[0] = p[2]


def p_simple_atom_app_paren(p):
    "simple_atom : atom TkOpenPar E TkClosePar"
    p[0] = ("call", p[1], p[3])


def p_error(p):
    if p:
        print(
            f"Syntax error in row '{p.lineno}' column {p.lexpos}, unexpected token '{p.value}'"
        )


parser = yacc.yacc(debug=False, write_tables=False)


def print_ast(node, indent=0, sequenced=False):
    prefix = "-" * indent
    if isinstance(node, tuple):
        tag = node[0]
        # Traducción de nombres de nodos
        if tag == "block":
            print(f"{prefix}Block")
            print_ast(node[1], indent + 1)
        elif tag == "declare":
            if not sequenced:
                print(f"{prefix}Declare")
                print(f"{prefix}-Sequencing")
            ids = ", ".join(
                [i[1] if isinstance(i, tuple) and i[0] == "id" else i for i in node[2]]
            )
            print(f"{prefix}--{ids} : {node[1]}")
        elif tag == "declare_func":
            if not sequenced:
                print(f"{prefix}Declare")
                print(f"{prefix}-Sequencing")
            ids = ", ".join(
                [i[1] if isinstance(i, tuple) and i[0] == "id" else i for i in node[2]]
            )
            print(f"{prefix}--{ids} : function[..Literal: {node[1]}]")
        elif tag == "assign":
            print(f"{prefix}Asig")
            print_ast(("id", node[1]), indent + 1)
            print_ast(node[2], indent + 1)
        elif tag == "print":
            print(f"{prefix}Print")
            print_ast(node[1], indent + 1)
        elif tag == "skip":
            print(f"{prefix}Skip")
        elif tag == "while":
            print(f"{prefix}While")
            print(f"{prefix}-Then")
            print_ast(node[1], indent + 2)
            print_ast(("Sequencing", node[2]), indent + 2)
        elif tag == "if":
            print(f"{prefix}If")
            for guard in node[1]:
                print_ast(("Guard", guard), indent + 1)
        elif tag == "Guard":
            guard = node[1]
            if guard[0] == "guard":
                print(f"{prefix}Guard")
                print_ast(("Guard", guard[1:]), indent + 1)
            else:
                print(f"{prefix}Then")
                print_ast(guard[0], indent + 1)
                print_ast(("Sequencing", guard[1]), indent + 1)
        elif tag == "Sequencing":
            # node[1] es una lista de sentencias
            stmts = node[1] if isinstance(node[1], list) else [node[1]]
            if len(stmts) == 1:
                print_ast(stmts[0], indent, sequenced=sequenced)
            else:

                print_ast(stmts[0], indent + 1, sequenced=sequenced)
                print_ast(("Sequencing", stmts[1:]), indent + 1, sequenced=True)
        elif tag == "binop":
            op_map = {
                "+": "Plus",
                "-": "Minus",
                "*": "Mult",
                "TkAnd": "And",
                "TkOr": "Or",
                "=": "Equal",
                "!=": "NEqual",
                "<": "Less",
                ">": "Greater",
                "<=": "Leq",
                ">=": "Geq",
                ",": "Comma",
                ":": "TwoPoints",
            }
            op = op_map.get(node[1], node[1])
            print(f"{prefix}{op}")
            print_ast(node[2], indent + 1)
            print_ast(node[3], indent + 1)
        elif tag == "uminus":
            print(f"{prefix}Minus")
            print_ast(node[1], indent + 1)
        elif tag == "not":
            print(f"{prefix}Not")
            print_ast(node[1], indent + 1)
        elif tag == "app":
            print(f"{prefix}App")
            print_ast(node[1], indent + 1)
            print_ast(node[2], indent + 1)
        elif tag == "call":
            print(f"{prefix}WriteFunction")
            print_ast(node[1], indent + 1)
            print_ast(node[2], indent + 1)
        elif tag == "id":
            print(f"{prefix}Ident: {node[1]}")
        elif tag == "num":
            print(f"{prefix}Literal: {node[1]}")
        elif tag == "string":
            print(f'{prefix}String: "{node[1]}"')
        elif tag == "true":
            print(f"{prefix}Literal: true")
        elif tag == "false":
            print(f"{prefix}Literal: false")
        else:
            # fallback
            print(f"{prefix}{tag}")
            for child in node[1:]:
                print_ast(child, indent + 1)
    elif isinstance(node, list):
        if not node:
            return
        if len(node) == 1:
            print_ast(node[0], indent)
        else:
            print_ast(("Sequencing", node), indent)
    else:
        print(f"{prefix}{node}")


def main():
    # Procesamiento del input por consola
    if len(sys.argv) != 2:
        print("Uso: python parse.py <archivo.imperat>")
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

        lexer.input(data)
        lexer.lineno = 1
        # Limpia errores previos del lexer si existen
        if hasattr(lexer, "errors"):
            del lexer.errors

        result = parser.parse(data, lexer=lexer)

        # Print the raw AST
        #print("Raw AST:")
        #print(result)

        # Print the formatted AST
        print("\nFormatted AST:")
        print_ast(result)

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)

if __name__ == "__main__":
    main()
