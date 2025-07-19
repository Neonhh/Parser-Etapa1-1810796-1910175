# --------------------------------------------------------------
# Etapa 3: Análisis de Contexto de GCL
# Proyecto: Traductor de Lenguaje Imperativo al Lenguaje del Lamba Cálculo
# Estudiantes: Néstor Herrera (18-10786) y Luis Isea (19-10175)
# --------------------------------------------------------------

import ply.yacc as yacc
from lexer import lexer, tokens
import sys


# Precedencia de operadores
precedence = (
    ("left", "TkOr"),
    ("left", "TkAnd"),
    ("left", "TkEqual", "TkNEqual"),
    ("left", "TkLess", "TkLeq", "TkGreater", "TkGeq"),
    ("left", "TkPlus", "TkMinus"),
    ("left", "TkMult"),
    ("right", "TkNot"),
    ("right", "UMINUS"),
    ("left", "TkApp"),
)


# Símbolo inicial
def p_S(p):
    "S : B"
    p[0] = p[1]


def p_B(p):
    "B : TkOBlock opt_stmt_list TkCBlock"
    # print("Parsed block:", p[2])
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
        # print("Parsed single statement:", p[1])
        p[0] = [p[1]]
    else:
        # print("Parsed statement list with semicolon:", p[1], p[3])
        p[0] = [p[1]] + p[3]


def p_statement(p):
    """statement : declaration_stmt
    | assignment_stmt
    | print_stmt
    | skip_stmt
    | if_stmt
    | while_stmt"""
    # print("Parsed statement:", p[1])
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
    line = p.lineno(1)
    col = find_column(p.lexer.lexdata, p.lexpos(1))
    p[0] = ("assign", p[1], p[3], line, col)


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
    line = p.lineno(2)
    col = find_column(p.lexer.lexdata, p.lexpos(2))
    p[0] = ("binop", p[2], p[1], p[3], line, col)


def p_E_uminus(p):
    "E : TkMinus E %prec UMINUS"
    line = p.lineno(1)
    col = find_column(p.lexer.lexdata, p.lexpos(1))
    p[0] = ("uminus", p[2], line, col)


def p_E_not(p):
    "E : TkNot E"
    line = p.lineno(1)
    col = find_column(p.lexer.lexdata, p.lexpos(1))
    p[0] = ("not", p[2], line, col)


def p_E_atom(p):
    "E : atom"
    p[0] = p[1]


def p_atom_app(p):
    "atom : atom TkApp simple_atom"

    def get_line_col(expr):
        # Elimina el print de debug, solo retorna la posición
        if isinstance(expr, tuple):
            if (
                len(expr) >= 4
                and isinstance(expr[-2], int)
                and isinstance(expr[-1], int)
            ):
                return expr[-2], expr[-1]
            if len(expr) >= 3 and isinstance(expr[1], int) and isinstance(expr[2], int):
                return expr[1], expr[2]
        return None, None

    line, col = get_line_col(p[3])
    p[0] = ("app", p[1], p[3], line, col)


def p_atom_simple(p):
    "atom : simple_atom"
    p[0] = p[1]


def p_simple_atom_id(p):
    "simple_atom : TkId"
    # Guardar línea y columna del identificador
    line = p.lineno(1)
    col = find_column(p.lexer.lexdata, p.lexpos(1))
    p[0] = ("id", p[1], line, col)


def p_simple_atom_num(p):
    "simple_atom : TkNum"
    p[0] = ("num", p[1])


def p_simple_atom_true(p):
    "simple_atom : TkTrue"
    line = p.lineno(1)
    col = find_column(p.lexer.lexdata, p.lexpos(1))
    p[0] = ("true", line, col)


def p_simple_atom_false(p):
    "simple_atom : TkFalse"
    line = p.lineno(1)
    col = find_column(p.lexer.lexdata, p.lexpos(1))
    p[0] = ("false", line, col)


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
    # print("\n\n", node)
    prefix = "-" * indent
    if isinstance(node, tuple):
        tag = node[0]
        # Traducción de nombres de nodos
        if tag == "block":
            print(f"{prefix}Block")
            print_ast(node[1], indent + 1)
        elif tag == "declare":
            # if not sequenced:
            #    print(f"{prefix}Declare")
            #    print(f"{prefix}-Sequencing")
            ids = ", ".join(
                [i[1] if isinstance(i, tuple) and i[0] == "id" else i for i in node[2]]
            )
            print(f"{prefix}--{ids} : {node[1]}")
        elif tag == "declare_func":
            # if not sequenced:
            #    print(f"{prefix}Declare")
            #    print(f"{prefix}-Sequencing")
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
            print(f"{prefix}skip")
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

            if not sequenced:
                print(f"{prefix}Sequencing")

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
                "and": "And",
                "or": "Or",
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

            def get_tuple_length(n):
                if isinstance(n, tuple) and n[0] == "binop" and n[1] == ",":
                    return get_tuple_length(n[2]) + get_tuple_length(n[3])
                else:
                    return 1

            if op == "Comma":
                length = get_tuple_length(node)
                print(f"{prefix}Comma | type: function with length BB={length}")
                print_expr_decorated(node[2], indent + 1)
                print_expr_decorated(node[3], indent + 1)
            elif op == "TwoPoints":
                print(f"{prefix}TwoPoints")
                print_expr_decorated(node[2], indent + 1)
                print_expr_decorated(node[3], indent + 1)
            else:
                if op in ["Plus", "Minus", "Mult"]:
                    typ = "int"
                elif op in ["And", "Or"]:
                    typ = "bool"
                elif op in ["Equal", "NotEqual", "Less", "Greater", "Leq", "Geq"]:
                    typ = "bool"
                else:
                    typ = "unknown"
                print(f"{prefix}{op} | type: {typ}")
                print_expr_decorated(node[2], indent + 1)
                print_expr_decorated(node[3], indent + 1)
        elif tag == "uminus":
            print(f"{prefix}Minus | type: int")
            print_expr_decorated(node[1], indent + 1)
        elif tag == "not":
            print(f"{prefix}Not | type: bool")
            print_expr_decorated(node[1], indent + 1)
        elif tag == "app":
            print(f"{prefix}ReadFunction | type: int")
            print_expr_decorated(node[1], indent + 1)
            print_expr_decorated(node[2], indent + 1)
        elif tag == "call":
            # Imprime el tipo real de la función
            typ = node[3] if len(node) > 3 else "int"
            print(f"{prefix}WriteFunction | type: {typ}")
            print_expr_decorated(node[1], indent + 1)
            print_expr_decorated(node[2], indent + 1)
        elif tag == "id":
            print(f"{prefix}Ident: {node[1]}")
        elif tag == "num":
            print(f"{prefix}Literal: {node[1]}")
        elif tag == "string":
            print(f"{prefix}String: {node[1]}")
        elif tag == "true":
            print(f"{prefix}Literal: true")
        elif tag == "false":
            print(f"{prefix}Literal: false")
        else:
            # Cualquier otro nodo
            print(f"{prefix}{tag}")
            for child in node[1:]:
                print_ast(child, indent + 1)
    elif isinstance(node, list):
        if not node:
            return

        declarations = [
            n
            for n in node
            if isinstance(n, tuple) and (n[0] == "declare" or n[0] == "declare_func")
        ]

        other_nodes = [n for n in node if n not in declarations]

        if declarations:
            print_ast(declarations[0], indent, len(declarations) == 1)
            for decl in declarations[1:]:
                print_ast(decl, indent, sequenced=True)

        if other_nodes:
            sequence_binary_ops(other_nodes, indent)
        # if len(node) == 1:
        #   print_ast(node[0], indent)
        # else:
        #    print_ast(("Sequencing", node), indent)
    else:
        print(f"{prefix}{node}")


def sequence_binary_ops(node, indent):
    """Toma una lista de nodos del AST y los ordena de tal forma que la
    secuenciacion tome maximo dos nodos a la vez."""
    if isinstance(node, list):
        if len(node) == 1:
            print_ast(node[0], indent)
        else:
            print(f"{'-' * indent}Sequencing")
            sequence_binary_ops(node[:-1], indent + 1)
            print_ast(node[-1], indent + 1)
    else:
        return node


# Tabla de símbolos para el análisis de contexto y tipos
class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    def declare(self, name, typ):
        if name in self.symbols:
            raise Exception(f"Redeclaration of variable '{name}'")
        self.symbols[name] = typ

    def lookup(self, name):
        if name in self.symbols:
            return self.symbols[name]
        elif self.parent:
            return self.parent.lookup(name)
        else:
            return None

    def in_current_scope(self, name):
        return name in self.symbols

    def __str__(self):
        result = ""
        for name, typ in self.symbols.items():
            result += f"--variable: {name} | type: {typ}\n"
        return result.rstrip()


# Función para verificar compatibilidad de tipos
def type_compatible(declared, exprtype):
    if declared == exprtype:
        return True
    # Para funciones, permitir asignar tuplas de argumentos a funciones
    if declared.startswith("function[..") and exprtype.startswith(
        "function with length="
    ):
        return True
    return False


# Decoración y verificación de tipos en expresiones
def analyze_expr(node, symtable):
    tag = node[0]
    if tag == "true":
        if len(node) >= 3 and isinstance(node[1], int) and isinstance(node[2], int):
            return ("true", "bool", node[1], node[2]), "bool"
        else:
            return ("true", "bool"), "bool"
    elif tag == "false":
        if len(node) >= 3 and isinstance(node[1], int) and isinstance(node[2], int):
            return ("false", "bool", node[1], node[2]), "bool"
        else:
            return ("false", "bool"), "bool"
    elif tag == "id":
        name = node[1]
        # Extraer línea y columna si están presentes
        line = node[2] if len(node) > 2 and isinstance(node[2], int) else None
        col = node[3] if len(node) > 3 and isinstance(node[3], int) else None
        typ = symtable.lookup(name)
        if typ is None:
            if line is not None and col is not None:
                raise Exception(
                    f"Variable not declared at line {line} and column {col}"
                )
            else:
                raise Exception(f"Variable not declared: {name}")
        # Devuelve el nodo decorado conservando línea y columna si existen
        if line is not None and col is not None:
            return ("id", name, typ, line, col), typ
        else:
            return ("id", name, typ), typ
    elif tag == "num":
        return ("num", node[1], "int"), "int"
    elif tag == "string":
        return ("string", node[1], "string"), "string"
    elif tag == "binop":
        op = node[1]
        left, ltype = analyze_expr(node[2], symtable)
        right, rtype = analyze_expr(node[3], symtable)
        # Extrae línea y columna del nodo binop (posición del operador)
        line = node[4] if len(node) > 4 else None
        col = node[5] if len(node) > 5 else None
        if op == "+":
            # Permitir string+int, int+string, string+string
            if (
                (ltype == "string" and rtype == "string")
                or (ltype == "string" and rtype == "int")
                or (ltype == "int" and rtype == "string")
            ):
                return ("Concat", left, right), "string"
            elif ltype == "int" and rtype == "int":
                return ("binop", op, left, right, line, col), "int"
            else:
                raise Exception(f"Type error at line {line} and column {col}")
        elif op in ["-", "*"]:
            if ltype != "int" or rtype != "int":
                raise Exception(
                    f"Type error: arithmetic operations require int at line {line} and column {col}"
                )
            return ("binop", op, left, right, line, col), "int"
        elif op in ["and", "or"]:
            if ltype != "bool" or rtype != "bool":
                raise Exception(f"Type error at line {line} and column {col}")
            return ("binop", op, left, right, line, col), "bool"
        elif op in ["==", "<>", "<", ">", "<=", ">="]:
            if ltype != rtype:
                raise Exception(f"Type error at line {line} and column {col}")
            return ("binop", op, left, right, line, col), "bool"
        elif op == ",":
            # Para funciones, devolver tipo especial
            # Soportar tuplas de más de dos elementos
            if isinstance(ltype, str) and ltype.startswith("function with length="):
                llen = int(ltype.split("=")[1])
                if rtype == "int":
                    rlen = 1
                elif isinstance(rtype, str) and rtype.startswith(
                    "function with length="
                ):
                    rlen = int(rtype.split("=")[1])
                else:
                    rlen = 1
                total_len = llen + rlen
            elif ltype == "int":
                if rtype == "int":
                    total_len = 2
                elif isinstance(rtype, str) and rtype.startswith(
                    "function with length="
                ):
                    total_len = 1 + int(rtype.split("=")[1])
                else:
                    total_len = 2
            else:
                total_len = 2
            return (
                "binop",
                op,
                left,
                right,
                line,
                col,
            ), f"function with length={total_len}"
        elif op == ":":
            return ("binop", op, left, right, line, col), "TwoPoints"
        else:
            return ("binop", op, left, right, line, col), "unknown"
    elif tag == "uminus":
        expr, typ = analyze_expr(node[1], symtable)
        # Extraer línea y columna si existen
        line = node[2] if len(node) > 2 else None
        col = node[3] if len(node) > 3 else None
        if typ != "int":
            raise Exception(f"Type error in line {line} and column {col}")
        return ("uminus", expr, "int"), "int"
    elif tag == "not":
        expr, typ = analyze_expr(node[1], symtable)
        line = node[2] if len(node) > 2 else None
        col = node[3] if len(node) > 3 else None
        if typ != "bool":
            raise Exception(f"Type error in line {line} and column {col}")
        return ("not", expr, "bool"), "bool"
    elif tag == "app":
        left, ltype = analyze_expr(node[1], symtable)
        right, rtype = analyze_expr(node[2], symtable)
        # Para este lenguaje, asumimos que app solo se usa con funciones
        if not (isinstance(ltype, str) and ltype.startswith("function[..")):
            raise Exception("Type error: app requires function on left")
        # Validar que el índice sea int
        if rtype != "int":
            line = node[3] if len(node) > 3 else None
            col = node[4] if len(node) > 4 else None
            raise Exception(
                f"Error. Not integer index for function at line {line} and column {col}"
            )
        return ("app", left, right, "int"), "int"  # Suponemos que retorna int
    elif tag == "call":
        left, ltype = analyze_expr(node[1], symtable)
        right, rtype = analyze_expr(node[2], symtable)
        if not (isinstance(ltype, str) and ltype.startswith("function[..")):
            raise Exception("Type error: call requires function on left")
        return (
            "call",
            left,
            right,
            ltype,
        ), ltype  # Suponemos que retorna el tipo de la función
    else:
        return node, "unknown"


# Decoración y verificación de contexto en el AST
def analyze_context(node, symtable=None):
    if symtable is None:
        symtable = SymbolTable()

    tag = node[0] if isinstance(node, tuple) else None

    if tag == "block":
        new_table = SymbolTable(parent=symtable)
        stmts = analyze_context(node[1], new_table)
        return ("Block", new_table, stmts)

    elif isinstance(node, list):
        return [analyze_context(n, symtable) for n in node]

    elif tag == "declare":
        typ = node[1]
        for var in node[2]:
            name = var[1] if isinstance(var, tuple) else var
            if symtable.in_current_scope(name):
                raise Exception(f"Redeclaration of variable '{name}'")
            symtable.declare(name, typ)
        return ("Declare", typ, node[2])

    elif tag == "declare_func":
        typ = f"function[..{node[1]}]"
        for var in node[2]:
            name = var[1] if isinstance(var, tuple) else var
            if symtable.in_current_scope(name):
                raise Exception(f"Redeclaration of variable '{name}'")
            symtable.declare(name, typ)
        return ("DeclareFunc", typ, node[2])

    elif tag == "assign":
        varname = node[1]
        vartype = symtable.lookup(varname)
        line = node[3] if len(node) > 3 else None
        col = node[4] if len(node) > 4 else None
        if vartype is None:
            raise Exception(
                f"Variable {varname} not declared at line {line} and column {col}"
            )
        expr, exprtype = analyze_expr(node[2], symtable)
        if not type_compatible(vartype, exprtype):
            raise Exception(
                f"Type error. Variable {varname} has different type than expression at line {line} and column {col}"
            )
        return ("Asig", (varname, vartype), (expr, exprtype))

    elif tag == "print":
        expr, exprtype = analyze_expr(node[1], symtable)
        return ("Print", (expr, exprtype))

    elif tag == "skip":
        return ("skip",)

    elif tag == "while":
        cond, condtype = analyze_expr(node[1], symtable)
        if condtype != "bool":
            raise Exception("Type error: while condition must be bool")
        body = analyze_context(node[2], symtable)
        return ("While", (cond, condtype), body)

    elif tag == "if":
        guards = [analyze_guard(g, symtable) for g in node[1]]
        return ("If", guards)

    elif tag == "Guard":
        return analyze_guard(node[1], symtable)

    else:
        return node


# Análisis de guardas en el contexto
def analyze_guard(guard, symtable):
    if guard[0] == "guard":
        cond, condtype = analyze_expr(guard[1], symtable)
        if condtype != "bool":
            raise Exception("Type error: guard condition must be bool")
        body = analyze_context(guard[2], symtable)
        return ("Guard", (cond, condtype), body)
    else:
        return guard


# Impresión del AST decorado y tabla de símbolos
def print_decorated_ast(node, indent=0, sequenced=False):
    prefix = "-" * indent
    if isinstance(node, tuple):
        tag = node[0]
        if tag == "Block":
            print(f"{prefix}Block")
            print(f"{prefix}-Symbols Table")
            # Imprime la tabla de símbolos sin indentación extra
            for name, typ in node[1].symbols.items():
                print(f"{prefix}--variable: {name} | type: {typ}")
            # print(f"{prefix}-Sequencing")
            print_decorated_ast(node[2], indent + 1)
        elif tag == "Declare":
            pass
        elif tag == "DeclareFunc":
            pass
        elif tag == "Asig":
            print(f"{prefix}Asig")
            print(f"{prefix}-Ident: {node[1][0]} | type: {node[1][1]}")
            print_expr_decorated(node[2][0], indent + 1)
        elif tag == "Print":
            print(f"{prefix}Print")
            print_expr_decorated(node[1][0], indent + 1)
        elif tag == "skip":
            print(f"{prefix}skip")
        elif tag == "While":
            print(f"{prefix}While")
            print(f"{prefix}-Then")
            print_expr_decorated(node[1][0], indent + 2)
            print_decorated_ast(node[2], indent + 2)
        elif tag == "If":
            print(f"{prefix}If")
            for i, guard in enumerate(node[1]):
                print_decorated_ast(guard, indent + 1, sequenced=(i > 0))
        elif tag == "Guard":
            # Imprime 'Guard' solo si no es la primera guarda
            if not sequenced:
                print(f"{prefix}Guard")
            print(f"{prefix}-Then")
            print_expr_decorated(node[1][0], indent + 2)
            print_decorated_ast(node[2], indent + 2)
        elif tag == "binop":
            print_expr_decorated(node, indent)
        elif tag == "uminus" or tag == "not" or tag == "app" or tag == "call":
            print_expr_decorated(node, indent)
        elif (
            tag == "id"
            or tag == "num"
            or tag == "string"
            or tag == "true"
            or tag == "false"
        ):
            print_expr_decorated(node, indent)
        elif tag == "Sequencing":
            print(f"{prefix}Sequencing")
            stmts = node[1] if isinstance(node[1], list) else [node[1]]
            if len(stmts) == 1:
                print_decorated_ast(stmts[0], indent + 1, sequenced=sequenced)
            else:
                print_decorated_ast(stmts[0], indent + 1, sequenced=sequenced)
                print_decorated_ast(
                    ("Sequencing", stmts[1:]), indent + 1, sequenced=True
                )
        else:
            print(f"{prefix}{tag}")
            for child in node[1:]:
                print_decorated_ast(child, indent + 1)
    elif isinstance(node, list):
        # print("Nodo es una lista")
        # print(node[0][0])
        if not node:
            return
        if len(node) == 1:
            print_decorated_ast(node[0], indent)
        else:
            # Separate declarations from other nodes
            declarations = [n for n in node if n[0] in ("Declare", "DeclareFunc")]
            other_nodes = [n for n in node if n[0] not in ("Declare", "DeclareFunc")]

            # Process declarations first
            for decl in declarations:
                print_decorated_ast(decl, indent)

            def process_nested_sequencing(nodes, current_indent):
                """Procesa una lista de nodos y los organiza en secuencias anidadas."""
                if not nodes:
                    return
                if len(nodes) == 1:
                    # Si solo hay un nodo, procesarlo directamente
                    print_decorated_ast(nodes[0], current_indent)
                else:
                    print(f"{'-' * current_indent}Sequencing")
                    # Procesar el resto de los nodos recursivamente
                    process_nested_sequencing(nodes[:-1], current_indent + 1)
                    print_decorated_ast(nodes[-1], current_indent + 1)

            if other_nodes:
                process_nested_sequencing(other_nodes, indent)
    else:
        print(f"{prefix}{node}")


def print_expr_decorated(node, indent=0):
    """
    Imprime nodos de expresión decorados (con tipo) y crudos (sin tipo),
    usando el mismo formato que print_decorated_ast para expresiones.
    """
    prefix = "-" * indent
    if isinstance(node, tuple):
        tag = node[0]
        # Si el nodo ya viene decorado con tipo
        if tag == "id":
            # Imprime el tipo si está en la posición 2 (aunque haya más elementos)
            if len(node) > 2 and isinstance(node[2], str):
                print(f"{prefix}Ident: {node[1]} | type: {node[2]}")
            else:
                print(f"{prefix}Ident: {node[1]}")
        elif tag == "num":
            print(f"{prefix}Literal: {node[1]} | type: int")
        elif tag == "string":
            print(f"{prefix}String: {node[1]}")
        elif tag == "true":
            print(f"{prefix}Literal: true | type: bool")
        elif tag == "false":
            print(f"{prefix}Literal: false | type: bool")
        elif tag == "Concat":
            print(f"{prefix}Concat | type: String")
            print_expr_decorated(node[1], indent + 1)
            print_expr_decorated(node[2], indent + 1)
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
                current_indent = indent
                while (
                    isinstance(current, tuple)
                    and current[0] == "binop"
                    and current[1] == ","
                ):
                    print(
                        f"{'-' * current_indent}Comma | type: function with length={length}"
                    )
                    current = current[3]  # Avanzar al siguiente operando izquierdo
                    current_indent += 1
                    length -= 1

                # Luego imprimir los operandos hoja
                def print_operands(n, op_indent):
                    if isinstance(n, tuple) and n[0] == "binop" and n[1] == ",":
                        print_operands(n[2], op_indent)
                        print_operands(n[3], op_indent - 1)
                    else:
                        print_expr_decorated(n, op_indent)

                print_operands(node[2], current_indent)
                print_operands(node[3], current_indent)
            elif op == "TwoPoints":
                print(f"{prefix}TwoPoints")
                print_expr_decorated(node[2], indent + 1)
                print_expr_decorated(node[3], indent + 1)
            else:
                if op in ["Plus", "Minus", "Mult"]:
                    typ = "int"
                elif op in ["And", "Or"]:
                    typ = "bool"
                elif op in ["Equal", "NotEqual", "Less", "Greater", "Leq", "Geq"]:
                    typ = "bool"
                else:
                    typ = "unknown"
                print(f"{prefix}{op} | type: {typ}")
                print_expr_decorated(node[2], indent + 1)
                print_expr_decorated(node[3], indent + 1)
        elif tag == "uminus":
            print(f"{prefix}Minus | type: int")
            print_expr_decorated(node[1], indent + 1)
        elif tag == "not":
            print(f"{prefix}Not | type: bool")
            print_expr_decorated(node[1], indent + 1)
        elif tag == "app":
            print(f"{prefix}ReadFunction | type: int")
            print_expr_decorated(node[1], indent + 1)
            print_expr_decorated(node[2], indent + 1)
        elif tag == "call":
            # Imprime el tipo real de la función
            typ = node[3] if len(node) > 3 else "int"
            print(f"{prefix}WriteFunction | type: {typ}")
            print_expr_decorated(node[1], indent + 1)
            print_expr_decorated(node[2], indent + 1)
        else:
            # Cualquier otro nodo
            print(f"{prefix}{tag}")
            for child in node[1:]:
                print_expr_decorated(child, indent + 1)
    elif isinstance(node, list):
        for n in node:
            print_expr_decorated(n, indent)
    else:
        print(f"{prefix}{node}")


def find_column(input, lexpos):
    last_newline = input.rfind("\n", 0, lexpos)
    if last_newline < 0:
        last_newline = -1
    return lexpos - last_newline


def generate_AST(data):
    
    lexer.input(data)
    lexer.lineno = 1
    # Limpia errores previos del lexer si existen
    if hasattr(lexer, "errors"):
        del lexer.errors

    # Procesa el parseo y análisis, pero no imprimas nada aún
    try:
        result = parser.parse(data, lexer=lexer)
        #print(f'Resultado: \n{result}')
        decorated = analyze_context(result)
        #print(f'Decorado: \n{decorated}')
    except Exception as e:
        print(e)
        sys.exit(1)

    # Si todo fue exitoso, imprime los resultados
    # print_decorated_ast(decorated)

    return result, decorated


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

        # Procesa el parseo y análisis, pero no imprimas nada aún
        try:
            result = parser.parse(data, lexer=lexer)
            decorated = analyze_context(result)
        except Exception as e:
            print(e)
            sys.exit(1)

        # Si todo fue exitoso, imprime los resultados
        print_decorated_ast(decorated)

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)


if __name__ == "__main__":
    main()
