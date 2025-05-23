# --------------------------------------------------------------
# Etapa 1: Análisis Lexicográfico
# Proyecto: Traductor de Lenguaje Imperativo al Lenguaje del Lamba Cálculo
# Estudiantes: Néstor Herrera (18-10786) y Luis Isea (19-10175)
# --------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc

# Se especifican las palabras reservadas para distinguirlas de los tokens que representan variables
reserved = {'or':'TkOr', 'and':'TkAnd', 'if':'TkIf', 'int': 'TkInt', 'while':'TkWhile',
            'end':'TkEnd','print':'TkPrint','function':'TkFunction','true':'TkTrue','false':'TkFalse'}

tokens = list(reserved.values()) + [
'TkId', 'TkNum', 'TkString', 
'TkOBlock', 'TkCBlock', 'TkSoForth', 'TkComma', 'TkOpenPar',
'TkClosePar', 'TkAsig', 'TkSemicolon', 'TkArrow', 'TkGuard',
'TkPlus', 'TkMinus', 'TkMult', 'TkNot', 'TkLess',
'TkLeq', 'TkGeq', 'TkGreater', 'TkEqual', 'TkNEqual', 'TkOBracket',
'TkCBracket', 'TkTwoPoints', 'TkApp',
]

# Definiciones de Tokens para separadores
t_TkOBlock = r'\{'
t_TkCBlock = r'\}'
t_TkSoForth = r'\.\.'
t_TkComma = r','
t_TkOpenPar = r'\('
t_TkClosePar = r'\)'
t_TkAsig = r':='
t_TkSemicolon = r';'
t_TkArrow = r'-->'
t_TkGuard = r'\[\]'

# Definiciones de Tokens para operadores
t_TkPlus = r'\+'
t_TkMinus = r'-'
t_TkMult = r'\*'
t_TkOr = r'or'
t_TkAnd = r'and'
t_TkNot = r'!'
t_TkLess = r'<'
t_TkLeq = r'<='
t_TkGeq = r'>='
t_TkGreater = r'>'
t_TkEqual = r'=='
t_TkNEqual = r'<>'
t_TkOBracket = r'\['
t_TkCBracket = r'\]'
t_TkTwoPoints = r':'
t_TkApp = r'\.'

# Definiciones de Tokens para palabras reservadas
t_TkIf = r'if'
t_TkInt = r'int'
t_TkWhile = r'while'
t_TkEnd = r'end'
t_TkPrint = r'print'
t_TkFunction = r'function'
t_TkTrue = r'true'
t_TkFalse = r'false'

# Definiciones de Tokens para identificadores y cadenas
def t_TkId(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    
    t.type = reserved.get(t.value,'TkId')
    
    return t
# todo lo que no sea un token conocido y sea alfa-numérico sin comillas lo consideramos un identificador?
# t_TkId = r'[a-zA-Z_][a-zA-Z0-9_]*' # Regex para identificadores alfanuméricos

# Definiciones de Tokens para cadenas de texto
t_TkString = r'\"([^\\\"]|\\.)*\"' # Regex para cadenas de texto con comillas dobles

# Conversion al leer numeros enteros
def t_TkNum(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Caracteres a ignorar
t_ignore = " \t"

# Comentarios
t_ignore_COMMENT = r'//.*'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    global has_errors 
    # Buscar el último salto de línea antes del error (t.lexpos)
    last_newline = t.lexer.lexdata.rfind('\n', 0, t.lexpos)
    column = (t.lexpos - last_newline - 1) if last_newline >= 0 else t.lexpos

    print(f"Error: Unexpected character \"{t.value[0]}\" in row {t.lineno}, column {column}")

    t.lexer.skip(1)


lexer = lex.lex()

lex.input("{\nint a, b, c;\nfunction[..2] d, e, f;\na := b + 3;\nprint e\n// Esto es un comentario. Debe ser ignorado.\n}")
for tok in iter(lex.token, None):
    tokId = tok.type

#Cambia la forma de representar estos tokens para que coincida con el output esperado
    if tokId == 'TkId':
        tokId = f"{tokId}(\"{tok.value}\")"
    elif tokId == 'TkNum':
        tokId = f"{tokId}({tok.value})"
    # Busca el último salto de línea antes de `lexpos`
    last_break = lexer.lexdata.rfind('\n', 0, tok.lexpos)
    column = tok.lexpos - last_break - 1 if last_break >= 0 else tok.lexpos

    print(f"{tokId} {tok.lineno} {column}")