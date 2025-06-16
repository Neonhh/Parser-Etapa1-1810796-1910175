# --------------------------------------------------------------
# Etapa 1: Análisis Lexicográfico
# Proyecto: Traductor de Lenguaje Imperativo al Lenguaje del Lamba Cálculo
# Estudiantes: Néstor Herrera (18-10786) y Luis Isea (19-10175)
# --------------------------------------------------------------

import ply.lex as lex
import sys

# Almacenamos aqui la informacion de tokens en lugar de imprimir directamente.
# Esto para poder evitar que se muestren tokens en caso de error
foundTokens = []

# Se especifican las palabras reservadas para distinguirlas de los tokens que representan variables
reserved = {'or':'TkOr', 'and':'TkAnd', 'if':'TkIf', 'fi':'TkFi', 'int': 'TkInt', 'while':'TkWhile',
            'end':'TkEnd','print':'TkPrint','function':'TkFunction', 'bool': 'TkBool', 'true':'TkTrue',
            'false':'TkFalse', 'skip': 'TkSkip'}

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
t_TkFi = r'fi'
t_TkInt = r'int'
t_TkWhile = r'while'
t_TkEnd = r'end'
t_TkPrint = r'print'
t_TkBool = r'bool'
t_TkFunction = r'function'
t_TkTrue = r'true'
t_TkFalse = r'false'
t_TkSkip = r'skip'

# Definiciones de Tokens para identificadores y cadenas
def t_TkId(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    
    t.type = reserved.get(t.value,'TkId')
    
    return t
# todo lo que no sea un token conocido y sea alfa-numérico sin comillas lo consideramos un identificador?
# t_TkId = r'[a-zA-Z_][a-zA-Z0-9_]*' # Regex para identificadores alfanuméricos

# Definiciones de Tokens para cadenas de texto
t_TkString = r'\"([^\\\"\n]|\\([\\n\"]))*\"' # Regex para cadenas de texto con comillas dobles
# Lado derecho de |: Caracteres no escapables (No se permiten saltos de linea dentro de string)
# Lado izquierdo: Caracteres escapados (Solo se pueden escapar \, \n y ")

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

# Se crea una clase para almacenar mensajes de error, en caso de haberlos
class LexerState:
    def __init__(self):
        self.errors = []
        self.valid = True  # Indica si hay errores

state = LexerState()

def t_error(t):
    # Buscar el último salto de línea antes del error (t.lexpos)
    last_newline = t.lexer.lexdata.rfind('\n', 0, t.lexpos)
    column = (t.lexpos - last_newline) if last_newline >= 0 else t.lexpos + 1

    state.errors.append(f"Error: Unexpected character \"{t.value[0]}\" in row {t.lineno}, column {column}")
    state.valid = False

    t.lexer.skip(1)

lexer = lex.lex()

def main():

    # Procesamiento del input por consola 
    if len(sys.argv) != 2:
        print("Uso: python lexer.py <archivo.imperat>")
        sys.exit(1)

    filename = sys.argv[1]
    
    # Chequear que el archivo tenga extensión .imperat
    if not filename.endswith('.imperat'):
        print("Error: El archivo debe tener extensión .imperat")
        sys.exit(1)

    try:
        # Lee el contenido del archivo
        with open(filename, 'r', encoding='utf-8') as file:
            data = file.read()

        lex.input(data)
            
        for tok in iter(lex.token, None):
            tokId = tok.type

            #Cambia la forma de representar estos tokens para que coincida con el output esperado
            if tokId == 'TkId':
                tokId = f"{tokId}(\"{tok.value}\")"
            elif tokId == 'TkNum' or tokId == 'TkString':
                tokId = f"{tokId}({tok.value})"
            
            # Busca el último salto de línea antes de `lexpos`
            last_break = lexer.lexdata.rfind('\n', 0, tok.lexpos)
            column = tok.lexpos - last_break if last_break >= 0 else tok.lexpos + 1

            foundTokens.append(f"{tokId} {tok.lineno} {column}")
        
        if state.valid:
            print("\n".join(foundTokens))
        else:
            print("\n".join(state.errors))
    
    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)


if __name__ == "__main__":
    main()