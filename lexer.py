import ply.lex as lex
import ply.yacc as yacc

tokens = (
'TkIf','TkWhile','TkEnd', 'TkInt', 'TkPrint', 'TkFunction', 
'TkId', 'TkNum', 'TkString', 'TkTrue', 'TkFalse',
'TkOBlock', 'TkCBlock', 'TkSoForth', 'TkComma', 'TkOpenPar',
'TkClosePar', 'TkAsig', 'TkSemicolon', 'TkArrow', 'TkGuard',
'TkPlus', 'TkMinus', 'TkMult', 'TkOr', 'TkAnd', 'TkNot', 'TkLess',
'TkLeq', 'TkGeq', 'TkGreater', 'TkEqual', 'TkNEqual', 'TkOBracket',
'TkCBracket', 'TkTwoPoints', 'TkApp',
)

# Definiciones de Tokens
t_TkPlus = r'\+'
t_TkMinus = r'-'
t_TkMult = r'\*'
t_TkAsig = r':='
t_TkOpenPar = r'\('
t_TkClosePar = r'\)'
t_TkInt = r'int'

# Conversion al leer numeros enteros
def t_TkNum(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Caracteres a ignorar
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lex.lex()

lex.input("2+3")
for tok in iter(lex.token, None):
    print(repr(tok.type), repr(tok.value))