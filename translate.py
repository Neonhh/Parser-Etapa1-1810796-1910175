import sys
from parse import SymbolTable, generate_AST
import ply.yacc as yacc
from lexer import lexer, tokens

def generate_python_file(input_filename, translated_code):
    # Genera el nombre del archivo de salida
    output_filename = f"{input_filename.replace('.input', '')}_output.py"
    
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

    # Escribe el archivo
    with open(output_filename, 'w', encoding='utf-8') as f:
        f.write(preamble)
        f.write(translated_code)
    
    return output_filename

def main():
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
            result, decorated = generate_AST(data)

            generate_python_file('prueb', '')

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)

if __name__ == "__main__":
    main()