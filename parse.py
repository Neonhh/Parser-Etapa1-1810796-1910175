# --------------------------------------------------------------
# Etapa 2: Análisis Sintáctico con Árbol Sintáctico
# Proyecto: Traductor de Lenguaje Imperativo al Lenguaje del Lamba Cálculo
# Estudiantes: Néstor Herrera (18-10786) y Luis Isea (19-10175)
# --------------------------------------------------------------

import ply.yacc as yacc
from lexer import lexer
import sys


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
        for token in lexer:
            tokenId = token.type

            # Cambia la forma de representar estos tokens para que coincida con el output esperado
            if tokenId == "TkId":
                tokenId = f'{tokenId}("{token.value}")'
            elif tokenId == "TkNum" or tokenId == "TkString":
                tokenId = f"{tokenId}({token.value})"

            # Busca el último salto de línea antes de `lexpos`
            last_break = lexer.lexdata.rfind("\n", 0, token.lexpos)
            column = token.lexpos - last_break if last_break >= 0 else token.lexpos + 1

            print(f"{tokenId} {token.lineno} {column}")

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)


if __name__ == "__main__":
    main()
