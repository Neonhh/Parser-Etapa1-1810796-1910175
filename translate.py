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

def traduce_to_lambda(node,lambda_state =[],current_lambda=''):
    """ 
    Recibe el AST y genera las instrucciones correspondientes para lambda-calculo en python
    """

    if isinstance(node,tuple):
        tag = node[0]

        if tag=="Block":
            for var in node[1].symbols.keys():
                lambda_state.append(var)
            
            traduce_to_lambda(node[2][1:],lambda_state,'') #El primer elemento tiene el tag declare y no lo necesitamos
        
        elif tag=="Asig":
            changed_var = node[1][0]
            new_val = node[2][0][1]

            new_state = ''
            for var in lambda_state:
                new_state = f'lambda {var}:{new_state}'
            
            state_string = 'nil'
            for var in lambda_state:
                if var == changed_var:
                    var = new_val
                    
                state_string = f'cons({var}) ({state_string})'

            new_state = f'apply({new_state} {state_string})'
            
            print(new_state)
            return new_state

    
    if isinstance(node,list):
        if not node:
            return
        if len(node) == 1:
            traduce_to_lambda(node[0],lambda_state,'')
        
        else:
            def process_nested_sequencing(nodes, lambda_state, current_seq = ''):
                """Procesa una lista de nodos y los organiza en secuencias anidadas."""
                if not nodes:
                    return
                if len(nodes) == 1:
                    # Si solo hay un nodo, procesarlo directamente   
                    return f'{current_seq} ({traduce_to_lambda(nodes[0], lambda_state)} x1)'
                elif len(nodes) == 2:
                    return f'{current_seq} (lambda x1: {traduce_to_lambda(nodes[1], lambda_state)} ({traduce_to_lambda(nodes[0], lambda_state)} x1))'
                else:
                    # Procesar el resto de los nodos recursivamente
                    return process_nested_sequencing(nodes[:-2], lambda_state,current_seq=f"{current_seq}(lambda x1: {traduce_to_lambda(nodes[-1], lambda_state)} (lambda x1: {traduce_to_lambda(nodes[-1], lambda_state)} x1)) ")
                
                #process_nested_sequencing(nodes[:-1], lambda_state, 
                 #                             current_seq=f'{current_seq} {traduce_to_lambda(nodes[-1], lambda_state)}')
            
            seq = process_nested_sequencing(node, lambda_state)
            seq = f'({seq})'
            print(seq)


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

            print(result)
            print()
            print(decorated)

            traduce_to_lambda(decorated)
            generate_python_file('prueb', '')

    except FileNotFoundError:
        print(f"Error: El archivo '{filename}' no existe.")
        sys.exit(1)

if __name__ == "__main__":
    main()