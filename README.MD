# Proyecto: Traductor de Lenguaje Imperativo al Lenguaje del Lamba Cálculo

![Made with Love](https://img.shields.io/badge/Made%20with-Love-pink?style=for-the-badge&logo=data:image/svg%2bxml;base64,PHN2ZyByb2xlPSJpbWciIHZpZXdCb3g9IjAgMCAyNCAyNCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48dGl0bGU+R2l0SHViIFNwb25zb3JzIGljb248L3RpdGxlPjxwYXRoIGQ9Ik0xNy42MjUgMS40OTljLTIuMzIgMC00LjM1NCAxLjIwMy01LjYyNSAzLjAzLTEuMjcxLTEuODI3LTMuMzA1LTMuMDMtNS42MjUtMy4wM0MzLjEyOSAxLjQ5OSAwIDQuMjUzIDAgOC4yNDljMCA0LjI3NSAzLjA2OCA3Ljg0NyA1LjgyOCAxMC4yMjdhMzMuMTQgMzMuMTQgMCAwIDAgNS42MTYgMy44NzZsLjAyOC4wMTcuMDA4LjAwMy0uMDAxLjAwM2MuMTYzLjA4NS4zNDIuMTI2LjUyMS4xMjUuMTc5LjAwMS4zNTgtLjA0MS41MjEtLjEyNWwtLjAwMS0uMDAzLjAwOC0uMDAzLjAyOC0uMDE3YTMzLjE0IDMzLjE0IDAgMCAwIDUuNjE2LTMuODc2QzIwLjkzMiAxNi4wOTYgMjQgMTIuNTI0IDI0IDguMjQ5YzAtMy45OTYtMy4xMjktNi43NS02LjM3NS02Ljc1em0tLjkxOSAxNS4yNzVhMzAuNzY2IDMwLjc2NiAwIDAgMS00LjcwMyAzLjMxNmwtLjAwNC0uMDAyLS4wMDQuMDAyYTMwLjk1NSAzMC45NTUgMCAwIDEtNC43MDMtMy4zMTZjLTIuNjc3LTIuMzA3LTUuMDQ3LTUuMjk4LTUuMDQ3LTguNTIzIDAtMi43NTQgMi4xMjEtNC41IDQuMTI1LTQuNSAyLjA2IDAgMy45MTQgMS40NzkgNC41NDQgMy42ODQuMTQzLjQ5NS41OTYuNzk3IDEuMDg2Ljc5Ni40OS4wMDEuOTQzLS4zMDIgMS4wODUtLjc5Ni42My0yLjIwNSAyLjQ4NC0zLjY4NCA0LjU0NC0zLjY4NCAyLjAwNCAwIDQuMTI1IDEuNzQ2IDQuMTI1IDQuNSAwIDMuMjI1LTIuMzcgNi4yMTYtNS4wNDggOC41MjN6Ii8+PC9zdmc+)


Proyecto práctico de las clases vistas en Traductores e Interpretadores (CI-3725) de la USB.

## Uso del proyecto

### Etapa 1

Para usar la etapa uno, es necesario ejecutar el archivo [lexer.py](lexer.py) de la siguiente forma:

```
python lexer.py <archivo.imperat>
```

Esto imprimirá el resultado del análisis léxico del archivo en la terminal. Es importante que el archivo tenga extensión _.imperat_ para que funcione.

### Etapas 2-3

Para usar las etapas dos y tres, es necesario ejecutar el archivo [parser.py](parser.py) de la siguiente forma:

```
python parser.py <archivo.imperat>
```

Esto imprimirá el resultado del análisis sintáctico y de contexto del archivo en la terminal, utilizando un Árbol Sintáctico Abstracto (AST) decorado, que a su vez realiza una verificación estática de errores e imprime estos errores en la terminal si los hay. Nuevamente, es importante que el archivo tenga extensión _.imperat_ para que funcione.

### Etapa 4 (entrega final)

Para usar la entrega final, que es la traducción y generación de la semántica basada en lambda cálculo, es necesario ejecutar el archivo [translate.py](translate.py) de la siguiente forma:

```
python translate.py <archivo.imperat>
```

Es importante que el archivo tenga extensión _.imperat_ para que funcione, esto generará el archivo _<archivo_output.py>_ que es el debe ser ejecutado finalmente como:

```
python <archivo_output.py>
```

## Grupo de Laboratorio

- Néstor Herrera [@Neonhh](https://github.com/Neonhh) (18-10786).
- Luis Isea [@lmisea](https://github.com/lmisea) (19-10175).
