/*****************************************************************************/
/**   Ejemplo de un posible fichero de cabeceras donde situar las           **/
/** definiciones de constantes, variables y estructuras para MenosC.        **/
/** Los alumos deberan adaptarlo al desarrollo de su propio compilador.     **/
/*****************************************************************************/
#ifndef _HEADER_H
#define _HEADER_H

typedef struct lis /********************************* Estructura para listas */
{              
  int talla;        
  int ref;         
}ListaP;

typedef struct refe {
	int ref1;
	int ref2;
	int ref3;
} REFE;

typedef struct expr { /*************************** Estructura para expresiones */
	int tipo;
	int d;
} EXPR;

int lMain;

/***************************************************** Constantes Simbolicas */
#define TALLA_TIPO_SIMPLE 1 /* Talla asociada a los tipos simples */
#define TALLA_SEGENLACES 2 /* Talla del segmento de Enlaces de Control */


/****************************************************** Constantes generales */
#define TRUE  1
#define FALSE 0

/**************************************************************** Operadores */
// Lògic
#define OP_AND 0
#define OP_OR 1
// Igualtat
#define OP_IGUAL 0
#define OP_NOIGUAL 1
// Relacional
#define OP_MAYOR 0
#define OP_MENOR 1
#define OP_MAYOROIG 2
#define OP_MENOROIG 3
// Addició
#define OP_SUMA 0
#define OP_RESTA 1
// Multiplicació
#define OP_MULT 0
#define OP_DIV 1
// Unari
#define OP_SUMAUN 0
#define OP_RESTAUN 1
#define OP_NOT 2

/*****************************************************************************/
/************************************* Variables externas definidas en el AL */
extern int yylex();
extern int yyparse();

extern FILE *yyin;                           /* Fichero de entrada           */
extern int   yylineno;                       /* Contador del numero de linea */
extern char *yytext;                         /* Patron detectado             */
/********* Funciones y variables externas definidas en el Programa Principal */
extern void yyerror(const char * msg) ;   /* Tratamiento de errores          */

extern int verbosidad;                   /* Flag si se desea una traza       */
extern int numErrores;              /* Contador del numero de errores        */


/************************ Variables externas definidas en Programa Principal */
extern int verTdS; /* Flag para saber si mostrar la TdS */
/**************************** Variables externas definidas en las librer´ıas */
extern int dvar; /* Desplazamiento en el Segmento de Variables */
extern int niv; /* Nivel de anidamiento "global" o "local" */

extern int si; /* Desplazamiento relativo en el Segmento de codigo*/
#endif  /* _HEADER_H */
/*****************************************************************************/
