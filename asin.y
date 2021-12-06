/*****************************************************************************/
/**  Ejemplo de BISON-I: S E M - 2         20201-2022 <jbenedi@dsic.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
%}

%union {
	int cent;
	char* ident;
}

%token WHILE_ IF_ ELSE_ READ_ PRINT_ RETURN_ BOOL_ INT_ MENOS_ DIV_ FALSE_ APAR_ ABRA_ ACOR_ PUNTOYCOMA_ ID_ STRUCT_ PUNTO_
%token CTE_ OPAND_ OPOR_ OPE_ OPNE_ OPMAYOR_ OPMENOR_ OPMI_ OPMENI_ MAS_ POR_ TRUE_ CPAR_ CBRA_ CCOR_ IGUAL_ COMA_ OPDISTINTO_

%token <cent> CTE_
%token <ident> ID_

%%

programa 	    : listaDeclaraciones
                    ;

listaDeclaraciones	: declaracion
                    | listaDeclaraciones declaracion
                    ;
			
declaracion	    : declaracionVariable
                    | declaracionFuncion
                    ;
			
declaracionVariable	: tipoSimple ID_ PUNTOYCOMA_
                    | tipoSimple ID_ ABRA_ CTE_ CBRA_ PUNTOYCOMA_
                    | STRUCT_ ACOR_ listaCampos CCOR_ ID_ PUNTOYCOMA_
                    ;
			
tipoSimple	    : INT_ { $$ = T_ENTERO; }
                    | BOOL_ { $$ = T_LOGICO; }
                    ;

listaCampos	    : tipoSimple ID_ PUNTOYCOMA_
                    | listaCampos tipoSimple ID_ PUNTOYCOMA_
                    ;

declaracionFuncion  : tipoSimple ID_ APAR_ parametrosFormales CPAR_ bloque
                    ;

parametrosFormales  : 
                    | listaParametrosFormales
                    ;

listaParametrosFormales: tipoSimple ID_
                       | tipoSimple ID_ COMA_ listaParametrosFormales
                       ;
			
bloque              : ACOR_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PUNTOYCOMA_ CCOR_
                    ;

declaracionVariableLocal: 
                    | declaracionVariableLocal declaracionVariable
                    ;
listaInstrucciones  : 
                    | listaInstrucciones instruccion
                    ;
                    
instruccion         : ACOR_ listaInstrucciones CCOR_ 
                    | instruccionAsignacion
                    | instruccionSeleccion
                    | instruccionEntradaSalida
                    | instruccionIteracion
                    ;
                    
instruccionAsignacion: ID_ IGUAL_ expresion PUNTOYCOMA_
                    | ID_ ABRA_ expresion CBRA_ IGUAL_ expresion PUNTOYCOMA_
                    | ID_ PUNTO_ ID_ IGUAL_ expresion PUNTOYCOMA_
                    ;

instruccionEntradaSalida: READ_ APAR_ ID_ CPAR_ PUNTOYCOMA_
                    | PRINT_ APAR_ expresion CPAR_ PUNTOYCOMA_
                    ;

instruccionSeleccion: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
                    ;
                
instruccionIteracion: WHILE_ APAR_ expresion CPAR_ instruccion
                    ;

expresion           : expresionIgualdad
                    | expresion operadorLogico expresionIgualdad
                    ;
                    
expresionIgualdad   : expresionRelacional
                    | expresionIgualdad operadorIgualdad expresionRelacional
                    ;
                    
expresionRelacional : expresionAditiva { $$.t = $1.t; }
                    | expresionRelacional operadorRelacional expresionAditiva
		    {
		        $$.t = T_ERROR;
		        if ($1.t != T_ERROR && $3.t != T_ERROR) {
		    	    if (!($1.t == $3.t && $1.t == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo tipo.");
		    	    } else {
		    	        $$.t = T_LOGICO;
		    	    }
		        }
		    }
                    ;
                    
expresionAditiva    : expresionMultiplicativa { $$.t = $1.t; }
                    | expresionAditiva operadorAditivo expresionMultiplicativa
		    {
		        $$.t = T_ERROR;
		        if ($1.t != T_ERROR && $3.t != T_ERROR) {
		    	    if (!($1.t == $3.t && $1.t == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo tipo");
		    	    } else {
		    	        $$.t = T_ENTERO;
		    	    }
		        }
		    }
                    ;
                    
expresionMultiplicativa: expresionUnaria { $$.t = $1.t; }
                    | expresionMultiplicativa operadorMultiplicativo expresionUnaria
		    {
		        $$.t = T_ERROR;
		        if ($1.t != T_ERROR && $3.t != T_ERROR) {
		    	    if (!($1.t == $3.t && $1.t == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo.");
		    	    } else {
		    	        $$.t = T_ENTERO;
		    	    }
		        }
		    }
                    ;
                    
expresionUnaria     : expresionSufija { $$.t = $1.t; }
                    | operadorUnario expresionUnaria
		    {
		    	$$.t = T_ERROR;
		    	if ($2.t != T_ERROR) {
		    	    if ($2.t == T_ENTERO) {
		    		if ($1 == OP_NOT) {
		    		    yyerror("Error con la incompatibilidad de tipos, no se puede negar un entero.");
		    		} else {
		    		    $$.t = T_ENTERO;
		    		}
		    	    } else if ($2.t == T_LOGICO) {
		    		if ($1 == OP_SUMA || $1 == OP_RESTA) {
		    		    yyerror("Error con la incompatibilidad de tipos, solo se puede aplicar el operador unario '+' o '-' a una expresión entera.");
		    		} else {
		    		    $$.t = T_LOGICO;
		    		}
		    	    } else {
		    		yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo.");
		    	    }
		    	}
		    }
                    ;
    
expresionSufija     : constante
                    | APAR_ expresion CPAR_
                    | ID_
                    | ID_ PUNTO_ ID_
                    | ID_ ABRA_ expresion CBRA_
                    | ID_ APAR_ parametrosActuales CPAR_
                    ;
                    
constante           : CTE_ { $$.t = T_ENTERO; }
                    | TRUE_ { $$.t = T_LOGICO; }
                    | FALSE_ { $$.t = T_LOGICO; }
                    ;
                    
parametrosActuales  :
                    | listaParametrosActuales
                    ;
                    
listaParametrosActuales: expresion
                    | expresion COMA_ listaParametrosActuales
                    ;
                    
operadorLogico      : OPAND_ { $$ = OP_AND; }
                    | OPOR_ { $$ = OP_OR; }
                    ;
                    
operadorIgualdad    : OPE_ { $$ = OP_IGUAL; }
                    | OPNE_ { $$ = OP_NOIGUAL; }
                    ;
                    
operadorRelacional  : OPMAYOR_ { $$ = OP_MAYOR; }
                    | OPMENOR_ { $$ = OP_MENOR; }
                    | OPMI_ { $$ = OP_MAYOROIG; }
                    | OPMENI_ { $$ = OP_MENOROIG; }
                    ;
                    
operadorAditivo     : MAS_ { $$ = OP_SUMA; }
                    | MENOS_ { $$ = OP_RESTA; }
                    ;
                    
operadorMultiplicativo: POR_ { $$ = OP_MULT; }
                    | DIV_ { $$ = OP_DIV; }
                    ;
                    
operadorUnario      : MAS_ { $$ = OP_SUMA; }
                    | MENOS_ { $$ = OP_RESTA; }
                    | OPDISTINTO_ { $$ = OP_NOT; }
                    ;
                    
%%

