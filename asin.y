/*****************************************************************************/
/**  Ejemplo de BISON-I: S E M - 2         20201-2022 <jbenedi@dsic.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
%}

%token WHILE_ IF_ ELSE_ READ_ PRINT_ RETURN_ BOOL_ INT_ MENOS_ DIV_ FALSE_ APAR_ ABRA_ ACOR_ PUNTOYCOMA_ ID_ STRUCT_ PUNTO_
%token CTE_ OPAND_ OPOR_ OPE_ OPNE_ OPMAYOR_ OPMENOR_ OPMI_ OPMENI_ MAS_ POR_ TRUE_ CPAR_ CBRA_ CCOR_ IGUAL_ COMA_ OPDISTINTO_

%%

programa 	      	: listaDeclaraciones
                    ;

listaDeclaraciones	: declaracion
                    | listaDeclaraciones declaracion
                    ;
			
declaracion		    : declaracionVariable
                    | declaracionFuncion
                    ;
			
declaracionVariable	: tipoSimple ID_ PUNTOYCOMA_
                    | tipoSimple ID_ ABRA_ CTE_ CBRA_ PUNTOYCOMA_
                    | STRUCT_ ACOR_ listaCampos CCOR_ ID_ PUNTOYCOMA_
                    ;
			
tipoSimple		    : INT_
                    | BOOL_
                    ;

listaCampos		    : tipoSimple ID_ PUNTOYCOMA_
                    | listaCampos tipoSimple ID_ PUNTOYCOMA_
                    ;

declaracionFuncion	: tipoSimple ID_ APAR_ parametrosFormales CPAR_ bloque
                    ;

parametrosFormales	: 
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
                    
expresionRelacional : expresionAditiva
                    | expresionRelacional operadorRelacional expresionAditiva
                    ;
                    
expresionAditiva    : expresionMultiplicativa
                    | expresionAditiva operadorAditivo expresionMultiplicativa
                    ;
                    
expresionMultiplicativa: expresionUnaria
                    | expresionMultiplicativa operadorMultiplicativo expresionUnaria
                    ;
                    
expresionUnaria     : expresionSufija
                    | operadorUnario expresionUnaria
                    ;
    
expresionSufija     : constante
                    | APAR_ expresion CPAR_
                    | ID_
                    | ID_ PUNTO_ ID_
                    | ID_ ABRA_ expresion CBRA_
                    | ID_ APAR_ parametrosActuales CPAR_
                    ;
                    
constante           : CTE_ 
                    | TRUE_
                    | FALSE_
                    ;
                    
parametrosActuales  :
                    | listaParametrosActuales
                    ;
                    
listaParametrosActuales: expresion
                    | expresion COMA_ listaParametrosActuales
                    ;
                    
operadorLogico      : OPAND_
                    | OPOR_
                    ;
                    
operadorIgualdad    : OPE_
                    | OPNE_
                    ;
                    
operadorRelacional  : OPMAYOR_
                    | OPMENOR_
                    | OPMI_
                    | OPMENI_
                    ;
                    
operadorAditivo     : MAS_
                    | MENOS_
                    ;
                    
operadorMultiplicativo: POR_
                    | DIV_
                    ;
                    
operadorUnario      : MAS_
                    | MENOS_
                    | OPDISTINTO_
                    ;
                    
%%

