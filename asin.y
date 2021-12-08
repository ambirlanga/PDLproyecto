/*****************************************************************************/
/**  Ejemplo de BISON-I: S E M - 2         20201-2022 <jbenedi@dsic.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"
%}

%union{
    int     cent;
    char*   ident;
    Listap  lis;
}
//Todos los simbolos terminales que no necesiten atributos van con token y ya.

%token WHILE_ IF_ ELSE_ READ_ PRINT_ RETURN_ BOOL_ INT_ MENOS_ DIV_ FALSE_ APAR_ ABRA_ ACOR_ PUNTOYCOMA_ STRUCT_ PUNTO_

%token OPAND_ OPOR_ OPE_ OPNE_ OPMAYOR_ OPMENOR_ OPMI_ OPMENI_ MAS_ POR_ TRUE_ CPAR_ CBRA_ CCOR_ IGUAL_ COMA_ OPDISTINTO_


//Solamente los que tienen atributos se definen con ellos.

%token<cent> CTE_
%token<ident> ID_



//No terminales con atributos se definen con %type<x>

%type<cent> declaracionVariable  declaracion declaracionFuncion listaDeclaraciones tipoSimple instruccionAsignacion
%type<cent> expresion expresionIgualdad expresionRelacional expresionAditiva expresionMultiplicativa expresionUnaria expresionSufija 
%type<cent> constante operadorLogico operadorIgualdad operadorRelacional operadorAditivo operadorMultiplicativo operadorUnario
%type<cent> parametrosFormales parametrosActuales listaParametrosActuales listaCampos
%type<lis> listaParametrosFormales


%%

programa 	    : {dvar=0; niv = 0; cargaContexto(niv);} listaDeclaraciones
        {
            if(verTdS) mostrarTdS();
        }
                    ;
    
listaDeclaraciones  : declaracion {$$ = $1;}
                    | listaDeclaraciones declaracion {$$ = $1 + $2;} 
                    ;
			
declaracion	    : declaracionVariable {$$ = 0;} 
                    | declaracionFuncion {$$ = $1;}
                    ;
			
declaracionVariable : tipoSimple ID_ PUNTOYCOMA_
            {
                if(! insTdS($2, VARIABLE, $1, niv, dvar, -1))
                    yyerror("Variable con identificador ya existente");
                else 
                    dvar += TALLA_TIPO_SIMPLE;
            }
                    | tipoSimple ID_ ABRA_ CTE_ CBRA_ PUNTOYCOMA_
                        {int numelem = $4;
                         if($4 <= 0){
                            yyerror("Talla inapropiada del array");
                            numelem = 0;
                         }
                         // Creamos una referencia en la tabla de arrays para este array
                         int refe = insTdA($1, numelem);
                         /*Insertamos en la tabla de simbolos 
                            ($2=ID,
                             VARIABLE = Codigo en la cabecera de la libreria que nos indica que estamos trantando con una
                                        variable y no con una funcion, parametro o nulo)
                             T_ARRAY  = El tipo que sabemos que en este caso es array
                             niv      = Nivel de anidamiento en el bloque
                             dvar     = Desplazamiento relativo en el segmento de variables
                             refe     = Enlace a la tabla de array
                           Esta funcion en el caso de devolver Falso significaria que el identificador ya existe
                         */
                         if( !insTdS($2, VARIABLE, T_ARRAY, niv, dvar, refe))
                            yyerror("Identificador repetido")
                         else dvar += numelem * TALLA_TIPO_SIMPLE;
                         }
                    | STRUCT_ ACOR_ listaCampos CCOR_ ID_ PUNTOYCOMA_
		    	{
                         if( !insTdS($5, VARIABLE, T_RECORD, niv, dvar, -1)){yyerror("Identificador repetido");}
                         else{
                         }
                        }
                    ;
			
tipoSimple	    : INT_  {$$ = T_ENTERO;}
                    | BOOL_ {$$ = T_LOGICO;}
                    ;

listaCampos	    : tipoSimple ID_ PUNTOYCOMA_
                    | listaCampos tipoSimple ID_ PUNTOYCOMA_
                    ;

declaracionFuncion  : tipoSimple ID_ APAR_ parametrosFormales CPAR_ bloque
                    ;

parametrosFormales  : {$$ = insTdD(-1, T_VACIO);}
                    | listaParametrosFormales {$$ = $1.ref;}
                    ;

listaParametrosFormales: tipoSimple ID_
        {   
            $$.ref = insTdD(-1, $1);
            $$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
            if(!insTdS($2, PARAMETRO, $1, niv, -talla, -1))
                yyerror("Parametro con identificador ya existente");
        }
                    | tipoSimple ID_ COMA_ listaParametrosFormales
        {   
            $$.ref = insTdD($4.ref, $1);
            $$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
            if(!insTdS($2, PARAMETRO, $1, niv, -talla, -1))
                yyerror("Parametro con identificador ya existente");
        }
                    ;
			
bloque              : ACOR_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PUNTOYCOMA_ CCOR_
        {
          INF inf = obtTdD(-1);
          if (inf.t != T_ERROR && inf.t != $5){ yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo tipo.")}
        }
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
                        {SIMB sim = obtTdS($1);
                         if(sim.t == T_ERROR)yerror("Objeto no declarado");
                         else if (!(sim.t == $3.t == T_ENTERO || sim.t == $3.t == T_LOGICO))
                            yyerror("Error de tipos en la instrucción de asignación’");
                        }
                    | ID_ ABRA_ expresion CBRA_ IGUAL_ expresion PUNTOYCOMA_
                        {SIMB sim = obtTdS($1);
                         if(sim.t == T_ERROR){yerror("Objeto no declarado");}
                         else{
                            if($6 != T_ERROR){
                                if(sim.t == T_ARRAY){yyerror("ID no es de un array");}
                                else{
                                    DIM dim = obtTdA(sim.ref);
                                    if(!(dim.telem == $6)){ yyerror("Error de tipos, no coincide tipo de array con expresion");}                                    
                                    else{
                                        if($3 != T_ENTERO){yyerror("Indice debe ser de tipo entero");}
                                        else{ $$ = sim.t; }
                                    }
                                }
                            }
                         }
                        }
                    | ID_ PUNTO_ ID_ IGUAL_ expresion PUNTOYCOMA_
                    ;

instruccionEntradaSalida: READ_ APAR_ ID_ CPAR_ PUNTOYCOMA_
                        {if($3 != T_ENTERO) {$$ = T_ERROR; yyerror("WRITE: El identificador no es un entero");}}
                    | PRINT_ APAR_ expresion CPAR_ PUNTOYCOMA_
                        {if($3 != T_ENTERO) {$$ = T_ERROR; yyerror("PRINT: La expresion no es un entero");}}
                    ;

instruccionSeleccion: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
                        {if($3 != T_LOGICO) {$$ = T_ERROR; yyerror("IF: Se esperaba una expresion logica");}}
                    ;
                
instruccionIteracion: WHILE_ APAR_ expresion CPAR_ instruccion
                        {if($3 != T_LOGICO) {$$ = T_ERROR; yyerror("WHILE: Se esperaba una expresion logica");}}
                    ;

expresion           : expresionIgualdad {$$ = $1}
                    | expresion operadorLogico expresionIgualdad
            {
                $$.t = T_ERROR;
                if ($1.t != T_ERROR || $3.t != T_ERROR) {
                    if ($1.t == $3.t && $1.t != T_LOGICO) {
                        $$.t = T_LOGICO;
                    } else {
                        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o son booleanos.");;
                    }
                }
            }
                    ;
                    
expresionIgualdad   : expresionRelacional {$$ = $1;}
                    | expresionIgualdad operadorIgualdad expresionRelacional
                        {if ($1 == $3 == T_ENTERO || $1 == $3 == T_LOGICO) {$$ = T_LOGICO;}
                         else{$$ = T_ERROR; yyerror("Los tipos (igualdad) no coinciden");}}
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
		    		if ($1 == OP_SUMAUN || $1 == OP_RESTAUN) {
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

expresionSufija     : constante { $$.t = $1.t; }
                    | APAR_ expresion CPAR_ { $$.t = $2.t; }
                    | ID_
            {   
                $$ = T_ERROR;
                SIMB sim = obtTdS($1);
                if (sim.t != T_ERROR) {$$.t = sim.t;}   
                else {yyerror("Variable mal declarada"); }
            }
                    | ID_ PUNTO_ ID_
                    | ID_ ABRA_ expresion CBRA_
		    	{       
                            $$ = T_ERROR;
                            SIMB sim = obtenerTDS($1);         
                            if (sim.t != T_ERROR) {yyerror("Variable no declarada")}
                            else if (sim.t != T_ARRAY) {yyerror("Se esperaba un tipo vector")}
                            else{
                                if ($3 != T_ENTERO) {yyerror("VECTOR: La variable con la que se accede ha de ser un entero");}                             
                            }                              
                        }
                    | ID_ APAR_ parametrosActuales CPAR_
                    ;
                    
constante           : CTE_   {$$ = T_ENTERO;}
                    | TRUE_  {$$ = T_LOGICO;}
                    | FALSE_ {$$ = T_LOGICO;}
                    ;
                    
parametrosActuales  : {$$ = insTdD(-1, T_VACIO);}
                    | listaParametrosActuales {$$ = $1;}
                    ;
                    
listaParametrosActuales: expresion {$$ = insTdD(-1,$1);}
                    | expresion COMA_ listaParametrosActuales {$$ = insTdD($3,$1);}
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
                    
operadorUnario      : MAS_ { $$ = OP_SUMAUN; }
                    | MENOS_ { $$ = OP_RESTAUN; }
                    | OPDISTINTO_ { $$ = OP_NOT; }
                    ;
%%
