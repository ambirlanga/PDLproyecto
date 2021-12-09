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
    ListaP  lis;
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
%type<cent> parametrosFormales parametrosActuales listaParametrosActuales 
%type<lis> listaParametrosFormales listaCampos


%type<cent> instruccionEntradaSalida instruccionSeleccion instruccionIteracion



%%

programa 	    : {dvar=0; niv = 0; cargaContexto(niv);} listaDeclaraciones
            {if($2 == 0){yyerror("el programa no tiene main");}}
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
                    yyerror("Identificador variable repetido");
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
                            yyerror("Identificador variable repetido");
                         else dvar += numelem * TALLA_TIPO_SIMPLE;
                         }
                    | STRUCT_ ACOR_ listaCampos CCOR_ ID_ PUNTOYCOMA_
		    	        {
                         if( !insTdS($5, VARIABLE, T_RECORD, niv, dvar, -1)){yyerror("Identificador repetido");}
                         else
                            dvar += TALLA_TIPO_SIMPLE;
                        }
                    ;
			
tipoSimple	    : INT_  {$$ = T_ENTERO;}
                    | BOOL_ {$$ = T_LOGICO;}
                    ;

listaCampos	    : tipoSimple ID_ PUNTOYCOMA_
            {
                int refe = insTdR(-1, $2, $1, 0);
                $$.talla = TALLA_TIPO_SIMPLE;
                $$.ref = refe;
            }
                    | listaCampos tipoSimple ID_ PUNTOYCOMA_
            {
                int refe = insTdR($1.ref, $3, $2, $1.talla);
                $$.ref = $1.ref;
                if(refe==-1){yyerror("Nombre de campo repetido");}
                else
                    $$.talla += TALLA_TIPO_SIMPLE;
            }
                    ;

declaracionFuncion  : tipoSimple ID_ {niv=1; cargaContexto(niv);} APAR_ parametrosFormales CPAR_ {$<cent>$=dvar; dvar = 0;}   
                    bloque 
                    { 
                    if(!insTdS($2, FUNCION, $1, 0, -1, $5)){
                        yyerror("Declaracion repetida");
                    }
                    if(strcmp($2, "main\0")==0) $$=-1; else $$=0;
                    if(verTdS) mostrarTdS(); descargaContexto(niv);
                    niv=0; dvar=$<cent>2;
                    }
                    ;

parametrosFormales  : {$$ = insTdD(-1, T_VACIO);}
                    | listaParametrosFormales {$$ = $1.ref;}
                    ;

listaParametrosFormales: tipoSimple ID_
        {   
            $$.ref = insTdD(-1, $1);
            $$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
            if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1))
                yyerror("Parametro con identificador ya existente");
        }
                    | tipoSimple ID_ COMA_ listaParametrosFormales
        {   
            $$.ref = insTdD($4.ref, $1);
            $$.talla = TALLA_TIPO_SIMPLE + $4.talla ;
            if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1))
                yyerror("Parametro con identificador ya existente");
        }
                    ;
			
bloque              : ACOR_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PUNTOYCOMA_ CCOR_
        {
          INF inf = obtTdD(-1);
          if (inf.tipo != T_ERROR && inf.tipo != $5){ yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo tipo.");}
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
                        if ($3 != T_ERROR){
                         if(sim.t == T_ERROR) {yyerror("Objeto no declarado");}
                         else if (!(sim.t == $3 && ($3 == T_ENTERO || $3 == T_LOGICO)))
                            yyerror("Error de tipos en la instrucción de asignación");
                        }
                        }
                    | ID_ ABRA_ expresion CBRA_ IGUAL_ expresion PUNTOYCOMA_
                        {SIMB sim = obtTdS($1);
                        if ($3 != T_ERROR && $6 != T_ERROR)
                        {
                            if(sim.t == T_ERROR) {yyerror("Objeto no declarado");}
                            else if(sim.t != T_ARRAY){yyerror("El identificador debe ser de tipo array");}
                            else if($3 != T_ENTERO){yyerror("Indice debe ser de tipo entero");}
                            else{
                                    DIM dim = obtTdA(sim.ref);
                                    if(!(dim.telem == $6)){ yyerror("Error de tipos en la instrucción de asignación");} 
                            }
                        }
                        }
                    | ID_ PUNTO_ ID_ IGUAL_ expresion PUNTOYCOMA_
                        {SIMB sim = obtTdS($1);
                        if ($5 != T_ERROR){
                            if (sim.t != T_RECORD) {yyerror("El identificador debe ser struct");}   
                            else if (sim.t != T_ERROR){
                                CAMP reg = obtTdR(sim.ref, $3);
                                if (reg.t == T_ERROR) {yyerror("Campo no declarado");}   
                                else if (!(reg.t == $5 && ($5 == T_ENTERO || $5 == T_LOGICO)))
                                    yyerror("Error de tipos en la instrucción de asignación");
                            }
                        }      
                        }
                    ;

instruccionEntradaSalida: READ_ APAR_ ID_ CPAR_ PUNTOYCOMA_
                        {
                            SIMB sim = obtTdS($3);
                            if(sim.t != T_ENTERO) {$$ = T_ERROR; yyerror("WRITE: El identificador no es un entero");}
                        }
                    | PRINT_ APAR_ expresion CPAR_ PUNTOYCOMA_
                        {if($3 != T_ENTERO) {$$ = T_ERROR; yyerror("PRINT: La expresion no es un entero");}}
                    ;

instruccionSeleccion: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
                        {if($3 != T_ERROR && $3 != T_LOGICO) {$$ = T_ERROR; yyerror("IF: Se esperaba una expresion logica");}}
                    ;
                
instruccionIteracion: WHILE_ APAR_ expresion CPAR_ instruccion 
{if($3 != T_ERROR && $3 != T_LOGICO) {$$ = T_ERROR; yyerror("WHILE: Se esperaba una expresion logica");}}
                    ;

expresion           : expresionIgualdad {$$ = $1;}
                    | expresion operadorLogico expresionIgualdad
            {
                $$ = T_ERROR;
                if ($1 != T_ERROR && $3 != T_ERROR) {
                    if ($1 == $3 && $1 != T_LOGICO) {
                        $$ = T_LOGICO;
                    } else {
                        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o son booleanos.");;
                    }
                }
            }
                    ;
                    
expresionIgualdad   : expresionRelacional {$$ = $1;}
                    | expresionIgualdad operadorIgualdad expresionRelacional
                        {if ($1 == $3 && ($3 == T_ENTERO || $3 == T_LOGICO)) {$$ = T_LOGICO;}
                         else{$$ = T_ERROR; yyerror("Los tipos (igualdad) no coinciden");}}
                    ;
                    
expresionRelacional : expresionAditiva { $$ = $1;}
                    | expresionRelacional operadorRelacional expresionAditiva
		    {
		        $$ = T_ERROR;
		        if ($1 != T_ERROR && $3 != T_ERROR) {
		    	    if (!($1 == $3 && $1 == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo tipo.");
		    	    } else {
		    	        $$ = T_LOGICO;
		    	    }
		        }
		    }
                    ;
                    
expresionAditiva    : expresionMultiplicativa { $$ = $1; }
                    | expresionAditiva operadorAditivo expresionMultiplicativa
		    {
		        $$ = T_ERROR;
		        if ($1 != T_ERROR && $3 != T_ERROR) {
		    	    if (!($1 == $3 && $1 == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo tipo");
		    	    } else {
		    	        $$ = T_ENTERO;
		    	    }
		        }
		    }
                    ;
                    
expresionMultiplicativa: expresionUnaria { $$ = $1; }
                    | expresionMultiplicativa operadorMultiplicativo expresionUnaria
		    {
		        $$ = T_ERROR;
		        if ($1 != T_ERROR && $3 != T_ERROR) {
		    	    if (!($1 == $3 && $1 == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo.");
		    	    } else {
		    	        $$ = T_ENTERO;
		    	    }
		        }
		    }
                    ;
                    
expresionUnaria     : expresionSufija { $$ = $1; }
                    | operadorUnario expresionUnaria
		    {
		    	$$ = T_ERROR;
		    	if ($2 != T_ERROR) {
		    	    if ($2 == T_ENTERO) {
		    		if ($1 == OP_NOT) {
		    		    yyerror("Error con la incompatibilidad de tipos, no se puede negar un entero.");
		    		} else {
		    		    $$ = T_ENTERO;
		    		}
		    	    } else if ($2 == T_LOGICO) {
		    		if ($1 == OP_SUMAUN || $1 == OP_RESTAUN) {
		    		    yyerror("Error con la incompatibilidad de tipos, solo se puede aplicar el operador unario '+' o '-' a una expresión entera.");
		    		} else {
		    		    $$ = T_LOGICO;
		    		}
		    	    } else {
		    		yyerror("Error con la incompatibilidad de tipos, no son tipos equivalentes o no son el mismo.");
		    	    }
		    	}
		    }
                    ;

expresionSufija     : constante { $$ = $1;}
                    | APAR_ expresion CPAR_ { $$ = $2; }
                    | ID_
            {   
                $$ = T_ERROR;
                SIMB sim = obtTdS($1);
                if (sim.t != T_ERROR) {$$ = sim.t;}   
                else {yyerror("Variable mal declarada"); }
            }
                    | ID_ PUNTO_ ID_
            {   
                $$ = T_ERROR;
                SIMB sim = obtTdS($1);
                if (sim.t != T_RECORD) {yyerror("El identificador debe ser struct");}   
                else if(sim.t != T_ERROR){
                    CAMP reg = obtTdR(sim.ref, $3);
                    if (reg.t != T_ERROR) {$$ = reg.t;}   
                    else {yyerror("Campo no declarado"); }
                }
            }
                    | ID_ ABRA_ expresion CBRA_
            {       
                $$ = T_ERROR;
                SIMB sim = obtTdS($1);         
                if (sim.t == T_ERROR) {yyerror("Variable no declarada");}
                else if (sim.t != T_ARRAY) {yyerror("Se esperaba un tipo Array");}
                else{
                    if ($3 != T_ENTERO) {yyerror("VECTOR: La variable con la que se accede ha de ser un entero");}                             
                    else {
                        DIM dim = obtTdA(sim.ref);
				        $$ = dim.telem;
                    }
                }                              
            }
                    | ID_ APAR_ parametrosActuales CPAR_
            {
                
                $$ = T_ERROR;
                SIMB sim = obtTdS($1);
                INF inf = obtTdD(sim.ref);
                if (sim.t == T_ERROR) {yyerror("No existe ninguna variable con ese identificador.");}
                if (inf.tipo == T_ERROR) {yyerror("No existe ninguna funcion con ese identificador.");} 
                else {$$ = inf.tipo;}
		    }
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