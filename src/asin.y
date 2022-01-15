/*****************************************************************************/
/**  Ejemplo de BISON-I: S E M - 2         20201-2022 <jbenedi@dsic.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"
#include "libgci.h"
%}

%union{
    int     cent;
    char*   ident;
    ListaP  lis;
    REFE refe;
    EXPR expr;
}

//Todos los simbolos terminales que no necesiten atributos

%token WHILE_ IF_ ELSE_ READ_ PRINT_ RETURN_ BOOL_ INT_ MENOS_ DIV_ FALSE_ APAR_ ABRA_ ACOR_ PUNTOYCOMA_ STRUCT_ PUNTO_

%token OPAND_ OPOR_ OPE_ OPNE_ OPMAYOR_ OPMENOR_ OPMI_ OPMENI_ MAS_ POR_ TRUE_ CPAR_ CBRA_ CCOR_ IGUAL_ COMA_ OPDISTINTO_


//Simbolos terminales con atributos

%token<cent> CTE_
%token<ident> ID_



//No terminales con atributos 

%type<cent> declaracionVariable  declaracion declaracionFuncion listaDeclaraciones tipoSimple instruccionAsignacion
%type<cent> operadorLogico operadorIgualdad operadorRelacional operadorAditivo operadorMultiplicativo operadorUnario
%type<cent> parametrosFormales parametrosActuales listaParametrosActuales 
%type<lis> listaParametrosFormales listaCampos
%type <expr> constante expresion expresionIgualdad expresionRelacional expresionAditiva expresionMultiplicativa expresionUnaria expresionSufija 






%%

programa 	    : {dvar=0; niv = 0; si=0; cargaContexto(niv);
                $<refe>$.ref1 = creaLans(si);
                emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-1));
                $<refe>$.ref2 = creaLans(si);
                lMain=$<refe>$.ref2;
                emite(GOTOS, crArgNul(), crArgNul(), crArgEnt(-1));}

            listaDeclaraciones


                {if($2 == 0){yyerror("el programa no tiene main");} 
                completaLans($<refe>1.ref1, crArgEnt(dvar));
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
                         int refe = insTdA($1, numelem);
                         if( !insTdS($2, VARIABLE, T_ARRAY, niv, dvar, refe))
                            yyerror("Identificador variable repetido");
                         else dvar += numelem * TALLA_TIPO_SIMPLE;
                         }
                    | STRUCT_ ACOR_ listaCampos CCOR_ ID_ PUNTOYCOMA_
		    	        {
                         int refe = insTdR($3.ref, $5, T_RECORD, $3.talla);
                         int tal = $3.talla;
                         if(refe==-1){yyerror("Nombre de struct invalido");}
                         else if( !insTdS($5, VARIABLE, T_RECORD, niv, dvar, refe)){yyerror("Identificador repetido");}
                         else
                            dvar +=  tal * TALLA_TIPO_SIMPLE;
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

declaracionFuncion  : tipoSimple ID_ {$<cent>$=dvar; dvar = 0; niv=1; cargaContexto(niv);} APAR_ parametrosFormales CPAR_ 
                    {$<cent>$=0;
                    if(!insTdS($2, FUNCION, $1, 0, -1, $5)){yyerror("Identificador de funcion repetido");
                        if(strcmp($2, "main\0")==0){$<cent>$=1;}}
                    if(strcmp("main",$2)==0 && $<cent>$==0){
                        completaLans(lMain,crArgEtq(si));
                    }
                    }
                    bloque 
                    { 
                    if($<cent>7 ==1) {yyerror("El programa tiene mas de un main");}
                    if (strcmp($2,"main") == 0) {$$ = 1;}
                    else {$$ = 0;}

                    if(verTdS) mostrarTdS(); 
                    descargaContexto(niv);
                    niv -= 1;
                    dvar= $<cent>3;
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
			
bloque          : 
        {
            emite(PUSHFP, crArgNul(), crArgNul(), crArgNul());
            emite(FPTOP, crArgNul(), crArgNul(), crArgNul());
            $<cent>$ = creaLans(si);
            emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-1));
        } 
        ACOR_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PUNTOYCOMA_
        {
          INF inf = obtTdD(-1);
          if (inf.tipo != T_ERROR){ 
               if (inf.tipo != $6.tipo){yyerror("Error con la incompatibilidad de tipos en Return");}
          }
          else{yyerror("Error en la declaracion de la funcion");}

          int dir_ret = TALLA_SEGENLACES + inf.tsp + TALLA_TIPO_SIMPLE;
          completaLans($<cent>1, crArgEnt(dvar)); 
          emite(EASIG, crArgPos(niv, $6.d), crArgNul(), crArgPos(niv, -dir_ret));
          emite(TOPFP, crArgNul(), crArgNul(), crArgNul());
          emite(FPPOP, crArgNul(), crArgNul(), crArgNul());
          if (strcmp(inf.nom,"main") == 0) { emite(FIN, crArgNul(), crArgNul(), crArgNul()); }
          else { emite(RET, crArgNul(), crArgNul(), crArgNul());}
        }
         CCOR_
                    ;

declaracionVariableLocal: 
                    | declaracionVariableLocal declaracionVariable
                    ;
/********************************************************************************************************/  










/********************************************************************************************************/  
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
                        {
                        SIMB sim = obtTdS($1);
                        if ($3.tipo != T_ERROR){
                         if(sim.t == T_ERROR) {yyerror("Objeto no declarado");}
                         else if (!(sim.t == $3.tipo && ($3.tipo == T_ENTERO || $3.tipo == T_LOGICO)))
                            yyerror("El identificador debe ser de tipo simple");
                        }
                        emite(EASIG, crArgPos(niv, $3.d), crArgNul(), crArgPos(sim.n, sim.d));
                        }
                    | ID_ ABRA_ expresion CBRA_ IGUAL_ expresion PUNTOYCOMA_
                        {
                        SIMB sim = obtTdS($1);
                        if ($3.tipo != T_ERROR && $6.tipo != T_ERROR)
                        {
                            if(sim.t == T_ERROR) {yyerror("Objeto no declarado");}
                            else if(sim.t != T_ARRAY){yyerror("El identificador debe ser de tipo array");}
                            else if($3.tipo != T_ENTERO){yyerror("Indice debe ser de tipo entero");}
                            else{
                                    DIM dim = obtTdA(sim.ref);
                                    if(!(dim.telem == $6.tipo)){ yyerror("Error de tipos en la asignacion");} 
                            }
                        }
                        emite(EVA, crArgPos(sim.n, sim.d), crArgPos(niv, $3.d), crArgPos(niv, $6.d)); 
                        }
                    | ID_ PUNTO_ ID_ IGUAL_ expresion PUNTOYCOMA_
                        {
                        SIMB sim = obtTdS($1);
                        if ($5.tipo != T_ERROR){
                            if (sim.t != T_RECORD) {yyerror("El identificador debe ser struct");}   
                            else if (sim.t != T_ERROR){
                                CAMP reg = obtTdR(sim.ref, $3);
                                if (reg.t == T_ERROR) {yyerror("Campo no declarado");}   
                                else if (!(reg.t == $5.tipo && ($5.tipo == T_ENTERO || $5.tipo == T_LOGICO)))
                                    yyerror("Error de tipos en la asignacion");
                                else{
                                    emite(EVA, crArgPos(sim.n, sim.d), crArgPos(niv, reg.d), crArgPos(niv, $5.d));
                                }   // REVISAR
                            }
                        } 
                        }
                    ;

instruccionEntradaSalida: READ_ APAR_ ID_ CPAR_ PUNTOYCOMA_
                        {
                            SIMB sim = obtTdS($3);
                            if(sim.t != T_ENTERO) {yyerror("El argumento del read debe ser entero");}
                            emite(EREAD, crArgNul(), crArgNul(), crArgPos(sim.n, sim.d));
                        }
                    | PRINT_ APAR_ expresion CPAR_ PUNTOYCOMA_
                        {if($3.tipo != T_ENTERO) {yyerror("La expresion del print debe ser entera");}
                        emite(EWRITE, crArgNul(), crArgNul(), crArgPos(niv, $3.d));
                        }
                    ;

instruccionSeleccion: IF_ APAR_ expresion CPAR_ 
                    {
                    if($3.tipo != T_ERROR && $3.tipo != T_LOGICO) {yyerror("La expresion del if debe ser logica");}
                    $<cent>$ = creaLans(si);
                    emite(EIGUAL, crArgPos(niv, $3.d), crArgEnt(0), crArgEtq(-1));
                    }
                    instruccion
                    {
                    $<cent>$ = creaLans(si);
                    emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
                    completaLans($<cent>5, crArgEtq(si));
                    }
                    ELSE_ instruccion {completaLans($<cent>7, crArgEtq(si));}            
                    ;
                
instruccionIteracion: WHILE_ APAR_ expresion CPAR_ 
                    {
                    if($3.tipo != T_ERROR && $3.tipo != T_LOGICO) {yyerror("La expresion del while debe ser logica");}
                    
                    $<refe>$.ref1 = creaLans(si);   // REVISAR
                    emite(EIGUAL, crArgPos(niv, $3.d), crArgEnt(1), crArgEtq(-1) );
                    $<refe>$.ref2 = creaLans(si);                              
                    emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
                    $<refe>$.ref3 = si;  
                    }
                    instruccion 
                    {
                    emite(GOTOS, crArgNul(), crArgNul(), crArgEtq($<refe>5.ref3));
                    completaLans($<refe>5.ref2, crArgEnt(si));
                    }
                    ;
/********************************************************************************************************/  









/********************************************************************************************************/  
expresion           : expresionIgualdad {$$.tipo = $1.tipo; $$.d = $1.d;}
                    | expresion operadorLogico expresionIgualdad
            {
                $$.tipo = T_ERROR;
                if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
                    if ($1.tipo == $3.tipo && $1.tipo != T_LOGICO) {
                        $$.tipo = T_LOGICO;
                    } else {
                        yyerror("Error con la incompatibilidad de tipos (logica).");
                    }
                }
                
                $$.d = creaVarTemp();
                if ($2 == EMULT) {
                    emite(EMULT, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                } 
                else {
                    emite(ESUM, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                    emite(EMENEQ, crArgPos(niv, $$.d), crArgEnt(1), crArgEtq(si+2));
                    emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
                }
            }
                    ;
                    
expresionIgualdad   : expresionRelacional {$$.tipo = $1.tipo; $$.d = $1.d;}
                    | expresionIgualdad operadorIgualdad expresionRelacional
                        {
                        if ($1.tipo == $3.tipo && ($3.tipo == T_ENTERO || $3.tipo == T_LOGICO)) {$$.tipo = T_LOGICO;}
                        else{$$.tipo = T_ERROR; yyerror("Error con la incompatibilidad de tipos (igualdad).");}
                         
                        $$.d = creaVarTemp();
                        emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
                        emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgEtq(si + 2));
                        emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.d));
                        }
                    ;
                    
expresionRelacional : expresionAditiva {$$.tipo = $1.tipo; $$.d = $1.d;}
                    | expresionRelacional operadorRelacional expresionAditiva
		    {
		        $$.tipo = T_ERROR;
		        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
		    	    if (!($1.tipo == $3.tipo && $1.tipo == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos (relacional).");
		    	    } else {
		    	        $$.tipo = T_LOGICO;
		    	    }
		        }
                $$.d = creaVarTemp();
                emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
                emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgEtq(si+2));
                emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.d));
		    }
                    ;
                    
expresionAditiva    : expresionMultiplicativa {$$.tipo = $1.tipo; $$.d = $1.d;}
                    | expresionAditiva operadorAditivo expresionMultiplicativa
		    {
		        $$.tipo = T_ERROR;
		        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
		    	    if (!($1.tipo == $3.tipo && $1.tipo == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos (Aditiva).");
		    	    } else {
		    	        $$.tipo = T_ENTERO;
                        $$.d = creaVarTemp();
                        emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
		    	    }
		        }
		    }
                    ;
                    
expresionMultiplicativa: expresionUnaria {$$.tipo = $1.tipo; $$.d = $1.d; }
                    | expresionMultiplicativa operadorMultiplicativo expresionUnaria
		    {
		        $$.tipo = T_ERROR;
		        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
		    	    if (!($1.tipo == $3.tipo && $1.tipo == T_ENTERO)) {
		    	        yyerror("Error con la incompatibilidad de tipos (Multiplicativa).");
		    	    } else {
		    	        $$.tipo = T_ENTERO;
                        $$.d = creaVarTemp();
                        emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
		    	    }
		        }
		    }
                    ;
                    
expresionUnaria     : expresionSufija {$$.tipo = $1.tipo; $$.d = $1.d; }
                    | operadorUnario expresionUnaria
		    {
		    	$$.tipo = T_ERROR;
		    	if ($2.tipo != T_ERROR) {
		    	    if ($2.tipo == T_ENTERO) {
		    		if ($1 == ESIG) {
		    		    yyerror("Error con la incompatibilidad de tipos, no se puede negar un entero.");
		    		} else {
		    		    $$.tipo = T_ENTERO;
		    		}
		    	    } else if ($2.tipo == T_LOGICO) {
		    		if ($1 == ESUM || $1 == EDIF) {
		    		    yyerror("Error con la incompatibilidad de tipos, solo se puede aplicar el operador unario '+' o '-' a una expresión entera.");
		    		} else {
		    		    $$.tipo = T_LOGICO;
		    		}
		    	    } else {
		    		yyerror("Error con la incompatibilidad de tipos (Unaria).");
		    	    }
		    	}

                $$.d = creaVarTemp();
                if ($1 == ESIG) {emite(EDIF, crArgEnt(1), crArgPos(niv, $2.d), crArgPos(niv, $$.d));}
                else {emite($1, crArgEnt(0), crArgPos(niv, $2.d), crArgPos(niv, $$.d));}
		    }
                    ;

expresionSufija     : constante {$$.tipo = $1.tipo; $$.d = creaVarTemp(); emite(EASIG, crArgEnt($1.d), crArgNul(), crArgPos(niv, $$.d)); }
                    | APAR_ expresion CPAR_ {$$.tipo = $2.tipo; $$.d = $2.d; }
                    | ID_
            {   
                $$.tipo = T_ERROR;
                SIMB sim = obtTdS($1);
                if (sim.t != T_ERROR) {$$.tipo = sim.t;}   
                else {yyerror("Variable mal declarada"); }

                $$.d = creaVarTemp();
                emite(EASIG, crArgPos(sim.n, sim.d), crArgNul(), crArgPos(niv, $$.d));
            }
                    | ID_ PUNTO_ ID_
            {   
                $$.tipo = T_ERROR;
                SIMB sim = obtTdS($1);
                if (sim.t != T_RECORD) {yyerror("El identificador debe ser struct");}   
                else if(sim.t != T_ERROR){
                    CAMP reg = obtTdR(sim.ref, $3);
                    if (reg.t == T_ERROR) {yyerror("Campo no declarado");}   
                    else{ 
                        $$.tipo  = reg.t;
                        $$.d = creaVarTemp();
                        emite(EAV, crArgPos(sim.n, sim.d), crArgPos(niv, reg.d), crArgPos(niv, $$.d));  // REVISAR
                    }
                }                
            }
                    | ID_ ABRA_ expresion CBRA_
            {       
                $$.tipo = T_ERROR;
                SIMB sim = obtTdS($1);         
                if (sim.t == T_ERROR) {yyerror("Variable no declarada");}
                else if (sim.t != T_ARRAY) {yyerror("Se esperaba un tipo Array");}
                else{
                    if ($3.tipo != T_ENTERO) {yyerror("VECTOR: La variable con la que se accede ha de ser un entero");}                             
                    else {
                        DIM dim = obtTdA(sim.ref);
				        $$.tipo = dim.telem;
                    }
                } 
                $$.d = creaVarTemp();
                emite(EAV, crArgPos(sim.n, sim.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));                             
            }
                    | ID_ {emite(EPUSH, crArgNul(), crArgNul(), crArgEnt(TALLA_TIPO_SIMPLE));} APAR_ parametrosActuales CPAR_
            {
                
                $$.tipo  = T_ERROR;
                SIMB sim = obtTdS($1);
                INF inf = obtTdD(sim.ref);
                if (sim.t == T_ERROR) {yyerror("No existe ninguna variable con ese identificador.");}
                if (inf.tipo == T_ERROR) {yyerror("No existe ninguna funcion con ese identificador.");} 
                else if (!cmpDom(sim.ref, $4)) {yyerror("Error en el dominio de los parametros actuales");}
                else {$$.tipo = inf.tipo;}

                emite(CALL, crArgNul(), crArgNul(), crArgEtq(sim.d)); 
                emite(DECTOP, crArgNul(), crArgNul(), crArgEnt(inf.tsp)); 
                $$.d = creaVarTemp();
                emite(EPOP, crArgNul(), crArgNul(), crArgPos(niv, $$.d));
		    }
            ;
/********************************************************************************************************/  








/********************************************************************************************************/     
parametrosActuales  : {$$ = insTdD(-1, T_VACIO);}
                    | listaParametrosActuales {$$ = $1;}
                    ;
                    
listaParametrosActuales: expresion 
                            {$$ = insTdD(-1,$1.tipo);
                             emite(EPUSH,crArgNul(),crArgNul(),crArgPos(niv, $1.d));
                            }
                    | expresion {emite(EPUSH,crArgNul(),crArgNul(),crArgPos(niv, $1.d));}
                      COMA_ listaParametrosActuales {$$ = insTdD($4,$1.tipo);}
                    ;

constante           : CTE_   {$$.tipo = T_ENTERO; $$.d = $1;}
                    | TRUE_  {$$.tipo = T_LOGICO; $$.d = 1;}
                    | FALSE_ {$$.tipo = T_LOGICO; $$.d = 0;}
                    ;
                    
                    
operadorLogico      : OPAND_ { $$ = EMULT; }
                    | OPOR_ { $$ = ESUM; }
                    ;
                    
operadorIgualdad    : OPE_ { $$ = EIGUAL; }
                    | OPNE_ { $$ = EDIST; }
                    ;

operadorRelacional  : OPMAYOR_ { $$ = EMAY; }
                    | OPMENOR_ { $$ = EMEN; }
                    | OPMI_ { $$ = EMAYEQ; }
                    | OPMENI_ { $$ = EMENEQ; }
                    ;
                    
operadorAditivo     : MAS_ { $$ = ESUM; }
                    | MENOS_ { $$ = EDIF; }
                    ;

operadorMultiplicativo: POR_ { $$ = EMULT; }
                    | DIV_ { $$ = EDIVI; }
                    ;
                    
operadorUnario      : MAS_ { $$ = ESUM; }
                    | MENOS_ { $$ = EDIF; }
                    | OPDISTINTO_ { $$ = ESIG; }
                    ;
/********************************************************************************************************/  
%%
