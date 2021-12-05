/*****************************************************************************/
/**  Ejemplo de FLEX: S E M - 1             2021-2022 <jbenedi@dsic.upv.es> **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.h"

#define  retornar(x) {if (verbosidad) ECHO; return(x);}

%}
/*-----------------------------------------------------------------------------
  Estas opciones evitan "warnings" innecesarios y permiten obtener el
  numero de linea actual en la variable global "yylineno".
-----------------------------------------------------------------------------*/
%option noinput       
%option nounput       
%option yylineno  

delimitador [ \t]+
saltoLinia  [\n]
letra   [a-zA-Z]
digito  [0-9]
entero  {digito}+
id    {letra}({digito}|{letra}|_)*
comentario  \/\/.*\n

/*-----------------------------------------------------------------------------
  Simbolos del lenguaje:
    (id,cte,struct,int,bool,return,print,read,if,while,else)
-----------------------------------------------------------------------------*/
%%
{delimitador}  { if (verbosidad) ECHO;  }
{saltoLinia}   { if (verbosidad) { ECHO; fprintf(stdout, "%3d.- ", yylineno); } }
{comentario}   { if (verbosidad) ECHO;  }

"while"        { retornar (WHILE_);     }
"if"           { retornar (IF_);        }
"else"         { retornar (ELSE_);      }
"read"         { retornar (READ_);      }
"print"        { retornar (PRINT_);     }
"struct"       { retornar (STRUCT_);    }
"return"       { retornar (RETURN_);    }
"bool"         { retornar (BOOL_);      } 
"int"          { retornar (INT_);       }
"true"         { retornar (TRUE_);      }
"false"        { retornar (FALSE_);     }

"&&"           { retornar (OPAND_);     }
"||"           { retornar (OPOR_);      }
"=="           { retornar (OPE_);       }
"!="           { retornar (OPNE_);      }
">"            { retornar (OPMAYOR_);   }
"<"            { retornar (OPMENOR_);   }
">="           { retornar (OPMI_);      }
"<="           { retornar (OPMENI_);    }
"!"            { retornar (OPDISTINTO_);}
"+"	           { retornar (MAS_);       }
"-"            { retornar (MENOS_);     }
"*"            { retornar (POR_);       }
"/"            { retornar (DIV_);       }
"("            { retornar (APAR_);      }
")"            { retornar (CPAR_);      }
"["            { retornar (ABRA_);      }
"]"            { retornar (CBRA_);      }
"{"            { retornar (ACOR_);      }
"}"            { retornar (CCOR_);      }
";"            { retornar (PUNTOYCOMA_);}
"="            { retornar (IGUAL_);     }
","            { retornar (COMA_);      }
"."            { retornar (PUNTO_);     }

{digito}+"."{digito}*   {retornar (CTE_);}
{digito}*"."{digito}+   {retornar (CTE_);}
{entero}       { retornar (CTE_);       }
{id}           { retornar (ID_); }

.	       { yyerror("Caracter desconocido"); }

%%
