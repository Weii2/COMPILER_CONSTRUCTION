/* Definition section */
%{
    #include "common.h"
    #include "y.tab.h"	/* header file generated by bison */
    extern YYSTYPE yylval;

    #define YY_NO_UNPUT
    #define YY_NO_INPUT
%}


/* Define regular expression label */
letter  [a-zA-Z_]
digit [0-9]
id {letter}+({letter}|{digit})*
inumber {digit}+
fnumber ({digit}*\.{digit}+)
newline		[\n]

%x CMT
%x STR
%option yylineno

/* Rules section */
%%

"/*"        { BEGIN(CMT); }
<CMT>"*/"   { BEGIN(INITIAL); }
<CMT>\n     {;}
<CMT>.      {;}
"//".*      {;}
"\""        { BEGIN(STR);
                //return '"';
            }
<STR>"\""   { BEGIN(INITIAL);
                //return '"';
            }
<STR>[^\"]* { yylval.s_val = strdup(yytext);
                return STRING_LIT;
            }

"print"     { return PRINT; }
"return" 	{ return RETURN; }
"if" 		{ return IF; }
"else" 		{ return ELSE; }
"for" 		{ return FOR; }
"while" 	{ return WHILE; }
"int" 		{ yylval.ctr.id_type = 'I';
                return INT; 
            }
"float" 	{ yylval.ctr.id_type = 'F';
                return FLOAT;  
            }
"string" 	{ yylval.ctr.id_type = 'S';
                return STRING; 
            }
"bool" 		{ yylval.ctr.id_type = 'B';
                return BOOL; 
            }
"true"      { yylval.b_val = true;
                return BOOL_LIT; 
            }
"false"     { yylval.b_val = false;
                return BOOL_LIT; 
            }
"continue"	{ return CONTINUE; }
"break" 	{ return BREAK; }
"void" 		{ return VOID; }

{inumber}   { yylval.i_val = atoi(yytext);
                return INT_LIT;
            }
{fnumber}   { yylval.f_val = atof(yytext);
                return FLOAT_LIT;
            }

{id}        { yylval.ctr.id_name = strdup(yytext);
                return IDENT;
            }


"+"         { return '+'; }
"-"         { return '-'; }
"*"         { return '*'; }
"/"         { return '/'; }
"%"         { return '%'; }
"++"		{ return INC; }
"--"		{ return DEC; }
">"         { return GTR; }
"<"			{ return LSS; }
">="		{ return GEQ; }
"<="		{ return LEQ; }
"=="		{ return EQL; }
"!="		{ return NEQ; }
"="			{ return ASSIGN; }
"+="		{ return ADD_ASSIGN; }
"-="		{ return SUB_ASSIGN; }
"*="		{ return MUL_ASSIGN; }
"/="		{ return QUO_ASSIGN; }
"%="		{ return REM_ASSIGN; }
"&&"        { return AND; }
"||"        { return OR;  }
"!"         { return '!'; }
"("		    { return '('; }
")"	    	{ return ')'; }
"[" 		{ return '['; }
"]" 		{ return ']'; }
"{" 		{ return '{'; }
"}" 		{ return '}'; }
[;]         { return SEMICOLON; }
[\n]        {;}
<<EOF>>     { static int once = 0;
                if (once++) {
                    yyterminate();
                }
            }
[ \t]+      {;}
[ \t\v\n\f]+ {;}
.           {;}
%%
/*  C Code section */
int yywrap(void)
{
    return 1;
}