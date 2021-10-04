/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    typedef struct symbol_table *table_ptr;
    struct symbol_table{
        int index[100];
        char *name[100];
        char type[100];
        int address[100];
        int lineno[100];
        char element_type[100];
        int scope;
        int index_num;
        table_ptr next;
        table_ptr prev;
    };
    table_ptr table_head, table_end;
    int address_glo=0;
    char *id_name_glo;
    char conver_type;
    char ass_err0, ass_err1;
    int cmp_err = 0;
    int undefine_err = 0;
    int asslit = 0;
    int check_asslit = 0;

    typedef struct lookup_fun_return l_r;
    struct lookup_fun_return{
        int add;
        int line;
        char type;
        int sco;
    };
    /* Symbol table function - you can add new function if needed. */
    static void create_symbol(int scope);
    static void insert_symbol(char *name, char type, int address, int lineno, char element_type);
    static l_r lookup_symbol(char *id_name);
    static void dump_symbol(int scope);
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    bool b_val;
    struct {
        char *id_name;
        char id_type;
    }ctr;
    /* ... */
}


/* Token without return */
%token PRINT RETURN IF ELSE FOR WHILE INT FLOAT BOOL STRING CONTINUE BREAK VOID
%token INC DEC GTR LSS GEQ LEQ EQL NEQ ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN AND OR SEMICOLON
%token IDENT

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <b_val> BOOL_LIT
%token <s_val> STRING_LIT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type TypeName Expression UnaryExpr unary_op cmp_op add_op mul_op IDENT assign_op PrimaryExpr Operand IndexExpr A B C D Literal ConversionExpr
/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : { 
        create_symbol(0);
    }
        StatementList
        {
            dump_symbol(0);
        }
;

Type
    : TypeName  { $$ = $1; }
;
TypeName
    : INT       { $$ = 'I'; }
    | FLOAT     { $$ = 'F'; }
    | STRING    { $$ = 'S'; }
    | BOOL      { $$ = 'B'; }
;

Expression
    : //UnaryExpr
    | Expression OR A {
        printf("OR\n");
        yylval.ctr.id_type = 'B';
    }
    | A { $$ = $1; }
;
UnaryExpr
    : PrimaryExpr { $$ = $1; }
    | unary_op UnaryExpr {
        printf("%s\n", $1);
    }
;
A
    : A AND B {        
        printf("AND\n");
    }
    | B { $$ = $1; }
;
B
    : B cmp_op C{
        cmp_err = 1;
        printf("%s\n", $2);
    }
    | C { $$ = $1; }
;
C
    : C add_op D {
        printf("%s\n", $2);
    }
    | D { $$ = $1; }
;
D
    : D mul_op D {
        printf("%s\n", $2);
    }
    | UnaryExpr { $$ = $1; }
;
cmp_op
    : EQL {
        $$ = "EQL";
    }
    | NEQ {
        $$ = "NEQ";
    }
    | LSS {
        $$ = "LSS";
    }
    | LEQ {
        $$ = "LEQ";
    }
    | GTR {
        $$ = "GTR";
    }
    | GEQ {
        $$ = "GEQ";
    }
;
add_op
    : '+' {
        $$ = "ADD";
    }
    | '-' {
        $$ = "SUB";
    }
;
mul_op
    : '*' {
        $$ = "MUL";
    }
    | '/' {
        $$ = "QUO";
    }
    | '%' {
        $$ = "REM";
    }
;
unary_op
    : '+' {
        $$ = "POS";
    }
    | '-' {
        $$ = "NEG";
    }
    | '!' {
        $$ = "NOT";
    }
;

PrimaryExpr 
    : Operand   { $$ = $1; }
    | IndexExpr
    | ConversionExpr { $$ = $1; }
;
Operand
    : Literal {
        asslit = 1;
        check_asslit = 0;
        $$ = $1;
    }
    | IDENT {
        if(!check_asslit) {
            asslit = 0;
            check_asslit = 0;
        }
        l_r fun_return = lookup_symbol(yylval.ctr.id_name);
        if(fun_return.add == -10) {
            undefine_err = 1;
            printf("error:%d: undefined: %s\n", yylineno, yylval.ctr.id_name);
        }
        else {
            yylval.ctr.id_type = fun_return.type;
            conver_type = fun_return.type;
            $$ = fun_return.type;
            printf("IDENT (name=%s, address=%d)\n", yylval.ctr.id_name, fun_return.add);
        }
        
    }
    | '(' Expression ')' { $$ = $2; }
;
Literal
    : INT_LIT {
        conver_type = 'I';
        yylval.ctr.id_type = 'I';
        $$ = 'I';
        printf("INT_LIT %d\n", $1);
    }
    | FLOAT_LIT {
        conver_type = 'F';
        yylval.ctr.id_type = 'F';
        $$ = 'F';
        printf("FLOAT_LIT %f\n", $1);
    }
    | BOOL_LIT {
        conver_type = 'B';
        yylval.ctr.id_type = 'B';
        $$ = 'B';
        if($1)
            printf("TRUE\n");
        else
            printf("FALSE\n");
    }
    | STRING_LIT {
        conver_type = 'S';
        yylval.ctr.id_type = 'S';
        $$ = 'S';
        printf("STRING_LIT %s\n", $1);
    }
;

IndexExpr
    : PrimaryExpr '[' Expression ']' {
        asslit = 0;
        yylval.ctr.id_type = $1;
    }
;

ConversionExpr
    : '(' Type ')' Expression {
        $$ = $2;
        yylval.ctr.id_type = $2;
        printf("%c to %c\n", conver_type, $2);
    }
;

Statement
    : DeclarationStmt
    | AssignmentStmt
    | IncDecStmt
    | Block
    | IfStmt
    | WhileStmt
    | ForStmt
    | PrintStmt
    | OpStmt
;
DeclarationStmt
    : Type IDENT SEMICOLON {
        l_r fun_return = lookup_symbol(yylval.ctr.id_name);
        if(fun_return.sco != table_end->scope)
            insert_symbol(yylval.ctr.id_name, yylval.ctr.id_type, address_glo++, yylineno, 'E');
        else
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, yylval.ctr.id_name, fun_return.line);
    }
    | Type IDENT {id_name_glo = strdup(yylval.ctr.id_name);} ASSIGN Expression SEMICOLON  {
        insert_symbol(id_name_glo, yylval.ctr.id_type, address_glo++, yylineno, 'E');
    }
    | Type IDENT {id_name_glo = strdup(yylval.ctr.id_name);} '[' Expression ']' SEMICOLON {
        insert_symbol(id_name_glo, 'A', address_glo++, yylineno, $1);
    }
;
AssignmentExpr
    : Expression {
        if(asslit)
            check_asslit = 1;
    } assign_op Expression {
        char *t1, *t3;
        if($1 == 'I')
            t1="int";
        else if($1 == 'F')
            t1="float";
        else if($1 == 'B')
            t1="bool";
        else if($1 == 'S')
            t1="string";
        if($4 == 'I')
            t3="int";
        else if($4 == 'F')
            t3="float";
        else if($4 == 'B')
            t3="bool";
        else if($4 == 'S')
            t3="string";
        if(check_asslit)
            printf("error:%d: cannot assign to %s\n", yylineno, t1);
        else if(($1 != $4) && !undefine_err) {
            printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno, t1, t3);
        }
        undefine_err = 0;
        asslit = 0;
        check_asslit = 0;
        printf("%s\n", $3);
    }
;
AssignmentStmt
    : AssignmentExpr SEMICOLON
;
assign_op
    : ASSIGN        { $$ = "ASSIGN"; }
    | ADD_ASSIGN    { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN    { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN    { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN    { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN    { $$ = "REM_ASSIGN"; }
;
IncDecExpr
    : Expression INC { printf("INC\n"); }
    | Expression DEC { printf("DEC\n"); }
;
IncDecStmt
    : IncDecExpr SEMICOLON
;
OpExpr
    : Operand '+' Operand { 
        if($1 != $3) {
            char *t1, *t3;
            if($1 == 'I')
                t1="int";
            else if($1 == 'F')
                t1="float";
            else if($1 == 'B')
                t1="bool";
            else if($1 == 'S')
                t1="string";
            if($3 == 'I')
                t3="int";
            else if($3 == 'F')
                t3="float";
            else if($3 == 'B')
                t3="bool";
            else if($3 == 'S')
                t3="string";
            printf("error:%d: invalid operation: ADD (mismatched types %s and %s)\n", yylineno, t1, t3);
        }
        printf("ADD\n");
    }
    | Operand '-' Operand {
        if($1 != $3) {
            char *t1, *t3;
            if($1 == 'I')
                t1="int";
            else if($1 == 'F')
                t1="float";
            else if($1 == 'B')
                t1="bool";
            else if($1 == 'S')
                t1="string";
            if($3 == 'I')
                t3="int";
            else if($3 == 'F')
                t3="float";
            else if($3 == 'B')
                t3="bool";
            else if($3 == 'S')
                t3="string";
            printf("error:%d: invalid operation: SUB (mismatched types %s and %s)\n", yylineno, t1, t3);
        }
        printf("SUB\n");
    }
    | Operand '*' Operand {
        if($1 != $3) {
            char *t1, *t3;
            if($1 == 'I')
                t1="int";
            else if($1 == 'F')
                t1="float";
            else if($1 == 'B')
                t1="bool";
            else if($1 == 'S')
                t1="string";
            if($3 == 'I')
                t3="int";
            else if($3 == 'F')
                t3="float";
            else if($3 == 'B')
                t3="bool";
            else if($3 == 'S')
                t3="string";
            printf("error:%d: invalid operation: MUL (mismatched types %s and %s)\n", yylineno, t1, t3);
        }
        printf("MUL\n");
    }
    | Operand '/' Operand {
        if($1 != $3) {
            char *t1, *t3;
            if($1 == 'I')
                t1="int";
            else if($1 == 'F')
                t1="float";
            else if($1 == 'B')
                t1="bool";
            else if($1 == 'S')
                t1="string";
            if($3 == 'I')
                t3="int";
            else if($3 == 'F')
                t3="float";
            else if($3 == 'B')
                t3="bool";
            else if($3 == 'S')
                t3="string";
            printf("error:%d: invalid operation: QUO (mismatched types %s and %s)\n", yylineno, t1, t3);
        }
        printf("QUO\n"); 
    }
    | Operand '%' Operand {
        if($1 == 'F' || $3 == 'F') {
            printf("error:%d: invalid operation: (operator REM not defined on float)\n", yylineno);
        }
        printf("REM\n");
    }
    | Operand AND Operand {
        if($1 == 'I' || $3 == 'I')
            printf("error:%d: invalid operation: (operator AND not defined on int)\n", yylineno);
        else if($1 == 'F' || $3 == 'F')
            printf("error:%d: invalid operation: (operator AND not defined on float)\n", yylineno);
        else if($1 == 'S' || $3 == 'S')
            printf("error:%d: invalid operation: (operator AND not defined on string)\n", yylineno);
        printf("AND\n");
    }
    | Operand OR Expression {
        if($1 == 'I' || $3 == 'I')
            printf("error:%d: invalid operation: (operator OR not defined on int)\n", yylineno);
        else if($1 == 'F' || $3 == 'F')
            printf("error:%d: invalid operation: (operator OR not defined on float)\n", yylineno);
        else if($1 == 'S' || $3 == 'S')
            printf("error:%d: invalid operation: (operator OR not defined on string)\n", yylineno);
        printf("OR\n");
    }
;
OpStmt
    : OpExpr SEMICOLON
;
Block
    : { create_symbol(table_end->scope+1);}
      '{' StatementList '}'
      { dump_symbol(table_end->scope); }
;
StatementList
    : Statement
    | StatementList Statement
    |
;
IfStmt
    : IF Condition Block
    | IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
;
Condition
    : Expression {
        if(!cmp_err) {
            if($1 == 'I')
                printf("error:%d: non-bool (type int) used as for condition\n", yylineno+1);
            else if($1 == 'F')
                printf("error:%d: non-bool (type float) used as for condition\n", yylineno+1);
            else if($1 == 'S')
                printf("error:%d: non-bool (type string) used as for condition\n", yylineno+1);
        }
        cmp_err = 0;
    }
;
WhileStmt
    : WHILE '(' Condition ')'Block
;
ForStmt
    : FOR '(' ForClause ')' Block
;
ForClause
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt
;
InitStmt
    : SimpleExpr
;
PostStmt
    : SimpleExpr
;
SimpleExpr
    : AssignmentExpr
    | Expression
    | IncDecExpr
;
PrintStmt
    : PRINT '(' Expression  ')' SEMICOLON {
        char *t;
        if(yylval.ctr.id_type == 'I')
            t="int";
        else if(yylval.ctr.id_type == 'F')
            t="float";
        else if(yylval.ctr.id_type == 'B')
            t="bool";
        else if(yylval.ctr.id_type == 'S')
            t="string";
        printf("PRINT %s\n", t);
    }
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}
static void create_symbol(int scope) {
    if(scope == 0) {
        table_head = malloc(sizeof(struct symbol_table));
        table_head->scope = 0;
        table_head->index_num = -1;
        table_head->next = NULL;
        table_head->prev = NULL;
        table_end = table_head;
    }
    else {
        table_ptr new_table = malloc(sizeof(struct symbol_table));
        new_table->scope = scope;
        new_table->index_num = -1;
        new_table->next = NULL;
        new_table->prev = table_end;
        table_end->next = new_table;
        table_end = new_table;
    }
}
static void insert_symbol(char *name, char type, int address, int lineno, char element_type) {
    table_end->index_num++;
    int num = table_end->index_num;
    table_end->index[num] = num;
    table_end->name[num] = name;
    table_end->type[num] = type;
    table_end->address[num] = address;
    table_end->lineno[num] = lineno;
    table_end->element_type[num] = element_type;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", table_end->name[num], table_end->scope);
}
static l_r lookup_symbol(char *id_name) {
    l_r temp_return;
    temp_return.add = -10;
    temp_return.line = -10;
    temp_return.type = 'Z';
    temp_return.sco = -10;
    table_ptr temp = table_end;
    while(temp) {
        for(int i=0; i<=temp->index_num; ++i) {
            if(!strcmp(temp->name[i], id_name)) {
                temp_return.add = temp->address[i];
                temp_return.line = temp->lineno[i];
                temp_return.sco = temp->scope;
                if(temp->type[i] == 'A')
                    temp_return.type = temp->element_type[i];
                else
                    temp_return.type = temp->type[i];
                return temp_return;
            }
        }
        temp = temp->prev;
    }
    return temp_return;
}
static void dump_symbol(int scope) {
    printf("> Dump symbol table (scope level: %d)\n", scope);
    printf("Index     Name      Type      Address   Lineno    Element type\n");
    for(int i=0; i <= table_end->index_num; ++i) {
        char *t;
        char *et = "-";
        if(table_end->type[i] == 'I')
            t="int";
        else if(table_end->type[i] == 'F')
            t="float";
        else if(table_end->type[i] == 'B')
            t="bool";
        else if(table_end->type[i] == 'S')
            t="string";
        else if(table_end->type[i] == 'A') {
            t="array";
            if(table_end->element_type[i] == 'I')
                et="int";
            else if(table_end->element_type[i] == 'F')
                et="float";
            else if(table_end->element_type[i] == 'B')
                et="bool";
            else if(table_end->element_type[i] == 'S')
                et="string";
        }
        printf("%-10d%-10s%-10s%-10d%-10d%s\n", table_end->index[i], table_end->name[i], t, table_end->address[i], table_end->lineno[i], et);
    }
    if(table_end->prev != NULL) {
        table_end = table_end->prev;
        table_end->next = NULL;
    }
}