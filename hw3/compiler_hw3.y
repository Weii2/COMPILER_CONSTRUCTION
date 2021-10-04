/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    #define codegen(...) \
        do { \
            for (int i = 0; i < INDENT; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    /* Other global variables */
    int INC_DEC_index = -1;
    int assign_index = -1;
    int assign = 0;
    int jump_num = 0;
    int while_num = 0;
    int if_num = 0;
    char arr_assign = 'Z';
    int is_print_array = 0;
    int is_inc_dec = -1;
    int is_if_block = 0;
    int else_num = 0;
    int is_con = 0;
    int one_if = 0;
    char arr_con = 'Z';
    char arr_con_type = 'Z';
    is_new_line = 0;
    FILE *fout = NULL;
    bool HAS_ERROR = false;
    int INDENT = 0;

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
        int index;
        int add;
        int line;
        char type;
        char arr_type;
        int sco;
    };
    l_r global_fun_return;
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
        int is_if;
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
        codegen("ior\n");
        yylval.ctr.id_type = 'B';
    }
    | A { $$ = $1; }
;
UnaryExpr
    : PrimaryExpr { $$ = $1; }
    | unary_op UnaryExpr {
        printf("%s\n", $1);
        if(!strcmp($1, "NEG")) {
            if($2 == 'I') {
                codegen("ineg\n");
                $$ = 'I';
            }
            else if($2 == 'F') {
                codegen("fneg\n");
                $$ = 'F';
            }
        }
        else if(!strcmp($1, "NOT")) {
            codegen("iconst_1\n");
            codegen("ixor\n");
        }
    }
;
A
    : A AND B {        
        printf("AND\n");
        codegen("iand\n");
    }
    | B { $$ = $1; }
;
B
    : B cmp_op C{
        cmp_err = 1;
        printf("%s\n", $2);
        if(!strcmp($2, "GTR")) {
            if($1 == 'I') {
                codegen("isub\n");
                codegen("ifgt L_cmp_%d\n", jump_num++);
                codegen("iconst_0\n");
                codegen("goto L_cmp_%d\n", jump_num++);
                codegen("L_cmp_%d:\n", jump_num-2);
                codegen("iconst_1\n");
                codegen("L_cmp_%d:\n", jump_num-1);
            }
            else if($1 == 'F') {
                codegen("fcmpl\n");
                codegen("iflt L_cmp_%d\n", jump_num++);
                codegen("iconst_1\n");
                codegen("goto L_cmp_%d\n", jump_num++);
                codegen("L_cmp_%d:\n", jump_num-2);
                codegen("iconst_0\n");
                codegen("L_cmp_%d:\n", jump_num-1);
            }
        }
        else if(!strcmp($2, "EQL")) {
            if($1 == 'I') {
                codegen("isub\n");
                codegen("ifeq L_cmp_%d\n", jump_num++);
                codegen("iconst_0\n");
                codegen("goto L_cmp_%d\n", jump_num++);
                codegen("L_cmp_%d:\n", jump_num-2);
                codegen("iconst_1\n");
                codegen("L_cmp_%d:\n", jump_num-1);
            }
        }
        else if(!strcmp($2, "NEQ")) {
            if($1 == 'I') {
                codegen("isub\n");
                codegen("ifne L_cmp_%d\n", jump_num++);
                codegen("iconst_0\n");
                codegen("goto L_cmp_%d\n", jump_num++);
                codegen("L_cmp_%d:\n", jump_num-2);
                codegen("iconst_1\n");
                codegen("L_cmp_%d:\n", jump_num-1);
            }
        }
        else if(!strcmp($2, "LEQ")) {
            if($1 == 'I') {
                codegen("isub\n");
                codegen("ifle L_cmp_%d\n", jump_num++);
                codegen("iconst_0\n");
                codegen("goto L_cmp_%d\n", jump_num++);
                codegen("L_cmp_%d:\n", jump_num-2);
                codegen("iconst_1\n");
                codegen("L_cmp_%d:\n", jump_num-1);
            }
        }
        else if(!strcmp($2, "LSS")) {
            if($1 == 'I') {
                codegen("isub\n");
                codegen("iflt L_cmp_%d\n", jump_num++);
                codegen("iconst_0\n");
                codegen("goto L_cmp_%d\n", jump_num++);
                codegen("L_cmp_%d:\n", jump_num-2);
                codegen("iconst_1\n");
                codegen("L_cmp_%d:\n", jump_num-1);
            }
        }
    }
    | C { $$ = $1; }
;
C
    : C add_op D {
        printf("%s\n", $2);
        if(!strcmp($2, "ADD")) {
            if($3 == 'I')
                codegen("iadd\n");
            else if($3 == 'F') {
                codegen("fadd\n");
            }
        }
        else if(!strcmp($2, "SUB")) {
            if($1 == 'I')
                codegen("isub\n");
            else if($1 == 'F')
                codegen("fsub\n");
        }
    }
    | D { $$ = $1; }
;
D
    : D mul_op D {
        printf("%s\n", $2);
        if(!strcmp($2, "MUL")) {
            if($1 == 'I')
                codegen("imul\n");
            else
                codegen("fmul\n");
        }
        else if(!strcmp($2, "QUO")) {
            if($1 == 'I')
                codegen("idiv\n");
            else
                codegen("fdiv\n");
        }
        else if(!strcmp($2, "REM")) {
            if($1 == 'I')
                codegen("irem\n");
        }
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
        global_fun_return = fun_return;
        if(fun_return.add == -10) {
            undefine_err = 1;
            printf("error:%d: undefined: %s\n", yylineno, yylval.ctr.id_name);
            HAS_ERROR = true;
        }
        else {
            yylval.ctr.id_type = fun_return.type;
            INC_DEC_index = fun_return.add;
            if(assign_index == -1)
                assign_index = fun_return.add;
            conver_type = fun_return.type;
            $$ = fun_return.type;
            printf("IDENT (name=%s, address=%d)\n", yylval.ctr.id_name, fun_return.add);

            if(fun_return.arr_type == 'A') {
                codegen("aload %d\n", fun_return.add);
                arr_con = 'A';
                arr_con_type = conver_type;
            }
            else if(fun_return.type == 'I' || fun_return.type == 'B')
                codegen("iload %d\n", fun_return.add);
            else if(fun_return.type == 'F')
                codegen("fload %d\n", fun_return.add);
            else if(fun_return.type == 'S')
                codegen("aload %d\n", fun_return.add);
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
        codegen("ldc %d\n", $1);
    }
    | FLOAT_LIT {
        conver_type = 'F';
        yylval.ctr.id_type = 'F';
        $$ = 'F';
        printf("FLOAT_LIT %f\n", $1);
        codegen("ldc %f\n", $1);
    }
    | BOOL_LIT {
        conver_type = 'B';
        yylval.ctr.id_type = 'B';
        $$ = 'B';
        if($1) {
            printf("TRUE\n");
            codegen("iconst_1\n");
        }
        else {
            printf("FALSE\n");
            codegen("iconst_0\n");
        }
    }
    | STRING_LIT {
        conver_type = 'S';
        yylval.ctr.id_type = 'S';
        $$ = 'S';
        printf("STRING_LIT %s\n", $1);
        codegen("ldc \"%s\"\n", $1);
    }
;

IndexExpr
    : PrimaryExpr '[' Expression ']' {
        if(assign || is_print_array) {
            if($1 == 'I')
                codegen("iaload\n");
            else if($1 == 'F')
                codegen("faload\n");
        }
        asslit = 0;
        yylval.ctr.id_type = $1;
        arr_assign = 'A';
    }
;

ConversionExpr
    : '('{is_con=1;} Type ')' Expression {
        $$ = $3;
        yylval.ctr.id_type = $3;
        printf("%c to %c\n", conver_type, $3);
        if(arr_con == 'A')
            codegen("%c2%c\n", arr_con_type+32, $3+32);
        else if(!(conver_type == $3))
            codegen("%c2%c\n", conver_type+32, $3+32);
        arr_con = 'Z';
        arr_con_type = 'Z';
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
        if(fun_return.sco != table_end->scope) {
            insert_symbol(yylval.ctr.id_name, yylval.ctr.id_type, address_glo++, yylineno, 'E');
            if(yylval.ctr.id_type == 'I') {
                codegen("ldc 0\n");
                codegen("istore %d\n", address_glo-1);
            }
            else if(yylval.ctr.id_type == 'B') {
                codegen("iconst_0\n");
                codegen("istore %d\n", address_glo-1);
            }
            else if(yylval.ctr.id_type == 'F') {
                codegen("ldc 0.0\n");
                codegen("fstore %d\n", address_glo-1);
            }
            else if(yylval.ctr.id_type == 'S') {
                codegen("ldc \"\"\n");
                codegen("astore %d\n", address_glo-1);
            }
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, yylval.ctr.id_name, fun_return.line);
            HAS_ERROR = true;
        }
    }
    | Type IDENT {id_name_glo = strdup(yylval.ctr.id_name);} ASSIGN Expression SEMICOLON  {
        insert_symbol(id_name_glo, yylval.ctr.id_type, address_glo++, yylineno, 'E');
        l_r fun_return = lookup_symbol(id_name_glo);
        if(yylval.ctr.id_type == 'I' || yylval.ctr.id_type == 'B')
            codegen("istore %d\n", fun_return.add);
        else if(yylval.ctr.id_type == 'F')
            codegen("fstore %d\n", fun_return.add);
        else if(yylval.ctr.id_type == 'S')
            codegen("astore %d\n", fun_return.add);
    }
    | Type IDENT {id_name_glo = strdup(yylval.ctr.id_name);} '[' Expression ']' SEMICOLON {
        insert_symbol(id_name_glo, 'A', address_glo++, yylineno, $1);
        if($1 == 'I')
            codegen("newarray int\n");
        else if($1 == 'F')
            codegen("newarray float\n");
        codegen("astore %d\n", address_glo-1);
    }
;
AssignmentExpr
    : Expression {
        assign = 1;
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
        if(check_asslit) {
            printf("error:%d: cannot assign to %s\n", yylineno, t1);
            HAS_ERROR = true;
        }
        else if(($1 != $4) && !undefine_err) {
            printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno, t1, t3);
            HAS_ERROR = true;
        }
        undefine_err = 0;
        asslit = 0;
        check_asslit = 0;
        printf("%s\n", $3);
        
        if(!strcmp($3, "ASSIGN")) {
            if(arr_assign == 'A') {
                if(yylval.ctr.id_type == 'I')
                    codegen("iastore\n");
                else if(yylval.ctr.id_type == 'F')
                    codegen("fastore\n");
                arr_assign = 'Z';
            }
            else {
                if(is_con) {
                    if(yylval.ctr.id_type == 'I')
                        codegen("istore %d\n", assign_index);
                    else if(yylval.ctr.id_type == 'B')
                        codegen("istore %d\n", assign_index);
                    else if(yylval.ctr.id_type == 'F')
                        codegen("fstore %d\n", assign_index);
                    else if(yylval.ctr.id_type == 'S')
                        codegen("astore %d\n", assign_index);
                    is_con = 0;
                }
                else {
                    if(yylval.ctr.id_type == 'I')
                        codegen("istore %d\n", global_fun_return.add);
                    else if(yylval.ctr.id_type == 'B')
                        codegen("istore %d\n", global_fun_return.add);
                    else if(yylval.ctr.id_type == 'F')
                        codegen("fstore %d\n", global_fun_return.add);
                    else if(yylval.ctr.id_type == 'S')
                        codegen("astore %d\n", global_fun_return.add);
                }
                codegen("pop\n");
            }
        }
        else if(!strcmp($3, "ADD_ASSIGN")) {
            if(yylval.ctr.id_type == 'I') {
                codegen("iadd\n");
                codegen("istore %d\n", global_fun_return.add);
            }
            else if(yylval.ctr.id_type == 'F') {
                codegen("fadd\n");
                codegen("fstore %d\n", global_fun_return.add);
            }
        }
        else if(!strcmp($3, "SUB_ASSIGN")) {
            if(yylval.ctr.id_type == 'I') {
                codegen("isub\n");
                codegen("istore %d\n", assign_index);
            }
            else if(yylval.ctr.id_type == 'F') {
                codegen("fsub\n");
                codegen("fstore %d\n", assign_index);
            }
        }
        else if(!strcmp($3, "MUL_ASSIGN")) {
            if(yylval.ctr.id_type == 'I') {
                codegen("imul\n");
                codegen("istore %d\n", assign_index);
            }
            else if(yylval.ctr.id_type == 'F') {
                codegen("fmul\n");
                codegen("fstore %d\n", assign_index);
            }
        }
        else if(!strcmp($3, "QUO_ASSIGN")) {
            if(yylval.ctr.id_type == 'I') {
                codegen("idiv\n");
                codegen("istore %d\n", assign_index);
            }
            else if(yylval.ctr.id_type == 'F') {
                codegen("fdiv\n");
                codegen("fstore %d\n", assign_index);
            }
        }
        else if(!strcmp($3, "REM_ASSIGN")) {
            if(yylval.ctr.id_type == 'I') {
                codegen("irem\n");
                codegen("istore %d\n", assign_index);
            }
        }
        assign_index = -1;
        assign = 0;
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
    : Expression INC {
        printf("INC\n");
        if($1 == 'I') {
            codegen("ldc 1\n");
            codegen("iadd\n");
            codegen("istore %d\n", INC_DEC_index);
            is_inc_dec = 1;
        }
        else if($1 == 'F') {
            codegen("ldc 1.0\n");
            codegen("fadd\n");
            codegen("fstore %d\n", INC_DEC_index);
            is_inc_dec = 1;
        }
        
        INC_DEC_index = -1;
    }
    | Expression DEC { 
        printf("DEC\n");
        
        if($1 == 'I') {
            codegen("ldc 1\n");
            codegen("isub\n");
            codegen("istore %d\n", INC_DEC_index);
            is_inc_dec = 0;
        }
        else if($1 == 'F') {
            codegen("ldc 1.0\n");
            codegen("fsub\n");
            codegen("fstore %d\n", INC_DEC_index);
            is_inc_dec = 0;
        }
        
        INC_DEC_index = -1;
    }
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
            HAS_ERROR = true;
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
            HAS_ERROR = true;
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
            HAS_ERROR = true;
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
            HAS_ERROR = true;
        }
        printf("QUO\n"); 
    }
    | Operand '%' Operand {
        if($1 == 'F' || $3 == 'F') {
            printf("error:%d: invalid operation: (operator REM not defined on float)\n", yylineno);
            HAS_ERROR = true;
        }
        printf("REM\n");
    }
    | Operand AND Operand {
        if($1 == 'I' || $3 == 'I') {
            printf("error:%d: invalid operation: (operator AND not defined on int)\n", yylineno);
            HAS_ERROR = true;
        }
        else if($1 == 'F' || $3 == 'F') {
            printf("error:%d: invalid operation: (operator AND not defined on float)\n", yylineno);
            HAS_ERROR = true;
        }
        else if($1 == 'S' || $3 == 'S') {
            printf("error:%d: invalid operation: (operator AND not defined on string)\n", yylineno);
            HAS_ERROR = true;
        }
        printf("AND\n");
        codegen("iand\n");
    }
    | Operand OR Expression {
        if($1 == 'I' || $3 == 'I') {
            printf("error:%d: invalid operation: (operator OR not defined on int)\n", yylineno);
            HAS_ERROR = true;
        }
        else if($1 == 'F' || $3 == 'F') {
            printf("error:%d: invalid operation: (operator OR not defined on float)\n", yylineno);
            HAS_ERROR = true;
        }
        else if($1 == 'S' || $3 == 'S') {
            printf("error:%d: invalid operation: (operator OR not defined on string)\n", yylineno);
            HAS_ERROR = true;
        }
        printf("OR\n");
        codegen("ior\n");
    }
;
OpStmt
    : OpExpr SEMICOLON
;
Block
    : { create_symbol(table_end->scope+1);
        if(yylval.ctr.is_if)
            is_if_block = 1;
    }
      '{' StatementList '}'
      { dump_symbol(table_end->scope);
        if(is_if_block) {
            codegen("goto L_if_exit_%d\n", table_end->scope + if_num);
            codegen("L_if_false_%d :\n", table_end->scope + if_num + else_num);
            one_if++;
        }
        is_if_block = 0;
    }
;
StatementList
    : Statement
    | StatementList Statement
    |
;
IfStmt
    : IF Condition Block {
        if(!(one_if==1))
            codegen("L_if_false_%d :\n", table_end->scope + if_num);
        else
            codegen("L_if_exit_%d :\n", table_end->scope + if_num);
        if(table_end->scope == 0)
            if_num++;
        yylval.ctr.is_if=0;
        one_if = 0;
    }
    | IF Condition Block ELSE {else_num++;} IfStmt {
        yylval.ctr.is_if=0;
        else_num = 0;
    }
    | IF Condition Block ELSE {else_num++;} Block {
        codegen("L_if_exit_%d :\n", table_end->scope + if_num);
        if(table_end->scope == 0)
            if_num++;
        yylval.ctr.is_if=0;
        else_num = 0;
    } 
;
Condition
    : Expression {
        if(!cmp_err) {
            if($1 == 'I') {
                printf("error:%d: non-bool (type int) used as for condition\n", yylineno+1);
                HAS_ERROR = true;
            }
            else if($1 == 'F') {
                printf("error:%d: non-bool (type float) used as for condition\n", yylineno+1);
                HAS_ERROR = true;
            }
            else if($1 == 'S') {
                printf("error:%d: non-bool (type string) used as for condition\n", yylineno+1);
                HAS_ERROR = true;
            }
        }
        if(yylval.ctr.is_if)
            codegen("ifeq L_if_false_%d\n", table_end->scope + if_num + else_num);
        cmp_err = 0;
    }
;
WhileStmt
    : WHILE {
        codegen("L_for_begin_%d :\n", table_end->scope + while_num);
    } '(' Condition {
        codegen("ifeq L_for_exit_%d\n", table_end->scope + while_num);
    } ')'Block {
        codegen("goto L_for_begin_%d\n", table_end->scope + while_num);
        codegen("L_for_exit_%d :\n", table_end->scope + while_num);
        if(table_end->scope == 0)
            while_num++;
    }
;
ForStmt
    : FOR '(' ForClause ')' {
        codegen("L_for_Block_%d :\n", table_end->scope + while_num);
    }Block {
        codegen("goto L_for_PostStmt_%d\n", table_end->scope + while_num);
        codegen("L_for_exit_%d :\n", table_end->scope + while_num);
        while_num++;
    }
;
ForClause
    : InitStmt SEMICOLON {
        codegen("L_for_begin_%d :\n", table_end->scope + while_num);
    } Condition {
        codegen("ifeq L_for_exit_%d\n", table_end->scope + while_num);
        codegen("goto L_for_Block_%d\n", table_end->scope + while_num);
        codegen("L_for_PostStmt_%d :\n", table_end->scope + while_num);
    } SEMICOLON PostStmt {
        codegen("goto L_for_begin_%d\n", table_end->scope + while_num);
    }
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
    : PRINT '(' {is_print_array=1;} Expression  ')' SEMICOLON {
        char *t;
        if(yylval.ctr.id_type == 'I') {
            t="int";
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            codegen("swap\n");
            codegen("invokevirtual java/io/PrintStream/print(I)V\n");
        }
        else if(yylval.ctr.id_type == 'F') {
            t="float";
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            codegen("swap\n");
            codegen("invokevirtual java/io/PrintStream/print(F)V\n");
        }
        else if(yylval.ctr.id_type == 'B') {
            t="bool";
            codegen("ifne L_cmp_%d\n", jump_num++);
            codegen("ldc \"false\"\n");
            codegen("goto L_cmp_%d\n", jump_num++);
            codegen("L_cmp_%d:\n", jump_num-2);
            codegen("ldc \"true\"\n");
            codegen("L_cmp_%d:\n", jump_num-1);
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            codegen("swap\n");
            codegen("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        }
        else if(yylval.ctr.id_type == 'S') {
            t="string";
            if(!is_new_line){
                codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                codegen("swap\n");
                codegen("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
            }
            is_new_line = 0;            
        }
        printf("PRINT %s\n", t);
        is_print_array = 0;
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

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    codegen(".source hw3.j\n");
    codegen(".class public Main\n");
    codegen(".super java/lang/Object\n");
    codegen(".method public static main([Ljava/lang/String;)V\n");
    codegen(".limit stack 100\n");
    codegen(".limit locals 100\n");
    INDENT++;

    yyparse();

	printf("Total lines: %d\n", yylineno);

    /* Codegen end */
    codegen("return\n");
    INDENT--;
    codegen(".end method\n");
    fclose(fout);
    fclose(yyin);

    if (HAS_ERROR) {
        remove(bytecode_filename);
    }
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
    temp_return.index = -10;
    temp_return.add = -10;
    temp_return.line = -10;
    temp_return.type = 'Z';
    temp_return.arr_type = 'Z';
    temp_return.sco = -10;
    table_ptr temp = table_end;
    while(temp) {
        for(int i=0; i<=temp->index_num; ++i) {
            if(!strcmp(temp->name[i], id_name)) {
                temp_return.index = temp->index[i];
                temp_return.add = temp->address[i];
                temp_return.line = temp->lineno[i];
                temp_return.sco = temp->scope;
                if(temp->type[i] == 'A') {
                    temp_return.type = temp->element_type[i];
                    temp_return.arr_type = 'A';
                }
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