%code top{
    #include <iostream>
    #include <assert.h>
    #include <cstring>
    #include <stack>
    #include "parser.h"
    extern Ast ast;
    int yylex();
    int yyerror( char const * );
    ArrayType* arrayType;
    int idx;
    int paramNo = 0;
    int* arrayValue;

    

    std::stack<StmtNode*> whileStk;
    int whileCnt = 0;
    #include <iostream>
    using namespace std;
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"
}

%union {
    int itype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
    SymbolEntry* se;
}

%start Program
%token <strtype> ID 
%token <itype> INTEGER
%token IF ELSE
%token WHILE FOR BREAK CONTINUE
%token INT VOID CONST FLOAT
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA LBRACKET RBRACKET  GETINT PUTINT PUTCH GETCH GETFLOAT PUTFLOAT
%token ASSIGN
%token LESS EQ MORE LESSQ MOREQ NOTEQ
%token MUL DIV MOD
%token OR AND NOT
%token ADD SUB 
%token RETURN


%nterm <stmttype> Stmts Stmt AssignStmt BlockStmt IfStmt WhileStmt BreakStmt ContinueStmt ReturnStmt DeclStmt ConstDeclStmt ConstDecls ConstDecl VarDeclStmt VarDecls VarDecl FuncDef  NullStmt
%nterm <exprtype> Exp AddExp MulExp Cond LOrExp PrimaryExp LVal RelExp LAndExp UnaryExp FuncCallExp CallList Istream Ostream FuncParams FuncParam  EqExp ConstExp ArrayIndex FuncArrayIndex ArrayValList ArrayVal ConstArrayValList ConstArrayVal
%nterm <type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts {
        ast.setRoot($1);
    }
    ;

Stmts
    : Stmt {$$=$1;}
    | Stmts Stmt{
        $$ = new SeqNode($1, $2);
    }
    ;
Stmt
    : AssignStmt {$$=$1;}
    | BlockStmt {$$=$1;}
    | IfStmt {$$=$1;}
    | WhileStmt {$$=$1;}
    | ReturnStmt {$$=$1;}
    | BreakStmt {$$=$1;}
    | ContinueStmt {$$=$1;}
    | DeclStmt {$$=$1;}
    | FuncDef {$$=$1;}
    | NullStmt {$$=$1;}
    ;



//---------------------数组--------------------

ConstExp
    : AddExp {$$ = $1;}
    ;














// 左值表达式   
LVal
    : ID {
        SymbolEntry *se;
        se = identifiers->lookup($1);
        if(se == nullptr)
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            //exit(EXIT_FAILURE);
            assert(se != nullptr);
        }
        $$ = new Id(se);
        delete []$1;
    }
    | ID ArrayIndex {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        if(se==nullptr) {
            fprintf(stderr, "Variable \"%s\" has been used before definition\n", (char*)$1);
            exit(EXIT_FAILURE);
        }
        $$ = new Id(se, $2);
        delete []$1;
    }
    ;

AssignStmt
    :
    LVal ASSIGN Exp SEMICOLON{
        $$ = new AssignStmt($1, $3);
    }
    ;

//语句块
BlockStmt
    :   LBRACE 
        {identifiers = new SymbolTable(identifiers);} 
        Stmts RBRACE 
        {
            $$ = new CompoundStmt($3);
            SymbolTable *top = identifiers;
            identifiers = identifiers->getPrev();
            delete top;
        }
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN {
        $$ = new IfStmt($3, $5);
    }
    | IF LPAREN Cond RPAREN LBRACE RBRACE {
        $$ = new IfStmt($3,nullptr);
    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt {
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;
WhileStmt
    : WHILE LPAREN Cond RPAREN {
        whileCnt ++;
        WhileStmt *whileNode = new WhileStmt($3);
        $<stmttype>$ = whileNode;
        whileStk.push(whileNode);
    }
    Stmt {
        StmtNode *whileNode = $<stmttype>5; 
        ((WhileStmt*)whileNode)->setStmt($6);
        $$=whileNode;
        whileStk.pop();
        whileCnt--;
    } 
    ;
BreakStmt
    : BREAK SEMICOLON {
        $$ = new BreakStmt(whileStk.top());
    }
    ;
ContinueStmt
    : CONTINUE SEMICOLON {
        $$ = new ContinueStmt(whileStk.top());
    }
    ;    
ReturnStmt
    :
    RETURN Exp SEMICOLON{  //有返回值 setRet()-》isret==true;
        Type* t= current->getType();//t是一个函数类型
        if(dynamic_cast<FunctionType*>(t)->getRetType()!=TypeSystem::intType){
            fprintf(stderr,"error: return value's type and the function's type do not match\n");
            exit(EXIT_FAILURE);
        }else{
            dynamic_cast<FunctionType*>(t)->setRet();
            $$ = new ReturnStmt($2);
        }
    }
    |
    RETURN SEMICOLON{    //没有返回值，
        Type* t= current->getType();
        if(dynamic_cast<FunctionType*>(t)->getRetType()==TypeSystem::intType){
            fprintf(stderr,"lack return value\n");
            exit(EXIT_FAILURE);
        }else{
            $$ = new ReturnStmt(nullptr);
        }        
    }
    ;

NullStmt
    :
    SEMICOLON 
    {
        $$=new NullStmt(nullptr);
    }
    |
    Exp SEMICOLON
    {
        $$=new NullStmt($1);
    }
    ;
Exp
    :
    RelExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;}
    ;    

AddExp
    :
    MulExp {$$ = $1;}
    |
    AddExp ADD MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    |
    AddExp SUB MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;

MulExp
    :
    UnaryExp {$$ = $1;}
    |
    MulExp MUL UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    |
    MulExp DIV UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    |
    MulExp MOD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;

UnaryExp
    :
    PrimaryExp {$$=$1;}
    |
    ADD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::ADD, $2);
    }
    |
    SUB UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
    }
    |
    NOT UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
    }
    ;


PrimaryExp
    :
    LVal {
        $$ = $1;
    }  
    |  
    LPAREN Exp RPAREN {
        $$=$2;
    }
    | 
    INTEGER {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    |
    FuncCallExp{
        $$=$1;
    }
    ;

RelExp
    :
    AddExp {$$ = $1;}
    |
    RelExp LESS AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    |
    RelExp MORE AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MORE, $1, $3);
    }
    |
    RelExp LESSQ AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSQ, $1, $3);
    }
    |
    RelExp MOREQ AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOREQ, $1, $3);
    }
    
    ;

// 逻辑与表达式

EqExp
    : RelExp {$$ = $1;}
    | EqExp EQ RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQ, $1, $3);
    }
    | EqExp NOTEQ RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NOTEQ, $1, $3);
    }
    ;

LAndExp
    :
    EqExp {$$ = $1;}
    |
    LAndExp AND EqExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;

//逻辑或表达式
LOrExp
    :
    LAndExp {$$ = $1;}
    |
    LOrExp OR LAndExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    | FLOAT {
        $$ = TypeSystem::floatType;
    }
    ;
DeclStmt
    : 
    VarDeclStmt {$$=$1;}
    |
    ConstDeclStmt {$$=$1;}
    ;
VarDeclStmt
    :Type VarDecls SEMICOLON {
        $$=$2;
    };
ConstDeclStmt
    :CONST Type ConstDecls SEMICOLON {
        $$=$3;
    };
VarDecls
    : VarDecl {$$=$1;}
    | 
    VarDecl COMMA VarDecls {
        $$ = new VarDecls($1, $3);
    };
ConstDecls
    : ConstDecl {$$=$1;}
    |
    ConstDecl COMMA ConstDecls {
        $$ = new ConstDecls($1, $3);
    }

//先加上InitVal 


VarDecl
    :
    ID {
        SymbolEntry *se;
        se = identifiers->lookup($1);
       if(se!=nullptr&&dynamic_cast<IdentifierSymbolEntry*>(se)->getScope()==identifiers->getLevel()){
            fprintf(stderr,"identifier \"%s\" is redefined\n", (char*)$1);
            exit(EXIT_FAILURE);
            delete [](char*)$1;
            assert(se != nullptr);
        }else{
            se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
            identifiers->install($1, se);
            $$ = new VarDecl(new Id(se),nullptr);
            delete []$1;
        
    }
    }
    //赋予初值的情况
    | ID ASSIGN Exp{
        SymbolEntry *se;
        se = identifiers->lookup($1);
         if(se!=nullptr&&dynamic_cast<IdentifierSymbolEntry*>(se)->getScope()==identifiers->getLevel()){
            fprintf(stderr,"identifier \"%s\" is redefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }else{ 
            //fprintf(stderr,"identifier \"%s\" is defined1\n", (char*)$1);
            se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
             ((IdentifierSymbolEntry*)se)->setValue($3->getValue());
            identifiers->install($1, se);
            $$ = new VarDecl(new Id(se),$3);
            delete []$1;
        }
     }
     | ID ArrayIndex {
        SymbolEntry* se;
        ExprNode* temp = $2;
        std::vector<int> vecIdx;
        while (temp) {
            vecIdx.push_back(temp->getValue());
            temp = (ExprNode*)(temp->getNext());
        }
        Type *type = TypeSystem::intType;
        Type* temp1;
        while(!vecIdx.empty()){
            temp1 = new ArrayType(type, vecIdx.back());
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp1);
            type = temp1;
            vecIdx.pop_back();
        }


        arrayType = (ArrayType*)type;
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setAllZero();
        int *p = new int[type->getSize()];  //设置整型空间 即长度*大小
        ((IdentifierSymbolEntry*)se)->setArrayValue(p);
        identifiers->install($1, se);
        $$ = new VarDecl(new Id(se));
        delete []$1;
    }
    | ID ArrayIndex ASSIGN ArrayVal {
        SymbolEntry* se;
        ExprNode* temp = $2;
        std::vector<int> vecIdx;
        while (temp) {
            vecIdx.push_back(temp->getValue());
            temp = (ExprNode*)(temp->getNext());
        }
        Type *type;
        while (!vecIdx.empty()) {
            type = new ArrayType(TypeSystem::intType, vecIdx.back());
            vecIdx.pop_back();
        }
        arrayType = (ArrayType*)type;
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        int *p = new int[type->getSize()];  //设置整型空间 即长度*大小
        ((IdentifierSymbolEntry*)se)->setArrayValue(p);
        identifiers->install($1, se);
        
        /*if(!identifiers->install($1, se)){
            fprintf(stderr, "Variable \"%s\" redefinition\n", (char*)$1);
            exit(EXIT_FAILURE);
        }*/
        $$ = new VarDecl(new Id(se), $4);
        delete []$1;
    }
    ;
ConstDecl
    :
    ID {
        SymbolEntry *se;
        se = identifiers->lookup($1);
        if(se!=nullptr&&dynamic_cast<IdentifierSymbolEntry*>(se)->getScope()==identifiers->getLevel()){
            fprintf(stderr,"identifier \"%s\" is redefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }else{
            se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
            //se->setConstant();
            identifiers->install($1, se);
            
            $$ = new ConstDecl(new Id(se),nullptr);
            delete []$1;
        }
    }
    |
    ID ASSIGN Exp{
        SymbolEntry *se;
        se = identifiers->lookup($1);
        if(se!=nullptr&&dynamic_cast<IdentifierSymbolEntry*>(se)->getScope()==identifiers->getLevel()){
            fprintf(stderr,"identifier \"%s\" is redefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }else{
            se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
            //se->setConstant();
           
            ((IdentifierSymbolEntry*)se)->setValue($3->getValue());
             identifiers->install($1, se);
            $$ = new ConstDecl(new Id(se),$3);
            delete []$1;
        }
    }
    | ID ArrayIndex ASSIGN ConstArrayVal {
        SymbolEntry* se;
        ExprNode* temp = $2;
        std::vector<int> vecIdx;
        while (temp) {
            vecIdx.push_back(temp->getValue());
            temp = (ExprNode*)(temp->getNext());
        }
        Type *type;
        while (!vecIdx.empty()) {
            type = new ArrayType(TypeSystem::intType, vecIdx.back());
            vecIdx.pop_back();
        }
        arrayType = (ArrayType*)type;
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        int *p = new int[type->getSize()];  //设置整型空间 即长度*大小
        ((IdentifierSymbolEntry*)se)->setArrayValue(p);
        ((IdentifierSymbolEntry*)se)->setConst();
        identifiers->install($1, se);
        /*if(!identifiers->install($1, se)){
            fprintf(stderr, "Variable \"%s\" redefinition\n", (char*)$1);
            exit(EXIT_FAILURE);
        }*/
        ((IdentifierSymbolEntry*)se)->setValue($4->getValue());
        $$ = new ConstDecl(new Id(se), $4);
        delete []$1;
    }



    ;

FuncDef
    :
    Type ID LPAREN{
        identifiers = new SymbolTable(identifiers);
        paramNo = 0;    //标记参数的id
    }
    FuncParams RPAREN 
    {
        Type *funcType;
        std::vector<SymbolEntry*> vecSe;
        std::vector<Type*> vec;
        FuncParams* temp = (FuncParams*)$5;
        while(temp){
            vecSe.push_back(temp->getParam()->getSymPtr());
            vec.push_back(temp->getParam()->getSymPtr()->getType());
            temp = (FuncParams*)(temp->getNext());
        }
        // funcType = new FunctionType($1,vec);
        funcType = new FunctionType($1, vec, vecSe);
        dynamic_cast<FunctionType*>(funcType)->setRetType($1);
        std::string name = $2+std::__cxx11::to_string(vec.size());
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, name, identifiers->getPrev()->getLevel());
        identifiers->getPrev()->install(name, se);       
        current = se;
    }BlockStmt{
        $$ = new FunctionDef(current, (FuncParams*)$5, $8);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    |
    Type ID LPAREN RPAREN 
    {
        Type *funcType;
        funcType = new FunctionType($1,{});
        dynamic_cast<FunctionType*>(funcType)->setRetType($1);
        std::string zero="0";
        std::string name=$2;
        if(name!="main"){
            name = $2+zero;
        }
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, name, identifiers->getLevel());
        identifiers->install(name, se);
        identifiers = new SymbolTable(identifiers);
        current = se;
    }BlockStmt{
        $$ = new FunctionDef(current, nullptr, $6);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;

FuncParams 
    :
    FuncParam {
        FuncParam* temp = (FuncParam*)$1;
        int type = temp ->getId()->dst->getType()->getKind();
        
        Type* type1 = temp->getSymPtr()->getType();
        SymbolEntry *se = new TemporarySymbolEntry(type1, SymbolTable::getLabel());
        $$=new FuncParams(se,$1,nullptr);
    }
    |
    FuncParam COMMA FuncParams {
        FuncParam* temp = (FuncParam*)$1;
        int type = temp ->getId()->dst->getType()->getKind();
        Type* type1 = temp->getSymPtr()->getType();
        SymbolEntry *se = new TemporarySymbolEntry(type1, SymbolTable::getLabel());
        $$=new FuncParams(se,$1,$3);
    }
    ;

FuncParam
    :
    Type ID {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new FuncParam(se, new Id(se),nullptr);
        delete []$2;
    }
    |
    Type ID ASSIGN Exp{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new FuncParam(se, new Id(se),$4);
        delete []$2;
    }
    | Type ID FuncArrayIndex {
        SymbolEntry* se;
        ExprNode* temp = $3;
        std::stack<ExprNode*> stackIdx;
        Type *arr;
        while (temp) {
            stackIdx.push(temp);
            temp = (ExprNode*)(temp->getNext());
        }
        while (!stackIdx.empty()) {
            arr = new ArrayType(TypeSystem::intType, stackIdx.top()->getValue());
            stackIdx.pop();
        }
        se = new IdentifierSymbolEntry(arr, $2, identifiers->getLevel());
        identifiers->install($2, se);
        //下面这两句话感觉可能会用不到
        //((IdentifierSymbolEntry*)se)->setLabel();
        //((IdentifierSymbolEntry*)se)->setAddr(new Operand(se));
       // $$ = new DeclStmt(new Id(se));
        $$ = new FuncParam(se,new Id(se),nullptr);
        delete []$2;
    }

    ;
Istream
    :GETINT LPAREN RPAREN
    {
        SymbolEntry *se;
        se = identifiers->lookup("getint");
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), "getint", identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,nullptr);
    }
    |
    GETCH LPAREN RPAREN
    {
        SymbolEntry *se;
        se = identifiers->lookup("getch");
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), "getch", identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,nullptr);
    }
    |
    GETFLOAT LPAREN RPAREN
    {
        SymbolEntry *se;
        se = identifiers->lookup("getfloat");
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), "getfloat", identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,nullptr);
    }


    ;
Ostream
    :PUTINT LPAREN CallList RPAREN
    {
        SymbolEntry *se;
        se = identifiers->lookup("putint");
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), "putint", identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,$3);
    }
    |
    PUTCH LPAREN CallList RPAREN
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::voidType, SymbolTable::getLabel());
        se = identifiers->lookup("putch");
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), "putch", identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,$3);
    }
    |
    PUTFLOAT LPAREN CallList RPAREN
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::voidType, SymbolTable::getLabel());
        se = identifiers->lookup("putfloat");
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), "putfloat", identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,$3);
    }


    ;


//-----------数组相关的东西---------
ArrayIndex
    : LBRACKET ConstExp RBRACKET {
        $$ = $2;
    }
    | ArrayIndex LBRACKET ConstExp RBRACKET {
        $$ = $1;
        $1->setNext($3);//从左到右存多维数组的元素大小
    }
    ;

FuncArrayIndex
    : LBRACKET RBRACKET {
        $$ = new ExprNode(nullptr);
    }
    | RBRACKET Exp RBRACKET {
        $$ = $2;
    }
    | FuncArrayIndex RBRACKET Exp RBRACKET {
        $$ = $1;
        $1->setNext($3);//从左到右存多维数组的元素大小
    }
    ;

ArrayValList
    : ArrayVal {
        $$ = $1;
    }
    | ArrayValList COMMA ArrayVal {
        $$ = $1;
        $1->setNext($3);
    }
ArrayVal
    : Exp {
        $$ = $1;
    }
    | LBRACE RBRACE {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        $$ = new Constant(se);
    }
    | LBRACE ArrayValList RBRACE {
        $$ = $2;
    }
    ;
ConstArrayValList
    : ConstArrayVal {
        $$ = $1;
    }
    | ConstArrayValList COMMA ConstArrayVal {
        $$ = $1;
        $1->setNext($3);
    }
ConstArrayVal
    : Exp {
        $$ = $1;
    }
    | LBRACE RBRACE {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        $$ = new Constant(se);
    }
    | LBRACE ConstArrayValList RBRACE {
        $$ = $2;
    }
    ;


















CallList
    :
    Exp 
    {
        $$=new CallList(nullptr,$1,nullptr);
    }
    |
    Exp COMMA CallList
    {
        $$=new CallList(nullptr,$1,$3);
    }
    ;

FuncCallExp
    :
    ID LPAREN RPAREN
    {
        SymbolEntry *se;
        std::string zero="0";//没有参数的
        std::string name=$1+zero;
        se = identifiers->lookup(name);
        if(se==nullptr){
            fprintf(stderr,"identifier \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }
        
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), $1, identifiers->getLevel());

        $$=new FuncCallExp(thisSe,se,nullptr);
    }
    |
    ID LPAREN CallList RPAREN
    {
        SymbolEntry *se;
        std::vector<ExprNode*> vec;//参数
        ExprNode* temp = $3; // calllist（实参）
        while(temp){
            ExprNode *tempParam = dynamic_cast<CallList*>(temp)->getParam();//获取参数
            vec.push_back(tempParam);
            temp = dynamic_cast<CallList*>(temp)->getNext();
        }
        std::string name=$1+std::__cxx11::to_string(vec.size()); //vec.size()是参数个数
        se = identifiers->lookup(name);
        if(se==nullptr){
            fprintf(stderr,"identifier \"%s\" with %dparam(s) is undefined\n", (char*)$1,vec.size());
            delete [](char*)$1;
            assert(se != nullptr);
        }
        SymbolEntry* thisSe= new IdentifierSymbolEntry(dynamic_cast<FunctionType*>(se->getType())->getRetType(), $1, identifiers->getLevel());
        $$=new FuncCallExp(thisSe,se,$3);
    }
    |
    Istream{$$ = $1;}
    |
    Ostream{$$ = $1;}
    ;

%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
