#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include "Operand.h"

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node
{
private:
    static int counter;
    int seq;
    Node* next;
protected:
    std::vector<Instruction*> true_list;
    std::vector<Instruction*> false_list;
    static IRBuilder *builder;
    void backPatch(std::vector<Instruction*> &list, BasicBlock*bb, bool isTrue=true);
    std::vector<Instruction*> merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2);

public:
    Node();
    int getSeq() const {return seq;};
    static void setIRBuilder(IRBuilder*ib) {builder = ib;};
    virtual void output(int level) = 0;
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;
    std::vector<Instruction*>& trueList() {return true_list;}
    std::vector<Instruction*>& falseList() {return false_list;}
    Node* getNext() {return next;}
    void setNext(Node* node){
    Node* n = this;
    while (n->getNext()) {
        n = n->getNext();
    }
    if (n == this) {
        this->next = node;
    } else {
        n->setNext(node);
    }}

};

class ExprNode : public Node
{
public:
    
   
    Type* type;
    SymbolEntry *symbolEntry;
    Operand *dst;   // The result of the subtree is stored into dst.
    bool iscond=0;  //条件
    bool isId=0;
public:
    ExprNode(SymbolEntry *symbolEntry=nullptr) :symbolEntry(symbolEntry){};
    Operand* getOperand() {return dst;};
    SymbolEntry* getSymPtr() {return symbolEntry;};
    bool IsId(){return isId;};
    bool IsCond(){return iscond;};
    virtual int getValue() { return -1; };
    virtual Type* getType() {return type;};
    void output(int level);
    void typeCheck();
    void genCode();
};


//----------------数组要用到的---------------






class BinaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr1, *expr2;
public:
     enum {ADD, DIV, MUL,MOD, SUB, AND, OR, LESS, MORE, LESSQ,MOREQ,EQ, NOTEQ};
    BinaryExpr(SymbolEntry *se, int op, ExprNode*expr1, ExprNode*expr2) : 
    ExprNode(se), op(op), expr1(expr1), expr2(expr2)
    {
        dst = new Operand(se);
        if(op>=AND){
            iscond=true;
        }
    };
    void output(int level);
    void typeCheck();
    int getValue();
    void genCode();
};

//一元表达式
class UnaryExpr : public ExprNode
{
    //  -int
private:
    int op;
    ExprNode *expr;
public:
    enum {ADD,SUB,NOT};
    UnaryExpr(SymbolEntry *se,int op, ExprNode* expr) : ExprNode(se),op(op),expr(expr) 
    {
        
    dst = new Operand(se);
    if(op==NOT)iscond=1;
    
    };
    void output(int level);
    void typeCheck();
    void genCode();
    int getValue();
  //  void setType(Type* type) { this->type = type; }
};

class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se){dst = new Operand(se);};
    void output(int level);
    void typeCheck();
    void genCode();
    int getValue();
};

class Id : public ExprNode
{

private:
    ExprNode* arrIdx;
    bool left = false;
public:
    Id(SymbolEntry *se,ExprNode* arrIdx = nullptr) ;
    ExprNode* getArrIdx() { return arrIdx; };
    bool isLeft() const { return left; };
    void setLeft() { left = true; } 
    void output(int level);
    void typeCheck();
    int getValue();
    void genCode();
};

class FuncCallExp : public ExprNode
{
private:
    ExprNode* callList;
    SymbolEntry *funcse;//调用的函数的符号表项
    Id* id;
public:
    FuncCallExp(SymbolEntry* se, SymbolEntry* funcse, ExprNode* callList): ExprNode(se),
     funcse(funcse),callList(callList) {dst=new Operand(se); id=new Id(se);};
    SymbolEntry* getFunc() {return funcse;};
    void output(int level);
    void typeCheck();
    void genCode();
    Operand* getOperand(){
        return id->getOperand();
    };
};

class StmtNode : public Node
{};

class CompoundStmt : public StmtNode
{
private:
    StmtNode *stmt;
public:
    CompoundStmt(StmtNode *stmt) : stmt(stmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode
{
private:
    StmtNode *stmt1, *stmt2;
public:
    SeqNode(StmtNode *stmt1, StmtNode *stmt2) : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class VarDeclStmt : public StmtNode
{
private:
    StmtNode *varDecls;
public:
    VarDeclStmt(StmtNode *varDecls): varDecls(varDecls){};
    void output(int level); 
    void typeCheck();
    void genCode();  
};


//常量声明语句
class ConstDeclStmt : public StmtNode
{
private:
    StmtNode *constDecls;
public:
    ConstDeclStmt(StmtNode *constDecls): constDecls(constDecls){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class VarDecls : public StmtNode
{
private:
    StmtNode *varDecl,*varDecls;
public:
    VarDecls(StmtNode *varDecl,StmtNode *varDecls): varDecl(varDecl), varDecls(varDecls){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ConstDecls : public StmtNode
{
private:
    StmtNode *constDecl,*constDecls;
public:
    ConstDecls(StmtNode *constDecl,StmtNode *constDecls): constDecl(constDecl), constDecls(constDecls){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class VarDecl : public StmtNode
{
private:
    Id *id;
    ExprNode *expr;
public:
    VarDecl(Id *id,ExprNode *expr= nullptr) : id(id),expr(expr){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ConstDecl : public StmtNode
{
private:
    Id *id;
    ExprNode *expr;
public:
    ConstDecl(Id *id, ExprNode* expr) : id(id), expr(expr){};
    void output(int level);
    void typeCheck();
    void genCode();
};


class CallList : public ExprNode
{ //实参
private:
    ExprNode* expr;
    ExprNode* callList;
public:
    CallList(SymbolEntry*se, ExprNode* expr, ExprNode* callList) : ExprNode(se),expr(expr), callList(callList) {};
    void output(int level);
    ExprNode* getNext() {return callList;};
    ExprNode* getParam() {return expr;};
    void typeCheck();
    void genCode();
};

class CallStmt : public StmtNode
{
private:
    ExprNode* callExp;
public:
    CallStmt(ExprNode* callExp) : callExp(callExp) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class NullStmt : public StmtNode
{
private:
    ExprNode* expr;
public:
    NullStmt(ExprNode* expr): expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class FuncParams : public ExprNode
{
private:
    ExprNode* funcParam,*funcParams;
public:
    FuncParams(SymbolEntry* se,ExprNode* funcParam, ExprNode* funcParams) : ExprNode(se), funcParam(funcParam), funcParams(funcParams) {};
    void output(int level);  
    ExprNode* getParam() {return funcParam;};
    ExprNode* getNext() {return this->funcParams;};
    void typeCheck();
    void genCode();
};

class FuncParam : public ExprNode
{
private:
    Id* id;
    ExprNode* expr;
public:
    FuncParam(SymbolEntry* se, Id* id,ExprNode* expr) : ExprNode(se), id(id), expr(expr) {};
    Id* getId() {return id;};
    void output(int level);
    void typeCheck();
    void genCode();
};

class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class IfElseStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
    StmtNode *elseStmt;
public:
    IfElseStmt(ExprNode *cond, StmtNode *thenStmt, StmtNode *elseStmt) : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;
public:
    ReturnStmt(ExprNode*retValue) : retValue(retValue) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class BreakStmt : public StmtNode
{
    private:
    StmtNode * whileStmt;
    public:
    BreakStmt(StmtNode* whileStmt){this->whileStmt=whileStmt;};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ContinueStmt : public StmtNode
{
    private:
    StmtNode *whileStmt;
    public:
    ContinueStmt(StmtNode* whileStmt){this->whileStmt=whileStmt;};
    void output(int level);
    void typeCheck();
    void genCode();
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;
public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class WhileStmt : public StmtNode
{
private:
    ExprNode *cond;//条件
    StmtNode *Stmt;
    BasicBlock *cond_bb;
    BasicBlock *end_bb;
public:
   // WhileStmt(ExprNode *cond, StmtNode *Stmt) : cond(cond), Stmt(Stmt){};
     WhileStmt(ExprNode* cond, StmtNode* stmt=nullptr) : cond(cond), Stmt(stmt) 
    {};
    void setStmt(StmtNode* stmt){this->Stmt = stmt;};
    void output(int level);
    void typeCheck();
    void genCode();
    BasicBlock* get_cond_bb(){return this->cond_bb;};
    BasicBlock* get_end_bb(){return this->end_bb;};
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    //参数的定义
    FuncParams *param;
    StmtNode *stmt;
public:
    FunctionDef(SymbolEntry *se,FuncParams* param, StmtNode *stmt) : se(se), param(param),stmt(stmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class Ostream : public ExprNode
{
private:
    ExprNode * exp;
public:
    Ostream(SymbolEntry* se,ExprNode * exp):ExprNode(se),exp(exp){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class Istream : public ExprNode
{
public:
    Istream(SymbolEntry* se): ExprNode(se){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class Ast
{
private:
    Node* root;
public:
    Ast() {root = nullptr;}
    void setRoot(Node*n) {root = n;}
    void output();
    void typeCheck();
    void genCode(Unit *unit);
};

#endif
