#ifndef __INSTRUCTION_H__
#define __INSTRUCTION_H__

#include "Operand.h"
#include "AsmBuilder.h"
#include <vector>
#include <map>
#include <sstream>
using namespace std;
class BasicBlock;

class Instruction
{
public:
    Instruction(unsigned instType, BasicBlock *insert_bb = nullptr);
    virtual ~Instruction();
    BasicBlock *getParent();
    bool isUncond() const {return instType == UNCOND;};
    bool isCond() const {return instType == COND;};
    bool isAlloc() const {return instType == ALLOCA;};
    bool isReturn() const {return instType == RET;};
    MachineOperand* genMachineOperand(Operand*);
    MachineOperand* genMachineReg(int reg);
    MachineOperand* genMachineVReg();
    MachineOperand* genMachineImm(int val);
    MachineOperand* genMachineLabel(int block_no);
    void setParent(BasicBlock *);
    void setNext(Instruction *);
    void setPrev(Instruction *);
    Instruction *getNext();
    Instruction *getPrev();
    virtual void output() const = 0;
    
    virtual void genMachineCode(AsmBuilder*) = 0;
protected:
    unsigned instType;
    unsigned opcode;
    Instruction *prev;
    Instruction *next;
    BasicBlock *parent;
    std::vector<Operand*> operands;
    enum {BINARY, COND, UNCOND, RET, LOAD, STORE, CMP, ALLOCA,GLOBALDEF,GLOBALDECL,UNARY, FUNCCALL,ZEXT,XOR,ARRAY};
};

// meaningless instruction, used as the head node of the instruction list.
class DummyInstruction : public Instruction
{
public:
    DummyInstruction() : Instruction(-1, nullptr) {};
    void output() const {};
    void genMachineCode(AsmBuilder*) {};
};

class AllocaInstruction : public Instruction
{
public:
    AllocaInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb = nullptr);
    ~AllocaInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
private:
    SymbolEntry *se;
};

class LoadInstruction : public Instruction
{
public:
    LoadInstruction(Operand *dst, Operand *src_addr, BasicBlock *insert_bb = nullptr);
    ~LoadInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
};

class StoreInstruction : public Instruction
{
public:
    StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb = nullptr,int num=-1);
    ~StoreInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    int paramnum;
};

class GlobalDefInstruction : public Instruction
{
public:
    GlobalDefInstruction(Operand *dst_addr, Operand *src,bool isConst,BasicBlock *insert_bb = nullptr,int _value=0);
    ~GlobalDefInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    bool isConst;
    int value;
};

class GlobalDeclInstruction : public Instruction
{
public:
    GlobalDeclInstruction(Operand *dst_addr, BasicBlock *insert_bb = nullptr);
    ~GlobalDeclInstruction();
    void genMachineCode(AsmBuilder*);
    void output() const;
};

class BinaryInstruction : public Instruction
{
public:
    BinaryInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb = nullptr);
    ~BinaryInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum {ADD, DIV, MUL,SREM, SUB, AND, OR, LESS, MORE, LESSQ,MOREQ,EQ, NOTEQ};
};

class UnaryInstruction : public Instruction
{
public:
    UnaryInstruction(unsigned opcode, Operand *dst, Operand *src, BasicBlock *insert_bb = nullptr);
    ~UnaryInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum {ADD, SUB,NOT};
};

class CmpInstruction : public Instruction
{
public:
    CmpInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb = nullptr);
    ~CmpInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum {E, NE, L, LE,G, GE};
};

// unconditional branch
class UncondBrInstruction : public Instruction
{
public:
    UncondBrInstruction(BasicBlock*, BasicBlock *insert_bb = nullptr);
    void output() const;
    void setBranch(BasicBlock *);
    BasicBlock *getBranch();
    void genMachineCode(AsmBuilder*);
protected:
    BasicBlock *branch;
};

// conditional branch
class CondBrInstruction : public Instruction
{
public:
    CondBrInstruction(BasicBlock*, BasicBlock*, Operand *, BasicBlock *insert_bb = nullptr);
    ~CondBrInstruction();
    void output() const;
    void setTrueBranch(BasicBlock*);
    BasicBlock* getTrueBranch();
    void setFalseBranch(BasicBlock*);
    BasicBlock* getFalseBranch();
    void genMachineCode(AsmBuilder*);
    void setOp(int op){opcode=op;};
    enum {E, NE, L, GE, G, LE};
protected:
    BasicBlock* true_branch;
    BasicBlock* false_branch;
};

class RetInstruction : public Instruction
{
public:
    RetInstruction(Operand *src, BasicBlock *insert_bb = nullptr);
    ~RetInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
};

class FuncCallInstruction : public Instruction
{
public:
    FuncCallInstruction(Operand* dst,vector<Operand *> params, SymbolEntry *se, BasicBlock *insert_bb = nullptr);
    void output() const;
    void genMachineCode(AsmBuilder*);
    ~FuncCallInstruction();
private:
    SymbolEntry *se;
    Operand*dst;
};

class XorInstruction : public Instruction {
public:
    XorInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb = nullptr);
    void genMachineCode(AsmBuilder*);
    void output() const;
};

class ZextInstruction: public Instruction {
public:
    ZextInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb = nullptr);
    void genMachineCode(AsmBuilder*);
    void output() const;
};

//------------------数组添加------------------

class ArrayInstruction: public Instruction {
private:
    bool isParam;
    bool first;
    bool last;
    Operand* init;
public:
    ArrayInstruction(Operand* dst, Operand* arr, Operand* idx, BasicBlock* insert_bb = nullptr, bool isParam = false,bool first = false);
    ~ArrayInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    void setFirst() {first = true;};
    void setLast() {last = true;};
    Operand* getInit() const {return init;};
    void setInit(Operand* init) {this->init = init;};
};







#endif
