#ifndef __MACHINECODE_H__
#define __MACHINECODE_H__
#include <vector>
#include <set>
#include <string>
#include <algorithm>
#include <fstream>
#include "SymbolTable.h"

/* Hint:
* MachineUnit: Compiler unit
* MachineFunction: Function in assembly code 
* MachineInstruction: Single assembly instruction  
* MachineOperand: Operand in assembly instruction, such as immediate number, register, address label */

/* Todo:
* We only give the example code of "class BinaryMInstruction" and "class AccessMInstruction" (because we believe in you !!!),
* You need to complete other the member function, especially "output()" ,
* After that, you can use "output()" to print assembly code . */

class MachineUnit;
class MachineFunction;
class MachineBlock;
class MachineInstruction;

class MachineOperand
{
private:
    MachineInstruction* parent;
    int type;
    int val;  // value of immediate number
    int reg_no; // register no
    std::string label; // address label
    bool isFunc=false;
    //添加一个符号表项
    SymbolEntry *mse;
public:
	//立即数，虚拟寄存器，物理寄存器，地址标签
    enum { IMM, VREG, REG, LABEL };
    MachineOperand(int tp, int val,SymbolEntry *se1);
    MachineOperand(int tp, int val);
    MachineOperand(std::string label);
    bool operator == (const MachineOperand&) const;
    bool operator < (const MachineOperand&) const;
    bool isImm() { return this->type == IMM; }; 
    bool isReg() { return this->type == REG; };
    bool isVReg() { return this->type == VREG; };
    bool isLabel() { return this->type == LABEL; };
    int getVal() {return this->val; };
    void setVal(int val) { this->val = val; };
    int getReg() {return this->reg_no; };
    void setReg(int regno) {this->type = REG; this->reg_no = regno;};
    std::string getLabel() {return this->label; };
    std::string getName() {return this->label.substr(1,label.size());};
    void setParent(MachineInstruction* p) { this->parent = p; };
    MachineInstruction* getParent() { return this->parent;};
    void PrintReg();
    void output();
    bool IsFunc(){return this->isFunc;};
    void setFunc(bool b){this->isFunc=b;};
};

class MachineInstruction
{
protected:
    MachineBlock* parent;
    int no;
    int type;  // Instruction type
    int cond = MachineInstruction::NONE;  // Instruction execution condition, optional !!
    int op;  // Instruction opcode
    // Instruction operand list, sorted by appearance order in assembly instruction
    std::vector<MachineOperand*> def_list;
    std::vector<MachineOperand*> use_list;
    void addDef(MachineOperand* ope) { def_list.push_back(ope); };
    void addUse(MachineOperand* ope) { use_list.push_back(ope); };
    // Print execution code after printing opcode
    void PrintCond();
    enum instType { BINARY, LOAD, STORE, MOV, BRANCH, CMP, STACK,GLOBAL,UXTB,EOR };
public:
    enum condType { EQ, NE, LT, LE , GT, GE, NONE };
    virtual void output() = 0;
    void setNo(int no) {this->no = no;};
    int getNo() {return no;};
    std::vector<MachineOperand*>& getDef() {return def_list;};
    std::vector<MachineOperand*>& getUse() {return use_list;};
    MachineBlock* getParent() { return this->parent;};
    bool isBX() const { return type == BRANCH && op == 2; };
    bool isStore() const { return type == STORE; };
    bool isAdd() const { return type == BINARY && op == 0; };
    void insertBefore(MachineInstruction*);
    void insertAfter(MachineInstruction*);
};

class BinaryMInstruction : public MachineInstruction
{
public:
    enum opType {ADD, DIV, MUL,SREM, SUB, AND, OR};
    BinaryMInstruction(MachineBlock* p, int op, 
                    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2, 
                    int cond = MachineInstruction::NONE);
    void output();
};


class LoadMInstruction : public MachineInstruction
{
public:
    LoadMInstruction(MachineBlock* p,
                    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2 = nullptr, 
                    int cond = MachineInstruction::NONE);
    void output();
};

class StoreMInstruction : public MachineInstruction
{
public:
    StoreMInstruction(MachineBlock* p,
                    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2 = nullptr, 
                    int cond = MachineInstruction::NONE);
    void output();
};

class MovMInstruction : public MachineInstruction
{
public:
    enum opType { MOV, MVN ,MOVS};
    MovMInstruction(MachineBlock* p, int op, 
                MachineOperand* dst, MachineOperand* src,
                int cond = MachineInstruction::NONE);
    void output();
};

class BranchMInstruction : public MachineInstruction
{
public:
    enum opType { B, BL, BX ,BEQ,BNE,BLT,BLE,BGT,BGE};
    BranchMInstruction(MachineBlock* p, int op, 
                MachineOperand* dst, 
                int cond = MachineInstruction::NONE);
    void output();
};

class CmpMInstruction : public MachineInstruction
{
public:
    enum opType { CMP };
    CmpMInstruction(MachineBlock* p, 
                MachineOperand* src1, MachineOperand* src2, 
                int cond = MachineInstruction::NONE);
    void output();
};

class StackMInstrcuton : public MachineInstruction {
   public:
    enum opType { PUSH, POP };
    StackMInstrcuton(MachineBlock* p,
                     int op,
                     std::vector<MachineOperand*> srcs,
                     MachineOperand* fpSrc,
                     MachineOperand* lrSrc = nullptr,
                     int cond = MachineInstruction::NONE);
    void output();
};

class GlobalMInstruction : public MachineInstruction
{
public:
    GlobalMInstruction(MachineBlock* p,MachineOperand* dst, MachineOperand* src, int cond= MachineInstruction::NONE,int value=0);
    void output();
    void outputAddr(int n);
    int value;//记录全局常变量声明的值
    //添加一个符号表项记录是数组还是int
    //可以由GlobalDefInstruction传递下来（目前设想的是这样）
    SymbolEntry *se;

};

class UxtbMInstruction : public MachineInstruction
{
public:
    UxtbMInstruction(MachineBlock* p,MachineOperand* dst, MachineOperand* src, int cond= MachineInstruction::NONE);
    void output();
};

class EorMInstruction : public MachineInstruction
{
public:
    EorMInstruction(MachineBlock* p,MachineOperand* dst, MachineOperand* src, int cond= MachineInstruction::NONE);
    void output();
};

class MachineBlock
{
private:
    MachineFunction* parent;
    int no;  
    std::vector<MachineBlock *> pred, succ;
    std::vector<MachineInstruction*> inst_list;
    std::set<MachineOperand*> live_in;
    std::set<MachineOperand*> live_out;
    int cmpCond;
    static int label;
    
public:
    std::vector<MachineInstruction*>& getInsts() {return inst_list;};
    std::vector<MachineInstruction*>::iterator begin() { return inst_list.begin(); };
    std::vector<MachineInstruction*>::iterator end() { return inst_list.end(); };
    MachineBlock(MachineFunction* p, int no) { this->parent = p; this->no = no; };
    void InsertInst(MachineInstruction* inst) { this->inst_list.push_back(inst); };
    void addPred(MachineBlock* p) { this->pred.push_back(p); };
    void addSucc(MachineBlock* s) { this->succ.push_back(s); };
    std::set<MachineOperand*>& getLiveIn() {return live_in;};
    std::set<MachineOperand*>& getLiveOut() {return live_out;};
    std::vector<MachineBlock*>& getPreds() {return pred;};
    std::vector<MachineBlock*>& getSuccs() {return succ;};
    bool empty() {return inst_list.size()==0;};
    int getSize() const { return inst_list.size(); };
    MachineFunction* getParent() const { return parent; };
    void output();
    int getCmpCond() const { return cmpCond; };
    void setCmpCond(int cond) { cmpCond = cond; };
};

class MachineFunction
{
private:
    MachineUnit* parent;
    std::vector<MachineBlock*> block_list;
    int stack_size;
    std::set<int> saved_regs;
    SymbolEntry* sym_ptr;
    bool leaf=true;
    int paramsNum;
public:
    std::vector<MachineBlock*>& getBlocks() {return block_list;};
    std::vector<MachineBlock*>::iterator begin() { return block_list.begin(); };
    std::vector<MachineBlock*>::iterator end() { return block_list.end(); };
    MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr);
    /* HINT:
    * Alloc stack space for local variable;
    * return current frame offset ;
    * we store offset in symbol entry of this variable in function AllocInstruction::genMachineCode()
    * you can use this function in LinearScan::genSpillCode() */
    int AllocSpace(int size) { this->stack_size += size; return this->stack_size; };
    void InsertBlock(MachineBlock* block) { this->block_list.push_back(block); };
    void addSavedRegs(int regno) {saved_regs.insert(regno);};
    void output();
    void setNotLeaf(){leaf=false;};
    bool isLeaf(){return leaf;};//叶函数，
    int getParamsNum(){return paramsNum;};
    std::vector<MachineOperand*> getSavedRegs();//这玩意时浩铭也没写。
	//应该是最后还得加上非叶函数的东西
    MachineUnit* getParent() const { return parent; };
};

class MachineUnit
{
private:
    std::vector<MachineFunction*> func_list;
    std::vector<MachineInstruction*> global_inst;
    std::vector<SymbolEntry*> global_list;
    
    int n;
public:
    std::vector<MachineFunction*>& getFuncs() {return func_list;};
    std::vector<MachineFunction*>::iterator begin() { return func_list.begin(); };
    std::vector<MachineFunction*>::iterator end() { return func_list.end(); };
    void InsertFunc(MachineFunction* func) { func_list.push_back(func);};
    void output();
    void InsertGlobal(MachineInstruction* inst) { global_inst.push_back(inst);};
    void insertGlobal(SymbolEntry* se) {global_list.push_back(se);}
    int getN() const { return n; };
    void PrintGlobalDecl();
    void PrintBridge();
};

#endif