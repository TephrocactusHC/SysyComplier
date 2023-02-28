#include "Unit.h"
#include "SymbolTable.h"
#include "Type.h"
#include <stack>
extern FILE* yyout;
void Unit::insertFunc(Function *f)
{
    func_list.push_back(f);
}

void Unit::removeFunc(Function *func)
{
    func_list.erase(std::find(func_list.begin(), func_list.end(), func));
}

void Unit::output() const
{
    globalBB->output();
    for (auto &func : func_list)
        func->output();
    //库函数
    for (auto it=sysy_list.begin();it!=sysy_list.end();it++){
        FunctionType* type=dynamic_cast<FunctionType*>((*it)->getType());
        string str=type->toStr();
        string name=dynamic_cast<IdentifierSymbolEntry*>(*it)->getName();
        fprintf(yyout,"declare %s @%s(",type->getRetType()->toStr().c_str(),name.c_str());
        if(type->getParamNum()!=0){
            fprintf(yyout,"i32");
        }
        fprintf(yyout,")\n");
    }
    for (auto se : global_list) {
        if(se->getType()->isArray()){
        //fprintf(yyout, "%s = global %s %d, align 4\n", se->toStr().c_str(), se->getType()->toStr().c_str(), ((IdentifierSymbolEntry*)se)->getValue());
        //按shm的改
        ArrayType* type = (ArrayType*)(se->getType());
            // int size = type->getSize() / TypeSystem::intType->getSize();
            int* val = ((IdentifierSymbolEntry*)se)->getArrayValue();
            int i = 0;
            fprintf(yyout, "%s = global ", se->toStr().c_str());
            if (((IdentifierSymbolEntry*)se)->isAllZero()) {
                fprintf(yyout, "%s zeroinitializer", type->toStr().c_str());
            } else {
                std::stack<ArrayType*> stk;
                std::stack<int> stk1;
                stk.push(type);
                stk1.push(0);
                ArrayType* temp;
                while (!stk.empty()) {
                    temp = stk.top();
                    if (temp->getElementType()->isInt()) {
                        fprintf(yyout, "%s [", temp->toStr().c_str());
                        for (int j = 0; j < temp->getLength(); j++) {
                            if (j != 0)
                                fprintf(yyout, ", ");
                            fprintf(yyout, "i32 %d", val[i++]);
                        }
                        fprintf(yyout, "]");
                        stk1.pop();
                        stk.pop();
                        if (stk.empty())
                            break;
                        stk1.top()++;
                        continue;
                    }
                    if (stk1.top() != temp->getLength()) {
                        stk.push((ArrayType*)(temp->getElementType()));
                        if (stk1.top() == 0)
                            fprintf(yyout, "%s [", temp->toStr().c_str());
                        if (stk1.top() != 0)
                            fprintf(yyout, ", ");
                        stk1.push(0);
                    } else {
                        fprintf(yyout, "]");
                        stk.pop();
                        stk1.pop();
                        if (stk.empty())
                            break;
                        stk1.top()++;
                        continue;
                    }
                }
            }
            fprintf(yyout, ", align 4\n");
        }
    
    
    
    
    }
    
}

void Unit::insertDecl(SymbolEntry* se){
    auto it = find(sysy_list.begin(),sysy_list.end(),se);
    if(it==sysy_list.end()){
        sysy_list.push_back(se);
    }
}

void Unit::genMachineCode(MachineUnit* munit) 
{
    AsmBuilder* builder = new AsmBuilder();
    builder->setUnit(munit);
    globalBB->genMachineCode(builder);
    for (auto &func : func_list)
        func->genMachineCode(builder);
}

Unit::~Unit()
{
    for(auto &func:func_list)
        delete func;
}

void Unit::insertGlobal(SymbolEntry* se) {
    global_list.push_back(se);
}
