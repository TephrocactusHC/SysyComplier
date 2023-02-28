#include "MachineCode.h"
#include "Type.h"
extern FILE* yyout;
int MachineBlock::label = 0;
//关于本部分，我个人认为，最容易的办法是自己用编译器生成一些汇编代码，然后对着看
//那样才能看懂点这玩意在干什么。。。
//出人意料的是，我目前认为加入数组的工作也许困难在于parser之中
//一旦数组已经被处理成ast的节点，应该就会顺利
//目前lab6-7的代码，没有看到特别的针对数组的
//也许可以对着BUAA的指导书看一下应该生成什么样的中间代码和汇编代码
MachineOperand::MachineOperand(int tp, int val)
{
    this->type = tp;
    if(tp == MachineOperand::IMM)
        this->val = val;
    else 
        this->reg_no = val;
}
MachineOperand::MachineOperand(int tp, int val,SymbolEntry *se1)
{
    this->type = tp;
    if(tp == MachineOperand::IMM)
        this->val = val;
    else 
        this->reg_no = val;
    this->mse=se1;
}

MachineOperand::MachineOperand(std::string label)
{
    this->type = MachineOperand::LABEL;
    this->label = label;
}

bool MachineOperand::operator==(const MachineOperand&a) const
{
    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

bool MachineOperand::operator<(const MachineOperand&a) const
{
    if(this->type == a.type)
    {
        if(this->type == IMM)
            return this->val < a.val;
        return this->reg_no < a.reg_no;
    }
    return this->type < a.type;

    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

void MachineOperand::PrintReg()
{
    switch (reg_no)
    {
    case 11:
        fprintf(yyout, "fp");
        break;
    case 13:
        fprintf(yyout, "sp");
        break;
    case 14:
        fprintf(yyout, "lr");
        break;
    case 15:
        fprintf(yyout, "pc");
        break;
    default:
        fprintf(yyout, "r%d", reg_no);
        break;
    }
}

void MachineOperand::output() 
{
    /* HINT：print operand
    * Example:
    * immediate num 1 -> print #1;
    * register 1 -> print r1;
    * lable addr_a -> print addr_a; */
    switch (this->type)
    {
    case IMM:
        fprintf(yyout, "#%d", this->val);
        break;
    case VREG:
        fprintf(yyout, "v%d", this->reg_no);
        break;
    case REG:
        PrintReg();
        break;
    case LABEL:
        if (this->label.substr(0, 2) == ".L")
            fprintf(yyout, "%s", this->getName().c_str());
        else if (this->label.substr(0, 1) == "@"&&this->IsFunc())
            fprintf(yyout, "%s", this->getLabel().c_str() + 1);
        else
            fprintf(yyout, "addr_%s%d", this->getName().c_str(),parent->getParent()->getParent()->getParent()->getN());
    default:
        break;
    }
}
//这块时浩铭写了一大堆，好像学姐是只处理了==和<的情况，她应该是简化了机器码的内容
//这块在考虑要不要加上，效率低能在部分程度规避查重
void MachineInstruction::PrintCond()
{
    // TODO
   switch (cond) {
        case EQ:
            fprintf(yyout, "eq");
            break;
        case NE:
            fprintf(yyout, "ne");
            break;
        case LT:
            fprintf(yyout, "lt");
            break;
        case LE:
            fprintf(yyout, "le");
            break;
        case GT:
            fprintf(yyout, "gt");
            break;
        case GE:
            fprintf(yyout, "ge");
            break;
        default:
            break;
    }
}

BinaryMInstruction::BinaryMInstruction(
    MachineBlock* p, int op, 
    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2, 
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::BINARY;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    src2->setParent(this);
}

void BinaryMInstruction::output() 
{
    // TODO: 
    // Complete other instructions
	// 这里面PrintCond似乎真的用不到
    switch (this->op)
    {
    case BinaryMInstruction::AND:
            fprintf(yyout, "\tand ");
            this->PrintCond();
            this->def_list[0]->output();
            fprintf(yyout, ", ");
            this->use_list[0]->output();
            fprintf(yyout, ", ");
            this->use_list[1]->output();
            fprintf(yyout, "\n");
            break;
        case BinaryMInstruction::OR:
            fprintf(yyout, "\torr ");
            this->PrintCond();
            this->def_list[0]->output();
            fprintf(yyout, ", ");
            this->use_list[0]->output();
            fprintf(yyout, ", ");
            this->use_list[1]->output();
            fprintf(yyout, "\n");
            break;
    case BinaryMInstruction::ADD:
        fprintf(yyout, "\tadd ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::SUB:
        fprintf(yyout, "\tsub ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::MUL:
        fprintf(yyout, "\tmul ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::DIV:
        fprintf(yyout, "\tsdiv ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::SREM:
        // no mod instruction
        break;
    default:
        break;
    }
}
//不知道为什么会有这玩意
UxtbMInstruction::UxtbMInstruction(MachineBlock* p,MachineOperand* dst, MachineOperand* src, int cond)
{
    this->parent=p;
    this->type=MachineInstruction::UXTB;
    this->op=-1;
    this->cond=cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}
//与上面同理，UXTB指令在功能上应该最后和ldr是类似的，都是取32位到寄存器
//好像根本没用到
void UxtbMInstruction::output()
{
    fprintf(yyout, "\tuxtb ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");

}
//也是时浩铭代码中没有的逻辑异或指令
EorMInstruction::EorMInstruction(MachineBlock* p,MachineOperand* dst, MachineOperand* src, int cond)
{
    this->parent=p;
    this->type=MachineInstruction::EOR;
    this->op=-1;
    this->cond=cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}
//迷惑行为，她这是实现啥了，不会是sysy没有的功能吧，惊了
//也根本没用到后续可以删掉
void EorMInstruction::output()
{
    fprintf(yyout, "\teor ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");

}

LoadMInstruction::LoadMInstruction(MachineBlock* p,
    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2,
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::LOAD;
    this->op = -1;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    if (src2)
        this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    if (src2)
        src2->setParent(this);
}

void LoadMInstruction::output()
{
    fprintf(yyout, "\tldr ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: ldr r1, =8
    if(this->use_list[0]->isImm())
    {
        fprintf(yyout, "=%d\n", this->use_list[0]->getVal());
        return;
    }

    // Load address
    if(this->use_list[0]->isReg()||this->use_list[0]->isVReg())
        fprintf(yyout, "[");

    this->use_list[0]->output();
    if( this->use_list.size() > 1 )
    {
        fprintf(yyout, ", ");
        this->use_list[1]->output();
    }

    if(this->use_list[0]->isReg()||this->use_list[0]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

StoreMInstruction::StoreMInstruction(MachineBlock* p,
    MachineOperand* dst,
    MachineOperand* src1, 
    MachineOperand* src2, 
    int cond)
{
    // TODO
    this->parent = p;
    this->type = MachineInstruction::STORE;
    this->op = -1;
    this->cond = cond;
    this->use_list.push_back(dst);
    this->use_list.push_back(src1);
    if (src2)
        this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    if (src2)
        src2->setParent(this);
}

void StoreMInstruction::output()
{
    // TODO
    fprintf(yyout, "\tstr ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: str r1, =8
    //好像这一小段没用
    if(this->use_list[1]->isImm())
    {
        fprintf(yyout, "=%d\n", this->use_list[1]->getVal());
        return;
    }

    // Load address
    if(this->use_list[1]->isReg()||this->use_list[1]->isVReg())
        fprintf(yyout, "[");

    this->use_list[1]->output();
    if( this->use_list.size() > 1 )
    {
        if(use_list[2]){
            fprintf(yyout, ", ");
            this->use_list[2]->output();
        }
    }

    if(this->use_list[1]->isReg()||this->use_list[1]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

MovMInstruction::MovMInstruction(MachineBlock* p, int op, 
    MachineOperand* dst, MachineOperand* src,
    int cond)
{
    // TODO
    this->parent=p;
    this->type = MachineInstruction::MOV;
    this->op=op;
    this->cond=cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}

void MovMInstruction::output() 
{
    // TODO
   /* switch (op)
    {
   case MOV:
        fprintf(yyout,"\tmov ");
        break;
    //下面这两种情况似乎都没有用到
    case MVN:
        fprintf(yyout,"\tmvn ");
        break;
    case MOVS:
        fprintf(yyout,"\tmovs ");
        break;
    default:
        break;
    }
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
    */
    fprintf(yyout, "\tmov");
    PrintCond();
    fprintf(yyout, " ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

BranchMInstruction::BranchMInstruction(MachineBlock* p, int op, 
    MachineOperand* dst, 
    int cond)
{
    // TODO
    this->parent=p;
    this->op=op;
    this->cond=cond;
    this->type=MachineInstruction::BRANCH;
    this->def_list.push_back(dst);
    dst->setParent(this);
}
//居然还比时浩铭多了几种汇编指令
void BranchMInstruction::output()
{
    // TODO
    switch (op)
    {
    case BX:
        fprintf(yyout,"\tbx ");
        break;
    case B:
        fprintf(yyout,"\tb .");
        break;
    case BL:
        fprintf(yyout,"\tbl ");
        break;
    case BEQ:
        fprintf(yyout,"\tbeq .");
        break;
    case BNE:
        fprintf(yyout,"\tbne .");
        break;
    case BGE:
        fprintf(yyout,"\tbge .");
        break;
    case BGT:
        fprintf(yyout,"\tbgt .");
        break;
    case BLT:
        fprintf(yyout,"\tblt .");
        break;
    case BLE:
        fprintf(yyout,"\tble .");
        break;
    default:
        break;
    }
    //fprintf(yyout, ".");
    this->def_list[0]->output();
    fprintf(yyout, "\n");
}
//与时浩铭不一致
CmpMInstruction::CmpMInstruction(MachineBlock* p, 
    MachineOperand* src1, MachineOperand* src2, 
    int cond)
{
    // TODO
    this->parent=p;
    this->type = MachineInstruction::CMP;
    this->op=-1;
    this->cond=cond;
    p->setCmpCond(cond);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    src1->setParent(this);
    src2->setParent(this);
}

void CmpMInstruction::output()
{
    // TODO
    // Jsut for reg alloca test
    // delete it after test
    fprintf(yyout, "\tcmp ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

//srcs:callee savedRegs
StackMInstrcuton::StackMInstrcuton(MachineBlock* p, int op, std::vector<MachineOperand*> srcs,
    MachineOperand* fpSrc,MachineOperand* lrSrc,
    int cond)
{
    // TODO
    this->parent=p;
    this->type = MachineInstruction::STACK;
    this->op=op;
    this->cond=cond;
    if(srcs.size())
    {
            for(auto it = srcs.begin(); it != srcs.end(); it++)  
            {
                this->use_list.push_back(*it);
                // (*it)->setParent(this);
            }
    }
    this->use_list.push_back(fpSrc);
    fpSrc->setParent(this);
    //如果当前函数调用了其他函数才需要保存lr
    if(lrSrc)
    {
        this->use_list.push_back(lrSrc);
        lrSrc->setParent(this);
    }
}

void StackMInstrcuton::output()
{
    // TODO
    switch (op)
    {
    case POP:
        fprintf(yyout, "\tpop {");
        break;
    case PUSH:
        fprintf(yyout, "\tpush {");
        break;
    default:
        break;
    }
    this->use_list[0]->output();
    for (long unsigned int i = 1; i < use_list.size(); i++) {
        fprintf(yyout, ", ");
        this->use_list[i]->output();
    }
    fprintf(yyout, "}\n");
}

MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr) 
{ 
    this->parent = p; 
    this->sym_ptr = sym_ptr; 
    this->stack_size = 0;
    this->paramsNum=((FunctionType*)(sym_ptr->getType()))->getParamSe().size();
    
};

//???
std::vector<MachineOperand*> MachineFunction::getSavedRegs() {
    std::vector<MachineOperand*> regs;
    for (auto it = saved_regs.begin(); it != saved_regs.end(); it++) {
        auto reg = new MachineOperand(MachineOperand::REG, *it);
        regs.push_back(reg);
    }
    return regs;
}

//此处与时浩铭的代码有巨大差别
//shm加了很多东西？
void MachineBlock::output()
{
    
    int count = 0;
    fprintf(yyout, ".L%d:\n", this->no);
    bool isfirst=true;
    //目前还avalible的寄存器
    int offset=(parent->getSavedRegs().size() + 2) * 4;
    int num = parent->getParamsNum();
    for(auto iter : inst_list)
    {
        //在bx前加pop指令
        if(iter->isBX())
        {
            auto fp = new MachineOperand(MachineOperand::REG, 11);
            auto lr = new MachineOperand(MachineOperand::REG, 14);
            //将之前push保存的信息pop恢复
            auto cur_inst =new StackMInstrcuton(this, StackMInstrcuton::POP,parent->getSavedRegs(), fp, lr);
            cur_inst->output();
        }
        //新添加
        iter->output();
        count++;
        if (count % 500 == 0) {
                fprintf(yyout, "\tb .B%d\n", label);
                fprintf(yyout, ".LTORG\n");
                parent->getParent()->PrintBridge();
                fprintf(yyout, ".B%d:\n", label++);
            }
    }
    




}


void MachineFunction::output()
{
    const char *func_name = this->sym_ptr->toStr().c_str() + 1;
    fprintf(yyout, "\t.global %s\n", func_name);
    fprintf(yyout, "\t.type %s , %%function\n", func_name);
    fprintf(yyout, "%s:\n", func_name);
    
    // TODO
    /* Hint:
    *  1. Save fp
    *  2. fp = sp
    *  3. Save callee saved register
    *  4. Allocate stack space for local variable */

    //在刚进入一个新的函数开始执行的时候，它们保存的是上个函数的信息，需要将它们入栈保存起来
    auto fp = new MachineOperand(MachineOperand::REG, 11);
    auto sp = new MachineOperand(MachineOperand::REG, 13);
    auto lr = new MachineOperand(MachineOperand::REG, 14);
    (new StackMInstrcuton(nullptr, StackMInstrcuton::PUSH, getSavedRegs(), fp,lr))->output();
    (new MovMInstruction(nullptr, MovMInstruction::MOV, fp, sp))->output();
    

    //之后需要生成 SUB 指令为局部变量分配栈内空间,此时已经知道实际的栈内空间大小
    int off = AllocSpace(0);
    auto size = new MachineOperand(MachineOperand::IMM, off);
     if (off < -255 || off > 255) {
        auto r4 = new MachineOperand(MachineOperand::REG, 4);
        (new LoadMInstruction(nullptr, r4, size))->output();
        (new BinaryMInstruction(nullptr, BinaryMInstruction::SUB, sp, sp, r4))
            ->output();
    } else {
    (new BinaryMInstruction(nullptr, BinaryMInstruction::SUB, sp, sp, size))->output();
    }
    int count = 0;
    for (auto iter : block_list) {
        if(iter->empty()){
            continue;
        }
        iter->output();
        count += iter->getSize();
        if(count > 160){
            fprintf(yyout, "\tb .F%d\n", parent->getN());
            fprintf(yyout, ".LTORG\n");
            parent->PrintBridge();
            fprintf(yyout, ".F%d:\n", parent->getN()-1);
            count = 0;
        }
    }
    fprintf(yyout, "\n");
}
//这TM是啥，又是一个框架里没有的新类是吗。。。
//也许可以像时浩铭一样合到那个MachineUnit节点里
//这个貌似可以不改，看了看这个全局变量声明还是啥玩意，实现和时浩铭那个差不多
//不过查重是按照汇编代码查重，在cpp里重写类不一定有用啊，生成的汇编代码是一样的
//此处要再考虑
GlobalMInstruction::GlobalMInstruction(MachineBlock* p,MachineOperand* dst, MachineOperand* src, int cond,int _value)
{
    this->parent=p;
    def_list.push_back(dst);
    use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
    this->value=_value;
}

void GlobalMInstruction::outputAddr(int n)
{
    fprintf(yyout, "addr_%s%d:\n", def_list[0]->getName().c_str(),n);
    fprintf(yyout, "\t.word %s\n",def_list[0]->getName().c_str());
}

void GlobalMInstruction::output()
{
    use_list[0]->setVal(this->value);
    fprintf(yyout,"\t.global %s\n",def_list[0]->getName().c_str());
    fprintf(yyout,"\t.align 4\n");
    //这里都默认是int型了
    fprintf(yyout,"\t.size %s, 4\n",def_list[0]->getName().c_str());
    fprintf(yyout,"%s:\n",def_list[0]->getName().c_str());
    fprintf(yyout,"\t.word %d\n",use_list[0]->getVal());
}

//shm这里可能涉及到了数组的东西
void MachineUnit::PrintGlobalDecl()
{
    // TODO:
    // You need to print global variable/const declarition code;
    if(!global_inst.empty())
    {
        fprintf(yyout, "\t.data\n");
    }
    else{
    if (!global_list.empty())
        fprintf(yyout, "\t.data\n");
    }
    //放后面
    for( auto it= global_inst.begin();it!=global_inst.end();it++)
    {
         (*it)->output();
    }
     
    //加一些东西
    //把数组的分出来换一种方法
    std::vector<int> consts;
    std::vector<int> zeroIdx;
    for (long unsigned int i = 0; i < global_list.size(); i++) {
    IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)global_list[i];
        if (se->getConst()) {
           // consts.push_back(i);
        } else if (se->isAllZero()) {
            zeroIdx.push_back(i);
        }
        else {
         //是数组的，才在这个代码块里运行
         if (se->getType()->isArray()) {   
            std::string name = se->toStr().c_str();;
            name = name.substr(1,name.size());
            fprintf(yyout, "\t.global %s\n", /*se->toStr().c_str()*/name.c_str());
            fprintf(yyout, "\t.align 4\n");
            fprintf(yyout, "\t.size %s, %d\n", se->toStr().c_str(), se->getType()->getSize() / 8);
            fprintf(yyout, "%s:\n", se->toStr().c_str());
            int n = se->getType()->getSize() / 32;
            int *p = se->getArrayValue();
            for (int i = 0; i < n; i++)
            fprintf(yyout, "\t.word %d\n", p[i]);
        }
        else{

            //不是数组，不在这里面输出

        }



     }



    }

    if (!zeroIdx.empty()) {
        for (auto i : zeroIdx) {
            IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)global_list[i];
            if (se->getType()->isArray()) {
                std::string name = se->toStr();
                name = name.substr(1,name.length());
                fprintf(yyout, "\t.comm %s, %d, 4\n", /*se->toStr().c_str()*/name.c_str(),
                        se->getType()->getSize() / 8);
            }
        }
    }
    // fprintf(yyout,"output head");
}

void MachineUnit::PrintBridge()
{
    for (auto s : global_list) {
        IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)s;
        if (se->getType()->isArray()) { 
        std::string name = se->toStr();
        name = name.substr(1,name.length());
        fprintf(yyout, "addr_%s%d:\n", /*se->toStr().c_str()*/name.c_str(),n);
        fprintf(yyout, "\t.word %s\n", name.c_str());}
    }
    
    for( auto it= global_inst.begin();it!=global_inst.end();it++)
    {
        dynamic_cast<GlobalMInstruction* >(*it)->outputAddr(n);
    }
    n++;
}

void MachineUnit::output()
{
    // TODO
    /* Hint:
    * 1. You need to print global variable/const declarition code;
    * 2. Traverse all the function in func_list to print assembly code;
    * 3. Don't forget print bridge label at the end of assembly code!! */
    fprintf(yyout, "\t.arch armv8-a\n");
    fprintf(yyout, "\t.arch_extension crc\n");
    fprintf(yyout, "\t.arm\n");
    PrintGlobalDecl();
    fprintf(yyout, "\t.text\n");
    for(auto iter : func_list)
        iter->output();
    PrintBridge();
}
void MachineInstruction::insertBefore(MachineInstruction* inst) {
    auto& instructions = parent->getInsts();
    auto it = std::find(instructions.begin(), instructions.end(), this);
    instructions.insert(it, inst);
}

void MachineInstruction::insertAfter(MachineInstruction* inst) {
    auto& instructions = parent->getInsts();
    auto it = std::find(instructions.begin(), instructions.end(), this);
    instructions.insert(++it, inst);
}