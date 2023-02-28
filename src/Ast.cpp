#include "Ast.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#include <string>
#include "Type.h"
extern Unit unit;
extern MachineUnit mUnit;
extern FILE *yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;

bool firstNotUnary=0;
bool firstUnary=1;
int paramnum=0;

//用于隐式转换
ConstantSymbolEntry*zeroSe=new ConstantSymbolEntry(TypeSystem::intType,0);
ExprNode *expr0=new Constant((SymbolEntry*)zeroSe);//

Node::Node()
{
    seq = counter++;
}
//设置它们跳转到哪里
//将bb作为目标标号块插入到list所值列表的各指令中
void Node::backPatch(std::vector<Instruction*> &list, BasicBlock*bb, bool isTrue)
{
    for(auto &inst:list)
    {   //将指令所属基本快的后继节点设置为bb
        inst->getParent()->addSucc(bb);
        //bb的前驱基本块设置为inst所属的基本块
        bb->addPred(inst->getParent());
        //是条件跳转指令
        if(inst->isCond()){
            if(isTrue){
                dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
            }else if(dynamic_cast<CondBrInstruction *>(inst)->getFalseBranch() == nullptr){
                dynamic_cast<CondBrInstruction*>(inst)->setFalseBranch(bb);
            }
        }
        //无条件跳转指令
        else if(inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

//将list1和list2列表合并，返回指向合并后列表的指针
std::vector<Instruction*> Node::merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2)
{
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit *unit)
{
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void FunctionDef::genCode()
{
    // put params in vector
    vector<Operand *> *vec = new vector<Operand*>;
    ExprNode *temp = this->param;

    while(temp){
        ExprNode *tempParam = dynamic_cast<FuncParams*>(temp)->getParam();
        vec->push_back(dynamic_cast<FuncParam*>(tempParam)->getId()->getOperand());
        temp = dynamic_cast<FuncParams*>(temp)->getNext();
    }
    
    Unit *unit = builder->getUnit();
    Function *func = new Function(unit, se);
    func->setParams(vec);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);
    if(param!=NULL){
        param->genCode();
    }
    paramnum=0;
    stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
    */
   for(std::vector<BasicBlock *>::iterator it=func->begin();it!=func->end();it++)
    {
        BasicBlock*curBB=(*it);

        Instruction* i = (*it)->begin();
        Instruction*end=(*it)->rbegin();
        //从块中删除条件型语句
        while(i!=end){
            if(i->isCond()||i->isUncond()){
                (*it)->remove(i);
            }
            i= i->getNext();
        }

        if(end->isUncond())
        {
            /*
            BasicBlock*branch=dynamic_cast<UncondBrInstruction*>(end)->getBranch();
            curBB->addSucc(branch);
            branch->addPred(curBB);
            */
            //此处得益于三个开源代码，但是注释全是我丁哥写的！丁哥！爹！
            BasicBlock* dst =
                dynamic_cast<UncondBrInstruction*>(end)->getBranch();
            (*it)->addSucc(dst);
            //如果无条件跳转的目标块为空 那么就插入return
            dst->addPred(*it);
            if (dst->empty()) {
                if (((FunctionType*)(se->getType()))->getRetType() ==
                    TypeSystem::intType)
                    new RetInstruction(new Operand(new ConstantSymbolEntry(
                                           TypeSystem::intType, 0)),
                                       dst);
                else if (((FunctionType*)(se->getType()))->getRetType() ==
                         TypeSystem::voidType)
                    new RetInstruction(nullptr, dst);
            }
        }
        else if (end->isCond())
        {
            BasicBlock*trueBranch=dynamic_cast<CondBrInstruction*>(end)->getTrueBranch();
            BasicBlock*falseBranch=dynamic_cast<CondBrInstruction*>(end)->getFalseBranch();

            curBB->addSucc(trueBranch);
            curBB->addSucc(falseBranch);
            trueBranch->addPred(curBB);
            falseBranch->addPred(curBB);
        }
        //最后一条语句不是返回以及跳转
        else if (!end->isReturn()) {
            if (((FunctionType*)(se->getType()))->getRetType() ==
                TypeSystem::voidType) {
                new RetInstruction(nullptr, *it);
            }
        }
    }    
}

void BinaryExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();//获取表达式语句应该插入的基本块
    Function *func = bb->getParent();//获取基本块所属的函数模块
    if (op == AND)
    {   
        //expr1表达式为真后，会跳转到expr2判断其真伪
        //所以trueBB来表示expr1的truelist的目的块
        BasicBlock *trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
        
        //如果不是条件表达式将其转换为bool类型条件表达式（利用与0比较）
        if(!expr1->IsCond())
        {
            SymbolEntry*se1=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
            expr1=new BinaryExpr(se1,BinaryInstruction::NOTEQ,expr1,expr0);
        }
        expr1->genCode();
        backPatch(expr1->trueList(), trueBB);
        builder->setInsertBB(trueBB);      //将插入点设置为bb,以便expr2生成的指令插入其中         // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
        if(!expr2->IsCond())
        {
            SymbolEntry*se2=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
            expr2=new BinaryExpr(se2,BinaryInstruction::NOTEQ,expr2,expr0);
        }
        expr2->genCode();
        // 无法确定表达式二的truelist的目标基本块，所以把他插入当前节点的truelist，之后再利用回填
        true_list = expr2->trueList();
        //同
        false_list = merge(expr1->falseList(), expr2->falseList());
    }
    else if(op == OR)
    {
        //给子表达式2new 一个truebb
        BasicBlock *trueBB = new BasicBlock(func); // if the result of lhs is true, jump to the trueBB.
        if(!expr1->IsCond())
        {
            SymbolEntry*se=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
            expr1=new BinaryExpr(se,BinaryInstruction::NOTEQ,expr1,expr0);
        }
        expr1->genCode();
        //子表达式1为假时跳转到子表达式2,回填
        backPatch(expr1->falseList(), trueBB,false);
        builder->setInsertBB(trueBB); // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
        if(!expr2->IsCond())
        {
            SymbolEntry*se=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
            expr2=new BinaryExpr(se,BinaryInstruction::NOTEQ,expr2,expr0);
        }
        expr2->genCode();
        true_list = merge(expr1->trueList(), expr2->trueList()); //子表达式1和2为真时跳到哪还不知道,放到当前节点的true_List里,交给父节点去填
        false_list = expr2->falseList();
    }
    else if(op >= LESS && op <= NOTEQ)
    {
        // Todo     
        expr1->genCode();
        expr2->genCode();
        //获得子表达式的目的操作数
        Operand *src1 = expr1->getOperand();
        Operand *src2 = expr2->getOperand();
        //如果是bool类型需要转换
        // if(dynamic_cast<IntType*>(expr1->getSymPtr()->getType())->isBool()){   // is a bool expr
        if (src1->getType()->getSize() == 1) {
            Operand* temp = new Operand(new TemporarySymbolEntry(
                TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(temp, src1, bb);
            src1=temp;
        }
       // if(dynamic_cast<IntType*>(expr2->getSymPtr()->getType())->isBool()){   // is a bool expr
        if (src1->getType()->getSize() == 1) {
            Operand* temp = new Operand(new TemporarySymbolEntry(
                TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(temp, src2, bb);
            src2=temp;
        }


        int cmpopcode;
        switch (op) {
            case LESS:
                cmpopcode = CmpInstruction::L;
                break;
            case LESSQ:
                cmpopcode = CmpInstruction::LE;
                break;
            case MORE:
                cmpopcode = CmpInstruction::G;
                break;
            case MOREQ:
                cmpopcode = CmpInstruction::GE;
                break;
            case EQ:
                cmpopcode = CmpInstruction::E;
                break;
            case NOTEQ:
                cmpopcode = CmpInstruction::NE;
                break;
        }
        
        if(this->getSymPtr()->getType()!=TypeSystem::boolType)
        {
            this->getSymPtr()->setType(TypeSystem::boolType);
            dst=new Operand(this->getSymPtr());
        }
        
        new CmpInstruction(cmpopcode, dst, src1, src2, bb);
        //这里先要准备好cond的trueList和FalseList，并且里面有空块，之后backPatch的时候填上
        BasicBlock *truebb, *falsebb, *tempbb;

        truebb = new BasicBlock(func);
        falsebb = new BasicBlock(func);
        tempbb = new BasicBlock(func);
        //
        //在后面这个cond要作为条件跳转和非条件跳转的指令，和后面的块相连
        //instruction准备好了，但是要跳转的块还是空的，后面填
        true_list.push_back(new CondBrInstruction(truebb, tempbb, dst, bb));

        false_list.push_back(new UncondBrInstruction(falsebb, tempbb));

    }
    else if(op >= ADD && op <= SUB)
    {
        expr1->genCode();
        expr2->genCode();
        Operand *src1 = expr1->getOperand();
        Operand *src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        case DIV:
            opcode = BinaryInstruction::DIV;
            break;
        case MUL:
            opcode = BinaryInstruction::MUL;
            break;
        case MOD:
            opcode = BinaryInstruction::SREM;
            break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void UnaryExpr::genCode()
{
   BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();

    expr->genCode();
    Operand *src = expr->getOperand();
    int opcode;
    ConstantSymbolEntry* zerose=new ConstantSymbolEntry(TypeSystem::intType,0);
    Operand* zero=new Operand(zerose);
    if(op==SUB){
        opcode = BinaryInstruction::SUB;
        if(dynamic_cast<IntType*>(expr->getSymPtr()->getType())->isBool()){   // is a bool expr
            Operand* temp = new Operand(new TemporarySymbolEntry(
                TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(temp, expr->getOperand(), bb);
            src=temp;
        }
        new BinaryInstruction(opcode, dst, zero, src, bb);
        // this->getSymPtr()->setType(TypeSystem::intType);
        // src=dst;
       
    }
    if(op== NOT){
        opcode = CmpInstruction::NE;
        if (!dynamic_cast<IntType*>(expr->getSymPtr()->getType())->isBool()) {   // not a bool expr
            Operand* temp = new Operand(new TemporarySymbolEntry(
                TypeSystem::boolType, SymbolTable::getLabel()));
            new CmpInstruction(opcode, temp, src, zero, bb);
            src=temp;
        }
        new XorInstruction(dst, src, bb);
       this->getSymPtr()->setType(TypeSystem::boolType);
        
    }
    if(op==ADD){                       
        dst=expr->getOperand();
        // this->getSymPtr()->setType(TypeSystem::intType);
    } 



}

void Constant::genCode()
{
    // we don't need to generate code.
}

//添加
Id::Id(SymbolEntry *se, ExprNode* arrIdx): ExprNode(se), arrIdx(arrIdx) {
    type = se->getType();
    SymbolEntry *temp;
    if (type->isInt()) 
        temp = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
    else if (type->isArray()) 
        temp = new TemporarySymbolEntry(new PointerType(((ArrayType*)type)->getElementType()), SymbolTable::getLabel());
    dst = new Operand(temp);
};





void Id::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    // new LoadInstruction(dst, addr, bb);

if (type->isInt()) {
        new LoadInstruction(dst, addr, bb);
    }

else if (type->isArray()) {
        // 参数为数组时
       if (arrIdx) {
            // 获取当前类型和元素类型
            Type* elemType = ((ArrayType*)(this->type))->getElementType();
            Type* thisType = this->type;
            Operand* tempSrc = addr;    // 中间目标地址
            Operand* tempDst = dst;     // 中间目标值
            ExprNode* idx = arrIdx;
            bool empty = false;         // 数组的维度是空，如a[]
            bool pointer = false;       // 指针
            bool firstIdx = true;       // 第一个维度
            // 遍历数组的所有维度
            while (true) {
                // 把基址加载到tempSrc
                if (((ArrayType*)thisType)->getLength() == -1) {
                    Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(elemType), SymbolTable::getLabel()));
                    tempSrc = dst1;
                    new LoadInstruction(dst1, addr, bb);
                    empty = true;
                    firstIdx = false;
                }
                // 如果维度遍历结束，将对应数组值传递到目的地址
                if (!idx) {
                    Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(elemType), SymbolTable::getLabel()));
                    Operand* idx = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
                    new ArrayInstruction(dst1, tempSrc, idx, bb);
                    tempDst = dst1;
                    pointer = true;
                    break;
                }
                idx->genCode();
                // 用于维度寻址，将tempSrc[idx]的值加载到tempDst
                auto gep = new ArrayInstruction(tempDst, tempSrc, idx->getOperand(), bb, empty);
                // 如果[]中不为空且是第一个维度
                if (!empty && firstIdx) {
                    gep->setFirst();
                    firstIdx = false;
                }
                if (empty)
                    empty = false;
                // 维度要全部换成整数   
                if (elemType == TypeSystem::intType )
                    break;
                elemType = ((ArrayType*)elemType)->getElementType();
                thisType = ((ArrayType*)thisType)->getElementType();
                tempSrc = tempDst;
                tempDst = new Operand(new TemporarySymbolEntry(new PointerType(elemType), SymbolTable::getLabel()));
                idx = (ExprNode*)(idx->getNext());
            }
            dst = tempDst;
            // 如果此ID是右值 需要再次load
            if (!left && !pointer) 
            {
                Operand* dst1 = new Operand(new TemporarySymbolEntry(
                    TypeSystem::intType, SymbolTable::getLabel()));
                new LoadInstruction(dst1, dst, bb);
                dst = dst1;
            }
        } 
        // 声明数组时
        else {
            if (((ArrayType*)(this->type))->getLength() == -1) {
                Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(((ArrayType*)(this->type))->getElementType()), SymbolTable::getLabel()));
                new LoadInstruction(dst1, addr, bb);
                dst = dst1;
            } 
            else {
                Operand* idx = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
                auto gep = new ArrayInstruction(dst, addr, idx, bb);
                gep->setFirst();
            }
        }
    }

   

}
void IfStmt::genCode()
{
    Function *func;
    BasicBlock *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    //if(cond->getSymPtr()->getType()!=TypeSystem::boolType)
    if(!cond->IsCond())
    {
        SymbolEntry*se=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
        cond=new BinaryExpr(se,BinaryInstruction::NOTEQ,cond,expr0);
    }
    cond->genCode();
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb,false);

    builder->setInsertBB(then_bb);
    if(thenStmt){
        thenStmt->genCode();
    }
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    //设置下面要开始生成的中间代码块
    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode()
{
    // Todo
    Function *func;
    BasicBlock *then_bb, *else_bb, *end_bb;
    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    //不是条件判断表达式
    if(!cond->IsCond())
    {
        SymbolEntry*se=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
        cond=new BinaryExpr(se,BinaryInstruction::NOTEQ,cond,expr0);
    }
    cond->genCode();
    //填上之前设置好List，List要有conditionalInstr还有unCondInstr，不然没办法和要跳转到的块连上
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), else_bb, false);

    builder->setInsertBB(then_bb);
    if(thenStmt!=NULL){
        thenStmt->genCode();
    }
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(else_bb);
    if(elseStmt!=NULL){
        elseStmt->genCode();
    }
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);

    builder->setInsertBB(end_bb);

}

void CompoundStmt::genCode()
{
    // Todo
    stmt->genCode();
}

void SeqNode::genCode()
{
    // Todo
    stmt1->genCode();
    stmt2->genCode();
}

//deleted DeclStmt::genCode()

void ReturnStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    // Todo
    Operand *src;
    if(retValue!=nullptr)
    {
        retValue->genCode();
        src = retValue->getOperand();
    }else{
        src = nullptr;
    }
    BasicBlock* emptybb=new BasicBlock;
    new RetInstruction(src,bb);
    builder->setInsertBB(emptybb);  // emptybb not in func
}

void AssignStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr;
    if(expr!=NULL){
        expr->genCode();
    }
    if (lval->getType()->isInt()){
     addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    }
    else if (lval->getType()->isArray()) {
        ((Id*)lval)->setLeft();
        lval->genCode();
        addr = lval->getOperand();
    }


    Operand *src;
    
    
    
    src = expr->getOperand();
    
    new StoreInstruction(addr, src, bb);
}

void VarDeclStmt::genCode()
{
    varDecls->genCode();
}

void ConstDeclStmt::genCode()
{
    constDecls->genCode();
}

void VarDecls::genCode()
{
    if(varDecl!=NULL){
        varDecl->genCode();
    }
    if(varDecls!=NULL){
        varDecls->genCode();
    }
}

void ConstDecls::genCode()
{
    if(constDecl!=NULL){
        constDecl->genCode();
    }
    if(constDecls!=NULL){
        constDecls->genCode();
    }
}

void VarDecl::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    //se的getvalue有值吗？？？
   // printf("%s%d\n","id type:",id->dst->getType()->getKind());
    if(se->isGlobal())
    {
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);

        BasicBlock*bb=builder->getUnit()->getGlobalBB();
        
        if(expr)
        {
            int _value = expr->getValue();
            if(!se->getType()->isArray()){
            new GlobalDefInstruction(se->getAddr(), expr->getOperand(),false, bb,_value);
        }}
        else
        {
            if(!se->getType()->isArray()){
            new GlobalDeclInstruction(se->getAddr(),bb);}
        }
        if(se->getType()->isArray()){
            unit.insertGlobal(se);
            mUnit.insertGlobal(se);
        }


    }
    //???没有对参数的
    else if(se->isLocal())
    {
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        Operand *addr;
        SymbolEntry *addr_se;
        Type *type;
        type = new PointerType(se->getType());
      //  type = new PointerType(TypeSystem::intType);
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);       // set the addr operand in symbol entry so that we can use it in subsequent code generation.
        if (expr)
        {
            expr->genCode();
            BasicBlock *bb;
            bb = builder->getInsertBB();
            new StoreInstruction(se->getAddr(), expr->getOperand(), bb);
        }
    }
}
//const to do
void ConstDecl::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    if(se->isGlobal())
    {
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        BasicBlock*bb=builder->getUnit()->getGlobalBB();
        if(expr)
        {
            int _value = expr->getValue();
            if(!se->getType()->isArray()){
             new GlobalDefInstruction(se->getAddr(), expr->getOperand(),true, bb,_value);
        }}
        else
        {
            if(!se->getType()->isArray()){
            new GlobalDeclInstruction(se->getAddr(),bb);}
        }
        if(se->getType()->isArray()){
            unit.insertGlobal(se);
            mUnit.insertGlobal(se);
        }
    }
    else if(se->isLocal())
    {
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        Operand *addr;
        SymbolEntry *addr_se;
        Type *type;
        type = new PointerType(se->getType());
        // type = new PointerType(TypeSystem::intType);
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                                          // set the addr operand in symbol entry so that we can use it in subsequent code generation.
        if (expr)
        {
            expr->genCode();
            BasicBlock *bb;
            bb = builder->getInsertBB();
            new StoreInstruction(se->getAddr(), expr->getOperand(), bb);
        }
    }
}

void FuncCallExp::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    vector<Operand* >vec;
    ExprNode *temp = this->callList;
    if(temp){
        callList->genCode();
    }
    while(temp){
        ExprNode *tempParam = dynamic_cast<CallList*>(temp)->getParam();
        vec.push_back(tempParam->getOperand());
        temp = dynamic_cast<CallList*>(temp)->getNext();
    }

    if(dynamic_cast<FunctionType*>(this->getFunc()->getType())->isSysy()){
        builder->getUnit()->insertDecl(this->getFunc());
    }
    if(dynamic_cast<FunctionType*>(this->getFunc()->getType())->getRetType()->isVoid()){
        new FuncCallInstruction(NULL,vec,this->getFunc(),bb);
    }else{
        new FuncCallInstruction(id->getOperand(),vec,this->getFunc(),bb);
    }
    dst=id->getOperand();
}

void CallList::genCode()
{
    expr->genCode();
    if(callList!=NULL){
        callList->genCode();
    }
}

void CallStmt::genCode()
{
    callExp->genCode();
}

void NullStmt::genCode()
{
    if(expr!=NULL){
        expr->genCode();
    }
}

void FuncParams::genCode()
{
    if(funcParam!=NULL){
        funcParam->genCode();
    }
    if(funcParams!=NULL){
        funcParams->genCode();
    }
}

void FuncParam::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    Function *func = builder->getInsertBB()->getParent();
    BasicBlock *entry = func->getEntry();
    Instruction *alloca;
    Operand *addr;
    SymbolEntry *addr_se;
    Type *type;
    type = new PointerType(se->getType());
    addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
    addr = new Operand(addr_se);
    alloca = new AllocaInstruction(addr, se);               
    entry->insertFront(alloca);      
    se->setAddr(addr);
    
    BasicBlock *bb;
    bb = builder->getInsertBB();
    new StoreInstruction(se->getAddr(), id->getOperand(), bb, paramnum);
    paramnum++;
}

void WhileStmt::genCode()
{
    Function *func;
    BasicBlock *stmt_bb, *end_bb, *cond_bb,*bb;

    func = builder->getInsertBB()->getParent();
    bb=builder->getInsertBB();

    cond_bb = new BasicBlock(func);
    stmt_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    this->cond_bb = cond_bb;
    this->end_bb = end_bb;

    new UncondBrInstruction(cond_bb,bb);
    builder->setInsertBB(cond_bb);


    if(!cond->IsCond())
    {
        SymbolEntry*se=new TemporarySymbolEntry(TypeSystem::boolType,SymbolTable::getLabel());
        cond=new BinaryExpr(se,BinaryInstruction::NOTEQ,cond,expr0);
    }
    cond->genCode();
    
    backPatch(cond->trueList(), stmt_bb);
    backPatch(cond->falseList(), end_bb, false);

    builder->setInsertBB(stmt_bb);
    if(Stmt!=NULL){
        Stmt->genCode();
    }
    stmt_bb = builder->getInsertBB();
    new UncondBrInstruction(cond_bb, stmt_bb);
    builder->setInsertBB(end_bb);
}

void BreakStmt::genCode()
{
    //break语句直接跳转到end
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_end_bb(), bb);
    BasicBlock* break_next_bb = new BasicBlock(func);
    builder->setInsertBB(break_next_bb);

}

void ContinueStmt::genCode()
{
    //continue语句跳转到cond
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_cond_bb(), bb);
    BasicBlock* continue_next_bb = new BasicBlock(func);
    builder->setInsertBB(continue_next_bb);
}
//运行时库咱们不需要管，直接调用，数组的运行时库这样不行，貌似没法这样处理
//不过反正也没加成功，就这样了
void Ostream::genCode()
{

}

void Istream::genCode()
{

}

void Ast::typeCheck()
{
    if(root != nullptr)
        root->typeCheck();
}

//函数表达式
void FunctionDef::typeCheck()
{
    // Todo
    // printf("%s\n","FunctionDef typecheck!");
    if(param!=NULL){
        param->typeCheck();
    }
    if(stmt!=NULL){
        stmt->typeCheck();
    }
    Type *t=se->getType();

   
    if(!dynamic_cast<IdentifierSymbolEntry*>(se)->isGlobal())
    {
        fprintf(stderr,"%s","error: function define at wrong scope\n");
        exit(EXIT_FAILURE);
    }
    if(dynamic_cast<FunctionType*>(t)->getRetType()->isInt()&&dynamic_cast<FunctionType*>(t)->haveRet()==false){
        fprintf(stderr,"%s","error: returnStmt loss\n");
        exit(EXIT_FAILURE);
    }

    //函数未声明
    std::string name = dynamic_cast<IdentifierSymbolEntry*>(se)->getName();
    SymbolEntry* se1 = identifiers->lookup(name);
    if(se1 == nullptr)
    {
        fprintf(stderr,"%s\n","error: function udefined");
        exit(EXIT_FAILURE);
        return ;
    }

    Type* type1 = se->getType();
    //获取这个函数在定义时的参数个数
    int num = dynamic_cast<FunctionType*>(type1)->getParamNum();

    //se1是通过查找符号表查出来的这个函数的定义（根据符号名）
    Type* type2 = se1->getType();
    while(1)
    {
        int a =dynamic_cast<FunctionType*>(type2)->getParamNum();
        //参数个数一样，符号名一样，但是不是同一个符号表项，所以说明是错误的重载
        if(a == num)
        {   
            if(se != se1)
            {
                fprintf(stderr,"%s\n","error: function overloading with wrong num of param");
                exit(EXIT_FAILURE);
                return ;
            }
            else
            break;
        }
        else{
            if(se1->next != nullptr)
                se1 = se1->next;
            else
                break;
        }
    }
}

//二元表达式
void BinaryExpr::typeCheck()
{
    // Todo
    // printf("%s%d\n","BinaryExpr typecheck",this->getSeq());
    expr1->typeCheck();
    expr2->typeCheck();
    Type *type1 = expr1->getSymPtr()->getType();
    Type *type2 = expr2->getSymPtr()->getType();
    if(type1->isVoid()){
        fprintf(stderr, "type %s in BinaryExpr is void!\n",
        type1->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    if(type2->isVoid()){
        fprintf(stderr, "type %s in BinaryExpr is void!\n",
        type2->toStr().c_str());
        exit(EXIT_FAILURE);
    }
   /* if(type1 != type2)
    {
        fprintf(stderr, "type %s and %s mismatch\n",
        type1->toStr().c_str(), type2->toStr().c_str());
        exit(EXIT_FAILURE);
    }
*/
    //如果是条件表达式
    if(op>=AND){
        iscond=true;
    }
    else{
    //如果是二元运算表达式，应该为整型
       // this->getSymPtr()->setType(TypeSystem::intType);
    }

    //将表达式的值的类型设为第一个运算表达式的类型
   //  symbolEntry->setType(type1);

}

//一元表达式
void UnaryExpr::typeCheck()
{
    // printf("%s%d\n","UnaryExpr typecheck",this->getSeq());
    expr->typeCheck();
    Type *type=expr->getSymPtr()->getType(); //获取这个表达式符号表项对应的类型
    
    if(type==TypeSystem::voidType)
    {
        this->symbolEntry->setType(TypeSystem::voidType);
    }else{
    if(type==TypeSystem::intType)
        this->symbolEntry->setType(TypeSystem::intType);
    }
    iscond=false;
}


void VarDeclStmt::typeCheck()
{
    // printf("%s%d\n","varDeclstmt typecheck",this->getSeq());
    varDecls->typeCheck();
}

void ConstDeclStmt::typeCheck()
{
    // printf("%s%d\n","constDeclstmt typecheck",this->getSeq());
    constDecls->typeCheck();
}

void VarDecls::typeCheck()
{
    // printf("%s%d\n","varDecls typecheck",this->getSeq());
    if(varDecls!=NULL){
        varDecls->typeCheck();
    }
    if(varDecl!=NULL){
        varDecl->typeCheck();
    }
}

void ConstDecls::typeCheck()
{
    // printf("%s%d\n","constDecls typecheck",this->getSeq());
    if(constDecls!=NULL){
        constDecls->typeCheck();
    }
    if(constDecl!=NULL){
        constDecl->typeCheck();
    }
}

void VarDecl::typeCheck()
{
    // printf("%s%d\n","varDecl typecheck",this->getSeq());
    id->typeCheck();
    if(expr!=NULL){
        expr->typeCheck();
    }

}

void ConstDecl::typeCheck()
{
    // printf("%s%d\n","constDecl typecheck",this->getSeq());
    id->typeCheck();
    if(expr!=NULL){
        expr->typeCheck();
    }else{
        fprintf(stderr,"%s","error: const value has not been initialized!\n");
        exit(EXIT_FAILURE);
    }
}

void FuncCallExp::typeCheck()
{
    // printf("%s%d\n","funcCallexp typecheck",this->getSeq());
    if(symbolEntry==NULL){
        printf("%s\n","error: function not define!");
    }
    vector<Operand* >vec;
    ExprNode *temp = this->callList;
    if(temp){
        callList->typeCheck();
    }

}

void CallList::typeCheck()
{
    // printf("%s%d\n","callList typecheck",this->getSeq());
    expr->typeCheck();

    if(callList!=NULL){
        callList->typeCheck();
    }
}

void CallStmt::typeCheck()
{
    // printf("%s%d\n","CallStmt typecheck",this->getSeq());
    callExp->typeCheck();
}

void NullStmt::typeCheck()
{
    // printf("%s%d\n","NullStmt typecheck",this->getSeq());
    if(expr!=NULL){
        expr->typeCheck();
    }
}

void FuncParams::typeCheck()
{
    // printf("%s%d\n","funcParams typecheck",this->getSeq());
    if(funcParam!=NULL){
        funcParam->typeCheck();
    }
    if(funcParams!=NULL){
        funcParams->typeCheck();
    }
}

void FuncParam::typeCheck()
{
    // printf("%s%d\n","funcParam typecheck",this->getSeq());
    if(expr!=NULL){
        expr->typeCheck();
        Type* type1=id->getSymPtr()->getType();
        Type* type2=expr->getSymPtr()->getType();
        if(!type2->isInt()){ //？因为ID只能int吗
            fprintf(stderr, "functioncall type %s and %s mismatch in line xx",
            type1->toStr().c_str(), type2->toStr().c_str());
            exit(EXIT_FAILURE);
        }
    }
    
}


//以后应该添加判断是否仅出现在while语句中
void BreakStmt::typeCheck()
{
    // printf("%s%d\n","breakStmt typecheck",this->getSeq());
}

void ContinueStmt::typeCheck()
{
    // printf("%s%d\n","continueStmt typecheck",this->getSeq());
}

void WhileStmt::typeCheck()
{
    // printf("%s%d\n","WhileStmt typecheck",this->getSeq());
    cond->typeCheck();
    if(Stmt!=NULL){
        Stmt->typeCheck();
    }

  //SymbolEntry *se=cond->getSymPtr();//获取条件表达式节点的符号表项
    //se->setType(TypeSystem::boolType);//将其类型设置为bool
    
}


void Ostream::typeCheck()
{
    // printf("%s%d\n","Ostream typecheck",this->getSeq());
    exp->typeCheck();
}

void Istream::typeCheck()
{
    // printf("%s%d\n","Istream typecheck",this->getSeq());
}

void Constant::typeCheck()
{
    // Todo
    // printf("%s%d\n","Constant typecheck",this->getSeq());
    if(!this->getSymPtr()->getType()->isInt()){
        printf("%s\n","error: constant type error!");
    }
}

void Id::typeCheck()
{
    // Todo,scope ..
    // printf("%s%d\n","Id typecheck",this->getSeq());
   // this->getSymPtr()->setType(TypeSystem::intType);//identifier的基本类型仅包括int
}

void IfStmt::typeCheck()
{
    // printf("%s%d\n","IfStmt typecheck",this->getSeq());
    cond->typeCheck();
    // cond->getSymPtr()->setType(TypeSystem::boolType);
    // Todo
}

void IfElseStmt::typeCheck()
{
    // Todo
    // printf("%s%d\n","IfElseStmt typecheck",this->getSeq());
    cond->typeCheck();
    cond->getSymPtr()->setType(TypeSystem::boolType);
}

void CompoundStmt::typeCheck()
{
    // Todo
    // printf("%s%d\n","CompoundStmt typecheck",this->getSeq());
    if(stmt!=NULL){
        stmt->typeCheck();
    }
}

void SeqNode::typeCheck()
{
    // Todo
    // printf("%s%d\n","SeqNode typecheck",this->getSeq());
    stmt1->typeCheck();
    stmt2->typeCheck();
}

void ReturnStmt::typeCheck()
{
    // Todo
    // printf("%s%d\n","returnStmt typecheck",this->getSeq());
    Type *type2;
    if(retValue!=NULL){
        retValue->typeCheck();
        type2=this->retValue->getSymPtr()->getType();
    }
    else{
        type2=TypeSystem::voidType;
    }
}

void AssignStmt::typeCheck()
{
    // printf("%s%d\n","AssignStmt typecheck",this->getSeq());
    lval->typeCheck();
    expr->typeCheck();
    // printf("%s%d\n","AssignStmt typecheck",this->getSeq());
    Type *type1 = lval->getSymPtr()->getType();
    Type *type2 = expr->getSymPtr()->getType();
    if(lval->getSymPtr()->isConstant()){ //lval是用于保存字面值常量属性值的符号表项
        fprintf(stderr, "constant %s in assignStmt is reinitialize!\n",lval->getSymPtr()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    if(type1->isVoid()){
        fprintf(stderr, "type %s in assignStmt is void!\n",
        type1->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    if(type2->isVoid()){
        fprintf(stderr, "type %s in assignStmt is void!\n",
        type2->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    /*
    if(type1 != type2)
    {
        fprintf(stderr, "Assign:type %s and %s mismatch in line xx",
        type1->toStr().c_str(), type2->toStr().c_str());
        exit(EXIT_FAILURE);
    }*/
}
int BinaryExpr::getValue() {
    int value = 0;
    switch (op) {
        case ADD:
            value = expr1->getValue() + expr2->getValue();
            break;
        case SUB:
            value = expr1->getValue() - expr2->getValue();
            break;
        case MUL:
            value = expr1->getValue() * expr2->getValue();
            break;
        case DIV:
            if(expr2->getValue())
                value = expr1->getValue() / expr2->getValue();
            break;
        case MOD:
            value = expr1->getValue() % expr2->getValue();
            break;
        case AND:
            value = expr1->getValue() && expr2->getValue();
            break;
        case OR:
            value = expr1->getValue() || expr2->getValue();
            break;
        case LESS:
            value = expr1->getValue() < expr2->getValue();
            break;
       
        case LESSQ:
            value = expr1->getValue() <= expr2->getValue();
            break;
        case MORE:
            value = expr1->getValue() > expr2->getValue();
            break;
        case MOREQ:
            value = expr1->getValue() >= expr2->getValue();
            break;
        case EQ:
            value = expr1->getValue() == expr2->getValue();
            break;
        case NOTEQ:
            value = expr1->getValue() != expr2->getValue();
            break;
    }
    return value;
}
int Constant::getValue() {
    // assert(symbolEntry->getType()->isInt());
    return ((ConstantSymbolEntry*)symbolEntry)->getValue();
}

int Id::getValue() {
    // assert(symbolEntry->getType()->isInt());
    return ((IdentifierSymbolEntry*)symbolEntry)->getValue();
}


//-----------新添加的-------------
void ExprNode::output(int level) {
    std::string name, type;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cconst string\ttype:%s\t%s\n", level, ' ', type.c_str(),
            name.c_str());
}



void ExprNode::typeCheck() {

}



void ExprNode::genCode() {
    
}



void BinaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
    {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case AND:
            op_str = "and";
            break;
        case OR:
            op_str = "or";
            break;
        case LESS:
            op_str = "less";
            break;
        case MORE:
            op_str = "more";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case EQ:
            op_str = "eq";
            break;    
        case NOTEQ:
            op_str = "noteq";
            break;
        case MOREQ:
            op_str = "moreq";
            break;
        case LESSQ:
            op_str = "lessq";
            break;        
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void UnaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
    {
        case ADD:
            op_str = "pos";
            break;
        case SUB:
            op_str = "nag";
            break;
        case NOT:
            op_str = "not";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}

//添加
int UnaryExpr::getValue() {
    int value = 0;
    switch (op) {
        case NOT:
            value = !(expr->getValue());
            break;
        case SUB:
            value = -(expr->getValue());
            break;
    }
    return value;
}



void Ast::output()
{
    fprintf(yyout, "program\n");
    if(root != nullptr)
        root->output(4);
}

void Constant::output(int level)
{
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

void Id::output(int level)
{
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    fprintf(yyout, "%*cSequence\n", level, ' ');
    stmt1->output(level + 4);
    stmt2->output(level + 4);
}

void VarDeclStmt::output(int level)
{
    fprintf(yyout, "%*cVarDeclStmt\n", level, ' ');
    varDecls->output(level + 4);
}

void ConstDeclStmt::output(int level)
{
    fprintf(yyout, "%*cConstDeclStmt\n", level, ' ');
    constDecls->output(level + 4);
}

void VarDecls::output(int level)
{
    fprintf(yyout, "%*cVarDecls\n", level, ' ');
    if(varDecl!=NULL){
        varDecl->output(level + 4);
    }
    if(varDecls!=NULL){
        varDecls->output(level + 4);
    }
}

void ConstDecls::output(int level)
{
    fprintf(yyout, "%*cConstDecls\n", level, ' ');
    if(constDecl!=NULL){
        constDecl->output(level + 4);
    }
    if(constDecls!=NULL){
        constDecls->output(level + 4);
    }
}

void VarDecl::output(int level)
{
    fprintf(yyout, "%*cVarDecl\n", level, ' ');
    id->output(level + 4);
    if(expr!=NULL){
        expr->output(level + 4);
    }
}

void ConstDecl::output(int level)
{
    fprintf(yyout, "%*cConstDecl\n", level, ' ');
    id->output(level + 4);
    if(expr!=NULL){
        expr->output(level + 4);
    }
}

void FuncParams::output(int level)
{
    fprintf(yyout, "%*cFuncParams\n", level, ' ');
    if(funcParam!=NULL){
        funcParam->output(level + 4);
    }
    if(funcParams!=NULL){
        funcParams->output(level + 4);
    }
}

void FuncParam::output(int level)
{
    fprintf(yyout, "%*cFuncParam\n", level, ' ');
    if(id!=NULL){
    id->output(level + 4);
    }
    if(expr!=NULL){
        expr->output(level + 4);
    }
}

void CallList::output(int level)
{
    fprintf(yyout, "%*cFuncCallList\n", level, ' ');
    expr->output(level+4);
    if(callList!=NULL){
        callList->output(level+4);
    }
}

void FuncCallExp::output(int level)
{
    fprintf(yyout, "%*cFuncCallExp\n", level, ' ');
    if(callList!=NULL){
        callList->output(level+4);
    }
}

void CallStmt::output(int level)
{
    fprintf(yyout, "%*cFuncCallStmt\n", level, ' ');
    callExp->output(level+4);
}

void NullStmt::output(int level)
{
    fprintf(yyout, "%*cNullStmt\n", level, ' ');
    if(expr!=NULL){
        expr->output(level+4);
    }
}

void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    if(thenStmt!=NULL){
        thenStmt->output(level + 4);
    }
}

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}
void WhileStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    Stmt->output(level + 4);
}
void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}
void BreakStmt::output(int level)
{
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level)
{
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}
void FunctionDef::output(int level)
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ', 
            name.c_str(), type.c_str());
    if(param!=NULL){
        param->output(level + 4);
    }
    stmt->output(level + 4);
    
}

void Ostream::output(int level)
{
    fprintf(yyout, "%*cOstream\n", level, ' ');
    exp->output(level + 4);
}
void Istream::output(int level)
{
    fprintf(yyout, "%*cIstream\n", level, ' ');
}



//----------------------数组可能会用到的-------------------




