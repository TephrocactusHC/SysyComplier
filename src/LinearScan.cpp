#include "LinearScan.h"
#include <algorithm>
#include <iostream>
#include "LiveVariableAnalysis.h"
#include "MachineCode.h"

LinearScan::LinearScan(MachineUnit* unit) {
    this->unit = unit;
    // 这里不对r0-r3做分配，这四个寄存器用来函数传参
    for (int i = 4; i < 11; i++)
    //可分配寄存器为4-10
        regs.push_back(i);
}
//对所有的函数进行遍历，以获得虚拟寄存器对应的物理寄存器
void LinearScan::allocateRegisters() {
    for (auto& f : unit->getFuncs()) {
        //然后开始遍历unit下的每个func
        //success是用来判断什么时候完成分配的
        //后面就是判断是否有溢出了，溢出就生成溢出代码，没有溢出就直接进行物理寄存器的映射
        func = f;
        bool success;
        success = false;
        while (!success)  // repeat until all vregs can be mapped
        {
            computeLiveIntervals();//旧区间重构
            success = linearScanRegisterAllocation();
            if (success)  // all vregs can be mapped to real regs
                modifyCode();
            else  // spill vregs that can't be mapped to real regs
                genSpillCode();
        }
    }
}

void LinearScan::makeDuChains() {
    LiveVariableAnalysis lva;
    //对函数进行遍历，添加使用和定义变量，分配寄存器
    lva.pass(func);
    du_chains.clear();
    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand*>> liveVar;
    for (auto& bb : func->getBlocks()) {
        liveVar.clear();
        for (auto& t : bb->getLiveOut())
            liveVar[*t].insert(t);
        int no;
        no = i = bb->getInsts().size() + i;
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend();
             inst++) {
            (*inst)->setNo(no--);
            //遍历指令定义的操作数
            for (auto& def : (*inst)->getDef()) {
                if (def->isVReg()) {
                    //得到定义变量的活跃寄存器，把所有对应的变量放入du_chains之中
                    auto& uses = liveVar[*def];
                    du_chains[def].insert(uses.begin(), uses.end());
                    auto& kill = lva.getAllUses()[*def];
                    std::set<MachineOperand*> res;
                    set_difference(uses.begin(), uses.end(), kill.begin(),
                                   kill.end(), inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }
            for (auto& use : (*inst)->getUse()) {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}

void LinearScan::computeLiveIntervals() {
    makeDuChains();
    intervals.clear();
    for (auto& du_chain : du_chains) {
        int t = -1;
        for (auto& use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());
        Interval* interval = new Interval({du_chain.first->getParent()->getNo(),
                                           t,
                                           false,
                                           0,
                                           0,
                                           {du_chain.first},
                                           du_chain.second});
        intervals.push_back(interval);
    }
    bool change;
    change = true;
    while (change) {
        change = false;
        std::vector<Interval*> t(intervals.begin(), intervals.end());
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++) {
                Interval* w1 = t[i];
                Interval* w2 = t[j];
                if (**w1->defs.begin() == **w2->defs.begin()) {
                    std::set<MachineOperand*> temp;
                    set_intersection(w1->uses.begin(), w1->uses.end(),
                                     w2->uses.begin(), w2->uses.end(),
                                     inserter(temp, temp.end()));
                    if (!temp.empty()) {
                        change = true;
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());
                        w1->start = std::min(w1->start, w2->start);
                        w1->end = std::max(w1->end, w2->end);
                        auto it =
                            std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }
    sort(intervals.begin(), intervals.end(), compareStart);
}

bool LinearScan::linearScanRegisterAllocation() {
    //用于判断能否分配成功的一个辅助变量
    bool helper = true;
    //先初始化
    actives.clear();
    regs.clear();
    //初始放入可用分配寄存器
    for (int i = 10; i >=4; i--)
    {
        regs.push_back(i);
    }
    //遍历每个unhandled interval没有分配寄存器的活跃区间
    for (auto& i : intervals) {
        //遍历 active 列表，看该列表中是否存在结束时间早于 
        //unhandled interval 的 interval
        //主要用于回收可用寄存器
        expireOldIntervals(i);
        //没有可分配的寄存器 溢出
        if (regs.empty()) {
            spillAtInterval(i); //溢出操作 
            helper = false;
        } else {
            //溢出操作
            i->rreg = regs.front();
            regs.erase(regs.begin());
            //放入已经分配的向量中
            actives.push_back(i);
            //右值引用 可以直接用i
            sort(actives.begin(), actives.end(), compareEnd);
        }
    }
    return helper;

}

void LinearScan::modifyCode() {
    for (auto& interval : intervals) {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg); 
    }
}

void LinearScan::genSpillCode() {
    for (auto& interval : intervals) {
        if (!interval->spill)
            continue;
        // TODO
        /* HINT:
         * The vreg should be spilled to memory.
         * 1. insert ldr inst before the use of vreg
         * 2. insert str inst after the def of vreg
         */
        //获取栈内相对偏移
        //注意要是负的 以FP为基准
        interval->disp = -func->AllocSpace(4);
        //获取偏移和FP寄存器的值
        auto off = new MachineOperand(MachineOperand::IMM, interval->disp);
        auto fp = new MachineOperand(MachineOperand::REG, 11);
        for (auto use : interval->uses) {
             //在use之前插入load指令 将其从栈内加载到目的虚拟寄存器中
            auto temp = new MachineOperand(*use);
            MachineOperand* operand = nullptr;
            // Todo:insert ldr and str
            //auto cur_func=func;
            //MachineInstruction* cur_inst=nullptr;
            //MachineBlock* cur_block;
            //int offset = cur_func->AllocSpace(4);
            //首先判断当前数据地址是否超过寻址空间 
            //超出寻址空间 不能直接加载 要分两步
            //首先加载到虚拟寄存器 ldr v1,off
            if (interval->disp > 255 || interval->disp < -255) {
                operand = new MachineOperand(MachineOperand::VREG,
                                             SymbolTable::getLabel());
                auto src1 = new LoadMInstruction(use->getParent()->getParent(),
                                                  operand, off);
                //USE指令前插入Load指令
                use->getParent()->insertBefore(src1);
            }
            //超出寻址空间的话 第二步ldr r0,[fp,v1]
            if (operand) {
                auto src =
                    new LoadMInstruction(use->getParent()->getParent(), temp,
                                         fp, new MachineOperand(*operand));
                use->getParent()->insertBefore(src);
            } else {
                //正常情况，直接从fp-off的地方加载
                auto src = new LoadMInstruction(use->getParent()->getParent(),
                                                 temp, fp, off);
                use->getParent()->insertBefore(src);
            }
        }
        //遍历其 DEF 指令的列表，
        //在 DEF 指令后插入 StoreMInstruction，将其从目前的虚拟寄存器中
        //存到栈内
        for (auto def : interval->defs) {
             //在def之后插入store指令
            auto temp = new MachineOperand(*def);
            MachineOperand* operand = nullptr;
            MachineInstruction *src1 = nullptr, *src = nullptr;
             //同样要考虑寻址空间
            if (interval->disp > 255 || interval->disp < -255) {
                operand = new MachineOperand(MachineOperand::VREG,
                                             SymbolTable::getLabel());
                src1 = new LoadMInstruction(def->getParent()->getParent(),
                                             operand, off);
                def->getParent()->insertAfter(src1);
            }
            //StoreMInstruction要插入DEF指令之后
            if (operand)
                src =
                    new StoreMInstruction(def->getParent()->getParent(), temp,
                                          fp, new MachineOperand(*operand));
            else
                src = new StoreMInstruction(def->getParent()->getParent(),
                                             temp, fp, off);
            if (src1)
                src1->insertAfter(src);
            else
                def->getParent()->insertAfter(src);
        }
    }
}

void LinearScan::expireOldIntervals(Interval* interval) {
    auto it = actives.begin();
    //查看active中是否有结束时间早于interval起始时间
    //active按照end时间升序排列，所以只用看头部
    //头部如果大于 那么直接返回
    //头部小于 那么active的寄存器可以回收
    while (it != actives.end()) {
        if ((*it)->end >= interval->start)
            return;
        regs.push_back((*it)->rreg);
        it = actives.erase(find(actives.begin(), actives.end(), *it));
        sort(regs.begin(), regs.end());
    }
}
//寄存器溢出操作
void LinearScan::spillAtInterval(Interval* interval) {
    //选择active列表末尾与当前unhandled的一个溢出到栈中
    auto spill = actives.back();
    //将结束时间更晚的溢出
    if (spill->end > interval->end) {
        spill->spill = true;
        interval->rreg = spill->rreg;
         //额外添加 处理寄存器
        func->addSavedRegs(interval->rreg);
       //再按照 unhandled interval活跃区间结束位置，将其插入到 active 列表中。
        actives.push_back(std::move(interval));
        //插入后再次按照结束时间对活跃区间进行排序
        sort(actives.begin(), actives.end(), [](Interval* a, Interval* b) {return a->end < b->end;});
    } else {
        //unhandle溢出更晚只需置位spill标志位
        interval->spill = true;
    }
}

bool LinearScan::compareStart(Interval* a, Interval* b) {
    return a->start < b->start;
}

bool LinearScan::compareEnd(Interval* a, Interval* b) {
    return a->end < b->end;
}
//后面的函数暂时保留，但是没啥用处
void LinearScan::allocReg(Interval* interval, int regno)
{
    interval_reg[regno]=interval;
    if(interval!=NULL)
    {
        interval->rreg=regno;
    }
}

int LinearScan::getFreeReg()
{
    for (int i = 4; i < 11; i++)
    {
        if(interval_reg[i]==nullptr)
        {
            return i;
        }
    }
    return 0;
}

void LinearScan::insertActive(Interval* interval)
{
    if(actives.size()==0)
    {
        actives.push_back(interval);
        return;
    }
    for (vector<Interval*>::iterator it=actives.begin();it!=actives.end();it++)
    {
        if((*it)->end>interval->end)
        {
            actives.insert(it,1,interval);
            return;
        }
    }
    actives.push_back(interval);
}