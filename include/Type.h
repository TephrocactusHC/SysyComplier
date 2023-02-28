#ifndef __TYPE_H__
#define __TYPE_H__
#include <vector>
#include <string>
#include <SymbolTable.h>

class Type
{
private:
    int kind;
protected:
    enum {INT, VOID, FLOAT, PTR,ARRAY, FUNC};
    int size;//大小
public:
    Type(int kind, int size = 0) : kind(kind), size(size){};
    virtual ~Type() {};
    virtual std::string toStr() = 0;
    bool isInt() const {return kind == INT;};
    bool isVoid() const {return kind == VOID;};
    bool isFunc() const {return kind == FUNC;};
    bool isPtr() const { return kind == PTR; };
    bool isFloat() const { return kind == FLOAT; };
    bool isArray() const { return kind == ARRAY; };
    int getKind() const { return kind; };
    int getSize() const { return size; };

};

class IntType : public Type
{
private:

    bool constant;//在添加数组的时候加的
public:
   // IntType(int size) : Type(Type::INT), size(size){};
    IntType(int size, bool constant = false) : Type(Type::INT, size), constant(constant){};
    bool isBool() {return size==1;};
    std::string toStr();
    bool isConst() const { return constant; };

};
//底下这个isbool，我不知道会不会还有隐式类型转换了
class FloatType : public Type
{
private:
    int size;
public:
    FloatType(int size) : Type(Type::FLOAT), size(size){};
    bool isBool() {return size==1;};
    std::string toStr();
};


class VoidType : public Type
{
public:
    VoidType() : Type(Type::VOID){};
    std::string toStr();
};

class FunctionType : public Type
{
private:
    Type *returnType;
    std::vector<Type*> paramsType;
    std::vector<SymbolEntry*>paramSe;
    bool isRet;
    bool sysy;
public:
    FunctionType(Type* returnType, std::vector<Type*> paramsType) : 
    Type(Type::FUNC), returnType(returnType), paramsType(paramsType){isRet=false; sysy=false;};
    FunctionType(Type* returnType, std::vector<Type*> paramsType, std::vector<SymbolEntry*> paramsSe) : 
    Type(Type::FUNC), returnType(returnType), paramsType(paramsType), paramSe(paramsSe){};
    Type* getRetType() {return returnType;};
    void setRetType(Type* type) {this->returnType=type;};
    void addParam(Type* type) {this->paramsType.push_back(type);};
    int getParamNum() {return this->paramsType.size();};
    std::vector<Type*> getParamsType() { return paramsType; };
    std::vector<SymbolEntry*> getParamSe() { return paramSe; };
    void setRet() {isRet=true;};
    bool haveRet() {return isRet;};
    void setSysy() {sysy=true;};
    bool isSysy() {return sysy;};
    std::string toStr();
};

//添加数组
class ArrayType : public Type {
   private:
    Type* elementType;//数组元素类型
    Type* arrayType = nullptr;
    int length;//数组长度
    bool constant;

   public:
    ArrayType(Type* elementType, int length, bool constant = false): Type(Type::ARRAY),
          elementType(elementType),
          length(length),
          constant(constant){size = elementType->getSize() * length;
};
    std::string toStr();
    int getLength() const { return length; };
    Type* getElementType() const { return elementType; };
    void setArrayType(Type* arrayType) { this->arrayType = arrayType; };
    bool isConst() const { return constant; };
    Type* getArrayType() const { return arrayType; };
};







class PointerType : public Type
{
private:
    Type *valueType;
public:
    PointerType(Type* valueType) : Type(Type::PTR) {this->valueType = valueType;};
    std::string toStr();
    Type* getValueType() const {return valueType;};
};

class TypeSystem
{
private:
    static IntType commonInt;
    static IntType commonBool;
    static VoidType commonVoid;
    static FloatType commonFloat;
public:
    static Type *intType;
    static Type *voidType;
    static Type *boolType;
    static Type *floatType;
};

#endif
