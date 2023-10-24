#ifndef DSM2_MODEL_INTERFACE_GATE_H_INCLUDED
#define DSM2_MODEL_INTERFACE_GATE_H_INCLUDED
#pragma warning(disable:4786)

#include <assert.h>
#include<string>
#include<vector>
#include "oprule/expression/ExpressionNode.h"

#include "oprule/rule/ModelInterface.h"
#include "oprule/parser/NamedValueLookup.h"
#include "dsm2_interface_fortran.h"


extern bool nocase_cmp(const std::string&, const std::string&);
class DSM2ModelInterfaceResolver;


class GateInstallInterface : public oprule::rule::ModelInterface<double>{
public:
    typedef GateInstallInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    friend class DSM2ModelInterfaceResolver;
    GateInstallInterface(const int index) : ndx(index){};

    static NodePtr create(const int index){
        return NodePtr(new NodeType(index));
    }

    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx));
    }

    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return false; }
    //virtual void setDataExpression(
    //    oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~GateInstallInterface(){};
    virtual bool operator==( const GateInstallInterface &);

private:
    int ndx;
};


class DeviceInterface : public oprule::rule::ModelInterface<double> {
public:
    typedef DeviceInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    friend class DSM2ModelInterfaceResolver;
    DeviceInterface(const int index, const int device_ndx)
        : ndx(index), devndx(device_ndx){}


    virtual bool operator==( const DeviceInterface &rhs){
        return ndx == rhs.ndx && devndx == rhs.ndx;
    }

protected:
    int ndx;
    int devndx;
};

class DeviceOpInterface :public DeviceInterface{
public:
    typedef DeviceOpInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    friend class DSM2ModelInterfaceResolver;
    DeviceOpInterface(const int index,
        const int device_ndx,
        const int direct)
        : DeviceInterface(index,device_ndx),
        direction(direct){}

    static NodePtr create(const int index, const int dev_ndx, const int direct){
        return NodePtr(new NodeType(index,dev_ndx,direct));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx,direction));
    }

    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return true; }  //gotta finish this
    virtual void setDataExpression(
        oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~DeviceOpInterface(){};
    virtual bool operator==( const DeviceOpInterface &);
private:
    int direction;

};



class DevicePositionInterface :public DeviceInterface{
public:
    typedef DevicePositionInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    DevicePositionInterface(const int index, const int device_ndx)
        : DeviceInterface(index,device_ndx){}
    static NodePtr create(const int index, const int dev_ndx){
        return NodePtr(new NodeType(index,dev_ndx));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx));
    }


    friend class DSM2ModelInterfaceResolver;
    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return true; }  //gotta finish this
    virtual void setDataExpression(
        oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~DevicePositionInterface(){};
    virtual bool operator==( const DevicePositionInterface &);

};

class DeviceHeightInterface :public DeviceInterface{
public:
    typedef DeviceHeightInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    DeviceHeightInterface(const int index, const int device_ndx)
        : DeviceInterface(index,device_ndx){}
    static NodePtr create(const int index, const int dev_ndx){
        return NodePtr(new NodeType(index,dev_ndx));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx));
    }
    friend class DSM2ModelInterfaceResolver;
    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return true; }
    virtual void setDataExpression(
        oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~DeviceHeightInterface(){};
    virtual bool operator==( const DeviceHeightInterface &);

};


class DeviceWidthInterface :public DeviceInterface{
public:
    typedef DeviceWidthInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    DeviceWidthInterface(const int index, const int device_ndx)
        : DeviceInterface(index,device_ndx){}
    static NodePtr create(const int index, const int dev_ndx){
        return NodePtr(new NodeType(index,dev_ndx));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx));
    }
    friend class DSM2ModelInterfaceResolver;
    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return true; }
    virtual void setDataExpression(
        oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~DeviceWidthInterface(){};
    virtual bool operator==( const DeviceWidthInterface &);

};

class DeviceElevInterface :public DeviceInterface{
public:
    typedef DeviceElevInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    DeviceElevInterface(const int index, const int device_ndx)
        : DeviceInterface(index,device_ndx){}
    static NodePtr create(const int index, const int dev_ndx){
        return NodePtr(new NodeType(index,dev_ndx));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx));
    }


    friend class DSM2ModelInterfaceResolver;
    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return true; }
    virtual void setDataExpression(
        oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~DeviceElevInterface(){};
    virtual bool operator==( const DeviceElevInterface &);
};


class DeviceNDuplicateInterface :public DeviceInterface{
public:
    typedef DeviceNDuplicateInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    DeviceNDuplicateInterface(const int index, const int device_ndx)
        : DeviceInterface(index,device_ndx){}
    static NodePtr create(const int index, const int dev_ndx){
        return NodePtr(new NodeType(index,dev_ndx));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx));
    }


    friend class DSM2ModelInterfaceResolver;
    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return false; }
    virtual ~DeviceNDuplicateInterface(){};
    virtual bool operator==( const DeviceNDuplicateInterface &);

};


class DeviceFlowCoefInterface :public DeviceInterface{
public:
    typedef DeviceFlowCoefInterface NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    DeviceFlowCoefInterface(const int index, const int device_ndx, const int direct)
        : DeviceInterface(index,device_ndx), direction(direct){}
    static NodePtr create(const int index, const int dev_ndx, const int direct){
        return NodePtr(new NodeType(index,dev_ndx,direct));}
    virtual oprule::expression::DoubleNodePtr copy(){
        return NodePtr(new NodeType(ndx,devndx,direction));
    }
    friend class DSM2ModelInterfaceResolver;
    virtual void set(double);
    virtual double eval();
    virtual bool isTimeDependent() const{ return false; }
    virtual ~DeviceFlowCoefInterface(){};
    virtual bool operator==( const DeviceFlowCoefInterface &);
private:
    int direction;
};





#endif