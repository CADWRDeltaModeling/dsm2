#ifndef DSM2_MODEL_INTERFACE_GATE_H_INCLUDED
#define DSM2_MODEL_INTERFACE_GATE_H_INCLUDED
#pragma warning(disable:4786)

#include <assert.h>
#include<sstream>
#include<string>
#include<vector>
#include "oprule/expression/ExpressionNode.h"

#include "oprule/rule/ModelInterface.h"
#include "oprule/rule/LogTypes.h"
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
    virtual std::string describe() const {
        std::ostringstream s; s << "gate_install(gate=" << ndx << ")"; return s.str();
    }
    virtual unsigned deviceProperties(int& gate, int& device) const {
        gate = ndx; device = 0; return 1u << oprule::rule::PROP_INSTALL;
    }

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
    // Fortran array indices (1-based); names are not kept by the interface.
    std::string id(const char* name) const {
        std::ostringstream s; s << name << "(gate=" << ndx << ",device=" << devndx; return s.str();
    }
    static std::string direction_name(int d) {
        if (d == direct_to_node()) return "to_node";
        if (d == direct_from_node()) return "from_node";
        if (d == direct_to_from_node()) return "to_from_node";
        std::ostringstream s; s << d; return s.str();
    }
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
    virtual std::string describe() const {
        return id("gate_op") + ",direction=" + direction_name(direction) + ")";
    }
    virtual unsigned deviceProperties(int& gate, int& device) const {
        gate = ndx; device = devndx;
        if (direction == direct_to_node()) return 1u << oprule::rule::PROP_OP_TO_NODE;
        if (direction == direct_from_node()) return 1u << oprule::rule::PROP_OP_FROM_NODE;
        if (direction == direct_to_from_node())
            return (1u << oprule::rule::PROP_OP_TO_NODE) | (1u << oprule::rule::PROP_OP_FROM_NODE);
        return 0;
    }
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
    virtual std::string describe() const { return id("gate_position") + ")"; }

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
    virtual std::string describe() const { return id("gate_height") + ")"; }
    virtual unsigned deviceProperties(int& gate, int& device) const {
        gate = ndx; device = devndx; return 1u << oprule::rule::PROP_HEIGHT;
    }

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
    virtual std::string describe() const { return id("gate_width") + ")"; }
    virtual unsigned deviceProperties(int& gate, int& device) const {
        gate = ndx; device = devndx; return 1u << oprule::rule::PROP_WIDTH;
    }

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
    virtual std::string describe() const { return id("gate_elev") + ")"; }
    virtual unsigned deviceProperties(int& gate, int& device) const {
        gate = ndx; device = devndx; return 1u << oprule::rule::PROP_ELEV;
    }
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
    virtual bool isTimeDependent() const{ return true; }
    virtual void setDataExpression(
        oprule::expression::ExpressionNode<double>::NodePtr express);
    virtual ~DeviceNDuplicateInterface(){};
    virtual bool operator==( const DeviceNDuplicateInterface &);
    virtual std::string describe() const { return id("gate_nduplicate") + ")"; }
    virtual unsigned deviceProperties(int& gate, int& device) const {
        gate = ndx; device = devndx; return 1u << oprule::rule::PROP_NDUPLICATE;
    }

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
    virtual std::string describe() const {
        return id("gate_coef") + ",direction=" + direction_name(direction) + ")";
    }
private:
    int direction;
};





#endif