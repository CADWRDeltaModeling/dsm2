#ifndef oprule_rule_MODELINTERFACE_H_INCLUDED_
#define oprule_rule_MODELINTERFACE_H_INCLUDED_
#include <stdexcept>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"
#include "oprule/expression/ValueNode.h"



namespace oprule {
namespace rule {
using oprule::expression::ExpressionNode;

template<class T>
class ModelInterface;

/** Interface to a controllable model component.
* A ModelInterface represents a read-write interface to some
* component of the model. Typically a controllable model
* component is either a boundary condition or a operable device.
* The critical methods that this interface provides are:
* 1. A method of reading or "getting" the value of the model variable.
*    -if this is a static model variable, it gets the current value
*    -if this is a value set by a data source, it is meant to get the
*     value of the variable before it is applied to the model.
* 2. A method of writing or "setting" the value on a one-time basis
* 3. A method of permanently attaching a value or time-varying expression
*    as a datasource to the variable, if such an attachment is sensible.
*
*/
template<typename T>
class ModelInterface : public oprule::expression::ExpressionNode<T>{

public:
    typedef ModelInterface<T> NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    /** Type of the state variable that this interface reads and writes.*/
    typedef T StateType;

    //ModelInterface() {}


    /** Evaluate the model component.
    * Evaluate the value of the variable to which this is an interface.
    */
    virtual StateType eval()=0;


    /**
    * Set the value of the model component
    */
    virtual void set(StateType val)=0;


    /**
    * Set the provided expression as the permanent
    * source of data for the model variable underlain by this interface.
    * This method will throw an exception if the interface model variable
    * cannot take an expression as a data source. Should be implemented if
    * isTimeDependent() returns true for this variable. If the variable is
    * one that CAN take a time series, but the expression in the argument happens
    * to be static, this implementation of this method may exploit the simplicity
    * of the expression.
    */
    virtual void setDataExpression( typename ExpressionNode<StateType>::NodePtr val  ){
        throw new std::logic_error("Expression could not be set as permanent data source.");
    }


    /** Test for equality with another ModelInterface */
    virtual bool operator==( const ModelInterface<T> & other){ return false; }

    /**
    * Tests whether this is the interface to a time dependent variable.
    * Returns true if the variable being manipulated is time varying
    * (e.g. a boundary condition object controlled by a time series). The
    * test is intended to reflect the way the model treats the underlying
    * variable, not the details of a specific model run.
    * For instance, if a boundary condition can take
    * a time series as a data source but is set to a constant valued data source
    * for a particular simulation, the variable is still time-dependent.
    * If this method returns true, some non-trivial implementation of
    * setDataExpression is expected for the variable.
    * @return true if the model variable is time-varying.
    */
    virtual bool isTimeDependent()const=0; //{return !isStaticParameter(); }

};


/** Shorthand for a model interface manipulating a double
*/
typedef ModelInterface<double> ModelInterfaceDouble;

}}     //namespace
#endif // include guard
