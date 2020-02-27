#ifndef oprule_parser_NAMEDVALUELOOKUP_H__INCLUDED_
#define oprule_parser_NAMEDVALUELOOKUP_H__INCLUDED_

#include "oprule/expression/ValueNode.h"
#include "oprule/rule/ModelInterface.h"
#include<string>
#include<vector>
#include<map>


namespace oprule{
namespace parser{

/** Factory to convert text descriptions into model expressions and interfaces.
 *  This factory must be subclassed by the model developer in order to
 *  use the op rule parsing package in a new model, perhaps using the default
 *  implementation NamedValueLookupImpl. A "model name" is a vague term, and
 *  this is deliberate. You can throw in anything that your model understands and
 *  name it anything you want. Examples 
 *   (name=DT, no parameters, evaluates a (fixed or variable) time step)
 *   (name=chan_flow, parameters (chan=128, dist=1223), evaluates a read-only model state
 *   (name=mtz, no parameters, evaluates to a time series managed by the model
 *   (name=external_flow, parameters(name="sjr") evaluates to a read-write model interface
 */
class NamedValueLookup  
{
public:
   typedef oprule::expression::DoubleNode DoubleNode;    //@todo get rid of hardwired double
   typedef oprule::expression::DoubleNodePtr DoubleNodePtr;

   

   /** type used for argument lists */
   typedef std::vector<std::string> ArgList;
   typedef std::map<std::string,std::string> ArgMap;

   NamedValueLookup(){};

   virtual ~NamedValueLookup(){};


   enum NameReadWriteType{ READONLY, READWRITE };

   /** Test whether a name is understood by the model. Most of the
    * member functions assume a name is a model name and throw an exception
    * if it is not, so the program idiom should be to call this method first.
    * Implementation note: looking up a name with this function
    * will pre-fetch the information for the name internally so subsequent operations
    * on the same name will be efficient.
    * @param name the name to test.
    * @return true if the model can interpret this name
    */
    virtual bool isModelName(const std::string& name)=0;

   /** Test whether a model name requires arguments.
    * Assumes that the model name is valid: see isModelName().
    * @return true if the model name requires a list of arguments
    */
    virtual bool takesArguments(const std::string& name)=0;


    /** Discover whether the node produced by the name is READWRITE or READONLY
     */ 
    virtual NameReadWriteType readWriteType(const std::string& name)=0;
     
   
   /**
    * Create a (read only ExpressionNode) which evaluates the current or
    * constant value of the model variable specified by name. Name is assumed
    * to exist and this should be verified with isModelName.
    * @param name The model name that is to be retrieved.
    * @param identifiers An argument map giving identifier labels and values.
    * @return an ExpressionNode representing the given name that can evaluate the model variable refered to by identifiers
    */
    virtual oprule::expression::DoubleNodePtr 
       getModelExpression(const std::string & name, 
       const ArgMap& identifiers )=0;

   /**
    * Create a (read-write ModelInterface) which evaluates the current or
    * constant value of the model variable specified by name. Name is assumed
    * to exist and this should be verified with isModelName. The fact that it
    * is read-write should also be verified using
    * @param name The model name that is to be retrieved.
    * @param identifiers An argument map giving identifier labels and values.
    * @return an ExpressionNode representing the given name that can evaluate the model variable refered to by identifiers
    */
    virtual oprule::rule::ModelInterface<double>::NodePtr 
      getModelInterface(const std::string & name, const ArgMap& identifiers) = 0;
   
};

}} //namespace   
#endif // include guard