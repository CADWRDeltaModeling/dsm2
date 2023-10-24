#ifndef oprule_parser_NAMEDVALUELOOKUP_H__INCLUDED_
#define oprule_parser_NAMEDVALUELOOKUP_H__INCLUDED_

#include "oprule/expression/ValueNode.h"
#include "oprule/rule/ModelInterface.h"
#include<string>
#include<deque>


namespace oprule{
namespace parser{

/** Factory to convert text descriptions into model expressions and interfaces.
 *  This factory must be subclassed by the model developer in order to
 *  use the op rule parsing package in a new model.
 *  The methods of this class are used to convert text string descriptions into
 *  expressions and model interfaces.
 */
class NamedValueLookup
{
public:
   /** type used for argument lists */
   typedef oprule::expression::DoubleNode DoubleNode;    //@todo get rid of hardwired double
   typedef oprule::expression::DoubleNodePtr DoubleNodePtr;
   typedef std::deque<std::string> ArgListType;
   NamedValueLookup(){};

   virtual ~NamedValueLookup(){};
   /** Create a node using a name.
    *  The name should make sense in the context
    *  of the model you are working with. It can be a parameter
    *  parameter of the model or a node that the model
    *  can retrieve with a single name, such as a node that refers to
    *  a named time series. The routine was not designed for state variables --
    *  use getModelVariableNode for that.
    * @param name The name that the model can convert to an expression
    * @return An expression node (ideally, optimized)
    */
   virtual oprule::expression::DoubleNodePtr getNamedValue(const std::string& name)=0;

   /** Test whether a name is understood by the model. May involve a long search
    * @param name the name to test.
    * @return true if the model can interpret this name
   */
   virtual bool isModelName(const std::string& name)=0;

   /**
    * Create a node that evaluates to the current value of a model variable.
    * @param identifiers A list of identifiers that specifies the model variable.
    * The syntax the parser expects from the user is:
    * <p><t>\p general_name(specifier1="Something", specifier2=3...)
    * <p>For example:
    * <p><t>\p chan_flow(channel=132, distance=2999)
    * <p>The parser will deliver the identifiers as follows:
    *   - identifiers[0]="chan_flow"
    *   - identifiers[1]="channel"
    *   - identifiers[2]="132"
    *   - identifiers[3]="distance"
    *   - identifiers[4]="2999"
    * The model provider should write this function so that it takes such a list and
    * returns an appropriate expression
    * @return an expression that can evaluate the model variable refered to by identifiers
    * @todo error handling
    */
   virtual oprule::expression::DoubleNodePtr getModelVariableNode(const ArgListType& identifiers )=0;
   virtual oprule::rule::ModelInterface<double>::NodePtr
      getModelInterface(const ArgListType& identifiers) = 0;


};

}} //namespace
#endif // include guard