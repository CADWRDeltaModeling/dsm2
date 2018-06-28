#ifndef oprule_expression_LAGGED_VALUE_HOLDER_H__INCLUDED_
#define oprule_expression_LAGGED_VALUE_HOLDER_H__INCLUDED_


#include <vector>
#include <assert.h>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"


namespace oprule{
namespace expression{

/** Class that takes care of the details of holding lagged values.
 *  Takes care of remembering past values, given that the method newVal
 *  is implemented to obtain the current value. Intended as a superclass.
 */
template<typename T>
class LaggedValueHolder{
 public :

  /** Create a lagged value holder capable of holding lagged values
   * @param lags number of lags to store.
   */
  LaggedValueHolder(int lags) :
     nlag(lags),
     heldVals(lags+1){} //vector holding lagged values
  
  virtual ~LaggedValueHolder(){}
  
  /** sets the value of the nth lag. intended for initialization 
   *@param lag to set. Current step is lag zero.
   *@param val value to be set at lag.
   */
  void set(int lag, T val){ 
    heldVals[lag]=val; 
  }

  /** get the value at the given lag.
   *@param lag to retrieve. Current step is lag zero.
   *@return lagged value.
   */
  T getLaggedValue(int lag){
    assert(heldVals.size() == (nlag+1));
    return heldVals[nlag-lag];
  }
  
  /**Return the number of lags this holder can store
   *@return number of stored lags
  */
  int maxLag(){
    return nlag;
  }
   
  /**Implementation-dependent way of obtaining a new value.
   *@todo is this used?
   */
  virtual T newVal()=0;

  /**Tell the lagged value holder that a step has gone by.
   * The lagged values will be adjusted
   * @todo Bet on memory copying being faster than calculation. Correct? Optimize?
   */
  void advanceStep(){
    heldVals.push_back(newVal());
    std::vector<T> newHeldVals(nlag+1);

    newHeldVals.assign( heldVals.begin() +1, heldVals.end() );
    heldVals.swap(newHeldVals);
  }
 

 protected:
  const int nlag;
  std::vector<T> heldVals;

};

 }}    // namespace
#endif // include guard

