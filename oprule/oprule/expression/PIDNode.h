
#ifndef oprule_expression_PID_NODE_H__INCLUDED_
#define oprule_expression_PID_NODE_H__INCLUDED_

#include "oprule/expression/ValueNode.h"

namespace oprule{
namespace expression{


/**Node that represents a PID control expression
*/
class PIDNode : public ExpressionNode<double>{
public:
  typedef PIDNode NodeType;
  typedef OE_NODE_PTR(NodeType) NodePtr;
  typedef ExpressionNode<double> ExpressionNode;
  typedef OE_NODE_PTR(ExpressionNode) ExpressionNodePtr;

   /** Create the node given the expression node it will extrapolate.
    *@param node node to extrapolate
    *@param stepsProjected number of steps to project ahead
    */

  PIDNode(
          ExpressionNodePtr y,
          ExpressionNodePtr ySet,
		  double ulow,
		  double uhigh,
          double k,
          double ti,
          double td,
          double tt,
          double b) :
	    _y(y),
        _ySet(ySet), 
		_ulow(ulow),
		_uhigh(uhigh),
        _k(k),
		_ti(ti),
		_td(td),
        _tt(tt),
        _b(b),
		_n(10.),   // "typical values are 8-20"
        _d(0.),
        _bi(HUGE_VAL),
        _ad(HUGE_VAL),
        _ao(HUGE_VAL),
        P(0.),I(0.),D(0.),
        _yold(HUGE_VAL),_ynew(HUGE_VAL),_u(HUGE_VAL)
        {}
     
      virtual ExpressionNodePtr copy(){
        return NodePtr(new PIDNode(_y->copy(),
                      _ySet->copy(),
                       _ulow,
                       _uhigh,
                       _k,
                       _ti,
                       _td,
                       _tt,
                       _b));
                     
        }

     /**
      * Factory function that assumes the control is bound between 
      * zero and one
      */
     static NodePtr create(ExpressionNodePtr y, 
                           ExpressionNodePtr ySet,
						   double ulow,
						   double uhigh,
                           double k, 
                           double ti, 
                           double td, 
                           double tt,
                           double b){ 
       return NodePtr(new NodeType(y,ySet,ulow,uhigh,k,ti,td,tt,b));
     }


    void init(){
       _ynew=_y->eval();
       _yold=_ynew;
    }

     double eval(){ 
      assert(_yold != HUGE_VAL); //assure that initialize() has been called
      return _u;
     } 

    void step(double h){
      _ySet->step(h);
	  _y->step(h);
      _ynew    = _y->eval();
	  //std::cout << "_ynew: " << _ynew << " " << h << std::endl;
      
	  //todo: this is a horrible workaround for what is apparently initialize() not getting called
	  if (_yold == HUGE_VAL) _yold=_ynew;
      double ysp   = _ySet->eval();

	  //std::cout << "ysp " << ysp << std::endl;

	  _bi=(_k*h/_ti);

      _ad=((2.0*_td-_n*h)/(2.0*_td+_n*h));
      _bd=((2.0*_k*_n*_td)/(2.0*_td+_n*h));
      _ao=(h/_tt);
	  //std::cout << "_bi " << _bi <<" "<<_ad <<" " << _bd << " " <<  _ao << std::endl;


      double P = _k*(_b*ysp - _ynew );   //proportional gain
      D = _ad*D -_bd*(_ynew-_yold);    //update derivative
      double v= P + I + D;             //update gain
      _u = _sat(v,_ulow,_uhigh);  //update control
	  //std::cout << "_u " << _u << " "<< P <<" " << D << " " 
		   //<< _ulow << " " << _uhigh << " " << I << " "<< std::endl;

	  I += _bi*(ysp-_ynew)+_ao*(_u-v);  //update integral part
      _yold = _ynew;
     }



    virtual ~PIDNode(){
       OE_NODE_DELETE(_node);
    }


   virtual bool isTimeDependent() const {return true;}



private:
   void lagVals();
   ExpressionNodePtr _y,_ySet;
   double _ulow, _uhigh;
   double _k,_ti,_td,_tt,_b,_n,_d;
   double _bi,_ad,_bd,_ao;
   double P,I,D;
   double _ynew,_yold,_u;

   double _sat(double val, double low, double high){
     return val < low ? low : (val > high ? high : val);
   }

};

/////////////////////////////////////////////////////////////////
/**Node that represents incremental PID control
*/
class IncrementalPIDNode : public ExpressionNode<double>{
public:
  typedef IncrementalPIDNode NodeType;
  typedef OE_NODE_PTR(NodeType) NodePtr;
  typedef ExpressionNode<double> ExpressionNode;
  typedef OE_NODE_PTR(ExpressionNode) ExpressionNodePtr;

   /** Create the node given the expression node it will extrapolate.
    *@param node node to extrapolate
    *@param stepsProjected number of steps to project ahead
    */

  IncrementalPIDNode(
          ExpressionNodePtr y,
          ExpressionNodePtr ySet,
		  ExpressionNodePtr uApplied,
		  double ulow,
		  double uhigh,
          double k,
          double ti,
          double td,
          double b) :
	    _y(y),
        _ySet(ySet),
		_uApplied(uApplied),
		_ulow(ulow),
		_uhigh(uhigh),
        _k(k),
		_ti(ti),
		_td(td),
        _b(b),
		_n(10.),   // "typical values are 8-20"
        _d(0.),
        _bi(HUGE_VAL),
        _ad(HUGE_VAL),
        dP(0.),dI(0.),dD(0.),
        _yold(HUGE_VAL),_yoldPrev(HUGE_VAL),_ynew(HUGE_VAL),_yspOld(HUGE_VAL),_u(HUGE_VAL)
        {}
     
      virtual ExpressionNodePtr copy(){
        return NodePtr(new IncrementalPIDNode(_y->copy(),
                      _ySet->copy(),
					  _uApplied->copy(),
                       _ulow,
                       _uhigh,
                       _k,
                       _ti,
                       _td,
                       _b));
                     
        }

     /**
      * Factory function that assumes the control is bound between 
      * zero and one
      */
     static NodePtr create(ExpressionNodePtr y, 
                           ExpressionNodePtr ySet,
						   ExpressionNodePtr uApplied,
						   double ulow,
						   double uhigh,
                           double k, 
                           double ti, 
                           double td, 
                           double b){ 
       return NodePtr(new NodeType(y,ySet,uApplied,ulow,uhigh,k,ti,td,b));
     }


    void init(){
       _ynew=_y->eval();
       _yold=_ynew;
	   _yoldPrev = _yold;
	   _yspOld = _ySet->eval();
    }

     double eval(){ 
      assert(_yold != HUGE_VAL); //assure that initialize() has been called
      return _u;
     } 

    void step(double h){
      _ySet->step(h);
	  _y->step(h);
	  _uApplied->step(h);
      _ynew    = _y->eval();
	  //std::cout << "_ynew: " << _ynew << " " << h << std::endl;
      
	  //todo: this is a horrible workaround for what is apparently initialize() not getting called
	  if (_yold == HUGE_VAL) _yold=_ynew;
	  if (_yoldPrev == HUGE_VAL) _yoldPrev = _yold;
      
	  double ysp   = _ySet->eval();
      if(_yspOld == HUGE_VAL) _yspOld = ysp; 
	  //std::cout << "ysp " << ysp << std::endl;

	  _bi=(_k*h/_ti);

      _ad=((2.0*_td-_n*h)/(2.0*_td+_n*h));
      _bd=((2.0*_k*_n*_td)/(2.0*_td+_n*h));

      double dP = _k*(_b*ysp - _ynew ) - _k*(_b*_yspOld - _yold );   //proportional gain
      dD = _ad*dD -_bd*(_ynew-2.*_yold + _yoldPrev);    //update derivative
      dI = _bi*(ysp - _ynew);
	  double incr = dP + dI + dD;
	  double v = _uApplied->eval() + incr;   //update gain
	  
      _u = _sat(v,_ulow,_uhigh);  //update control
	  //std::cout << "_u " << _u << " "<< dP <<" " << dD << " " 
	  //	   << dI << " "<< std::endl;
      _yold = _ynew;
	  _yoldPrev = _yold;
      _yspOld = ysp;
	}



    virtual ~IncrementalPIDNode(){
       OE_NODE_DELETE(_node);
    }


   virtual bool isTimeDependent() const {return true;}



private:
   void lagVals();
   ExpressionNodePtr _y,_ySet,_uApplied;
   double _ulow, _uhigh;
   double _k,_ti,_td,_b,_n,_d;
   double _bi,_ad,_bd;
   double dP,dI,dD;
   double _ynew,_yold,_yoldPrev,_yspOld,_u;

   double _sat(double val, double low, double high){
     return val < low ? low : (val > high ? high : val);
   }

};

 }}    // namespace


#endif // include guard
