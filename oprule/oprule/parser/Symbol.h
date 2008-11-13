#ifndef oprule_SYMBOL_H__INCLUDED_
#define oprule_SYMBOL_H__INCLUDED_

#include<assert.h>
#include<string>
#include "oprule/expression/ExpressionNode.h"
#include <iostream>
namespace oprule{
   namespace parser{

/** Class to facilitate having a single type of named symbols. Basically this is 
    just a union, and it isn't very safe.
   @todo change this to something safer.*/
class symbol {
   typedef oprule::expression::BoolNodePtr  BoolNodePtr;
   typedef oprule::expression::DoubleNodePtr DoubleNodePtr;
public:
   enum type_id{ DOUBLE, BOOL, STRING, EMPTY };
   type_id _t;

   /** Create a symbol with no value */
   symbol(): _t(EMPTY){}
   /** Create a symbol that holds a double node*/
   symbol(oprule::expression::DoubleNodePtr dval) : dblval(dval), _t(DOUBLE){}
   /** Create a symbol that holds a bool node*/
   symbol(oprule::expression::BoolNodePtr bval) : boolval(bval), _t(BOOL){}
   /** Create a symbol that holds a string*/
   symbol(const std::string& name) : name(name), _t(STRING){}
   
   symbol& operator=(const symbol& rhs){
      if (&rhs != this){
         dblval=rhs.dblval;
         boolval=rhs.boolval;
         name=rhs.name;
      }
      return *this;
   }

   type_id type(){return _t;}

   /**value in case this symbol is a name.*/
   std::string name;
   /**value in case this symbol is a bool.*/
   oprule::expression::BoolNodePtr boolval;
   /**value in case this symbol is a double.*/
	oprule::expression::DoubleNodePtr dblval;

   void erase(){ 
      std::cout << "deleting bool" << std::endl;

      OE_NODE_DELETE(boolval);
      OE_NODE_DELETE(dblval);

      std::cout << "deleted" << std::endl;

   }

   /** Copy a symbol */
   symbol(const symbol& s) : name(s.name), 
                             boolval(s.boolval), 
                             dblval(s.dblval),
                             _t(s._t){}







};

}}// namespace oprule


#endif