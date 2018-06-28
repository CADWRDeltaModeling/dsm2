#ifndef oprule_parser_PTR_WRAPPER_H__
#define oprule_parser_PTR_WRAPPER_H__
#include "oprule/expression/ExpressionNode.h"

namespace oprule{
namespace parser{

template<typename NodeT>
class ptr_wrapper{

typedef typename OE_NODE_PTR(NodeT) NodePtrT;

public:
  ptr_wrapper(NodePtrT p) : ptr(p){}
  
  ptr_wrapper(){}

  NodePtrT operator->(){
    return ptr;
  }

  NodeT& operator*(){ return *ptr; }

  ptr_wrapper<NodePtrT> operator=(const ptr_wrapper<NodePtrT>& rhs){
    if(&rhs != this){
      this->ptr=rhs.ptr;
    }
    return *this;
  }

  ptr_wrapper<NodePtrT> operator=(const NodePtrT& rhs){
      this->ptr=rhs;
    return *this;
  }


private:
  ptr_wrapper(const ptr_wrapper<NodeT> &){} // copy not allowed
  NodePtrT ptr;

};

}} // namespace
#endif //include guard
