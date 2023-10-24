#ifndef oprule_expression_TERNARYOPNODE_H__INCLUDED_
#define oprule_expression_TERNARYOPNODE_H__INCLUDED_

#include <boost/shared_ptr.hpp>
#include "oprule/expression/ExpressionNode.h"
namespace oprule{
namespace expression{

/** Ternary op that provides if-else evaluation.
 *  When asked to evaluate itself, the Node first evaluates a
 *  boolean node -- if the result is true it evaluates and returns one node,
 *  and if the result is false it returns the value from another node.
 */
template<typename T>
class TernaryOpNode : public ExpressionNode<T >
{
public:
    typedef TernaryOpNode<T> NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;
    typedef double ResultType; //@todo why hardwire?
    typedef OE_NODE_PTR(ExpressionNode<ResultType>) ResultNodePtr;

    typedef OE_NODE_PTR(ExpressionNode<ResultType>) BaseNodePtr;

    /** Create the node, using a boolean and two result nodes
    @param pIf node on which result is conditioned
    @param pLeft node to evaluate if condition is true
    @param pRight node to evaluate and return if condition is false
    */
    TernaryOpNode(
        BoolNodePtr pIf,
        ResultNodePtr pLeft,
        ResultNodePtr pRight)
        : _pIf(pIf), _pLeft (pLeft), _pRight (pRight),
        _timeDependent( pLeft->isTimeDependent()
        ||  pRight->isTimeDependent()
        ||  pIf ->isTimeDependent()  ){}



    virtual ~TernaryOpNode()
    {
        OE_NODE_DELETE(_pIf);
        OE_NODE_DELETE(_pLeft);
        OE_NODE_DELETE(_pRight);
    }

    static NodePtr create(BoolNodePtr pIf,
        ResultNodePtr pLeft,
        ResultNodePtr pRight)
    {
            return NodePtr(new NodeType(pIf,pLeft,pRight));
    }

    virtual BaseNodePtr copy(){
        return NodePtr(new NodeType(_pIf->copy(),
            _pLeft->copy(),
            _pRight->copy()));
    }

    virtual ResultType eval() {
        return _pIf->eval() ? _pLeft->eval(): _pRight->eval();
    }

    virtual bool isTimeDependent() const{return _timeDependent;}
    virtual void init(){
		_pIf->init();
        _pLeft->init();
        _pRight->init();
    }

    virtual void step(double dt){
		_pIf->step(dt);
        _pLeft->step(dt);
        _pRight->step(dt);
    }

private:
    ResultNodePtr const _pLeft;
    ResultNodePtr const _pRight;
    BoolNodePtr const _pIf;
    bool _timeDependent;
};


/** Ternary op that is max of three values.

*/
template<typename T>
class Max3Node : public ExpressionNode<double >
{
public:
    typedef Max3Node<T> NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;
    typedef T ArgType;
    typedef typename ExpressionNode<T>::NodePtr ArgNodePtr;
    typedef double ResultType;

    typedef OE_NODE_PTR(ExpressionNode<ResultType>) BaseNodePtr;

    /** Create the node, using three values
    @param pFirst node in comparison
    @param pSecond node in comparison
    @param pThird node in comparison
    */
    Max3Node(
        ArgNodePtr pFirst,
        ArgNodePtr pSecond,
        ArgNodePtr pThird)
        : _pFirst(pFirst), _pSecond (pSecond), _pThird (pThird),
        _timeDependent( pFirst->isTimeDependent()
        ||  pSecond->isTimeDependent()
        ||  pThird ->isTimeDependent()  ){}



    virtual ~Max3Node()
    {
        OE_NODE_DELETE(_pFirst);
        OE_NODE_DELETE(_pSecond);
        OE_NODE_DELETE(_pThird);
    }

    static NodePtr create(ArgNodePtr pFirst,
        ArgNodePtr pSecond,
        ArgNodePtr pThird){
            return NodePtr(new NodeType(pFirst,pSecond,pThird));
    }

    virtual BaseNodePtr copy(){
        return NodePtr(new NodeType(_pFirst->copy(),
            _pSecond->copy(),
            _pThird->copy()));
    }

    virtual ResultType eval() {
        T first= _pFirst->eval();
        T second=_pSecond->eval();
        T third = _pThird->eval();
        return first > second ?
            (first > third  ? first : third) :
            (second > third ? second : third);
    }

    virtual bool isTimeDependent() const{return _timeDependent;}
    virtual void init(){
        _pFirst->init();
        _pSecond->init();
        _pThird->init();
    }
    virtual void step(double dt){
        _pFirst->step(dt);
        _pSecond->step(dt);
        _pThird->step(dt);
    }

private:
    ArgNodePtr _pFirst;
    ArgNodePtr _pSecond;
    ArgNodePtr _pThird;
    bool _timeDependent;
};


/** Minimum of three values.
*/
template<typename T>
class Min3Node : public ExpressionNode<double>
{
public:
    typedef Min3Node NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;
    typedef T ArgType;
    typedef typename ExpressionNode<T>::NodePtr ArgNodePtr;
    typedef double ResultType;
    typedef ExpressionNode<ResultType>::NodePtr BaseNodePtr;

    /** Create the node, using three values
    @param pFirst node in comparison
    @param pSecond node in comparison
    @param pThird node in comparison
    */
    Min3Node(
        ArgNodePtr pFirst,
        ArgNodePtr pSecond,
        ArgNodePtr pThird)
        : _pFirst(pFirst), _pSecond (pSecond), _pThird (pThird),
        _timeDependent( pFirst->isTimeDependent()
        ||  pSecond->isTimeDependent()
        ||  pThird ->isTimeDependent()  ){}



    virtual ~Min3Node()
    {
        OE_NODE_DELETE(_pFirst);
        OE_NODE_DELETE(_pSecond);
        OE_NODE_DELETE(_pThird);
    }

    static NodePtr create(ArgNodePtr pFirst,
        ArgNodePtr pSecond,
        ArgNodePtr pThird){
            return NodePtr(new NodeType(pFirst,pSecond,pThird));
    }

    virtual BaseNodePtr copy(){
        return NodePtr(new NodeType(_pFirst->copy(),
            _pSecond->copy(),
            _pThird->copy()));
    }


    virtual ResultType eval() {
        ArgType first= _pFirst->eval();
        ArgType second=_pSecond->eval();
        ArgType third = _pThird->eval();
        return first < second ?
            (first < third  ? first : third) :
            (second < third ? second : third);
    }

    virtual bool isTimeDependent() const{return _timeDependent;}
    virtual void init(){
        _pFirst->init();
        _pSecond->init();
        _pThird->init();}
    virtual void step(double dt){
        _pFirst->step(dt);
        _pSecond->step(dt);
        _pThird->step(dt);
    }

private:
    ArgNodePtr _pFirst;
    ArgNodePtr _pSecond;
    ArgNodePtr _pThird;
    bool _timeDependent;
};


}}     // namespace
#endif // include guard

