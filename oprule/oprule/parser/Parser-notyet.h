// Parser.h: interface for the Parser class.
//
//////////////////////////////////////////////////////////////////////

#ifndef PARSER_H__DAD4FB2A_93A9_BFCAD4DE6A0E__INCLUDED_
#define PARSER_H__DAD4FB2A_93A9_BFCAD4DE6A0E__INCLUDED_


class Parser  
{
public:
	Parser();
	virtual ~Parser();
   enum parse_type{
     BOOL_EXPRESS=0,
     NUMERICAL_EXPRESS=1,
     OP_RULE=2,
     REASSIGNMENT=3
   };
};

#endif // include guard