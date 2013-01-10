from parser.generic import GenericScanner, GenericParser, \
     GenericASTBuilder, GenericASTTraversal
from parser.token import Token
from parser.ast import AST
class Scanner(GenericScanner):
    def __init__(self):
	GenericScanner.__init__(self)
    def tokenize(self,input):
	self.rv = []
	GenericScanner.tokenize(self,input)
	return self.rv
    def t_whitespacet(self,s):
	r'[ \t\r\n]+'
	pass
    def t_add_opt(self,s):
	r'\+|-'
	self.rv.append(Token(type=s,attr=s))
    def t_mul_opt(self,s):
	r'\*|\/'
	self.rv.append(Token(type='mulop',attr=s))
    def t_shift_opt(self,s):
	r'<<|>>'
	self.rv.append(Token(type=s,attr=s))
    def t_relational_opt(self,s):
	r'<(?!(<|=))|>(?!(>|=))|<=|>=|\!=|=='
	self.rv.append(Token(type = 'relop', attr = s))
    def t_logical_opt(self,s):
	r'and|or'
	self.rv.append(Token(type = 'logop', attr=s))
    def t_nott(self,s):
	r'not'
	self.rv.append(Token(type = 'not', attr=s))
    def t_groupt(self,s):
	r'\(|\)'
	self.rv.append(Token(type=s,attr=s))
    def t_separatort(self,s):
	r'[,]'
	self.rv.append(Token(type=s,attr=s))
    def t_numbert(self,s):
	r'(\d+(\.(\d+)?)?|\.(\d+)?)((e|E)(\+|-)?\d+)?'
	self.rv.append(Token(type='number',attr=s))
    def t_stringt(self,s):
	r'(\"|\')([^\"\'])*(\"|\')'
	self.rv.append(Token(type='string',attr=s))
    def t_bracketst(self,s):
	r'\[|\]|\(|\)'
	self.rv.append(Token(type=s,attr=s))
    def t_func_idt(self,s):
	r'[a-zA-Z_][\.a-zA-Z_0-9]*'
	self.rv.append(Token(type='id',attr=s))
    def error(selfs,s,pos):
	print "Lexical error at position %s in string %s"%(pos,s)
	raise "Lexical error"
def scan(x):
    scanner = Scanner()
    return scanner.tokenize(x)
from parser.ast import AST
class Parser(GenericASTBuilder):
    def __init__(self,start='bool_expr'):
	GenericASTBuilder.__init__(self, AST, start)
    def p_bool_exp(self, args):
	'''
	bool_expr        ::= bool_expr logop logic_expr
	bool_expr        ::= logic_expr
	logic_expr       ::= not relational_expr
	logic_expr       ::= relational_expr
	relational_expr  ::= arithmetic_expr relop arithmetic_expr
	relational_expr  ::= arithmetic_expr
	arithmetic_expr  ::= arithmetic_expr addop sum_expr
	addop            ::= +
	addop            ::= -
	arithmetic_expr  ::= sum_expr
	sum_expr         ::= sum_expr mulop mul_expr 
	sum_expr         ::= mul_expr
	mul_expr         ::= - factor
	mul_expr         ::= factor
	factor           ::= ( bool_expr )
	factor           ::= id
	factor           ::= number
	factor           ::= factor shift_op number
	factor           ::= id ( args )
	factor           ::= [ list ]
	list             ::= member , list
	list             ::= member
	member           ::= factor
	member           ::= - number
	member           ::= number
	shift_op         ::= <<
	shift_op         ::= >>
	args             ::= args , arg
	args             ::= arg
	args             ::=
	arg              ::= factor
	arg              ::= bool_expr
	arg              ::= string
	'''
	pass
    def terminal(self, token):
	#
	#  Homogeneous AST.
	#
	rv = AST(token.type)
	rv.attr = token.attr
	return rv
    
    def nonterminal(self, type, args):
	#
	return GenericASTBuilder.nonterminal(self, type, args)
    def error(self,token):
	print "Syntax error at or near `%s' token" % token
	raise "Syntax error"
#
def parse(tokens,start='bool_expr'):
    parser = Parser()
    return parser.parse(tokens,start)
#
def _dumpnode(root,indent_level=0):
    str = '' +' '*indent_level
    print str + repr(root)
    for x in root:
	if len(x) == 0:
	    print str + repr(x)
	else:
	    _dumpnode(x,indent_level+1)
#
_opmap = {}
	   
def _translate(root,str=''):
    rel = 0
    for x in root:
	if x.type == 'relop':
	    str = str + '.'+_opmap[x.attr]+'( '
	    rel = 1
	else:
	    if len(x) == 0:
		str = str + ' ' +x.attr
	    else:
		str = _translate(x,str)
    if rel: str = str + ' )'
    return str
#
