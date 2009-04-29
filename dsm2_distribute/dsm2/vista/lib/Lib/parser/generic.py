#  Copyright (c) 1998-1999 John Aycock
#  
#  Permission is hereby granted, free of charge, to any person obtaining
#  a copy of this software and associated documentation files (the
#  "Software"), to deal in the Software without restriction, including
#  without limitation the rights to use, copy, modify, merge, publish,
#  distribute, sublicense, and/or sell copies of the Software, and to
#  permit persons to whom the Software is furnished to do so, subject to
#  the following conditions:
#  
#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.
#  
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
#  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
#  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import re
import sys
import string

def _update_from_map(dict,map_or_dict):
    for x in map_or_dict.keys():
	dict[x] = map_or_dict[x]
def _namelist(instance):
	namedict, classlist = {}, [instance.__class__]
	for c in classlist:
		for b in c.__bases__:
			classlist.append(b)
		_update_from_map(namedict,c.__dict__)
		#namedict.update(c.__dict__)
	return namedict.keys()

class GenericScanner:
	def __init__(self):
		pattern = self.reflect()
		self.re = re.compile(pattern, re.VERBOSE)

		self.index2func = {}
		for name, number in self.re.groupindex.items():
			self.index2func[number-1] = getattr(self, 't_' + name)

	def makeRE(self, name):
		doc = string.strip(getattr(self, name).__doc__)
		rv = '(?P<%s>(%s))' % (name[2:], doc)
		return rv

	def reflect(self):
		rv = []
		for name in _namelist(self):
			if name[:2] == 't_' and name != 't_default':
				rv.append(self.makeRE(name))

		rv.append(self.makeRE('t_default'))
		return string.join(rv, '|')

	def error(self, s, pos):
		print "Lexical error at position %s" % pos
		raise SystemExit

	def tokenize(self, s):
		pos = 0
		n = len(s)
		while pos < n:
			m = self.re.match(s, pos)
			if m is None:
				self.error(s, pos)
			groups = m.groups()
			for i in range(len(groups)):
				if groups[i] and self.index2func.has_key(i):
					self.index2func[i](groups[i])
			pos = m.end()

	def t_default(self, s):
		r'( . | \n )+'
		pass

class GenericParser:
	def __init__(self, start):
		self.rules, self.rule2func, self.rule2name = self.collectRules()
		self.startRule = self.augment(start)

	_START = 'START'
	_EOF = 'EOF'

	#
	#  A hook for GenericASTBuilder and GenericASTMatcher.
	#
	def preprocess(self, rule, func):	return rule, func

	def collectRules(self):
		rv = {}
		r2f, r2n = {}, {}
		for name in _namelist(self):
			if name[:2] == 'p_':
				func = getattr(self, name)
				doc = func.__doc__
				rules = string.split(doc)

				index = []
				for i in range(len(rules)):
					if rules[i] == '::=':
						index.append(i-1)
				index.append(len(rules))

				for i in range(len(index)-1):
					lhs = rules[index[i]]
					rhs = rules[index[i]+2:index[i+1]]
					rule = (lhs, tuple(rhs))

					rule, fn = self.preprocess(rule, func)

					if rv.has_key(lhs):
						rv[lhs].append(rule)
					else:
						rv[lhs] = [ rule ]
					r2f[rule] = fn
					r2n[rule] = name[2:]
		return rv, r2f, r2n

	def augment(self, start):
		startRule = (self._START, ( start, self._EOF ))
		self.rule2func[startRule] = lambda args: args[0]
		self.rules[self._START] = [ startRule ]
		self.rule2name[startRule] = ''
		return startRule

	#
	#  An Earley parser, as per J. Earley, "An Efficient Context-Free
	#  Parsing Algorithm", CACM 13(2), pp. 94-102.
	#
	
	def error(self, token):
		print "Syntax error at or near `%s' token" % token
		raise SystemExit

	def parse(self, tokens):
		tree = {}
		tokens.append(self._EOF)
		states = { 0: [ (self.startRule, 0, 0) ] }
		
		for i in xrange(len(tokens)):
			states[i+1] = []

			if states[i] == []:
				break				
			self.buildState(tokens[i], states, i, tree)

		if i < len(tokens)-1 or states[i+1] != [(self.startRule, 2, 0)]:
			del tokens[-1]
			self.error(tokens[i-1])
		rv = self.buildTree(tokens, tree, ((self.startRule, 2, 0), i+1))
		del tokens[-1]
		return rv
	
	def buildState(self, token, states, i, tree):
		state = states[i]
		predicted = {}
		
		for item in state:
			rule, pos, parent = item
			lhs, rhs = rule

			#
			#  A -> a . (completer)
			#
			if pos == len(rhs):
				for pitem in states[parent]:
					prule, ppos, pparent = pitem
					plhs, prhs = prule

					if prhs[ppos:ppos+1] == (lhs,):
						new = (prule,
						       ppos+1,
						       pparent)
						if new not in state:
							state.append(new)
							tree[(new, i)] = [(item, i)]
						else:
							tree[(new, i)].append((item, i))
				continue

			nextSym = rhs[pos]

			#
			#  A -> a . B (predictor)
			#
			if self.rules.has_key(nextSym):
				#
				#  Has this been predicted already?
				#
				if predicted.has_key(nextSym):
					continue

				predicted[nextSym] = 1
				for prule in self.rules[nextSym]:
					#
					#  Smarter predictor, as per Grune &
					#  Jacobs' _Parsing Techniques_.  Not
					#  as good as FIRST sets though.
					#
					prhs = prule[1]
					if len(prhs) > 0 and \
					   not self.rules.has_key(prhs[0]) and \
					   token != prhs[0]:
						continue
					state.append((prule, 0, i))

			#
			#  A -> a . c (scanner)
			#
			elif token == nextSym:
				#assert new not in states[i+1]
				states[i+1].append((rule, pos+1, parent))

	def buildTree(self, tokens, tree, root):
		stack = []
		self.buildTree_r(stack,
				 tokens, -1,
				 tree, root)
		return stack[0]

	def buildTree_r(self, stack, tokens, tokpos, tree, root):
		item, state = root
		rule, pos, parent = item
		lhs, rhs = rule
		
		while pos > 0:
			want = ((rule, pos, parent), state)
			if not tree.has_key(want):
				#
				#  Since pos > 0, it didn't come from closure,
				#  and if it isn't in tree[], then there must
				#  be a terminal symbol to the left of the dot.
				#  (It must be from a "scanner" step.)
				#
				pos = pos - 1
				state = state - 1
				stack.insert(0, tokens[tokpos])
				tokpos = tokpos - 1
			else:
				#
				#  There's a NT to the left of the dot.
				#  Follow the tree pointer recursively (>1
				#  tree pointers from it indicates ambiguity).
				#  Since the item must have come about from a
				#  "completer" step, the state where the item
				#  came from must be the parent state of the
				#  item the tree pointer points to.
				#
				children = tree[want]
				if len(children) > 1:
					child = self.ambiguity(children)
				else:
					child = children[0]
				
				tokpos = self.buildTree_r(stack,
							  tokens, tokpos,
							  tree, child)
				pos = pos - 1
				citem, cstate = child
				crule, cpos, cparent = citem
				state = cparent
				
		result = self.rule2func[rule](stack[:len(rhs)])
		stack[:len(rhs)] = [result]
		return tokpos

	def ambiguity(self, children):
		#
		#  XXX - problem here and in collectRules() if the same
		#	 rule appears in >1 method.  But in that case the
		#	 user probably gets what they deserve :-)  Also
		#	 undefined results if rules causing the ambiguity
		#	 appear in the same method.
		#
		sortlist = []
		name2index = {}
		for i in range(len(children)):
			((rule, pos, parent), index) = children[i]
			lhs, rhs = rule
			name = self.rule2name[rule]
			sortlist.append((len(rhs), name))
			name2index[name] = i
		sortlist.sort()
		list = map(lambda (a,b): b, sortlist)
		return children[name2index[self.resolve(list)]]

	def resolve(self, list):
		#
		#  Resolve ambiguity in favor of the shortest RHS.
		#  Since we walk the tree from the top down, this
		#  should effectively resolve in favor of a "shift".
		#
		return list[0]

#
#  GenericASTBuilder automagically constructs a concrete/abstract syntax tree
#  for a given input.  The extra argument is a class (not an instance!)
#  which supports the "__setslice__" and "__len__" methods.
#
#  XXX - silently overrides any user code in methods.
#

class GenericASTBuilder(GenericParser):
	def __init__(self, AST, start):
		GenericParser.__init__(self, start)
		self.AST = AST

	def preprocess(self, rule, func):
		rebind = lambda lhs, self=self: \
				lambda args, lhs=lhs, self=self: \
					self.buildASTNode(args, lhs)
		lhs, rhs = rule
		return rule, rebind(lhs)

	def buildASTNode(self, args, lhs):
		children = []
		for arg in args:
			if isinstance(arg, self.AST):
				children.append(arg)
			else:
				children.append(self.terminal(arg))
		return self.nonterminal(lhs, children)

	def terminal(self, token):	return token

	def nonterminal(self, type, args):
		rv = self.AST(type)
		rv[:len(args)] = args
		return rv

#
#  GenericASTTraversal is a Visitor pattern according to Design Patterns.  For
#  each node it attempts to invoke the method n_<node type>, falling
#  back onto the default() method if the n_* can't be found.  The preorder
#  traversal also looks for an exit hook named n_<node type>_exit (no default
#  routine is called if it's not found).  To prematurely halt traversal
#  of a subtree, call the prune() method -- this only makes sense for a
#  preorder traversal.
#

class GenericASTTraversalPruningException:
	pass

class GenericASTTraversal:
	def __init__(self, ast):
		self.ast = ast

	def prune(self):
		raise GenericASTTraversalPruningException

	def preorder(self, node=None):
		if node is None:
			node = self.ast

		try:
			name = 'n_' + node.type
			if hasattr(self, name):
				func = getattr(self, name)
				func(node)
			else:
				self.default(node)
		except GenericASTTraversalPruningException:
			return

		for kid in node:
			self.preorder(kid)

		name = name + '_exit'
		if hasattr(self, name):
			func = getattr(self, name)
			func(node)

	def postorder(self, node=None):
		if node is None:
			node = self.ast

		for kid in node:
			self.postorder(kid)

		name = 'n_' + node.type
		if hasattr(self, name):
			func = getattr(self, name)
			func(node)
		else:
			self.default(node)


	def default(self, node):
		pass

#
#  GenericASTMatcher.  AST nodes must have "__getitem__" and "__cmp__"
#  implemented.
#
#  XXX - makes assumptions about how GenericParser walks the parse tree.
#

class GenericASTMatcher(GenericParser):
	def __init__(self, start, ast):
		GenericParser.__init__(self, start)
		self.ast = ast

	def preprocess(self, rule, func):
		rebind = lambda func, self=self: \
				lambda args, func=func, self=self: \
					self.foundMatch(args, func)
		lhs, rhs = rule
		rhslist = list(rhs)
		rhslist.reverse()

		return (lhs, tuple(rhslist)), rebind(func)

	def foundMatch(self, args, func):
		func(args[-1])
		return args[-1]

	def match_r(self, node):
		self.input.insert(0, node)
		children = 0

		for child in node:
			if children == 0:
				self.input.insert(0, '(')
			children = children + 1
			self.match_r(child)

		if children > 0:
			self.input.insert(0, ')')

	def match(self, ast=None):
		if ast is None:
			ast = self.ast
		self.input = []

		self.match_r(ast)
		self.parse(self.input)

	def resolve(self, list):
		#
		#  Resolve ambiguity in favor of the longest RHS.
		#
		return list[-1]
