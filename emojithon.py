#######################################
# Code largely inspired and guided by the work by David Callanan 
# in his youtube series "Make YOUR OWN Programming Language"
# There were a large number of changes I had to make, including:
#   - Adding support for variable assignment without keywords
#   - Adding a "bracket compound" node type to support multiple nested statements in conditionals
#   - Implementing Skip
# 	- Implementing Backet support
# 	- Implementing Lists
#   - Allowing access for undefined variables
#   - Changing a bunch of syntax
# 	- Extra built-in functions
# 	- dicts
# And of course, implementing all this made me deeply familiar with the base code
# Lots of fun!
#######################################

import string

# CONSTANTS
## Get symbols to use for lexer
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS + '¬'

#####################################################################################################################
# ERRORS
## Define error parent class to help with better debugging
class Error:
	# would be useful to have the start and end positions of the errors, the error type and the details
	def __init__(self, pos_start, pos_end, error_name, details):
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.error_name = error_name
		self.details = details
	# formatting function
	def as_string(self):
		result  = f'{self.error_name}: {self.details}\n'
		result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result
# Sub classes for errors
# illegal character			-> invalid character type passed
# invalid syntax			-> valid character, but not what the parser is expecting
# runtime error				-> error encountered in runtime, e.g. adding a list to a number
class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Illegal Character', details)

class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.context

		while ctx:
			result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent

		return 'Traceback (most recent call last):\n' + result

# POSITION
## need to track the position of everything
## useful for lexer
class Position:
	def __init__(self, idx, ln, col, fn, ftxt):
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt

	def advance(self, current_char=None):
		self.idx += 1
		self.col += 1

		if current_char == '\n':
			self.ln += 1
			self.col = 0

		return self

	def copy(self):
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

#####################################################################################################################
# TOKENS
## Here's where we define preprovided token types, useful for the lexer and just cleans everything up a bit

TT_INT				= 'INT'
TT_FLOAT			= 'FLOAT'
TT_IDENTIFIER		= 'IDENTIFIER'
TT_KEYWORD			= 'KEYWORD'
TT_PLUS				= 'PLUS'
TT_MINUS			= 'MINUS'
TT_MUL				= 'MUL'
TT_DIV				= 'DIV'
TT_POW				= 'POW'
TT_ASSIGN			= 'ASSIGN'
TT_EQ				= 'EQ'
TT_LPAREN			= 'LPAREN'
TT_RPAREN			= 'RPAREN'
TT_LBRAC			= 'LBRAC'
TT_RBRAC			= 'RBRAC'
TT_LSQUARE          = 'LSQUARE'
TT_RSQUARE          = 'RSQUARE'
TT_EE				= 'EE'
TT_NE				= 'NE'
TT_LT				= 'LT'
TT_GT				= 'GT'
TT_LTE				= 'LTE'
TT_GTE				= 'GTE'
TT_EOF				= 'EOF'
TT_SEMI				= 'SEMI'
TT_COMMA			= 'COMMA'
TT_ARROW			= 'ARROW'
TT_STRING			= 'STRING'
TT_NEWLINE			= 'NEWLINE'

# These are the keywords in our language
# Note that built-in function definitons are added to our global symbol table at runtime,
# are named identifiers by the lexer, and are picked up in the call() level of the parser
KEYWORDS = [
	'∧',
	'∨',
	'¬',
	'for',
	'to',
	'step',
	'if',
	'else',
	'while',
	'then', 
	'do',
	'true',
	'false', 
	'skip',
	'func',
	'map'
]

## A token class that is used by the lexer. It tracks start and end positions, 
## the type and value of a token, and has a helper function to determine if a token matches one of interest
class Token:
	def __init__(self, type_, value=None, pos_start=None, pos_end=None):
		self.type = type_
		self.value = value

		if pos_start:
			self.pos_start = pos_start.copy()
			self.pos_end = pos_start.copy()
			self.pos_end.advance()

		if pos_end:
			self.pos_end = pos_end.copy()

	def matches(self, type_, value):
		return self.type == type_ and self.value == value
	
	def __repr__(self):
		if self.value: return f'{self.type}:{self.value}'
		return f'{self.type}'

#####################################################################################################################
# LEXER
# generates a list of tokens given text
class Lexer:
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.current_char = None
		self.advance()
	# helper function for advancing the current position and current character
	def advance(self):
		self.pos.advance(self.current_char)
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
	# Helper function for peeking ahead in the case of multi-char tokens that share starting chars
	# (e.g. = vs ==)
	def peek(self):
		peek_pos = self.pos.idx + 1
		if peek_pos > len(self.text) - 1:
			return None
		else:
			return self.text[peek_pos]
	# meat and potatoes
	def make_tokens(self):
		# start off with an empty list
		tokens = []
		# check for different character types, deal with each accordingly
		while self.current_char != None:
			# ignore whitespace
			if self.current_char in ' \t':
				self.advance()
			if self.current_char in '\n':
				tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
				self.advance()
			# If it's a digit, make a number
			# can't start variable names with numbers in Emojithon
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			# identifiers
			elif self.current_char in LETTERS or self.current_char in ('¬', '∧','∨'):
				tokens.append(self.make_identifier())
			# adder
			elif self.current_char == '+':
				tokens.append(Token(TT_PLUS, pos_start=self.pos))
				self.advance()
			# statement separator 
			elif self.current_char == ';':
				tokens.append(Token(TT_SEMI, pos_start=self.pos))
				self.advance()
			# arrow (for function body)
			elif self.current_char == '-' and self.peek() == '>':
				self.advance()
				tokens.append(self.make_arrow())
			# minus
			elif self.current_char == '-':
				tokens.append(Token(TT_MINUS, pos_start=self.pos))
				self.advance()
			# multiply
			elif self.current_char == '*':
				tokens.append(Token(TT_MUL, pos_start=self.pos))
				self.advance()
			# divide
			elif self.current_char == '/':
				tokens.append(Token(TT_DIV, pos_start=self.pos))
				self.advance()
			# power
			elif self.current_char == '^':
				tokens.append(Token(TT_POW, pos_start=self.pos))
				self.advance()
			# left parenths
			elif self.current_char == '(':
				tokens.append(Token(TT_LPAREN, pos_start=self.pos))
				self.advance()
			# right parenths
			elif self.current_char == ')':
				tokens.append(Token(TT_RPAREN, pos_start=self.pos))
				self.advance()
			# left curly brace
			elif self.current_char == '{':
				tokens.append(Token(TT_LBRAC, pos_start=self.pos))
				self.advance()
			# right curly brace
			elif self.current_char == '}':
				tokens.append(Token(TT_RBRAC, pos_start=self.pos))
				self.advance()
			# left bracket
			elif self.current_char == '[':
				tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
				self.advance()
			# right bracket
			elif self.current_char == ']':
				tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
				self.advance()
			# comma
			elif self.current_char == ',':
				tokens.append(Token(TT_COMMA, pos_start=self.pos))
				self.advance()
			# quote
			elif self.current_char == '"':
				tokens.append(self.make_string())
			# assignment
			elif self.current_char == ':' and self.peek() == '=':
				self.advance()
				tokens.append(self.make_assign())
			# equals op
			elif self.current_char == '=':
				tokens.append(self.make_equals())
			# less than op
			elif self.current_char == '<':
				tokens.append(self.make_less_than())
			# greater than op
			elif self.current_char == '>':
				tokens.append(self.make_greater_than())
			# if not recognized, raise error
			else:
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")
		# when done, append EOF token
		tokens.append(Token(TT_EOF, pos_start=self.pos))
		return tokens, None
	
	# Helper functions for lexer

	# make a number-- emojithon support floats!
	def make_number(self):
		num_str = ''
		dot_count = 0
		pos_start = self.pos.copy()

		while self.current_char != None and self.current_char in DIGITS + '.':
			if self.current_char == '.':
				if dot_count == 1: break
				dot_count += 1
			num_str += self.current_char
			self.advance()

		if dot_count == 0:
			return Token(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TT_FLOAT, float(num_str), pos_start, self.pos)
	# create identifiers
	def make_identifier(self):
		id_str = ''
		pos_start = self.pos.copy()

		while self.current_char != None and self.current_char in LETTERS_DIGITS + '_' + '¬' +'∧'+'∨':
			id_str += self.current_char
			self.advance()

		tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
		return Token(tok_type, id_str, pos_start, self.pos)
	# create string! Emojithon supports stardard escape characters, identified with \
	def make_string(self):
		pos_start = self.pos.copy()
		self.advance()
		escape_char = False
		string = ''
		escape_dict = {
			'n':'\n',
			't' : '\t'
		}

		# While current character exists and character is not end of string (")
		while self.current_char != None and (self.current_char != '"' or escape_char):
			if escape_char:
				# Either gets the current character from the escape dict, or just the current character
				string += escape_dict.get(self.current_char, self.current_char)

			else:
				# Check for escape characters
				# Need the double backslash to check for a single backslash 
				if self.current_char == '\\':
					escape_char = True
				else:
					string += self.current_char
			self.advance()
			## Add in escape char check above?
			escape_char = False

		self.advance()
		pos_end = self.pos.copy()
		return Token(TT_STRING, string, pos_start,pos_end)
	
	# make a not equals token
	def make_not_equals(self):
		pos_start = self.pos.copy()
		self.advance()
		return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
	# make an assign token
	def make_assign(self):
		tok_type = TT_EQ
		pos_start = self.pos.copy()
		self.advance()
		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make an arrow token
	def make_arrow(self):
		tok_type = TT_ARROW
		pos_start = self.pos.copy()
		self.advance()
		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make an equals token
	def make_equals(self):
		pos_start = self.pos.copy()
		self.advance()
		tok_type = TT_EE

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make a less than token
	def make_less_than(self):
		tok_type = TT_LT
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			tok_type = TT_LTE

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make a greater than token	
	def make_greater_than(self):
		tok_type = TT_GT
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			tok_type = TT_GTE

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

#####################################################################################################################
# NODES
## These are the node classes that the parser will generate for us
## Most of it's pretty straightforward, tracking start and end positions of nodes as well as their relevant tokens
## Certain nodes have representation classes too for pretty printing

# Number is simple. The token start/end is the number start/end
class NumberNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

# strings are similar, start and end are just the token's start/end
class StringNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

# t/f simple too
class TF_Node:
	def __init__(self, tok):
		self.tok = tok
		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

# var access start/end are just the identifier's start/end
class VarAccessNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

# var assign finally gets a little interesting; start is the variable start, end is the the value_node end position
class VarAssignNode:
	def __init__(self, tok, value_node):
		self.tok = tok
		self.value_node = value_node

		self.pos_start = self.tok.pos_start
		self.pos_end = self.value_node.pos_end
# tracks NODE OP NODE utilities. Start/end similar to var assign
class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end

	def __repr__(self):
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

# Unary op (OP NODE format)is similar but it tracks op tok start positon
class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node

		self.pos_start = self.op_tok.pos_start
		self.pos_end = node.pos_end

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'
# Skip does nothing. Inherited from WHILE homework, thought I might as well leave it in
class SkipNode:
	def __init__(self, tok):
		self.tok = tok
		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end
# If node tracks cases (comparison and body) and else case
class IfNode:
	def __init__(self, cases, else_case):
		self.cases = cases
		self.else_case = else_case

		self.pos_start = self.cases[0][0].pos_start
		self.pos_end = (self.else_case or self.cases[len(self.cases) - 1][0]).pos_end
# While node just needs to track a condition and a body
class WhileNode:
	def __init__(self, condition_node, body_node):
		self.condition_node = condition_node
		self.body_node = body_node

		self.pos_start = self.condition_node.pos_start
		self.pos_end = self.body_node.pos_end
# For node is a bit more complex
# it requires a variable name for your counter, a start value, an end value, 
# an optional step value, and a body node
class ForNode:
	def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node):
		self.var_name_tok = var_name_tok
		self.start_value_node = start_value_node
		self.end_value_node = end_value_node
		self.step_value_node = step_value_node
		self.body_node = body_node

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.body_node.pos_end
# Functions require function names, arguments, and a body node
class FuncNode:
	def __init__(self, func_name_tok, args_toks, body_node):
		self.func_name_tok = func_name_tok
		self.args_toks = args_toks
		self.body_node = body_node

		if self.func_name_tok:
			self.pos_start = self.func_name_tok.pos_start
		elif (len(self.args_toks)) > 0:
			self.pos_start = self.args_toks[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start

		self.pos_end = self.body_node.pos_end
# Node required to call a function
class CallFuncNode:
	def __init__(self, func_to_call, arg_nodes):
		self.func_to_call = func_to_call
		self.arg_nodes = arg_nodes

		self.pos_start = self.func_to_call.pos_start

		if len(self.arg_nodes)>0:
			self.pos_end = self.arg_nodes[len(self.arg_nodes)-1].pos_end
		else:
			self.pos_end = self.func_to_call.pos_end
# Bracket compound node for nested-multi statements. Keeps children as a list, start/end pos determined in parser
class BracCompoundNode:
	"""Represents a block of statements"""
	def __init__(self, children, start_pos, end_pos):
		self.children = children
		self.pos_start = start_pos
		self.pos_end = end_pos

## List node just has elements. Start/end pos determined in parser, as empty lists can't determine start/end
class ListNode:
	def __init__(self, elements, start_pos, end_pos):
		self.elements = elements
		self.pos_start = start_pos
		self.pos_end = end_pos

## Map node has elements of type list. Start/end pos determined in parser
class MapNode:
	def __init__(self, map_, start_pos, end_pos):
		self.map_ = map_
		self.pos_start = start_pos
		self.pos_end = end_pos

#####################################################################################################################
# PARSE RESULT
# this is a helper class to track the results of parsing. Super helpful for debugging.
# Within it we have:
# init 					-> keeps an error variable for raising flags when something goes wrong, node variable for tracking success, and position tracker
# register_advancement 	-> tracks movement through our tokens list
# register				-> updates position based on how many tokens a node touches
# success				-> generally if there's no error we call success on the result (or none if nothing returns)
# failure				-> only calls if something terrible has gone wrong

class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
		self.advance_count = 0

	def register_advancement(self):
		self.advance_count += 1

	def register(self, res):
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node):
		self.node = node
		return self

	def failure(self, error):
		if not self.error or self.advance_count == 0:
			self.error = error
		return self

#####################################################################################################################
# PARSER
# The parser class is defined at runtime on our tokens. Usually at each significant "level" we instantiate a ParseResult


class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.tok_idx = -1
		self.advance()

	def advance(self, ):
		self.tok_idx += 1
		if self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]
		return self.current_tok
	
	## GRAMMAR 
	## Parse starts with a statement_list (either separated by new lines or semicolons)
	## Statement lists are collections of expressions
	## Expressions are assignment statments or compound expressions (potentially and/or'ed together)
	## Assignment statements set an identifier value to an expression
	## Compound statments are either unary operations (i.e. not) or binary operations 
	## Order of ops for Binary Operations: Function Calls -> Power -> Mul/Div -> Plus/Minus -> Comparison
	## These operations can all act on "Atoms"
	## Atoms can be:
	## 		Ints | Floats | Strings | Identifiers
	## 		(expr) [parenthesized expressions]
	## 		{statement lists} [bracketed lists of statements]
	##		Lists
	##		If Expressions
	##		For Expressions
	##		While Expressions
	##		Function Definitons
	## Lists are in the form [expression (, expression)*]
	## If expressions are in the form if <expr> then <expr> else <expr>
	## For expressions are in the form for <identifier> equals <expr> to <expr> then <expr>
	## While expressions are in the form while <expr> then <expr>
	## Function definitions are in the form func(identifier (comma, identifier)*) arrow <expr>
	## To return a value from a function, simply add it as the final expression
	## To return a list of values from a function, simply add a list of values as the final expression
	def parse(self):
		res = self.statement_list()
		if any(not result.error for result in res) and self.current_tok.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', '∧' or '∨'"
			))
		return res

	## 
	def statement_list(self):

		"""
        statement_list : expr ((SEMI | NEWLINE)* expr)*
        """
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()

		## Skip to first expression
		while self.current_tok.type == TT_SEMI or self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()
		# Instantiate statement list, populate with first expression
		statements = []
		expression = self.expr()
		if res.error: return res
		statements.append(expression)

		while True:
			num_lines = 0
			while self.current_tok.type == TT_SEMI or self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()
				num_lines += 1
			if num_lines == 0:
				break
			expression = self.expr()
			statements.append(expression)

		if self.current_tok.type == TT_IDENTIFIER:
			self.error()
		return statements

	def peek(self):
		if self.tok_idx < len(self.tokens)-1:
			return self.tokens[self.tok_idx + 1]
		else:
			return None

	def expr(self):
		res = ParseResult()
		
		next_tok = self.peek()
		if self.current_tok.type == TT_IDENTIFIER and next_tok.type == TT_EQ:
			var_name = self.current_tok
			## Advance to := 
			res.register_advancement()
			self.advance()
			## Advance to expr
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			return res.success(VarAssignNode(var_name, expr))

		node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, '∧'), (TT_KEYWORD, '∨'))))
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'VAR', int, float, identifier, '+', '-', '(' or '¬'"
			))
		return res.success(node)

	def comp_expr(self):
		res = ParseResult()

		if self.current_tok.matches(TT_KEYWORD, '¬'):
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()

			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode(op_tok, node))
		

		node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
		
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected int, float, identifier, '+', '-', '(' or '¬'"
			))

		return res.success(node)

	def bin_op(self, func_a, ops, func_b=None):
		if func_b == None:
			func_b = func_a
		
		res = ParseResult()
		left = res.register(func_a())
		if res.error: return res

		while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
			if res.error: return res
			left = BinOpNode(left, op_tok, right)
		return res.success(left)

	def arith_expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV))

	def factor(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_PLUS, TT_MINUS):
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))

		return self.power()

	def power(self):
		return self.bin_op(self.call, (TT_POW, ), self.factor)

	def call(self):
		res = ParseResult()
		## get a function
		atom = res.register(self.atom())
		if res.error: return res
		## Get args for function
		if self.current_tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			arg_nodes = []
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
			else:
				arg_nodes.append(res.register(self.expr()))
				if res.error:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Expected ')', 'VAR', 'if', 'for', 'while', 'func', int, float, identifier, '+', '-', '(' or '¬'"
					))
				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()
					arg_nodes.append(res.register(self.expr()))
					if res.error: return res
					
				if not self.current_tok.type == TT_RPAREN:
					return res.failure(InvalidSyntaxError(
							self.current_tok.pos_start, self.current_tok.pos_end,
							f"Function instantiation should end in ')"
						))
				res.register_advancement()
				self.advance()
			
			## Return function call node
			return res.success(CallFuncNode(atom, arg_nodes))
		## If not a function, return atom
		return res.success(atom)

	def atom(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_INT, TT_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))

		if tok.type in (TT_STRING):
			res.register_advancement()
			self.advance()
			return res.success(StringNode(tok))

		if tok.matches(TT_KEYWORD, 'true') or tok.matches(TT_KEYWORD, 'false'):
			res.register_advancement()
			self.advance()
			return res.success(TF_Node(tok))

		elif tok.type == TT_IDENTIFIER:
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode(tok))

		elif tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))

		elif tok.type == TT_LBRAC:
			res.register_advancement()
			self.advance()
			start_pos = self.current_tok.pos_start
			expr = res.register(self.expr())
			results = [expr]
			while self.current_tok.type == TT_SEMI:
				res.register_advancement()
				self.advance()
				expr = res.register(self.expr())
				results.append(expr)
			if self.current_tok.type == TT_RBRAC:
				end_pos = self.current_tok.pos_start
				res.register_advancement()
				self.advance()
				return res.success(BracCompoundNode(results, start_pos, end_pos))
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected '}'"
				))

		elif tok.type == TT_LSQUARE:
			list_expr = res.register(self.list_expr())
			if res.error: return res
			return res.success(list_expr)

		elif tok.matches(TT_KEYWORD, 'skip'):
			res.register_advancement()
			self.advance()
			return res.success(SkipNode(tok))

		elif tok.matches(TT_KEYWORD, 'if'):
			if_expr = res.register(self.if_expr())
			if res.error: return res
			return res.success(if_expr)

		elif tok.matches(TT_KEYWORD, 'for'):
			for_expr = res.register(self.for_expr())
			if res.error: return res
			return res.success(for_expr)

		elif tok.matches(TT_KEYWORD, 'while'):
			while_expr = res.register(self.while_expr())
			if res.error: return res
			return res.success(while_expr)

		elif tok.matches(TT_KEYWORD, 'func'):
			func_def = res.register(self.func_def())
			if res.error: return res
			return res.success(func_def)

		elif tok.matches(TT_KEYWORD, 'map'):
			map_expr = res.register(self.map_expr())
			if res.error: return res
			return res.success(map_expr)

		return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Expected if, for, while, func, skip, int, float, identifier, '+', '-', '('"
		))


	def if_expr(self):
		res = ParseResult()
		cases = []
		else_case = None

		if not self.current_tok.matches(TT_KEYWORD, 'if'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'if'"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'then'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'then'"
			))

		res.register_advancement()
		self.advance()

		expr = res.register(self.expr())
		if res.error: return res
		cases.append((condition, expr))

		if self.current_tok.matches(TT_KEYWORD, 'else'):
			res.register_advancement()
			self.advance()

			else_case = res.register(self.expr())
			if res.error: return res
		return res.success(IfNode(cases, else_case))

	def while_expr(self):
		res = ParseResult()

		if not self.current_tok.matches(TT_KEYWORD, 'while'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'WHILE'"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'do'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'do'"
			))

		res.register_advancement()
		self.advance()

		body = res.register(self.expr())
		if res.error: return res
		return res.success(WhileNode(condition, body))

	def for_expr(self):
		res = ParseResult()

		if not self.current_tok.matches(TT_KEYWORD, 'for'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'for'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected identifier"
			))

		var_name = self.current_tok
		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_EQ:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '='"
			))
		
		res.register_advancement()
		self.advance()

		start_value = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'to'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'TO'"
			))
		
		res.register_advancement()
		self.advance()

		end_value = res.register(self.expr())
		if res.error: return res

		if self.current_tok.matches(TT_KEYWORD, 'step'):
			res.register_advancement()
			self.advance()

			step_value = res.register(self.expr())
			if res.error: return res
		else:
			step_value = None

		if not self.current_tok.matches(TT_KEYWORD, 'then'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'THEN'"
			))

		res.register_advancement()
		self.advance()

		body = res.register(self.expr())
		if res.error: return res

		return res.success(ForNode(var_name, start_value, end_value, step_value, body))

	def func_def(self):
		res = ParseResult()
		## We should have a func token if we're here
		if not self.current_tok.matches(TT_KEYWORD, 'func'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'func'"
			))
		## advance past function token
		res.register_advancement()
		self.advance()
		## get function name
		if self.current_tok.type == TT_IDENTIFIER:
			func_name_tok = self.current_tok
			res.register_advancement()
			self.advance()
			if not self.current_tok.type == TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Function name must be followed by '("
				))
		else:
			func_name_tok = None
			if not self.current_tok.type == TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Anonymous function def should start with '('"
				))
		
		res.register_advancement()
		self.advance()
		## Extract Args (if any)
		args_toks = []
		if self.current_tok.type == TT_IDENTIFIER:
			args_toks.append(self.current_tok)
			res.register_advancement()
			self.advance()

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()
				if not self.current_tok.type == TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Commas in function instantiation need to be followed by identifiers"
					))
				args_toks.append(self.current_tok)
				res.register_advancement()
				self.advance()
			
			if not self.current_tok.type == TT_RPAREN:
				return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Function instantiation should end in ')"
					))
		else:
			if not self.current_tok.type == TT_RPAREN:
				return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Function instantiation should end in ')"
					))
		res.register_advancement()
		self.advance()
		## Check for arrow
		if not self.current_tok.type == TT_ARROW:
			return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Function instantiation should be followed by '->"
				))
		res.register_advancement()
		self.advance()

		body_node = res.register(self.expr())
		if res.error: return res

		return res.success(FuncNode(
			func_name_tok,
			args_toks,
			body_node
		))
	## Maps are of form map([key, value] (comma [key, value])*)
	def map_expr(self):
		res = ParseResult()
		map_ = {}
		start_pos = self.current_tok.pos_start.copy()
		## We should have a map token if we're here
		if not self.current_tok.matches(TT_KEYWORD, 'map'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'map'"
			))
		# advance past map keyword
		res.register_advancement()
		self.advance()
		# check for left pareths
		if not self.current_tok.type == TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"map keyword must be followed by '("
				))
		# advance past left pareths
		res.register_advancement()
		self.advance()
		
		## Populate map
		while True:
			if self.current_tok.type == TT_RPAREN:
				break
			if not self.current_tok.type == TT_LSQUARE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"map must be populated with elements in form ['key', val]"
				))
			# Advance to first key
			res.register_advancement()
			self.advance()
			# Get first key
			key = res.register(self.expr())
			if res.error:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Broken key passed to map"
				))
			if not isinstance(key, StringNode):
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"map keys must be strings"
				))
			# Advance past comma
			if not self.current_tok.type == TT_COMMA:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"elements within map definition must be comma separated"
				))
			else:
				res.register_advancement()
				self.advance()
			# Get value
			value = res.register(self.expr())
			if res.error:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Broken value passed to map"
				))
			# Check for end bracket
			if not self.current_tok.type == TT_RSQUARE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"map must be populated with elements in form [key, val]"
				))
			res.register_advancement()
			self.advance()
			# Check for comma
			if self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()
				if not self.current_tok.type == TT_LSQUARE:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"map must be instantited in form map([key, val],[key, val])"
					))
			# Assign key value pair to map_
			map_[key.tok.value] = value
		# Advance past right pareths
		res.register_advancement()
		self.advance()
		# Return a map node
		return res.success(MapNode(map_, start_pos, self.current_tok.pos_end.copy()))

	def list_expr(self):
		res = ParseResult()
		elements = []
		start_pos = self.current_tok.pos_start.copy()

		res.register_advancement()
		self.advance()
		if self.current_tok.type == TT_RSQUARE:
			res.register_advancement()
			self.advance()
		else:
			elements.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Broken elements passed to list"
				))

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				elements.append(res.register(self.expr()))
				if res.error: return res

			if self.current_tok.type != TT_RSQUARE:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected ',' or ']'"
				))

			res.register_advancement()
			self.advance()

		return res.success(ListNode(elements, start_pos, self.current_tok.pos_end.copy()))

#######################################
# RUNTIME RESULT
#######################################

class RTResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if res.error: self.error = res.error
		return res.value

	def success(self, value):
		self.value = value
		return self

	def failure(self, error):
		self.error = error
		return self

#######################################
# VALUES
#######################################
class Value:
	def __init__(self):
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self
	def added_to(self, other):
		return None, self.illegal_operation(other)
	def subbed_by(self, other):
		return None, self.illegal_operation(other)
	def multed_by(self, other):
		return None, self.illegal_operation(other)
	def dived_by(self, other):
		return None, self.illegal_operation(other)
	def powed_by(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)
	def anded_by(self, other):
		return None, self.illegal_operation(other)
	def ored_by(self, other):
		return None, self.illegal_operation(other)
	def notted(self):
		return None, self.illegal_operation()
	def copy(self):
		return None, self.illegal_operation()
	def is_true(self):
		return None, self.illegal_operation()
	def __repr__(self):
		return None, self.illegal_operation()
	def execute(self, args):
		return RTResult().failure(self.illegal_operation(args))
	def illegal_operation(self, other=None):
		if not other: other = self
		return RTError(
			self.pos_start, other.pos_end,
			'Illegal operation',
			self.context
		)
		
class String(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value
		self.set_pos()
		self.set_context()

	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value+other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def multed_by(self, other):
		if isinstance(other, Number):
			return String(self.value*other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def subbed_by(self, other):
		if isinstance(other, String):
			original_val = self.value
			to_delete = other.value
			if len(to_delete) > len(original_val):
				return String(self.value).set_context(self.context), None
			else:
				new_str = ''
				len_to_delete = len(to_delete)
				while len(original_val) > len_to_delete:
					to_check = original_val[:len_to_delete]
					if to_check == to_delete:
						original_val = original_val[len_to_delete:]
					else:
						new_str+= original_val[0]
						original_val = original_val[1:]
				if len(original_val) <= len_to_delete and original_val != to_delete:
					new_str+= original_val
				return String(new_str).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def dived_by(self, other):
		if isinstance(other, Number):
			chunks = []
			if other.value > 0:
				chunk_len = -1 * (-len(self.value) // other.value)
			else:
				return List([self.value]), None
			string = self.value
			while len(string) >= chunk_len:
				chunks.append(string[:chunk_len])
				string = string[chunk_len:]
			if len(string) > 0:
				chunks.append(string)
			return List(chunks), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def is_true(self):
		return len(self.value) >0

	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	def __repr__(self):
		return self.value

class Number(Value):
	def __init__(self, value):
		self.value = value
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RTError(
					other.pos_start, other.pos_end,
					'Division by zero',
					self.context
				)

			return Number(self.value / other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def powed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value ** other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(int(self.value != other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value < other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value > other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value <= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value >= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value and other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value or other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def notted(self):
		return Number(1 if self.value == 0 else 0).set_context(self.context), None
		
	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def is_true(self):
		return self.value != 0
	
	def __repr__(self):
		return str(self.value)

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)

class BaseFunction(Value):
	def __init__(self, func_name):
		super().__init__()
		self.func_name = func_name or 'anonymous'

	def generate_new_context(self):
		sub_context = Context(self.func_name, self.context, self.pos_start)
		sub_context.symbol_table = SymbolTable(sub_context.parent.symbol_table)
		return sub_context

	def check_args(self, arg_names, args):
		res = RTResult()
		if len(args) > len(arg_names):
			return res.failure(RTError(
					self.pos_start, self.pos_end,
					"{} too many arguments passed into {}".format(len(args)-len(arg_names), self.func_name),
					self.context
				))
		if len(args) < len(arg_names):
			return res.failure(RTError(
					self.pos_start, self.pos_end,
					"{} too few arguments passed into {}".format(len(arg_names)-len(args), self.func_name),
					self.context
				))
		return res.success(None)
	
	def fill_args(self, arg_names, args, sub_context):
		for i in range(len(args)):
			name = arg_names[i]
			value = args[i]
			value.set_context(sub_context)
			sub_context.symbol_table.set(name, value)

	def check_and_fill_args(self, arg_names, args, sub_context):
		res = RTResult()
		res.register(self.check_args(arg_names, args))
		if res.error: return res
		self.fill_args(arg_names, args, sub_context)
		return res.success(None)

class Function(BaseFunction):
	def __init__(self, func_name, body_node, arg_names):
		super().__init__(func_name)
		self.func_name = func_name or 'anonymous'
		self.body_node = body_node
		self.arg_names = arg_names

	def execute(self, args):
		res = RTResult()
		interp = Interpreter()
		sub_context = Context(self.func_name, self.context, self.pos_start)
		sub_context.symbol_table = SymbolTable(sub_context.parent.symbol_table)

		if len(args) > len(self.arg_names):
			return res.failure(RTError(
					self.pos_start, self.pos_end,
					"{} too many arguments passed into {}".format(len(args)-len(self.arg_names), self.func_name),
					self.context
				))
		if len(args) < len(self.arg_names):
			return res.failure(RTError(
					self.pos_start, self.pos_end,
					"{} too few arguments passed into {}".format(len(self.arg_names)-len(args), self.func_name),
					self.context
				))
		
		for i in range(len(args)):
			name = self.arg_names[i]
			value = args[i]
			value.set_context(sub_context)
			sub_context.symbol_table.set(name, value)
		
		val = res.register(interp.visit(self.body_node, sub_context))
		if res.error: return res
		return res.success(val)

	def copy(self):
		copy = Function(self.func_name, self.body_node, self.arg_names)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy
	def __repr__(self):
		return f"Function: {self.func_name}"

class BuiltInFunction(BaseFunction):
	def __init__(self, func_name):
		super().__init__(func_name)

	def execute(self, args):
		res = RTResult()
		sub_context = self.generate_new_context()
		method_name = f'execute_{self.func_name}'
		method = getattr(self, method_name, self.no_visit_method)
		res.register(self.check_and_fill_args(method.arg_names, args, sub_context))
		if res.error: return res
		return_value = res.register(method(sub_context))
		if res.error: return res
		return res.success(return_value)

	def copy(self):
		copy = BuiltInFunction(self.func_name)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __repr__(self):
		return f"Built-In Function: {self.func_name}"

	def no_visit_method(self, node, context):
		raise Exception(f'No execute_{self.name} method implemented')
	
	## BUILT IN FUNCTIONS PROVIDED
	# print 		-> print(value): prints value to output
	# print_assign 	-> print_assign(value): returns value that would be printed
	# input 		-> input(): allows user to input a value. If an int returns Number else String
	# is_number 	-> is_number(value): returns true if value is Number else False
	# is_string 	-> is_string(value): returns true if value is String else False
	# is_func 		-> is_func(value): returns true if value is String else False
	# append 		-> append(List, value): new list with value appended
	# pop	 		-> pop(Map, key) | pop(List, number): pops element from list at desired index
	# get			-> get(Map, key) | get(List, number): gets a key from a dict or an element at List[index(number)]
	# set			-> set(Map, key, value) | set(List, number, value): sets a value for a dict or a list at index number

	# print function
	# prints string representation of values passed
	def execute_print(self, sub_context):
		print(str(sub_context.symbol_table.get('value')))
		return RTResult().success(Number.null)
	execute_print.arg_names = ['value']
	# print assign function
	# return the value that would be printed
	def execute_print_assign(self, sub_context):
		return RTResult().success(String(str(sub_context.symbol_table.get('value'))))
	execute_print_assign.arg_names = ['value']
	## Allow user-defined input
	def execute_input(self, sub_context):
		text = input()
		try: 
			number = int(text)
			return RTResult().success(Number(number))
		except:
			return RTResult().success(String(text))
	execute_input.arg_names = []
	# Check if a value is a number
	def execute_is_number(self, sub_context):
		is_number = isinstance(sub_context.symbol_table.get('value'), Number)
		return RTResult().success(Number.true if is_number else Number.false)
	execute_is_number.arg_names = ['value']
	# check if a value is a string
	def execute_is_string(self, sub_context):
		is_string = isinstance(sub_context.symbol_table.get('value'), String)
		return RTResult().success(Number.true if is_string else Number.false)
	execute_is_string.arg_names = ['value']
	# check if a value is a function
	def execute_is_func(self, sub_context):
		is_func = isinstance(sub_context.symbol_table.get('value'), Function) or isinstance(sub_context.symbol_table.get('value'), BuiltInFunction)
		return RTResult().success(Number.true if is_func else Number.false)
	execute_is_func.arg_names = ['value']
	# check if a value is a list
	def execute_is_list(self, sub_context):
		is_list = isinstance(sub_context.symbol_table.get('value'), List)
		return RTResult().success(Number.true if is_list else Number.false)
	execute_is_list.arg_names = ['value']
	# add append for lists
	def execute_append(self, sub_context):
		list_to_append_to = sub_context.symbol_table.get('List')
		val_to_append = sub_context.symbol_table.get('value')

		if not isinstance(list_to_append_to, List):
			return RTResult().failue(RTError(
				self.pos_start, self.pos_end,
				'First argument to append must be a list',
				sub_context
			)
			)
		list_to_append_to.elements.append(val_to_append)
		return RTResult().success(Number.null)
	execute_append.arg_names = ['List', 'value']
	# add pop for lists
	def execute_pop(self, sub_context):
		retreivable_ob = sub_context.symbol_table.get('retreivable')
		address = sub_context.symbol_table.get('addr')
		if isinstance(retreivable_ob, List):
			if not isinstance(address, Number):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Second argument to pop on list must be a number',
					sub_context
				)
				)
			list_length = len(retreivable_ob.elements)
			if not address.value>=-list_length or not address.value<list_length:
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Index for pop not valid',
					sub_context
				)
				)
			element = retreivable_ob.elements.pop(address.value)
			return RTResult().success(element)
		elif isinstance(retreivable_ob, Map):
			if not isinstance(address, String):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'If trying to pop an item from a Map, second argument must be a String',
					sub_context
				)
				)
			keys = retreivable_ob.map_.keys()
			if not address.value in keys:
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Key not found in Map',
					sub_context
				)
				)
			element = retreivable_ob.map_.pop(address.value)
			return RTResult().success(element)
		else:
			return RTResult().failue(RTError(
				self.pos_start, self.pos_end,
				'get can only be called on a list or map',
				sub_context
			)
			)
	execute_pop.arg_names = ['retreivable', 'addr']

	# add get for Maps and Lists
	def execute_get(self, sub_context):
		retreivable_ob = sub_context.symbol_table.get('retreivable')
		address = sub_context.symbol_table.get('addr')
		if isinstance(retreivable_ob, List):
			if not isinstance(address, Number):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'If trying to index a List, second argument must be a number',
					sub_context
				)
				)
			list_length = len(retreivable_ob.elements)
			if not address.value>=-list_length or not address.value<list_length:
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Index for get not valid',
					sub_context
				)
				)
			element = retreivable_ob.elements[address.value]
			return RTResult().success(element)
		elif isinstance(retreivable_ob, Map):
			if not isinstance(address, String):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'If trying to get an item from a Map, second argument must be a String',
					sub_context
				)
				)
			keys = retreivable_ob.map_.keys()
			if not address.value in keys:
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Key not found in Map',
					sub_context
				)
				)
			element = retreivable_ob.map_[address.value]
			return RTResult().success(element)
		else:
			return RTResult().failue(RTError(
				self.pos_start, self.pos_end,
				'get can only be called on a list or map',
				sub_context
			)
			)
	execute_get.arg_names = ['retreivable', 'addr']

	# add get for Maps and Lists
	def execute_set(self, sub_context):
		retreivable_ob = sub_context.symbol_table.get('retreivable')
		address = sub_context.symbol_table.get('addr')
		value = sub_context.symbol_table.get('value')
		if isinstance(retreivable_ob, List):
			if not isinstance(address, Number):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'If trying to set a value in List, second argument must be a number',
					sub_context
				)
				)
			list_length = len(retreivable_ob.elements)
			if not address.value>=-list_length or not address.value<list_length:
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Index for set not valid',
					sub_context
				)
				)
			retreivable_ob.elements[address.value] = value
			return RTResult().success(Number.null)
		elif isinstance(retreivable_ob, Map):
			if not isinstance(address, String):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'If trying to get an item from a Map, second argument must be a String',
					sub_context
				)
				)
			retreivable_ob.map_[address.value] = value
			return RTResult().success(Number.null)
		else:
			return RTResult().failue(RTError(
				self.pos_start, self.pos_end,
				'set can only be called on a list or map',
				sub_context
			)
			)
	execute_set.arg_names = ['retreivable', 'addr', 'value']

BuiltInFunction.print 			= BuiltInFunction('print')
BuiltInFunction.print_assign 	= BuiltInFunction('print_assign')
BuiltInFunction.input 			= BuiltInFunction('input')
BuiltInFunction.is_number 		= BuiltInFunction('is_number')
BuiltInFunction.is_string 		= BuiltInFunction('is_string')
BuiltInFunction.is_func 		= BuiltInFunction('is_func')
BuiltInFunction.is_list 		= BuiltInFunction('is_list')
BuiltInFunction.append 			= BuiltInFunction('append')
BuiltInFunction.pop 			= BuiltInFunction('pop')
BuiltInFunction.get 			= BuiltInFunction('get')
BuiltInFunction.set 			= BuiltInFunction('set')

class Map(Value):
	def __init__(self, map_):
		super().__init__()
		self.map_ = map_

	# map + [key, val]
	def added_to(self, other):
		new_map = self.copy()
		if isinstance(other, List):
			list_vals = other.elements
			if len(list_vals) != 2:
				return None, Value.illegal_operation(self.pos_start, other.pos_end)
			else:
				if not isinstance(list_vals[0], String):
					return None, Value.illegal_operation(self.pos_start, other.pos_end)
				new_map.map_[(list_vals[0].value)] = list_vals[1]
				return new_map, None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	# map - key
	def subbed_by(self, key):
		new_map = self.copy()
		if not isinstance(key, String):
			return None, Value.illegal_operation(self.pos_start, other.pos_end)
		try:
			new_map.map_.pop(key.value)
			return new_map, None
		except:
			return None, RTError(key.pos_start, key.pos_end, 
			'Invalid Key'
			)
	def copy(self):
		copy = Map(self.map_)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def __repr__(self):
		# out_str = '{'

		return f"{self.map_}"

class List(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements

	def added_to(self, item):
		new_list = self.copy()
		new_list.elements.append(item)
		return new_list, None
	
	def multed_by(self, val):
		# new_els = []
		# for element in self.elements:
		# 	print(type(element))
		# 	if 
		new_els = [Number(element.value * val.value) for element in self.elements]
		new_list = List(new_els)
		return new_list, None
	
	def get_comparison_eq(self, other):
		if isinstance(other, List):
			self_els = self.elements
			other_els = other.elements
			if len(self_els) == len(other_els):
				for i, el in enumerate(self_els):
					if el.value!=other_els[i].value:
						return  Number(int(el == other_els[i])).set_context(self.context), None
				return Number(int(len(self_els) == len(other_els))).set_context(self.context), None
			else:
				return  Number(int(len(self_els) == len(other_els))).set_context(self.context), None
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def subbed_by(self, val):
		new_list = self.copy()
		try:
			new_list.elements.pop(val.value)
			return new_list, None
		except:
			return None, RTError(val.pos_start, val.pos_end, 
			'Invalid Index'
			)

	def anded_by(self, other):
		if isinstance(other, Number):
			try:
				return self.elements[other.value], None
			except:
				return None, RTError(other.pos_start, other.pos_end, 
				'Invalid Index'
				)
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	def copy(self):
		copy = List(self.elements)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def __repr__(self):
		return f"{self.elements}"





#######################################
# CONTEXT
#######################################

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None

#######################################
# SYMBOL TABLE
#######################################

class SymbolTable:
	def __init__(self, parent=None):
		self.symbols = {}
		self.parent = None

	def get(self, name):
		value = self.symbols.get(name, None)
		if value == None and self.parent:
			return self.parent.get(name)
		return value

	def set(self, name, value):
		self.symbols[name] = value

	def remove(self, name):
		del self.symbols[name]

#######################################
# INTERPRETER
#######################################

class Interpreter:
	def visit(self, node, context):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context):
		raise Exception(f'No visit_{type(node).__name__} method defined')

	###################################

	def visit_NumberNode(self, node, context):
		return RTResult().success(
			Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

	def visit_SkipNode(self, node, context):
		return RTResult()

	def visit_TF_Node(self, node, context):
		res = RTResult()
		if node.tok.matches(TT_KEYWORD, 'true'):
			result, error = Number(1).set_context(context), None
		elif node.tok.matches(TT_KEYWORD, 'false'):
			result, error = Number(0).set_context(context), None
		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_StringNode(self, node, context):
		return RTResult().success(
			String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)


	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.tok.value
		if var_name not in list(context.symbol_table.symbols.keys()):
			value = Number(0)
		else:
			value = context.symbol_table.get(var_name)

		if not value and value != 0:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"'{var_name}' is not defined",
				context
			))
		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(value)

	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.tok.value
		if var_name not in list(context.symbol_table.symbols.keys()):
			context.symbol_table.set(var_name, Number(0))
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res

		context.symbol_table.set(var_name, value)
		return res.success(value)

	def visit_BinOpNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.error: return res
		right = res.register(self.visit(node.right_node, context))
		if res.error: return res

		if node.op_tok.type == TT_PLUS:
			result, error = left.added_to(right)
		elif node.op_tok.type == TT_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TT_MUL:
			result, error = left.multed_by(right)
		elif node.op_tok.type == TT_DIV:
			result, error = left.dived_by(right)
		elif node.op_tok.type == TT_POW:
			result, error = left.powed_by(right)
		elif node.op_tok.type == TT_EE:
			result, error = left.get_comparison_eq(right)
		elif node.op_tok.type == TT_NE:
			result, error = left.get_comparison_ne(right)
		elif node.op_tok.type == TT_LT:
			result, error = left.get_comparison_lt(right)
		elif node.op_tok.type == TT_GT:
			result, error = left.get_comparison_gt(right)
		elif node.op_tok.type == TT_LTE:
			result, error = left.get_comparison_lte(right)
		elif node.op_tok.type == TT_GTE:
			result, error = left.get_comparison_gte(right)
		elif node.op_tok.matches(TT_KEYWORD, '∧'):
			result, error = left.anded_by(right)
		elif node.op_tok.matches(TT_KEYWORD, '∨'):
			result, error = left.ored_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node, context):
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.error: return res

		error = None

		if node.op_tok.type == TT_MINUS:
			number, error = number.multed_by(Number(-1))
		elif node.op_tok.matches(TT_KEYWORD, '¬'):
			number, error = number.notted()

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))

	def visit_IfNode(self, node, context):
		res = RTResult()

		for condition, expr in node.cases:
			condition_value = res.register(self.visit(condition, context))
			if res.error: return res

			if condition_value.is_true():
				expr_value = res.register(self.visit(expr, context))
				if res.error: return res
				return res.success(expr_value)

		if node.else_case:
			else_value = res.register(self.visit(node.else_case, context))
			if res.error: return res
			return res.success(else_value)

		return res.success(None)

	def visit_WhileNode(self, node, context):
		res = RTResult()
		while True:
			condition = res.register(self.visit(node.condition_node, context))
			if res.error: return res

			if not condition.is_true(): break

			val = res.register(self.visit(node.body_node, context))
			if res.error: return res

		return res.success(val)

	def visit_ForNode(self, node, context):
		res = RTResult()

		start_value = res.register(self.visit(node.start_value_node, context))
		if res.error: return res

		end_value = res.register(self.visit(node.end_value_node, context))
		if res.error: return res

		if node.step_value_node:
			step_value = res.register(self.visit(node.step_value_node, context))
			if res.error: return res
		else:
			step_value = Number(1)

		i = start_value.value

		if step_value.value >= 0:
			condition = lambda: i < end_value.value
		else:
			condition = lambda: i > end_value.value
		
		while condition():
			context.symbol_table.set(node.var_name_tok.value, Number(i))
			i += step_value.value

			last_value = res.register(self.visit(node.body_node, context))
			if res.error: return res

		return res.success(last_value)

	def visit_BracCompoundNode(self, node, context):
		res = RTResult()
		for child in node.children:
			value = res.register(self.visit(child, context))
			if res.error: return res
		return res.success(value)

	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []

		for element in node.elements:
			elements.append(res.register(self.visit(element, context)))
		return res.success(List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_MapNode(self, node, context):
		res = RTResult()
		map_ = {}
		for key in node.map_.keys():
			map_[key] = (res.register(self.visit(node.map_[key], context)))
		return res.success(Map(map_).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_FuncNode(self, node, context):
		res = RTResult()
		func_name = node.func_name_tok.value if node.func_name_tok else None
		body_node = node.body_node
		args_names = [name.value for name in node.args_toks]
		func_value = Function(func_name, body_node, args_names).set_context(context).set_pos(node.pos_start, node.pos_end)

		if node.func_name_tok:
			context.symbol_table.set(func_name, func_value)
		
		return res.success(func_value)
	
	def visit_CallFuncNode(self, node, context):
		res = RTResult()
		args = []

		val_to_call = res.register(self.visit(node.func_to_call, context))
		if res.error: return res
		val_to_call = val_to_call.copy().set_pos(node.pos_start, node.pos_end)

		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.error: return res

		return_val = res.register(val_to_call.execute(args))
		if res.error: return res
		return_val = return_val.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(return_val)



#######################################
# RUN
#######################################

global_symbol_table = SymbolTable()
global_symbol_table.set('null', Number.null)
global_symbol_table.set('false', Number.false)
global_symbol_table.set('true', Number.true)
global_symbol_table.set('print', BuiltInFunction('print'))
global_symbol_table.set('print_assign', BuiltInFunction('print_assign'))
global_symbol_table.set('input', BuiltInFunction('input'))
global_symbol_table.set('is_number', BuiltInFunction('is_number'))
global_symbol_table.set('is_string', BuiltInFunction('is_string'))
global_symbol_table.set('is_list', BuiltInFunction('is_list'))
global_symbol_table.set('append', BuiltInFunction('append'))
global_symbol_table.set('pop', BuiltInFunction('pop'))
global_symbol_table.set('is_func', BuiltInFunction('is_func'))
global_symbol_table.set('get', BuiltInFunction('get'))
global_symbol_table.set('set', BuiltInFunction('set'))


# def run(fn, text):
# 	# Generate tokens
# 	lexer = Lexer(fn, text)
# 	tokens, error = lexer.make_tokens()
# 	if error: return None, error
	
# 	# Generate AST
# 	parser = Parser(tokens)
# 	ast = parser.parse()
# 	if ast.error: return None, ast.error

# 	# Run program
# 	interpreter = Interpreter()
# 	context = Context('<program>')
# 	context.symbol_table = global_symbol_table
# 	result = interpreter.visit(ast.node, context)

# 	return result.value, result.error



def run(fn, text):
	# Generate tokens
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	#print(tokens)
	if error: return None, error
	# Generate AST
	parser = Parser(tokens)
	ast_list = parser.parse()
	#print([item.node for item in ast_list])
	# Run program
	interpreter = Interpreter()
	context = Context('<program>')
	context.symbol_table = global_symbol_table
	for ast in ast_list:
		if type(ast) is list:
			for a in ast:
				result = interpreter.visit(ast.node, context)
				#print(result.value)
		else:
			result = interpreter.visit(ast.node, context)
			#print(result.value)

	pre_defined_symbols = [
		'null',
		'false',
		'true',
		'print',
		'print_assign',
		'input',
		'is_number',
		'is_string',
		'is_list',
		'append',
		'pop',
		'is_func',
		'get',
		'set'
	]

	keys = list(context.symbol_table.symbols.keys())
	keys.sort()
	out_str = '{'
	num_iter = -1
	for val in keys:
		if val in pre_defined_symbols:
			continue
		else:
			# would use enumerate, but skipping through pre-defined symbols messes with things
			num_iter += 1
		if num_iter>0:
			out_str += ', '
		if isinstance(context.symbol_table.symbols[val], List):
			list_str = '['
			for j, element in enumerate(context.symbol_table.symbols[val].elements):
				if j>0:
					list_str += ', '
				list_str += str(element)
			list_str += ']'
			out_str += str(val)+' → ' + list_str
		else:
			out_str += str(val)+' → '+str(context.symbol_table.symbols[val])
	out_str += '}'
	print(out_str)

def main():
	# while True:
	# 	try:
	# 		try:
	# 			text = raw_input()
	# 		except NameError:  # Python3
	# 			text = input()
	# 	except EOFError:
	# 		break
	# 	if not text:
	# 		continue
	# 	run('<stdin>', text)
	while True:
		text = input('emoji > ')
		run('<stdin>', text)

if __name__ == '__main__':
    main()