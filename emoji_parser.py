#######################################
# Code largely inspired and guided by the work by David Callanan 
# in his youtube series "Make YOUR OWN Programming Language"
# There were a large number of changes I had to make, including:
#   - Adding support for variable assignment without keywords
#   - Adding a "bracket compound" node type to support multiple nested expressions
#   - Implementing Skip
# 	- Implementing Backet support
# 	- Implementing Lists
#   - Allowing access for undefined variables
#   - Changing a bunch of syntax
# 	- Extra built-in functions
# 	- Extra arithmetic functionality for datatypes
# 	- Queue support
# 	- Dictionaries
# And of course, implementing all this made me deeply familiar with the base code
# Lots of fun!
#######################################

import string
from emoji_lexer import Error, IllegalCharError, InvalidSyntaxError, RTError, Position, Token, Lexer

# CONSTANTS
## Get symbols to use for lexer
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS + '🚫'

TOKEN_INT				= 'INT'
TOKEN_FLOAT			    = 'FLOAT'
TOKEN_IDENTIFIER		= 'IDENTIFIER'
TOKEN_KEYWORD			= 'KEYWORD'
TOKEN_PLUS				= 'PLUS'
TOKEN_MINUS			    = 'MINUS'
TOKEN_MUL				= 'MUL'
TOKEN_DIV				= 'DIV'
TOKEN_POW				= 'POW'
TOKEN_ASSIGN			= 'ASSIGN'
TOKEN_EQ				= 'EQ'
TOKEN_LPAREN			= 'LPAREN'
TOKEN_RPAREN			= 'RPAREN'
TOKEN_LBRAC			    = 'LBRAC'
TOKEN_RBRAC			    = 'RBRAC'
TOKEN_LSQUARE           = 'LSQUARE'
TOKEN_RSQUARE           = 'RSQUARE'
TOKEN_QLSQUARE          = 'QLSQUARE'
TOKEN_QRSQUARE          = 'QRSQUARE'
TOKEN_EE				= 'EE'
TOKEN_LT				= 'LT'
TOKEN_GT				= 'GT'
TOKEN_EOF				= 'EOF'
TOKEN_SEMI				= 'SEMI'
TOKEN_COMMA			    = 'COMMA'
TOKEN_STRING			= 'STRING'
TOKEN_NEWLINE			= 'NEWLINE'

# These are the keywords in our language
# Note that built-in function definitons are added to our global symbol table at runtime,
# are named identifiers by the lexer, and are picked up in the call() level of the parser
KEYWORDS = [
	'👏',
	'🙌',
	'🚫',
	'⏰',
	'🚶',
	'👣',
	'🤷',
	'👈',
	'⏳',
	'👉',
	'👌',
	'🙅', 
	'😶',
	'💪',
	'📚'
]

pre_defined_symbols = [
		'🙅',
		'👌',
		'🦜',
		'🐝',
		'👀',
		'🔢',
		'🔤',
		'📜',
		'🔧',
		'🐌',
		'📏',
		'🎉',
		'🐙',
		'🦥', 
		'🌚',
		'🍫',
		'🍪'
	]

pre_defined_symbols_vals = ''.join(pre_defined_symbols)
pre_defined_symbols_vals = pre_defined_symbols_vals.join(KEYWORDS)

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

## Queues are basically the same as Lists, but with more restricted functionality
class QueueNode:
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
		self.to_reverse_count = 0

	def register_advancement(self):
		self.advance_count += 1

	def register(self, res):
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node):
		self.node = node
		return self

	def try_register(self, res):
		if res.error:
			self.to_reverse_count = res.advance_count
			return None
		return self.register(res)

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
		self.update_current_token()
		return self.current_tok

	def update_current_token(self):
		if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]

	def reverse(self, amt = 1):
		self.tok_idx -= amt
		self.update_current_token()
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
	## 		Ints | Floats | Strings | Variables
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
	## While expressions are in the form while <expr> do <expr>
	## Function definitions are in the form func(identifier (comma, identifier)*) arrow <expr>
	## To return a value from a function, simply add it as the final expression
	## To return a list of values from a function, simply add a list of values as the final expression
	def parse(self):
		res = self.statement_list()
		if not res.error and self.current_tok.type != TOKEN_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', '∧' or '∨'"
			))
		return res

	## Statement lists are expressions followed by semi colons or new lines
	## Plus more expressions
	def statement_list(self):

		"""
		statement_list : expr ((SEMI | NEWLINE)* expr)*
		"""
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()

		## Skip to first expression
		while self.current_tok.type == TOKEN_SEMI or self.current_tok.type == TOKEN_NEWLINE:
			res.register_advancement()
			self.advance()
		# Instantiate statement list, populate with first expression
		statements = []
		expression = res.register(self.expr())
		if res.error: return res
		statements.append(expression)

		more_expression = True
		# Look for some more expressions
		while True:
			num_lines = 0
			# Advance past semis and new lines
			while self.current_tok.type == TOKEN_SEMI or self.current_tok.type == TOKEN_NEWLINE:
				res.register_advancement()
				self.advance()
				num_lines += 1
			if num_lines == 0:
				more_expression = False
			# If no more expressions, break
			if not more_expression:
				break
			# If there's an issue with registering an expression, reverse
			expression = res.try_register(self.expr())
			if not expression:
				self.reverse(res.to_reverse_count)
				more_expression = False
				continue
			statements.append(expression)
		# Statement list is a list of expressions (using the internal list datatype)
		return res.success(ListNode(
			statements, 
			pos_start, 
			self.current_tok.pos_end.copy()
		))
	
	# Helper function for peeking ahead to check for variable assignment vs. just variable access
	def peek(self):
		if self.tok_idx < len(self.tokens)-1:
			return self.tokens[self.tok_idx + 1]
		else:
			return None
	
	# Expression is either variable assignment or a (series of) compound expression(s)
	def expr(self):
		res = ParseResult()
		# If it's an assign statement, make an assign node with variable name and expression
		next_tok = self.peek()
		if self.current_tok.type == TOKEN_IDENTIFIER and next_tok.type == TOKEN_EQ:
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
        # otherwise, binary operation of compound expression (and/or compound expr)
		node = res.register(self.bin_op(self.comp_expr, ((TOKEN_KEYWORD, '👏'), (TOKEN_KEYWORD, '🙌'))))
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'VAR', int, float, identifier, '+', '-', '(' or '🚫'"
			))
		return res.success(node)
    
	# Compound expression checks for unary op (NOT), otherwise registers a binary operation with 
	# an arithmetic expression
	def comp_expr(self):
		res = ParseResult()

		if self.current_tok.matches(TOKEN_KEYWORD, '🚫'):
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()

			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode(op_tok, node))
		

		node = res.register(self.bin_op(self.arith_expr, (TOKEN_EE, TOKEN_LT, TOKEN_GT)))
		
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected int, float, identifier, '+', '-', '(' or '🚫'"
			))

		return res.success(node)
	# Operates on one or more elements
	def bin_op(self, func_a, ops, func_b=None):

		if func_b == None:
			func_b = func_a
		
		res = ParseResult()
		left = res.register(func_a())
		if res.error: return res
		# If there's an operation, return binary node with left op right
		while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
			if res.error: return res
			left = BinOpNode(left, op_tok, right)
		# If no operation, just return left
		return res.success(left)

	# This series of definitions is how we set up precedence:
	
	# Arithmetic-- term +/- term
	def arith_expr(self):
		return self.bin_op(self.term, (TOKEN_PLUS, TOKEN_MINUS))
	
	# Term-- factor *// factor
	def term(self):
		return self.bin_op(self.factor, (TOKEN_MUL, TOKEN_DIV))
	
	# Factor-- +/-(expression) or power
	def factor(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TOKEN_PLUS, TOKEN_MINUS):
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))

		return self.power()

	# Binary operator with call, followed by optional power token and factor
	def power(self):
		return self.bin_op(self.call, (TOKEN_POW, ), self.factor)
	
	# Call catches function calls-- of form IDNETIFIER(args*)
	# If no function call, returns an atom
	def call(self):
		res = ParseResult()
		## get a function
		atom = res.register(self.atom())
		if res.error: return res
		## Get args for function
		if self.current_tok.type == TOKEN_LPAREN:
			res.register_advancement()
			self.advance()
			arg_nodes = []
			if self.current_tok.type == TOKEN_RPAREN:
				res.register_advancement()
				self.advance()
			else:
				arg_nodes.append(res.register(self.expr()))
				if res.error:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Expected ')', 'if', '⏰', '⏳', '💪', int, float, identifier, '+', '-', '(' or '🚫'"
					))
				while self.current_tok.type == TOKEN_COMMA:
					res.register_advancement()
					self.advance()
					arg_nodes.append(res.register(self.expr()))
					if res.error: return res
					
				if not self.current_tok.type == TOKEN_RPAREN:
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
    
	# The "base unit" of our parser
	def atom(self):
		res = ParseResult()
		tok = self.current_tok
        # Number nodes
		if tok.type in (TOKEN_INT, TOKEN_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))
        # String nodes
		if tok.type in (TOKEN_STRING):
			res.register_advancement()
			self.advance()
			return res.success(StringNode(tok))
        # True/False node
		if tok.matches(TOKEN_KEYWORD, '👌') or tok.matches(TOKEN_KEYWORD, '🙅'):
			res.register_advancement()
			self.advance()
			return res.success(TF_Node(tok))
        # Identifier node
		elif tok.type == TOKEN_IDENTIFIER:
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode(tok))
        # Left parenths-- for a grouping expressions i.e. (4+2)/2 vs 4+2/2
		elif tok.type == TOKEN_LPAREN:
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TOKEN_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))
        # Left bracket-- also for grouped expressions, but expressed as a list separated by semi colons
        # Return value is the value of the last item in the list-- this behavior is defined in the interpreter
        # This is how we "return" values in functions currently
		elif tok.type == TOKEN_LBRAC:
			res.register_advancement()
			self.advance()
			start_pos = self.current_tok.pos_start
			expr = res.register(self.expr())
			results = [expr]
			while self.current_tok.type == TOKEN_SEMI:
				res.register_advancement()
				self.advance()
				expr = res.register(self.expr())
				results.append(expr)
			if self.current_tok.type == TOKEN_RBRAC:
				end_pos = self.current_tok.pos_start
				res.register_advancement()
				self.advance()
				return res.success(BracCompoundNode(results, start_pos, end_pos))
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected '}'"
				))
        # Square brackets define lists
		elif tok.type == TOKEN_LSQUARE:
			list_expr = res.register(self.list_expr())
			if res.error: return res
			return res.success(list_expr)
		# Square q-left brackets define queues
		elif tok.type == TOKEN_QLSQUARE:
			queue_expr = res.register(self.queue_expr())
			if res.error: return res
			return res.success(queue_expr)
        # Skip node
		elif tok.matches(TOKEN_KEYWORD, '😶'):
			res.register_advancement()
			self.advance()
			return res.success(SkipNode(tok))
        # If expression
		elif tok.matches(TOKEN_KEYWORD, '🤷'):
			if_expr = res.register(self.if_expr())
			if res.error: return res
			return res.success(if_expr)
        # For expression
		elif tok.matches(TOKEN_KEYWORD, '⏰'):
			for_expr = res.register(self.for_expr())
			if res.error: return res
			return res.success(for_expr)
        # While expression
		elif tok.matches(TOKEN_KEYWORD, '⏳'):
			while_expr = res.register(self.while_expr())
			if res.error: return res
			return res.success(while_expr)
        # Function definition
		elif tok.matches(TOKEN_KEYWORD, '💪'):
			func_def = res.register(self.func_def())
			if res.error: return res
			return res.success(func_def)
        # Dictionary (map)
		elif tok.matches(TOKEN_KEYWORD, '📚'):
			map_expr = res.register(self.map_expr())
			if res.error: return res
			return res.success(map_expr)
        # If
		return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Expected 🤷, ⏰, ⏳, 💪, 😶, int, float, identifier, grouper or string"
		))

    # If expression looks for:
    #   - If keymoji
    #   - condition
    #   - then
    #   - body1
    #   - else
    #   - body2
	def if_expr(self):
		res = ParseResult()
		cases = []
		else_case = None
        # should see an if token here
		if not self.current_tok.matches(TOKEN_KEYWORD, '🤷'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '🤷'"
			))

		res.register_advancement()
		self.advance()
        # register condition
		condition = res.register(self.expr())
		if res.error: return res
        # should see a then here
		if not self.current_tok.matches(TOKEN_KEYWORD, '👉'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '👉'"
			))

		res.register_advancement()
		self.advance()
        # Register body1
		expr = res.register(self.expr())
		if res.error: return res
        # Cases tracks condition and body1
		cases.append((condition, expr))
        # can see a then
		if self.current_tok.matches(TOKEN_KEYWORD, '👈'):
			res.register_advancement()
			self.advance()
            # if so register the body2
			else_case = res.register(self.expr())
			if res.error: return res
        # Return an if node
		return res.success(IfNode(cases, else_case))

    # While expression looks for:
    #   - while keymoji
    #   - condition
    #   - then
    #   - body
	def while_expr(self):
		res = ParseResult()
		# double check token type
		if not self.current_tok.matches(TOKEN_KEYWORD, '⏳'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '⏳'"
			))

		res.register_advancement()
		self.advance()
        # get condition
		condition = res.register(self.expr())
		if res.error: return res
        # check for then
		if not self.current_tok.matches(TOKEN_KEYWORD, '👉'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '👉'"
			))
		res.register_advancement()
		self.advance()
        # get body
		body = res.register(self.expr())
		if res.error: return res
        # return while node
		return res.success(WhileNode(condition, body))

    # For expression looks for:
    #   - for keymoji
    #   - var assign
    #   - to
    #   - expression
    #   - optional step
    #   - optional step value
    #   - body
	def for_expr(self):
		res = ParseResult()
        # double check keyword
		if not self.current_tok.matches(TOKEN_KEYWORD, '⏰'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '⏰'"
			))

		res.register_advancement()
		self.advance()
        # Need an internal variable to track condition
		if self.current_tok.type != TOKEN_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected identifier"
			))

		var_name = self.current_tok
		res.register_advancement()
		self.advance()

        # Get assignment
		if self.current_tok.type != TOKEN_EQ:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected ':='"
			))
		
		res.register_advancement()
		self.advance()
        # Get starting val
		start_value = res.register(self.expr())
		if res.error: return res
        # Get "to"
		if not self.current_tok.matches(TOKEN_KEYWORD, '🚶'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '🚶'"
			))
		
		res.register_advancement()
		self.advance()
        # get end val
		end_value = res.register(self.expr())
		if res.error: return res
        # If there's a step, register step value
		if self.current_tok.matches(TOKEN_KEYWORD, '👣'):
			res.register_advancement()
			self.advance()

			step_value = res.register(self.expr())
			if res.error: return res
		else:
			step_value = None
        # Check for then 
		if not self.current_tok.matches(TOKEN_KEYWORD, '👉'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '👉'"
			))

		res.register_advancement()
		self.advance()
        # Get main body of for loop
		body = res.register(self.expr())
		if res.error: return res
        # Return for node
		return res.success(ForNode(var_name, start_value, end_value, step_value, body))

    # Function defintion looks for:
    #   - func keymoji
    #   - function name
    #   - args in parenths
    #   - then
    #   - body
	def func_def(self):

		res = ParseResult()
		## We should have a func token if we're here
		if not self.current_tok.matches(TOKEN_KEYWORD, '💪'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '💪'"
			))
		## advance past function token
		res.register_advancement()
		self.advance()
		## get function name
		if self.current_tok.type == TOKEN_IDENTIFIER:
			func_name_tok = self.current_tok
			res.register_advancement()
			self.advance()
			if not self.current_tok.type == TOKEN_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Function name must be followed by '("
				))
        # Supports anonymous functions
		else:
			func_name_tok = None
			if not self.current_tok.type == TOKEN_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Anonymous function def should start with '('"
				))

		res.register_advancement()
		self.advance()
		## Extract Args (if any)
		args_toks = []
		if self.current_tok.type == TOKEN_IDENTIFIER:
            # Get arg tokens
			args_toks.append(self.current_tok)
			res.register_advancement()
			self.advance()
            # Look for more (, arg)
			while self.current_tok.type == TOKEN_COMMA:
				res.register_advancement()
				self.advance()
				if not self.current_tok.type == TOKEN_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Commas in function instantiation need to be followed by identifiers"
					))
				args_toks.append(self.current_tok)
				res.register_advancement()
				self.advance()
			# Need a right parenths to close it off
			if not self.current_tok.type == TOKEN_RPAREN:
				return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Function instantiation should end in ')"
					))
		else:
            # Need a right parenths to close it off
			if not self.current_tok.type == TOKEN_RPAREN:
				return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Function instantiation should end in ')"
					))
		res.register_advancement()
		self.advance()
		
		## Check for then
		if not self.current_tok.matches(TOKEN_KEYWORD, '👉'):
			return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Function instantiation should be followed by '👉"
				))
		res.register_advancement()
		self.advance()

        # Get body node out
		body_node = res.register(self.expr())
		if res.error: return res
        # Return a function node
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
		if not self.current_tok.matches(TOKEN_KEYWORD, '📚'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '📚'"
			))
		# advance past map keyword
		res.register_advancement()
		self.advance()
		# check for left pareths
		if not self.current_tok.type == TOKEN_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"map keyword must be followed by '("
				))
		# advance past left pareths
		res.register_advancement()
		self.advance()
		
		## Populate map
		while True:
			if self.current_tok.type == TOKEN_RPAREN:
				break
			if not self.current_tok.type == TOKEN_LSQUARE:
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
			if not self.current_tok.type == TOKEN_COMMA:
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
			if not self.current_tok.type == TOKEN_RSQUARE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"map must be populated with elements in form [key, val]"
				))
			res.register_advancement()
			self.advance()
			# Check for comma
			if self.current_tok.type == TOKEN_COMMA:
				res.register_advancement()
				self.advance()
				if not self.current_tok.type == TOKEN_LSQUARE:
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
    # Lists look for:
    #   - Left square
    #   - Optional comma separated expressions
    #   - Right Square
	def list_expr(self):
		res = ParseResult()
        # instantiate internal list
		elements = []
		start_pos = self.current_tok.pos_start.copy()

        # Double check initial token
		if not self.current_tok.type == TOKEN_LSQUARE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Lists should start with left square bracket"
				))
        
		res.register_advancement()
		self.advance()
        # If no elements, we can return an empty list
		if self.current_tok.type == TOKEN_RSQUARE:
			res.register_advancement()
			self.advance()
        # Otherwise, look for comma separated expressions
		else:
            # Append first expression
			elements.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Broken elements passed to list"
				))
            # While comma, look for more expressions
			while self.current_tok.type == TOKEN_COMMA:
				res.register_advancement()
				self.advance()

				elements.append(res.register(self.expr()))
				if res.error: return res
            # List must end in right square bracket
			if self.current_tok.type != TOKEN_RSQUARE:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected ',' or ']'"
				))

			res.register_advancement()
			self.advance()
        # Return list node
		return res.success(ListNode(elements, start_pos, self.current_tok.pos_end.copy()))

	# Queues look for:
    #   - Left square
    #   - Optional comma separated expressions
    #   - Right Square
	def queue_expr(self):
		res = ParseResult()
        # instantiate internal list
		elements = []
		start_pos = self.current_tok.pos_start.copy()

        # Double check initial token
		if not self.current_tok.type == TOKEN_QLSQUARE:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Lists should start with left square bracket"
				))
        
		res.register_advancement()
		self.advance()
        # If no elements, we can return an empty queue
		if self.current_tok.type == TOKEN_QRSQUARE:
			res.register_advancement()
			self.advance()
        # Otherwise, look for comma separated expressions
		else:
            # Append first expression
			elements.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Broken elements passed to list"
				))
            # While comma, look for more expressions
			while self.current_tok.type == TOKEN_COMMA:
				res.register_advancement()
				self.advance()

				elements.append(res.register(self.expr()))
				if res.error: return res
            # List must end in right square bracket
			if self.current_tok.type != TOKEN_QRSQUARE:
				return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected ',' or ']'"
				))

			res.register_advancement()
			self.advance()
        # Return list node
		return res.success(QueueNode(elements, start_pos, self.current_tok.pos_end.copy()))
