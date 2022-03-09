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

# CONSTANTS
## Get symbols to use for lexer
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS + '🚫'

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
				tokens.append(Token(TOKEN_NEWLINE, pos_start=self.pos))
				self.advance()
			# If it's a digit, make a number
			# can't start variable names with numbers in Emojithon
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			# identifiers
			elif self.current_char in LETTERS+pre_defined_symbols_vals or self.current_char in ('🚫', '👏','🙌'):
				tokens.append(self.make_identifier())
			# adder
			elif self.current_char == '🍐':
				tokens.append(Token(TOKEN_PLUS, pos_start=self.pos))
				self.advance()
			# statement separator 
			elif self.current_char == '🛑':
				tokens.append(Token(TOKEN_SEMI, pos_start=self.pos))
				self.advance()
			# arrow (for function body)
			elif self.current_char == '-' and self.peek() == '>':
				self.advance()
				tokens.append(self.make_arrow())
			# minus
			elif self.current_char == '🧂':
				tokens.append(Token(TOKEN_MINUS, pos_start=self.pos))
				self.advance()
			# multiply
			elif self.current_char == '🍈':
				tokens.append(Token(TOKEN_MUL, pos_start=self.pos))
				self.advance()
			# divide
			elif self.current_char == '🍆':
				tokens.append(Token(TOKEN_DIV, pos_start=self.pos))
				self.advance()
			# power
			elif self.current_char == '🐳':
				tokens.append(Token(TOKEN_POW, pos_start=self.pos))
				self.advance()
			# left parenths
			elif self.current_char == '🐣':
				tokens.append(Token(TOKEN_LPAREN, pos_start=self.pos))
				self.advance()
			# right parenths
			elif self.current_char == '🐓':
				tokens.append(Token(TOKEN_RPAREN, pos_start=self.pos))
				self.advance()
			# left curly brace
			elif self.current_char == '🌱':
				tokens.append(Token(TOKEN_LBRAC, pos_start=self.pos))
				self.advance()
			# right curly brace
			elif self.current_char == '🌳':
				tokens.append(Token(TOKEN_RBRAC, pos_start=self.pos))
				self.advance()
			# left bracket
			elif self.current_char == '🐛':
				tokens.append(Token(TOKEN_LSQUARE, pos_start=self.pos))
				self.advance()
			# right bracket
			elif self.current_char == '🦋':
				tokens.append(Token(TOKEN_RSQUARE, pos_start=self.pos))
				self.advance()
			# comma
			elif self.current_char == ',':
				tokens.append(Token(TOKEN_COMMA, pos_start=self.pos))
				self.advance()
			# quote
			elif self.current_char == '💬':
				tokens.append(self.make_string())
			# assignment
			elif self.current_char == '🙏':
				tokens.append(Token(TOKEN_EQ, pos_start=self.pos, pos_end=self.pos))
				self.advance()
			# equals op
			elif self.current_char == '😼':
				tokens.append(self.make_equals())
			# less than op
			elif self.current_char == '😾':
				tokens.append(self.make_less_than())
			# greater than op
			elif self.current_char == '😸':
				tokens.append(self.make_greater_than())
			# if not recognized, raise error
			else:
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")
		# when done, append EOF token
		tokens.append(Token(TOKEN_EOF, pos_start=self.pos))
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
			return Token(TOKEN_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TOKEN_FLOAT, float(num_str), pos_start, self.pos)
	# create identifiers
	def make_identifier(self):
		id_str = ''
		pos_start = self.pos.copy()

		while self.current_char != None and self.current_char in LETTERS_DIGITS+pre_defined_symbols_vals + '_' + '🚫' +'👏'+'🙌':
			id_str += self.current_char
			self.advance()
		
		tok_type = TOKEN_KEYWORD if id_str in KEYWORDS else TOKEN_IDENTIFIER
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
		while self.current_char != None and (self.current_char != '💬' or escape_char):
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
		return Token(TOKEN_STRING, string, pos_start,pos_end)
	
	# make an assign token
	def make_assign(self):
		tok_type = TOKEN_EQ
		pos_start = self.pos.copy()
		self.advance()
		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make an equals token
	def make_equals(self):
		pos_start = self.pos.copy()
		self.advance()
		tok_type = TOKEN_EE
		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make a less than token
	def make_less_than(self):
		tok_type = TOKEN_LT
		pos_start = self.pos.copy()
		self.advance()

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
	# make a greater than token	
	def make_greater_than(self):
		tok_type = TOKEN_GT
		pos_start = self.pos.copy()
		self.advance()

		return Token(tok_type, pos_start=pos_start, pos_end=self.pos)