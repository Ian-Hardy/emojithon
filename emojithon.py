#######################################
# Code largely inspired and guided by the work by David Callanan 
# in his youtube series "Make YOUR OWN Programming Language"
# As I followed along there were quite a few changes I made, including:
# 	- Adding support for variable assignment without keywords
# 	- Adding a "bracket compound" node type to support multiple nested expressions
# 	- Implementing Skip
# 	- Implementing Backet support
# 	- Implementing Lists
#   	- Allowing access for undefined variables
# 	- Changing a bunch of syntax
# 	- Extra built-in functions
# 	- Extra arithmetic functionality for datatypes
# 	- Queue support
# 	- Dictionaries
#	- Built-in functions
# And of course, implementing all this made me deeply familiar with the base code.
# Still to do: 
#	- Multi-line support for control statements and functions
#	- Class support
#	- Recursive function callls
# Lots of fun!
#######################################

import string
from emoji_lexer import RTError, Lexer
from emoji_parser import Parser


# Need to reference these in the binop and unary op visits
TOKEN_KEYWORD			= 'KEYWORD'
TOKEN_PLUS				= 'PLUS'
TOKEN_MINUS				= 'MINUS'
TOKEN_MUL				= 'MUL'
TOKEN_DIV				= 'DIV'
TOKEN_POW				= 'POW'
TOKEN_EE				= 'EE'
TOKEN_LT				= 'LT'
TOKEN_GT				= 'GT'

#####################################################################################################################
# RUNTIME RESULT
# This class is used to evaluate the results of expressions at runtime

class RTResult:
	# It gets a return value (sometimes None) and an error (Hopefully none... but useful for debugging)
	def __init__(self):
		self.value = None
		self.error = None
	# If we register an expression, if there's no error, we return the value of the expression
	def register(self, res):
		if res.error: self.error = res.error
		return res.value
	# Success sets the value of the runtime result
	def success(self, value):
		self.value = value
		return self
	# Failure sets the error value of the runtime result
	def failure(self, error):
		self.error = error
		return self

#####################################################################################################################
# CONTEXT
# This tacks the context (dictionary) of variables and their values,
# with pointers to parent dictionaries in the case of nested contexts (i.e. in functions)

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None

#####################################################################################################################
# SYMBOL TABLE
# Symbol tables are basically dictionaries with parent pointers

class SymbolTable:
	# Instantiate empty symbol table and parent reference, if there is one 
	def __init__(self, parent=None):
		self.symbols = {}
		self.parent = parent
	
	# returns the value of a given key, or the value of the parent (recursively) if there is one
	def get(self, name):
		value = self.symbols.get(name, None)
		if value == None and self.parent:
			return self.parent.get(name)
		return value

	# Sets the value of a key to the passed value
	def set(self, name, value):
		self.symbols[name] = value

	# Deletes an element from a symbol table
	def remove(self, name):
		del self.symbols[name]

#####################################################################################################################
# VALUES
# These are the "types" of our language
# They all inherit from the "value" class, which instantiates functions because atoms can be operated on
# Some atoms cannot be operated on, and so they will throw an error in that case
# The supported values are:
#	- String
#	- Number
#	- Built in Function and Function (both inheriting from the base function class)
#	- Map
#	- List
# 	- Queue

class Value:
	# Values have a start and end position and a context
	# Initialized at None until set
	def __init__(self):
		self.set_pos()
		self.set_context()

	# set position
	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self
	# set context
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
	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)
	def get_comparison_gt(self, other):
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

# String value class
class String(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value
		self.set_pos()
		self.set_context()

	# Adding strings basically concats them
	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value+other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)
	
	# Multiplying strings by numbers repeats them N times
	def multed_by(self, other):
		if isinstance(other, Number):
			return String(self.value*other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)

	# Subtracting string A from string B deletes all instances of A from B reading left to right
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

	# Dividing a string by a number N breaks it into N equal pieces 
	# (with the Nth item potentially having fewer than (len/N) elements)
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

	# Returns true if string has a length greater than 0
	def is_true(self):
		return len(self.value) >0
	# Copy functionality
	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	# Printing representation
	def __repr__(self):
		return self.value

# Number value class
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

	# All of the functions below, unless otherwise noted, are what you'd expect

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

	# Inherits python behavior
	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value and other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)
	
	# Inherits python behavior
	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value or other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self.pos_start, other.pos_end)
	
	# Inherits python behavior
	def notted(self):
		return Number(1 if self.value == 0 else 0).set_context(self.context), None
		
	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	
	# Inherits python behavior
	def is_true(self):
		return self.value != 0
	
	def __repr__(self):
		return str(self.value)

# Setting some constants that are used throughout
Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)

# This is the base function class that Function and BuiltInFunction inherit from
class BaseFunction(Value):
	# If no function name, function name is anonymous
	def __init__(self, func_name):
		super().__init__()
		self.func_name = func_name or 'anonymous'

	# Generates the new sub context for use in the function
	# Passes in the current context too, to reference external variables
	def generate_new_context(self):
		sub_context = Context(self.func_name, self.context, self.pos_start)
		sub_context.symbol_table = SymbolTable(sub_context.parent.symbol_table)
		return sub_context

	# Checks that the number of arguments is correct
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
	
	# Sets the arg values passed to their corresponding names in the symbol table
	def fill_args(self, arg_names, args, sub_context):
		for i in range(len(args)):
			name = arg_names[i]
			value = args[i]
			value.set_context(sub_context)
			sub_context.symbol_table.set(name, value)

	# Calls the above functions
	def check_and_fill_args(self, arg_names, args, sub_context):
		res = RTResult()
		res.register(self.check_args(arg_names, args))
		if res.error: return res
		self.fill_args(arg_names, args, sub_context)
		return res.success(None)

# User-defined functions
class Function(BaseFunction):
	def __init__(self, func_name, body_node, arg_names):
		super().__init__(func_name)
		self.func_name = func_name or 'anonymous'
		self.body_node = body_node
		self.arg_names = arg_names

	# Execute function that is called when people call the function
	def execute(self, args):
		res = RTResult()
		interp = Interpreter()
		# This is basically the same functionality as in the parent class
		# Parent class was added later when creating built-in functions, I still need to clean this up
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
		
		# Once arguments are filled in, visit the body node in the context of the sub context
		val = res.register(interp.visit(self.body_node, sub_context))
		if res.error: return res
		return res.success(val)

	def copy(self):
		copy = Function(self.func_name, self.body_node, self.arg_names)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy
	# Label representation as a function in the symbol table
	def __repr__(self):
		return f"Function: {self.func_name}"

class BuiltInFunction(BaseFunction):
	def __init__(self, func_name):
		super().__init__(func_name)

	def execute(self, args):
		res = RTResult()
		sub_context = self.generate_new_context()
		# The method name is called execute_{name}, where 'name' is the built in function name
		method_name = f'execute_{self.func_name}'
		# If no method defined, return error message below
		method = getattr(self, method_name, self.no_visit_method)
		# Fill arguments and execute function
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

	# Below each funtion, we define the arg names so we cal pull them out in the function body

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

	# check if a value is a map
	def execute_is_map(self, sub_context):
		is_map = isinstance(sub_context.symbol_table.get('value'), Map)
		return RTResult().success(Number.true if is_map else Number.false)
	execute_is_map.arg_names = ['value']

	# check if a value is a queue
	def execute_is_queue(self, sub_context):
		is_queue = isinstance(sub_context.symbol_table.get('value'), Queue)
		return RTResult().success(Number.true if is_queue else Number.false)
	execute_is_queue.arg_names = ['value']
	
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

	# Basic length functionality
	def execute_len(self, sub_context):
		list_to_measure = sub_context.symbol_table.get('List')

		if not isinstance(list_to_measure, List) and not isinstance(list_to_measure, Queue):
			return RTResult().failue(RTError(
				self.pos_start, self.pos_end,
				'len argument must be a list or queue',
				sub_context
			)
			)
		length = len(list_to_measure.elements)
		return RTResult().success(Number(length))
	execute_len.arg_names = ['List']

	# pops first element form queue
	def execute_queue_pop(self, sub_context):
		retreivable_ob = sub_context.symbol_table.get('retreivable')
		if not isinstance(retreivable_ob, Queue):
			return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Queue pop can only be called on queues',
					sub_context
				)
			)
		if len(retreivable_ob.elements) == 0:
			return RTResult().failue(RTError(
				self.pos_start, self.pos_end,
				'Queue must have at least one element',
				sub_context
			)
			)
		element = retreivable_ob.elements.pop(0)
		return RTResult().success(element)
	execute_queue_pop.arg_names = ['retreivable']
	
	# sets last element of queues	
	def execute_queue_set(self, sub_context):
		retreivable_ob = sub_context.symbol_table.get('retreivable')
		val = sub_context.symbol_table.get('val')
		if not isinstance(retreivable_ob, Queue):
			return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'Queue pop can only be called on queues',
					sub_context
				)
			)
		retreivable_ob.elements.append(val)
		return RTResult().success(Number.null)
	execute_queue_set.arg_names = ['retreivable', 'val']

	# Pop from string or map at index or key
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

	# add set for Maps and Lists
	# Most of this is checks, all of the functionality is what you'd expect
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

	# Include support for loading and running external files
	def execute_load(self, sub_context):
		fp = sub_context.symbol_table.get('fp')
		if not isinstance(fp, String):
				return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'file path in load() must be a string',
					sub_context
				)
				)
		# Get string from String value
		fp = fp.value
		# Try opening the file, assign it to script (text)
		try:
			with open(fp, "r") as f:
				script = f.read()
		except Exception as ex:
			return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'failed to load file {}'.format(fp),
					sub_context
				)
				)
		# Run text
		_, error = run(fp, script)
		# If there's an error, return an error
		if error:
			return RTResult().failue(RTError(
					self.pos_start, self.pos_end,
					'failed to execute code from file {}'.format(fp),
					sub_context
				)
				)
		return RTResult().success(Number.null)
	execute_load.arg_names = ['fp']

BuiltInFunction.print 			= BuiltInFunction('print')
BuiltInFunction.print_assign 	= BuiltInFunction('print_assign')
BuiltInFunction.input 			= BuiltInFunction('input')
BuiltInFunction.is_number 		= BuiltInFunction('is_number')
BuiltInFunction.is_string 		= BuiltInFunction('is_string')
BuiltInFunction.is_func 		= BuiltInFunction('is_func')
BuiltInFunction.is_list 		= BuiltInFunction('is_list')
BuiltInFunction.len 			= BuiltInFunction('len')
BuiltInFunction.append 			= BuiltInFunction('append')
BuiltInFunction.pop 			= BuiltInFunction('pop')
BuiltInFunction.get 			= BuiltInFunction('get')
BuiltInFunction.set 			= BuiltInFunction('set')
BuiltInFunction.load 			= BuiltInFunction('load')
BuiltInFunction.queue_set 		= BuiltInFunction('queue_set')
BuiltInFunction.queue_pop 		= BuiltInFunction('queue_pop')

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
	
	# Multiply all elements by value
	# TODO: add more type checking here to prevent errors
	def multed_by(self, val):
		new_els = [Number(element.value * val.value) for element in self.elements]
		new_list = List(new_els)
		return new_list, None
	
	# If two lists are the same, return true
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

	# remove element at index val
	def subbed_by(self, val):
		new_list = self.copy()
		try:
			new_list.elements.pop(val.value)
			return new_list, None
		except:
			return None, RTError(val.pos_start, val.pos_end, 
			'Invalid Index'
			)

	# You can index a list by running list(and)index
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

# Queues are restricted in their functionality to the queue set and queue pop methods
# So this is a lot more limited
class Queue(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements

	def copy(self):
		copy = Queue(self.elements)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def __repr__(self):
		return f"{self.elements}"

#####################################################################################################################
# INTERPRETER
# This is where the magic happens!
# We get a node and a context and we visit them
# Recall that statement lists are just list types, so we're really just hitting 
# visit_ListNode first, and visiting each node in its elements

class Interpreter:
	# If there's no method for the node provided, throw an error
	def visit(self, node, context):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context):
		raise Exception(f'No visit_{type(node).__name__} method defined')

	# Just returns a number! Easy
	def visit_NumberNode(self, node, context):
		return RTResult().success(
			Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

	# Does nothing! Easy
	def visit_SkipNode(self, node, context):
		return RTResult()

	# If we have a true or false value passed, return the ccoreewsponding number
	def visit_TF_Node(self, node, context):
		res = RTResult()
		if node.tok.matches(TOKEN_KEYWORD, '👌'):
			result, error = Number(1).set_context(context), None
		elif node.tok.matches(TOKEN_KEYWORD, '🙅'):
			result, error = Number(0).set_context(context), None
		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	# Just returns a string! easy
	def visit_StringNode(self, node, context):
		return RTResult().success(
			String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

	# Looks for a variable in the symbol table; if none is there, it returns the value 0
	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.tok.value
		if var_name not in list(context.symbol_table.symbols.keys()):
			value = Number(0)
		else:
			value = context.symbol_table.get(var_name)

		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(value)

	# Assigns a new variable name (node token value) to the result of visiting the node value (expr node)
	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.tok.value
		if var_name not in list(context.symbol_table.symbols.keys()):
			context.symbol_table.set(var_name, Number(0))
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res

		context.symbol_table.set(var_name, value)
		return res.success(value)

	# Visit left and right nodes, plus any optional tokens 
	def visit_BinOpNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.error: return res
		right = res.register(self.visit(node.right_node, context))
		if res.error: return res

		if node.op_tok.type == TOKEN_PLUS:
			result, error = left.added_to(right)
		elif node.op_tok.type == TOKEN_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TOKEN_MUL:
			result, error = left.multed_by(right)
		elif node.op_tok.type == TOKEN_DIV:
			result, error = left.dived_by(right)
		elif node.op_tok.type == TOKEN_POW:
			result, error = left.powed_by(right)
		elif node.op_tok.type == TOKEN_EE:
			result, error = left.get_comparison_eq(right)
		elif node.op_tok.type == TOKEN_LT:
			result, error = left.get_comparison_lt(right)
		elif node.op_tok.type == TOKEN_GT:
			result, error = left.get_comparison_gt(right)
		elif node.op_tok.matches(TOKEN_KEYWORD, '👏'):
			result, error = left.anded_by(right)
		elif node.op_tok.matches(TOKEN_KEYWORD, '🙌'):
			result, error = left.ored_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	# Visit node, plus optional op_token
	def visit_UnaryOpNode(self, node, context):
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.error: return res

		error = None

		if node.op_tok.type == TOKEN_MINUS:
			number, error = number.multed_by(Number(-1))
		elif node.op_tok.matches(TOKEN_KEYWORD, '🚫'):
			number, error = number.notted()

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))

	# If the condition is true, visit body1
	# If not, and there's an else case, visit body2
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

	# While the condition is true (which we get from visiting condition node)
	# we repeatedly visit the body node
	def visit_WhileNode(self, node, context):
		res = RTResult()
		while True:
			condition = res.register(self.visit(node.condition_node, context))
			if res.error: return res

			if not condition.is_true(): break

			val = res.register(self.visit(node.body_node, context))
			if res.error: return res

		return res.success(val)

	# Get start, end and step
	# set up an internal condition to check if we still need to loop the body
	# Then loop the body with that condition
	def visit_ForNode(self, node, context):
		res = RTResult()

		start_value = res.register(self.visit(node.start_value_node, context))
		if res.error: return res

		end_value = res.register(self.visit(node.end_value_node, context))
		if res.error: return res

		# If no step is passed, use a step of 1
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
		last_value = None

		while condition():
			context.symbol_table.set(node.var_name_tok.value, Number(i))
			i += step_value.value

			last_value = res.register(self.visit(node.body_node, context))
			if res.error: return res

		return res.success(last_value)

	# For a bracketed list of expressions, we just visit each one
	# The value returned is the last value in the bracket list
	# This is currently how we return values from functions
	# TODO: revist this, would like some kind of early return statement
	def visit_BracCompoundNode(self, node, context):
		res = RTResult()
		for child in node.children:
			value = res.register(self.visit(child, context))
			if res.error: return res
		return res.success(value)

	# Just visit each element in the list, return a list with the elements returned from those visits
	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []

		for element in node.elements:
			elements.append(res.register(self.visit(element, context)))
		return res.success(List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	# Just visit each element in the list, return a list with the elements returned from those visits
	def visit_QueueNode(self, node, context):
		res = RTResult()
		elements = []

		for element in node.elements:
			elements.append(res.register(self.visit(element, context)))
		return res.success(Queue(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	# Similar to the above^
	def visit_MapNode(self, node, context):
		res = RTResult()
		map_ = {}
		for key in node.map_.keys():
			map_[key] = (res.register(self.visit(node.map_[key], context)))
		return res.success(Map(map_).set_context(context).set_pos(node.pos_start, node.pos_end))

	# Visiting a function node sets the function name, body, argument name, and gets the internal context set
	def visit_FuncNode(self, node, context):
		res = RTResult()
		func_name = node.func_name_tok.value if node.func_name_tok else None
		body_node = node.body_node
		args_names = [name.value for name in node.args_toks]
		func_value = Function(func_name, body_node, args_names).set_context(context).set_pos(node.pos_start, node.pos_end)
		# if it's a named function, set its value in the symbol table
		if node.func_name_tok:
			context.symbol_table.set(func_name, func_value)
		
		return res.success(func_value)
	
	# To visit a called function, get the function to call, the args, and execute
	def visit_CallFuncNode(self, node, context):
		res = RTResult()
		args = []
		# Val to call is the function name
		val_to_call = res.register(self.visit(node.func_to_call, context))
		if res.error: return res
		val_to_call = val_to_call.copy().set_pos(node.pos_start, node.pos_end)
		# Register the arguments, which could be expressions
		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.error: return res
		# Execute function with the arguments
		return_val = res.register(val_to_call.execute(args))
		if res.error: return res
		# Return result!
		return_val = return_val.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(return_val)

#####################################################################################################################
# RUN
# FINALLY we can bring it allll together.
# First of all, we'll set up our global symbol table with Null, True, False, 
# and all of our built-in functions

global_symbol_table = SymbolTable()
global_symbol_table.set('null', Number.null)
global_symbol_table.set('🙅', Number.false)
global_symbol_table.set('👌', Number.true)
global_symbol_table.set('🦜', BuiltInFunction('print'))
global_symbol_table.set('🐝', BuiltInFunction('print_assign'))
global_symbol_table.set('👀', BuiltInFunction('input'))
global_symbol_table.set('🔢', BuiltInFunction('is_number'))
global_symbol_table.set('🔤', BuiltInFunction('is_string'))
global_symbol_table.set('📜', BuiltInFunction('is_list'))
global_symbol_table.set('🐌', BuiltInFunction('append'))
global_symbol_table.set('🎉', BuiltInFunction('pop'))
global_symbol_table.set('🔧', BuiltInFunction('is_func'))
global_symbol_table.set('🐙', BuiltInFunction('get'))
global_symbol_table.set('🦥', BuiltInFunction('set'))
global_symbol_table.set('🌚', BuiltInFunction('load'))
global_symbol_table.set('📏', BuiltInFunction('len'))
global_symbol_table.set('🍫', BuiltInFunction('queue_pop'))
global_symbol_table.set('🍪', BuiltInFunction('queue_set'))

## This is the main function for running everything
# It's also called by "load"
def run(fn, text):
	# Generate tokens
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: print(error.as_string())
	if error: return None, error
	# Generate AST
	parser = Parser(tokens)
	ast_list = parser.parse()
	if ast_list.error: return None, ast_list.error
	# Run program
	interpreter = Interpreter()
	context = Context('<program>')
	context.symbol_table = global_symbol_table
	result = interpreter.visit(ast_list.node, context)
	if result.error: print(result.error.as_string())
	if result.error: return None, result.error
	# When we print our final global state, we don't want our built-in functions listed,
	# so we define a list here
	pre_defined_symbols = [
		'null',
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
	
	# We print our global symbol table alphabetically
	keys = list(context.symbol_table.symbols.keys())
	keys.sort()
	out_str = '{'
	num_iter = -1
	# This check ensures we don't print the symbol table twice while calling "load"
	if tokens[0].value != '🌚':
		# This just pretty-prints the symbol table
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
	
	# Finally, we return the result and value
	return result.value, result.error

# Main just calls run
def main():
	while True:
		text = input('emoji > ')
		run('<stdin>', text)

if __name__ == '__main__':
    main()