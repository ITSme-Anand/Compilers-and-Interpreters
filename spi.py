""" SPI - Simple Pascal Interpreter. Part 13. """

###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################

# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
INTEGER       = 'INTEGER'
FLOAT         = 'FLOAT'
INTEGER_CONST = 'INTEGER_CONST'
FLOAT_CONST    = 'FLOAT_CONST'
PLUS          = 'PLUS'
MINUS         = 'MINUS'
MUL           = 'MUL'
INTEGER_DIV   = 'INTEGER_DIV'
LPAREN        = 'LPAREN'
RPAREN        = 'RPAREN'
ID            = 'ID'
ASSIGN        = 'ASSIGN'
SEMI          = 'SEMI'
PROGRAM       = 'PROGRAM'
VAR           = 'VAR'
COLON         = 'COLON'
COMMA         = 'COMMA'
EOF           = 'EOF'
MAIN          = 'MAIN'
LCURLY        = 'LCURLY'
RCURLY        = 'RCURLY'
EQ            = 'EQ'
NEQ           = 'NEQ'
LT            = 'LT'
GT            = 'GT'
LTE           = 'LTE'
GTE           = 'GTE'   
AND           =   'AND'
OR            =   'OR'
NOT           =   'NOT'


class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        """String representation of the class instance.

        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
            Token(MUL, '*')
        """
        return 'Token({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )

    def __repr__(self):
        return self.__str__()


RESERVED_KEYWORDS = {
    'int': Token('INTEGER', 'int'),
    'float': Token('FLOAT', 'float'),
    'main' : Token('MAIN', 'MAIN'),
}

class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception('Invalid character')

    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '*' or self.peek() != '/':
            self.advance()
        self.advance() # close the asterik
        self.advance() # close the forward slash

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while (
                self.current_char is not None and
                self.current_char.isdigit()
            ):
                result += self.current_char
                self.advance()

            token = Token('FLOAT_CONST', float(result))
        else:
            token = Token('INTEGER_CONST', int(result))

        return token

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()

        token = RESERVED_KEYWORDS.get(result, Token(ID, result))
        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:

            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '/'  and self.peek() == '*':
                self.advance()
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isalpha():
                return self._id()

            if self.current_char.isdigit():
                return self.number()

            
            if  self.current_char == '=' and self.peek() == '=':
                self.advance()
                self.advance()
                return  Token(EQ, '==')
            
            if  self.current_char == '!' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(NEQ, '!=')
            
            if  self.current_char  == '<' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(LTE, '<=')
            
            if  self.current_char  == '<':
                self.advance()
                return Token(LT, '<')
            
            if  self.current_char  == '>' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(GTE, '>=')
            
            if  self.current_char  == '>':
                self.advance()
                return Token(GT, '>')
            
            if  self.current_char == '&' and self.peek() == '&':
                self.advance()
                self.advance()
                return Token(AND, '&&')
            
            if   self.current_char == '|' and self.peek() == '|':
                self.advance()
                self.advance()
                return Token(OR, '||')
                
            if self.current_char == '=':
                self.advance()
                return Token(ASSIGN, '=')

            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_char == ':':
                self.advance()
                return Token(COLON, ':')

            if self.current_char == ',':
                self.advance()
                return Token(COMMA, ',')

            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')

            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')

            if self.current_char == '/':
                self.advance()
                return Token(INTEGER_DIV, '/')

            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')

            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')
            
            if self.current_char == '{':
                self.advance()
                return Token(LCURLY, '{')

            if  self.current_char == '}':
                self.advance()
                return Token(RCURLY, '}')
                       
            self.error()

        return Token(EOF, None)


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################
class AST(object):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr




class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token):
        self.token = token
        self.value = token.value


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, main_node):
        self.main = main_node
        
class Main(AST):
    def __init__(self, type_node , name , block_node):
        self.type = type_node
        self.name = name
        self.block = block_node


class Block(AST):
    def __init__(self, statement_nodes):
        self.statements = statement_nodes
        

class Assignment_statement(AST):
    def __init__(self, statements):
        self.statements = statements

class declaration_statement(AST):
    def __init__(self, statements):
        self.statements = statements
        
class VarDecl(AST):
    def __init__(self, type_node , var_node, val_node):
        self.var_node = var_node
        self.type_node = type_node
        self.val_node = val_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class ProcedureDecl(AST):
    def __init__(self, proc_name, block_node):
        self.proc_name = proc_name
        self.block_node = block_node


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        """program : main"""
        main_node = self.main()
        program_node = Program(main_node)
        return program_node

    def main(self):
        """main : type_spec main LPAREN RPAREN LCURLY block RCURLY """
        type_node = self.type_spec()
        self.eat(MAIN)
        self.eat(LPAREN)
        self.eat(RPAREN)
        self.eat(LCURLY)
        block_node = self.block()
        self.eat(RCURLY)
        main_node = Main(type_node, 'main', block_node)
        return  main_node
    
    def block(self):
        """block : statement_list"""
        statement_nodes = self.statement_list()
        
        node = Block(statement_nodes)
        return node


    def type_spec(self):
        """type_spec : INTEGER
                     | REAL
        """
        
        token = self.current_token
        if self.current_token.type == INTEGER:
            self.eat(INTEGER)
        else:
            self.eat(FLOAT)
        node = Type(token)
        return node



    def statement_list(self):
        """
        statement_list : (statement)*
        """
        results = []
        while(self.current_token.type!=RCURLY):
            results.append(self.statement())


        return results

    def statement(self):
        """
        statement : declaration_statement
                  | assignment_statement
                  | empty
        """
        #print('the current token inside statements is', self.current_token)
        if self.current_token.type in [INTEGER , FLOAT]:
            node = self.declaration_statement()
        elif self.current_token.type == ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node
    
    def  declaration_statement(self):
        """declaration_statement : type variable_declaration (COMMA variable_declaration)* SEMI
        """
        type_node = self.type_spec()
        node = self.variable_declaration(type_node)
        results = [node]
        while(self.current_token.type == COMMA):
            self.eat(COMMA)
            node = self.variable_declaration(type_node)
            results.append(node)
        self.eat(SEMI)
        declaration_statement_node = declaration_statement(results)
        return declaration_statement_node
        
        
    def variable_declaration(self, type_node):
        """variable_declaration : ID | ID ASSIGN expr"""
        type_n = type_node
        var_node = self.variable()
        print('current token is ', self.current_token)
        val_node = None
        if(self.current_token.type == ASSIGN):
            self.eat(ASSIGN)
            val_node = self.expr()
        
        node = VarDecl(type_n , var_node , val_node )
        return node


    def assignment_statement(self):
        """
        assignment_statement : variable_assignment (COMMA variable_assignment)* SEMI
        """
        node = self.variable_assignment()
        results = [node]
        while self.current_token.type == COMMA:
            self.eat(COMMA)
            node = self.variable_assignment()
            results.append(node)
        
        self.eat(SEMI)
        Assignment_statement_node = Assignment_statement(results)
        return Assignment_statement_node
    
    def  variable_assignment(self):
        """variable_assignment : ID ASSIGN expr"""
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        node =  Assign(left ,  token, right)
        return node




#from here you dont need to change
    def variable(self):
        """
        variable : ID
        """
        node = Var(self.current_token)
        self.eat(ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def expr(self):
        """
        expr : term5 (OR term5)*
        """
        node = self.term5()

        while self.current_token.type == OR:
            token = self.current_token
            self.eat(OR)
            node = BinOp(left=node, op=token, right=self.term5())

        return node
    
    def term5(self):
        """
        term5 : term4 (&& term4)*
        """
        node = self.term4()
        while self.current_token.type == AND:
            token = self.current_token
            self.eat(AND)
            node = BinOp(left=node, op=token, right=self.term4())
        
        return node
    
    def term4(self):
        """
        term4 : term3 ((EQ | NEQ ) term3)*
        """
        node = self.term3()

        while self.current_token.type in (EQ, NEQ):
            token = self.current_token
            if token.type == EQ:
                self.eat(EQ)
            elif token.type == NEQ:
                self.eat(NEQ)

            node = BinOp(left=node, op=token, right=self.term3())

        return node
        
    
    def term3(self):
        """
        term3 : term2 (( LT | LTE | GT | GTE ) term2)*
        """
        node = self.term2()

        while self.current_token.type in (LT, LTE, GT , GTE):
            token = self.current_token
            if token.type == LT:
                self.eat(LT)
            elif token.type == LTE:
                self.eat(LTE)
            elif token.type == GT:
                self.eat(GT) 
            elif token.type == GTE:
                self.eat(GTE)

            node = BinOp(left=node, op=token, right=self.term2())

        return node
    
    
    def term2(self):
        node = self.term1()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
            elif token.type == MINUS:
                self.eat(MINUS)

            node = BinOp(left=node, op=token, right=self.term1())

        return node

    def term1(self):
        """term1 : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
        node = self.factor()

        while self.current_token.type in (MUL, INTEGER_DIV):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == INTEGER_DIV:
                self.eat(INTEGER_DIV)
            

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def factor(self):
        """factor : PLUS factor
                  | MINUS factor
                  | INTEGER_CONST
                  | FLOAT_CONST
                  | LPAREN expr RPAREN
                  | variable
        """
        token = self.current_token
        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.factor())
            return node  
        elif token.type == INTEGER_CONST:
            self.eat(INTEGER_CONST)
            return Num(token)
        elif token.type == FLOAT_CONST:
            self.eat(FLOAT_CONST)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def parse(self):
        """
        program : main
        
        main : type_spec MAIN LPAREN RPAREN LCURLY block RCURLY
        
        type_spec : INTEGER| FLOAT

        block : statement_list

        statement_list : (statement)*

        statement : declaration_statement
                  | assignment_statement
                  | empty
                  
        declaration_statement :  type_spec variable_list SEMICOLON

        variable _list : variable_declaration (COMMA variable_declaration)*
        
        variable_declaration : variable | variable ASSIGN expr

        assignment_statement : variable_assignment (COMMA variable_assignment) SEMI
        
        variable_assignment : variable ASSIGN expr

        empty :

        expr :  term5 ((OR) term5)*
        
        term5 : term4 (AND) term4)*
                
        term4 : term3 ((EQ | NEQ) term3)*
        
        term3 : term2 ((LT | LTE |  GT | GTE) term2)*

        term2 : term1 ((PLUS | MINUS))  term1)*

        term1 : factor ((MUL | INTEGER_DIV) factor)*

        factor : PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | REAL_CONST
               | LPAREN expr RPAREN
               | variable

        variable: ID
        """
        node = self.program()
        if self.current_token.type != EOF:
            self.error()

        return node


###############################################################################
#                                                                             #
#  AST visitors (walkers)                                                     #
#                                                                             #
###############################################################################

class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


###############################################################################
#                                                                             #
#  SYMBOLS, TABLES, SEMANTIC ANALYSIS                                         #
#                                                                             #
###############################################################################

class Symbol(object):
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class SymbolTable(object):
    def __init__(self):
        self._symbols = {}
        self._init_builtins()

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('int'))
        self.insert(BuiltinTypeSymbol('float'))

    def __str__(self):
        symtab_header = 'Symbol table contents'
        lines = ['\n', symtab_header, '_' * len(symtab_header)]
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        print('Insert: %s' % symbol.name)
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print('Lookup: %s' % name)
        symbol = self._symbols.get(name)
        # 'symbol' is either an instance of the Symbol class or None
        return symbol


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.symtab = SymbolTable()

    def visit_Block(self, node):
        for child in node.statements:
            self.visit(child)
            
    def visit_declaration_statement(self, node):
        for child in node.statements:
            self.visit(child)
    
    def visit_Assignment_statement(self , node):
        for child in node.statements:
            self.visit(child)
    
    

    def visit_Program(self, node):
        self.visit(node.main)
        
    def visit_Main(self , node):
        self.visit(node.block)

    def visit_Num(self, node):
        return node.value
    
    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.symtab.lookup(type_name)

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        # Signal an error if the table alrady has a symbol
        # with the same name
        if self.symtab.lookup(var_name) is not None:
            raise Exception(
                "Error: Duplicate identifier '%s' found" % var_name
        )
            

        self.symtab.insert(var_symbol)

    def visit_Assign(self, node):
        # right-hand side
        self.visit(node.right)
        # left-hand side
        self.visit(node.left)

    def visit_Var(self, node):
        
        var_name = node.value
        var_symbol = self.symtab.lookup(var_name)
        print('var symbol is' , var_symbol)
        if var_symbol is None:
            raise Exception(
                "Error: Symbol(identifier) not found '%s'" % var_name
        )

###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.GLOBAL_MEMORY = {}

    def visit_Program(self, node):
        self.visit(node.main)
    
    def visit_Main(self , node):
        self.visit(node.block)

    def visit_Block(self, node):
        for statement in node.statements:
            self.visit(statement)
    
    def visit_declaration_statement(self , node):
        for statement in node.statements:
            self.visit(statement)
    
    def visit_Assignment_statement(self , node):
        for statement in node.statements:
            self.visit(statement)

    def visit_VarDecl(self, node):
        var_name = node.var_node.value
        if(node.val_node is not None):
            var_value = self.visit(node.val_node)
            self.GLOBAL_MEMORY[var_name] = var_value

    def visit_Type(self, node):
        # Do nothing
        pass

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == INTEGER_DIV:
            return self.visit(node.left) / self.visit(node.right)
        elif node.op.type == EQ:
            if  self.visit(node.left) == self.visit(node.right):
                return 1
            return 0
        elif node.op.type == NEQ:
            if  self.visit(node.left) != self.visit(node.right):
                return 1
            return 0
        elif node.op.type == LT:
            if  self.visit(node.left) < self.visit(node.right):
                return 1
            return 0
        elif node.op.type == LTE:
            if  self.visit(node.left) <= self.visit(node.right):
                return 1
            return 0
        elif node.op.type == GT:
            if  self.visit(node.left) > self.visit(node.right):
                return 1
            return 0
        elif node.op.type == GTE:
            if  self.visit(node.left) >= self.visit(node.right):
                return 1
            return 0
        elif node.op.type == AND:
            if self.visit(node.left) != 0 and self.visit(node.right) != 0:
                return 1
            return 0
        elif node.op.type == OR:
            if self.visit(node.left) !=0 or  self.visit(node.right) != 0:
                return 1
            return 0

        
            

    def visit_Num(self, node):
        return node.value

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Assign(self, node):
        var_name = node.left.value
        var_value = self.visit(node.right)
        self.GLOBAL_MEMORY[var_name] = var_value

    def visit_Var(self, node):
        var_name = node.value
        var_value = self.GLOBAL_MEMORY.get(var_name)
        return var_value

    def visit_NoOp(self, node):
        pass

    def visit_ProcedureDecl(self, node):
        pass

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)


text = open('5.ip', 'r').read()

lexer = Lexer(text)
parser = Parser(lexer)
tree = parser.parse()

semantic_analyzer = SemanticAnalyzer()
""" try:
    semantic_analyzer.visit(tree)
except Exception as e:
    print(e) """
semantic_analyzer.visit(tree)

print(semantic_analyzer.symtab)

interpreter = Interpreter(tree)
result = interpreter.interpret()
print('')
print('Run-time GLOBAL_MEMORY contents:')
for k, v in sorted(interpreter.GLOBAL_MEMORY.items()):
    print('{} = {}'.format(k, v))