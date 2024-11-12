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
CLASS         = 'CLASS'
DOT           = 'DOT'
NEW           = 'NEW'


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
    'class' : Token('CLASS', 'CLASS'),
    'new' : Token('NEW' , 'new'),
    
}

class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]
        self.virtual_pos = 0
        self.virtual_char = self.text[self.virtual_pos]

    def error(self):
        raise Exception('Invalid character')

    def advance(self , onlyPeek = False):
        """Advance the `pos` pointer and set the `current_char` variable."""
        if(onlyPeek==False):
            self.pos += 1
            if self.pos > len(self.text) - 1:
                self.current_char = None  # Indicates end of input
            else:
                self.current_char = self.text[self.pos]
        else:
            self.virtual_pos+=1
            if self.virtual_pos > len(self.text) - 1:
                self.virtual_char = None  # Indicates end of input
            else:
                self.virtual_char = self.text[self.virtual_pos]
                """ print('current virtual character after space removal is' , self.virtual_char)
                print('self pos is ', self.pos)
                print('self virtual pos is', self.virtual_pos) """

    def peek(self, onlyPeek = False ):
        if not onlyPeek:
            peek_pos = self.pos + 1
            if peek_pos > len(self.text) - 1:
                return None
            else:
                return self.text[peek_pos]
        else:
            peek_pos = self.virtual_pos + 1
            if peek_pos > len(self.text) - 1:
                return None
            else:
                return self.text[peek_pos]

    def skip_whitespace(self, onlyPeek = False):
        if(onlyPeek==False):
            while self.current_char is not None and self.current_char.isspace():
                self.advance()
        else:
            while self.virtual_char is not None and self.virtual_char.isspace():
                self.advance(True)
                
        
    def skip_comment(self , onlyPeek = False):
        if onlyPeek==False:
            while self.current_char != '*' or self.peek() != '/':
                self.advance()
            self.advance() # close the asterik
            self.advance() # close the forward slash
        else:
            while self.virtual_char!= '*' or self.text[self.virtual_pos+1]!= '/':
                self.advance(True)
            self.advance(True)
            self.advance(True)

    def number(self, onlyPeek = False):
        """Return a (multidigit) integer or float consumed from the input."""
        if onlyPeek==False:
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
        else:
            result = ''
            while self.virtual_char is not None and self.virtual_char.isdigit():
                result += self.virtual_char
                self.advance(True)

            if self.virtual_char == '.':
                result += self.virtual_char
                self.advance(True)

                while (
                    self.virtual_char is not None and
                    self.virtual_char.isdigit()
                ):
                    result += self.virtual_char
                    self.advance(True)

                token = Token('FLOAT_CONST', float(result))
            else:
                token = Token('INTEGER_CONST', int(result))

            return token
            

    def _id(self , onlyPeek = False):
        """Handle identifiers and reserved keywords"""
        if(onlyPeek==False):
            result = ''
            while self.current_char is not None and self.current_char.isalnum():
                result += self.current_char
                self.advance()

            token = RESERVED_KEYWORDS.get(result, Token(ID, result))
            return token
        else:
            result = ''
            while self.virtual_char is not None and self.virtual_char.isalnum():
                #print(self.virtual_char)
                result += self.virtual_char
                self.advance(True)

            token = RESERVED_KEYWORDS.get(result, Token(ID, result))
            return token

    
    def get_next_token(self, onlyPeek = False):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        if onlyPeek==False:
            while self.current_char is not None:

                if self.current_char.isspace():
                    self.skip_whitespace(onlyPeek)
                    continue

                if self.current_char == '/'  and self.peek() == '*':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    self.skip_comment(onlyPeek)
                    continue

                if self.current_char.isalpha():
                    return self._id(onlyPeek)

                if self.current_char.isdigit():
                    return self.number(onlyPeek)

                
                if  self.current_char == '=' and self.peek() == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return  Token(EQ, '==')
                
                if  self.current_char == '!' and self.peek() == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(NEQ, '!=')
                
                if  self.current_char  == '<' and self.peek() == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(LTE, '<=')
                
                if  self.current_char  == '<':
                    self.advance(onlyPeek)
                    return Token(LT, '<')
                
                if  self.current_char  == '>' and self.peek() == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(GTE, '>=')
                
                if  self.current_char  == '>':
                    self.advance(onlyPeek)
                    return Token(GT, '>')
                
                if  self.current_char == '&' and self.peek() == '&':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(AND, '&&')
                
                if   self.current_char == '|' and self.peek() == '|':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(OR, '||')
                    
                if self.current_char == '=':
                    self.advance(onlyPeek)
                    return Token(ASSIGN, '=')

                if self.current_char == ';':
                    self.advance(onlyPeek)
                    return Token(SEMI, ';')

                if self.current_char == ':':
                    self.advance(onlyPeek)
                    return Token(COLON, ':')

                if self.current_char == ',':
                    self.advance(onlyPeek)
                    return Token(COMMA, ',')

                if self.current_char == '+':
                    self.advance(onlyPeek)
                    return Token(PLUS, '+')

                if self.current_char == '-':
                    self.advance(onlyPeek)
                    return Token(MINUS, '-')

                if self.current_char == '*':
                    self.advance(onlyPeek)
                    return Token(MUL, '*')

                if self.current_char == '/':
                    self.advance(onlyPeek)
                    return Token(INTEGER_DIV, '/')

                if self.current_char == '(':
                    self.advance(onlyPeek)
                    return Token(LPAREN, '(')

                if self.current_char == ')':
                    self.advance(onlyPeek)
                    return Token(RPAREN, ')')
                
                if self.current_char == '{':
                    self.advance(onlyPeek)
                    return Token(LCURLY, '{')

                if  self.current_char == '}':
                    self.advance(onlyPeek)
                    return Token(RCURLY, '}')
                
                if self.current_char == '.':
                    self.advance(onlyPeek)
                    return Token(DOT, '.')
                
                
                        
                self.error()

            return Token(EOF, None)
        else:
            while self.virtual_char is not None:

                if self.virtual_char.isspace():
                    self.skip_whitespace(onlyPeek)
                    continue

                if self.virtual_char == '/'  and self.peek(True) == '*':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    self.skip_comment(onlyPeek)
                    continue

                if self.virtual_char.isalpha():
                    return self._id(onlyPeek)

                if self.virtual_char.isdigit():
                    return self.number(onlyPeek)

                
                if  self.virtual_char == '=' and self.peek(True) == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return  Token(EQ, '==')
                
                if  self.virtual_char == '!' and self.peek(True) == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(NEQ, '!=')
                
                if  self.virtual_char  == '<' and self.peek(True) == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(LTE, '<=')
                
                if  self.virtual_char  == '<':
                    self.advance(onlyPeek)
                    return Token(LT, '<')
                
                if  self.virtual_char  == '>' and self.peek(True) == '=':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(GTE, '>=')
                
                if  self.virtual_char  == '>':
                    self.advance(onlyPeek)
                    return Token(GT, '>')
                
                if  self.virtual_char == '&' and self.peek(True) == '&':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(AND, '&&')
                
                if   self.virtual_char == '|' and self.peek(True) == '|':
                    self.advance(onlyPeek)
                    self.advance(onlyPeek)
                    return Token(OR, '||')
                    
                if self.virtual_char == '=':
                    self.advance(onlyPeek)
                    return Token(ASSIGN, '=')

                if self.virtual_char == ';':
                    self.advance(onlyPeek)
                    return Token(SEMI, ';')

                if self.virtual_char == ':':
                    self.advance(onlyPeek)
                    return Token(COLON, ':')

                if self.virtual_char == ',':
                    self.advance(onlyPeek)
                    return Token(COMMA, ',')

                if self.virtual_char == '+':
                    self.advance(onlyPeek)
                    return Token(PLUS, '+')

                if self.virtual_char == '-':
                    self.advance(onlyPeek)
                    return Token(MINUS, '-')

                if self.virtual_char == '*':
                    self.advance(onlyPeek)
                    return Token(MUL, '*')

                if self.virtual_char == '/':
                    self.advance(onlyPeek)
                    return Token(INTEGER_DIV, '/')

                if self.virtual_char == '(':
                    self.advance(onlyPeek)
                    return Token(LPAREN, '(')

                if self.virtual_char == ')':
                    self.advance(onlyPeek)
                    return Token(RPAREN, ')')
                
                if self.virtual_char == '{':
                    self.advance(onlyPeek)
                    return Token(LCURLY, '{')

                if  self.virtual_char == '}':
                    self.advance(onlyPeek)
                    return Token(RCURLY, '}')
                
                if self.current_char == '.':
                    self.advance(onlyPeek)
                    return Token(DOT, '.')
                        
                self.error()
            
            return Token(EOF , None)
        
        
        
    
    def peekNextToken(self):
        #print('we are into the peekNexttoken function ')
        self.virtual_pos = self.pos
        if(self.virtual_pos> len(text)-1):
            return
        self.virtual_char = text[self.virtual_pos]
        #print('virtual character is ' , self.virtual_char)
        #print('next virtual character is ' , text[self.virtual_pos+1])
        token = self.get_next_token(True)
        return token

    

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
    def __init__(self, token , type = None):
        self.token = token
        self.value = token.value
        self.type = type

class NoOp(AST):
    pass

#edited
class Program(AST):
    def __init__(self, classDeclarations):
        self.classDeclarations = classDeclarations
        
class ClassDeclaration(AST):
    def __init__(self, name , body):
        self.name = name
        self.body = body
        
class Constructor(AST):
    def __init__(self, name , Params , body):
        self.name = name
        self.Params = Params
        self.body = body

class MethodDeclaration(AST):
    def  __init__(self, type , name , Params , body):
        self.type = type
        self.name = name
        self.Params = Params
        self.body = body

class VariableDeclaration(AST):
    def __init__(self, type, variables):
        self.type = type
        self.variables = variables

class ParamDeclaration(AST):
    def  __init__(self, type, name):
        self.type = type
        self.name = name

class MethodCall(AST):
    def __init__(self, name, params , object=None):
        self.object = object
        self.name = name
        self.arguments = params

class Block(AST):
    def __init__(self, statement_nodes):
        self.statements = statement_nodes
        

class ObjectAttribute(AST):
    def __init__(self, objectName , attributeName):
        self.objectName = objectName
        self.attributeName = attributeName
        
class ObjectCreation(AST):
    def __init__(self, className, arguments):
        self.className = className
        self.arguments = arguments


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class ObjectType(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


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
        """program : (Class_declarations)"""
        #print('we are at top of program')
        class_declarations = []
        #print('current token is' , self.current_token)
        #print('the next token is ' , self.lexer.peekNextToken())
        while self.current_token.type == CLASS:
            node = self.ClassDeclaration()
            class_declarations.append(node)
        
        program_node = Program(class_declarations)
        return program_node
    
    def ClassDeclaration(self):
        """ClassDeclaration : CLASS variable LCURLY class_body RCURLY""" 
        self.eat(CLASS)
        #print(self.current_token)
        className = self.variable()
        self.eat(LCURLY)
        classBody = self.ClassBody()
        self.eat(RCURLY)
        Class_node = ClassDeclaration(className, classBody)
        return Class_node
        
    def ClassBody(self):
        """
            ClassBody : (field_declaration | constructor | method_Declaration | method call)*
        """
        class_body = []
        while self.current_token.type!=RCURLY:
            if self.current_token.type==ID and self.lexer.peekNextToken().type == LPAREN:
                node = self.Constructor()
                class_body.append(node)
            else:
                #either a field declaration or method declaration
                #field declaration : int|float|object variable = expr;
                #method declaration : int|float|object variable LPAREN parameter_list RPAREN LCURLY statement_list RCURLY
                """ print('we are inside else statement of classbody ')
                print(self.current_token)
                print(self.lexer.peekNextToken()) """
                type_node = None
                if(self.current_token.type==ID):
                    type_node = self.objectType()
                else:
                    type_node = self.type_spec()
                var_node = self.variable()
                if self.current_token.type == LPAREN:
                    #then it is a method declaration
                    node = self.methodDeclaration(type_node , var_node)
                else:
                    node = self.fieldDeclaration(type_node , var_node)
                class_body.append(node)
                    
                
            
        return class_body
    
    def Constructor(self):
        class_Name = self.variable()
        self.eat(LPAREN)
        params = self.paramsList()
        self.eat(RPAREN)
        self.eat(LCURLY)
        constructorBlock = self.block()
        self.eat(RCURLY)
        ConstructorNode = Constructor(class_Name,  params, constructorBlock)
        return ConstructorNode
    
    def  fieldDeclaration(self, type_node, var_node):
        assign_nodes = []
        if self.current_token.type == ASSIGN:
            token =  self.current_token
            self.eat(ASSIGN)
            valNode = self.expr()
            assn_node = Assign(var_node , token , valNode )
            assign_nodes.append(assn_node)
        else:
            valNode = None
            assn_node = Assign(var_node , None , valNode )
            assign_nodes.append(assn_node)
        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_node = self.variable()
            if self.current_token.type == ASSIGN:
                token =  self.current_token
                self.eat(ASSIGN)
                valNode = self.expr()
                assn_node = Assign(var_node , token , valNode )
                assign_nodes.append(assn_node)
            else:
                assn_node = Assign(var_node , None , None)
                assign_nodes.append(assn_node)
        self.eat(SEMI)
        return VariableDeclaration(type_node , assign_nodes)
    
    def  methodDeclaration(self, type_node, var_node):
        self.eat(LPAREN)
        params = self.paramsList()
        self.eat(RPAREN)
        self.eat(LCURLY)
        block = self.block()
        self.eat(RCURLY)
        return MethodDeclaration(type_node, var_node, params, block)
         
    def paramsList(self):
        params = []
        typeNode = None
        if self.current_token.type!=RPAREN:
            if(self.current_token.type==ID):
                typeNode = self.objectType()
            else:
                typeNode = self.type_spec()
            varNode = self.variable()
            params.append(ParamDeclaration(typeNode , varNode))
        while self.current_token.type==COMMA:
            self.eat(COMMA)
            if(self.current_token.type==ID):
                typeNode = self.objectType()
            else:
                typeNode = self.type_spec()
            varNode = self.variable()
            params.append(ParamDeclaration(typeNode , varNode))
        return params
     
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
        #print('we are in type_spec ')
        #print(self.current_token)
        if self.current_token.type == INTEGER:
            self.eat(INTEGER)
        else:
            self.eat(FLOAT)
        node = Type(token)
        return node
    
    def objectType(self):
        """type_spec : INTEGER
                     | REAL
        """
        token = self.current_token
        self.eat(ID)
        node = ObjectType(token)
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
        statement : declaration_statement (object declaration | built-in declaration)
                  | assignment_statement (object assignment | object attribute assignment | built-in assignment)
                  | method call (static method | instance method)
                  | empty
        """
        ##print('the current token inside statements is', self.current_token)
        #this handles declaration statment of built-in types
        if self.current_token.type in [INTEGER , FLOAT]:
            node = self.declaration_statement()
        
        #this handles object attribute assignment and instance method call
        elif self.current_token.type == ID and self.lexer.peekNextToken().type== DOT:
            
            objectName = self.variable()
            self.eat(DOT)
            methodName = self.variable()
            if(self.current_token.type == ASSIGN):
                #then it is an assignment statement
                token = self.current_token
                self.eat(ASSIGN)
                exprNode = self.expr()
                node = Assign(ObjectAttribute(objectName, methodName), token , exprNode)
                self.eat(SEMI)
                return node
            self.eat(LPAREN)
            arguments = []
            while True:
                if(self.current_token.type==RPAREN):
                        break
                val_node = self.expr()
                arguments.append(val_node)
                if self.current_token.type == COMMA:
                    self.eat(COMMA)
                else:
                    break
            self.eat(RPAREN)
            #print('we encountered a method call')
            node = MethodCall(methodName , arguments , objectName)
            self.eat(SEMI)
            return node
        
        #this handles static method call        
        elif self.current_token.type == ID and self.lexer.peekNextToken().type == LPAREN:
            #print('we are in static method call')
            #this is a static method call
            var_node = self.variable()
            self.eat(LPAREN)
            arguments = []
            while True:
                if(self.current_token.type==RPAREN):
                        break
                val_node = self.expr()
                arguments.append(val_node)
                if self.current_token.type == COMMA:
                    self.eat(COMMA)
                else:
                    break
            self.eat(RPAREN)
            #print('we encountered a method call')
            node = MethodCall(var_node , arguments)
            self.eat(SEMI)
            return node
        
        #this handles assignment statements of built-in type variables and object-type variables
        elif self.current_token.type == ID and self.lexer.peekNextToken().type == ASSIGN:
            #this is for parsing the assignment statements
            node = self.assignment_statement()
        
        #we need to handle object declaration (creation) using the new keyword
        elif self.current_token.type == ID and self.lexer.peekNextToken().type == ID:
            #this is the case to handle object creation using new keyword
            node = self.declaration_statement(True)
            return node
        else:
            #print('current token is ', self.current_token)
            #print(self.current_token)
            #print(self.lexer.peekNextToken())
            node = self.empty()
        return node
    
    def declaration_statement(self , objectDeclaration=False):
        """declaration_statement : type variable_declaration (COMMA variable_declaration)* SEMI
        """
        type_node = None
        if(objectDeclaration==True):
            type_node = self.objectType()
        else:
            type_node = self.type_spec()
        node = self.variable_declaration()
        assignNodes = [node]
        while(self.current_token.type == COMMA):
            self.eat(COMMA)
            node = self.variable_declaration()
            assignNodes.append(node)
        self.eat(SEMI)
        declaration_statement_node = VariableDeclaration(type_node , assignNodes)
        return declaration_statement_node
            
    def variable_declaration(self):
        """variable_declaration : ID | ID ASSIGN expr"""
        var_node = self.variable()
        #print('current token is ', self.current_token)
        token = None
        val_node = None
        if(self.current_token.type == ASSIGN):
            token = self.current_token
            self.eat(ASSIGN)
            val_node = self.expr()
        
        node = Assign(var_node , token,  val_node )
        return node

    def assignment_statement(self):
        """
        assignment_statement : variable_assignment SEMI
        """
        node = self.variable_assignment()
        self.eat(SEMI)
        return node
    
    def  variable_assignment(self):
        """variable_assignment : ID ASSIGN expr"""
        """ print('we are inside variable assignment')
        print(self.current_token) """
        
        left = self.variable()
        """ print(self.current_token)
        print('next token is :',self.lexer.peekNextToken()) """
        if(self.current_token.type == ASSIGN):
            #print('so we are inside this')
            token = self.current_token
            self.eat(ASSIGN)
            right = self.expr()
            node =  Assign(left , token, right)
            return node
        elif(self.current_token.type == DOT):
            print('we are inside elif block of variable assignment')
            self.eat(DOT)
            var_name = self.variable()
            token = self.current_token
            self.eat(ASSIGN)
            right = self.expr()
            node =  Assign(ObjectAttribute(left , var_name) , token, right)
            return node
        else:
            self.error()

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
        elif token.type == NEW:
            self.eat(NEW)
            className = self.variable()
            self.eat(LPAREN)
            arguments = []
            while self.current_token.type!=RPAREN:
                
                argument = self.expr()
                arguments.append(argument)
                if(self.current_token.type==COMMA):
                    self.eat(COMMA)
                    continue
                else:
                    break
            self.eat(RPAREN)
            #print('arguments before pushing in are' , arguments)
            objectInitializer_node = ObjectCreation(className , arguments)
            return objectInitializer_node
        
        else:
            var_node = self.variable()
            if(self.current_token.type == DOT):
                #print('current token type is DOT')
                self.eat(DOT)
                methodName = self.variable()
                if self.current_token.type == LPAREN:
                    self.eat(LPAREN)
                    arguments = []
                    while True:
                        if(self.current_token.type==RPAREN):
                            break
                        val_node = self.expr()
                        arguments.append(val_node)
                        if(self.current_token.type == COMMA):
                            self.eat(COMMA)
                        else:
                            break
                        
                    self.eat(RPAREN)
                    node = MethodCall(methodName , arguments , var_node )
                    return node
                else:
                    node = ObjectAttribute(var_node, methodName)
                    return node
            if(self.current_token.type == LPAREN):
                self.eat(LPAREN)
                arguments = []
                while True:
                    if(self.current_token.type==RPAREN):
                        break
                    val_node = self.expr()
                    arguments.append(val_node)
                    if(self.current_token.type == COMMA):
                        self.eat(COMMA)
                    else:
                        break
                self.eat(RPAREN)
                node = MethodCall(var_node , arguments)
                return node
            else:
                return var_node

    def parse(self):
        """
        program : main
        
        main : type_spec MAIN LPAREN RPAREN LCURLY block RCURLY
        
        type_spec : INTEGER

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

        expr : term4 ((PLUS | MINUS) term4)*
                
        term4 : term3 ((EQ | NEQ) term3)*
        
        term3 : term2 ((LT | LTE |  GT | GTE) term2)*

        term2 : term1 ((PLUS | MINUS))  term1)*

        term1 : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | REAL_CONST
               | LPAREN expr RPAREN
               | variable

        variable: ID
        """
        #print(self.current_token)
        node = self.program()
        #print('we reached here')
        if self.current_token.type != EOF:
            print(self.current_token)
            self.error()

        return node


###############################################################################
#                                                                             #
#  AST visitors (walkers)                                                     #
#                                                                             #
###############################################################################

class NodeVisitor(object):
    def visit(self, node , visitBlock = False):
        
        method_name = 'visit_' + type(node).__name__
        #print('inside visit method: ', method_name )
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node , visitBlock )

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
    def __init__(self, name, type , value=None):
        super().__init__(name, type)
        self.value = value

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__
    
    def getValue(self):
        return self.value
    
    def getName(self):
        return self.name
    
class ObjVarSymbol(Symbol):
    def __init__(self, name, type , value=None):
        super().__init__(name, type)
        self.value = value
    def __str__(self):
        if self.value is None:
            return "<{class_name}(name='{name}', type='{type}')>".format(
                class_name=self.__class__.__name__,
                name=self.name,
                type=self.type,
            )
        return "<{class_name}(name='{name}', type='{type}'  \nvalue='{value}')>".format(
                class_name=self.__class__.__name__,
                name=self.name,
                type=self.type,
                value = self.value
            )

    __repr__ = __str__
    
    def getClass(self):
        return self.type
    
    def getValue(self):
        return self.value

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

class ClassSymbol(Symbol):
    def __init__(self, name, Signature=None , containsMain = False):
        super().__init__(name)
        self.signature = Signature
        self.containsMain = containsMain

    def __repr__(self):
        if self.signature is None:
            return "<{class_name}(name='{name}')>".format(
                class_name=self.__class__.__name__,
                name = self.name
                
            )
        result0 = "Class Declaration \n\nSignature:\n"
        result1 = "\n    Variable Declarations:\n\t{}".format("\n\t".join(f"{key} : {value}" for key, value in self.signature[0].items()))
        result2 = "\n\n    Constructor Declarations:\n\t{}".format("\n\t".join(f"{key} : {value}" for key, value in self.signature[1].items()))
        result3 = "\n\n    Methods Declarations:\n\t{}".format("\n\t".join(f"{key} : {value}" for key, value in self.signature[2].items()))

        return result0 + result1 + result2 + result3
    
    __str__ = __repr__
    
    def getName(self):
        return self.name
    
    def getAttributes(self):
        return self.signature[0]
    
    def getConstructors(self):
        return self.signature[1]
    
    def getMethods(self):
        return self.signature[2]
    
class ConstructorSymbol(Symbol):
    def __init__(self, name, params , body):
        super().__init__(name)
        self.params = params
        self.body = body
    
    
    def __repr__(self):
        return "<{class_name}(name='{name}'  params='{params}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params = self.params
    )
    __str__ = __repr__
    
    def getBody(self):
        return self.body
    
    def getParams(self):
        return self.params
    
class MethodSymbol(Symbol):
    def __init__(self, name, params, return_type , body , isMain=False):
        super().__init__(name)
        self.params = params
        self.return_type = return_type
        self.body = body
        self.isMain= isMain
        
    
    def getBody(self):
        return self.body
    
    def getParams(self):
        return self.params
    
    def __repr__(self):
        return "<{class_name}(name='{name}'   params='{params}'   returntype='{ret_type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params = self.params,
            ret_type = self.return_type
    )
    
    __str__ = __repr__

class Scope:
    def __init__(self, level):
        self.level = level
        self.symbols = {}  # Dictionary to store symbols within this scope
        
    def print_scope(self):
        print(f"---- Scope Level {self.level} ----")
        if not self.symbols:
            print("  (No symbols)")
        else:
            for symbol, value in self.symbols.items():
                print(f"{symbol}: {value}")
        print("----------------------------")

class ScopeStack:
    def __init__(self):
        self.stack = []
        self.current_level = 0

    def push_scope(self):
        #print(self.current_level)
        scope = Scope(self.current_level)
        self.stack.append(scope)
        self.current_level += 1
        #print(self.current_level)

    def pop_scope(self):
        self.stack.pop()
        self.current_level -= 1

    def current_scope(self):
        #print(self.stack[-1].level)
        return self.stack[-1] if self.stack else None
    
    

class SymbolTable(object):
    def __init__(self , scope_stack ):
        self._builtTypes = {}
        self._classes = {}
        self._classInfo = {}
        self.scope_stack = scope_stack
        self.scope_stack.push_scope()
        self._init_builtins()
        self.memoryScopeStack = None


    def _init_builtins(self):
        self.insert_builtType(BuiltinTypeSymbol('int'))
        self.insert_builtType(BuiltinTypeSymbol('float'))
        
    def getClassInfo(self, name):
        return self._classInfo[name]

    def initializeMemoryScopeStack(self):
        self.memoryScopeStack = ScopeStack()
        return self.memoryScopeStack
    
    def insert(self, symbol):
        self.scope_stack.current_scope().symbols[symbol.name] = symbol
        
    def insertMemory(self , key , value):
        self.memoryScopeStack.current_scope().symbols[key] = value
        
    def lookUpMemory(self , key):
        return self.memoryScopeStack.current_scope().symbols.get(key)

    def lookup(self, name):
        # Traverse the stack from top (current scope) to bottom (global scope)
        for scope in reversed(self.scope_stack.stack):
            if name in scope.symbols:
                return scope.symbols[name]
        return None  # Symbol not found
    
    def lookup_currentScope(self, name):
        scope = self.scope_stack.current_scope()
        #print(scope.symbols)
        #print('name searched is:', name)
        return scope.symbols.get(name)
        
    
    def insert_class(self, symbol):
        self._classes[symbol.name] = symbol
    
    def lookup_class(self, name):
        symbol = self._classes.get(name)
        return symbol
    
    def insert_builtType(self , symbol):
        self._builtTypes[symbol.name] = symbol
    
    def lookup_builtType(self, name):
        symbol = self._builtTypes.get(name)
        return symbol
    
    def lookup_types(self, name):
        symbol = self.lookup_class(name)
        if symbol is None:
            symbol = self.lookup_builtType(name)
        return symbol


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.symtab = SymbolTable(ScopeStack())

    def visit_Program(self, node , visitBlock=False):
        self.symtab.scope_stack.push_scope()
        
        for Class in node.classDeclarations:
            className = Class.name.value
            self.symtab.insert(ClassSymbol(className))
            self.symtab.insert_class(ClassSymbol(className))
             
        for Class in node.classDeclarations:
            symbols = self.visit(Class)
            #print('symbols of class are: \n')
            method_symbols = {}
            var_symbols = {}
            constructor_symbols = {}
            containsMain = False
            for i in symbols:
                if isinstance(symbols[i], VarSymbol):
                    var_symbols[i] = symbols[i]
                elif isinstance(symbols[i], ObjVarSymbol):
                    var_symbols[i] = symbols[i]
                elif isinstance(symbols[i], MethodSymbol):
                    if symbols[i].isMain:
                        containsMain = True
                    method_symbols[i] = symbols[i]
                elif isinstance(symbols[i], ConstructorSymbol):
                    constructor_symbols[i] = symbols[i]
            newClassSymbol = ClassSymbol(Class.name.value , [var_symbols , constructor_symbols , method_symbols], containsMain)
            self.symtab._classInfo[Class.name.value] = newClassSymbol
            self.symtab.scope_stack.current_scope().symbols[ Class.name.value] = newClassSymbol
        
        for Class in node.classDeclarations:
            self.visit(Class, True)
        #self.symtab.scope_stack.current_scope().print_scope()
        self.symtab.scope_stack.pop_scope()
                 
    def visit_ClassDeclaration(self , node , visitBlock=False):
        className = node.name.value
        self.symtab.scope_stack.push_scope()
        self.symtab.insert(ClassSymbol(className))
        for statement in node.body:
            # statement : field declaration | method declaration | constructor , 
            self.visit(statement , visitBlock)
        #once you finish visiting all the statements in the class body, you can take all the symbols
        #in the current scope and add them to the class symbol
        #self.symtab.scope_stack.current_scope().print_scope()
        class_Scope_symbols = self.symtab.scope_stack.current_scope().symbols.copy()
        self.symtab.scope_stack.pop_scope()
        return class_Scope_symbols
            
    def visit_VariableDeclaration(self, node , blockVisit=False):
        #what are we doing here so far? 
        """
        we get the type. if type does not exist , we report that error.
        we also check if assigned expression is a variable , we made sure that we cannot use new creation for built in types
        what else to do? if right side is a variable , check if its type is correctly assigned to left
        
        """
        type_name = node.type.value
        type_symbol = self.symtab.lookup_types(type_name)
        if type_symbol is None:
            raise Exception("Type '%s' not found" %type_name)
        
        for variable in node.variables:
            
            if variable.token is not None:
                #we make sure the assigned expression (if a variable/object.Attribute) already exists
                objCreationVal = self.visit(variable.right, blockVisit)
                if isinstance(type_symbol , BuiltinTypeSymbol):
                    if isinstance(variable.right , ObjectCreation):
                        raise Exception("Cannot assign object to primitive type")
                    #we also need to check if the variable is assigned to the newly created object.
                    #if it is , then we should check the type of the variable - maybe we will do it in runtime
                    
            var_name = variable.left.value
            if isinstance(variable.right, ObjectCreation) and objCreationVal is not None:
                #print('*'+ type_name + '*')
                #print('*' + objCreationVal.getName() + '*')
                if(type_name!=objCreationVal.getName()):
                    
                    raise Exception("Type mismatch for variable '%s'" %var_name)
            var_symbol = None
            if isinstance(type_symbol, BuiltinTypeSymbol):
                var_symbol = VarSymbol(var_name, type_symbol, variable.right)
            if isinstance(type_symbol , ClassSymbol):
                var_symbol = ObjVarSymbol(var_name , type_symbol, variable.right)
            #if current scope already has a variable with the same name , then signal an error
            if self.symtab.lookup_currentScope(var_name) is not None:
                raise Exception(
                    "Error: Duplicate identifier '%s' found" % var_name
            ) 
            self.symtab.insert(var_symbol)
        return
    
    def visit_Constructor(self, node , blockVisit=False):
        #what did do so far?
        """
        it looks if the constructor name is same as the class name. otherwise error is raised
        parameter type checking and declaration checking is done.
        we are yet to completely check the constructor block.
        """
        #print('we are in visit_constructor')
        name = node.name.value
        #self.symtab.scope_stack.current_scope().print_scope()
        class_symbol = self.symtab.lookup_currentScope(name)
        #print(class_symbol)
        if class_symbol is None or not isinstance(class_symbol, ClassSymbol):
            raise Exception("Error: constructor and class name does not match")
        self.symtab.scope_stack.push_scope()
        params = []
        for param in node.Params:
            #we need to add params to the current scope as well as we need to put it into the class symbol
            params.append(self.visit(param, blockVisit))
        #we need to work on the body of the constructor/method yet
        if blockVisit:
            self.visit(node.body, blockVisit)
        cons_symbol = ConstructorSymbol(name , params , node.body)
        self.symtab.scope_stack.pop_scope()
        self.symtab.scope_stack.current_scope().symbols[name] = cons_symbol
        #self.symtab.insert(cons_symbol)   name of cons_symbol already exists, we need to replace it
        return
    
    def visit_ParamDeclaration(self, node , visitBlock = False):
        #print('inside visit ParamDeclaration')
        #what did we do here so far?
        """
        checked if the type exists.
        checked if there is a variable already declared in the constructor/method scope (before the current parameter)
        """
        type_name = node.type.value
        type_symbol = self.symtab.lookup_types(type_name)
        if type_symbol is None:
            raise Exception("Error: Type '%s' not found" % type_name)
        var_name = node.name.value
        var_symbol = None
        if isinstance(type_symbol, BuiltinTypeSymbol):
            var_symbol = VarSymbol(var_name, type_symbol)
        if isinstance(type_symbol , ClassSymbol):
            var_symbol = ObjVarSymbol(var_name , type_symbol)
        #if current scope already has a variable with the same name , then signal an error
        if self.symtab.lookup_currentScope(var_name) is not None:
            raise Exception(
                "Error: Duplicate identifier '%s' found" % var_name
        ) 
        self.symtab.insert(var_symbol)
        return var_symbol
    
    def visit_MethodDeclaration(self, node, blockVisit=False):
        #we need to create a method symbol and push it onto the class scope.
        #what did we do so far?
        """
        checked if the return type is a valid return type
        checked if the method name already exists in the class scope
        we also checked the params - types and distinct declaration
        """
        return_type_name = node.type.value
        type_symbol = self.symtab.lookup_types(return_type_name)
        if type_symbol is None:
            raise Exception("Error: Type '%s' not found" % return_type_name)
        method_name = node.name.value
        mainFunction = False
        if method_name=="main":
            mainFunction = True
        if self.symtab.lookup_currentScope(method_name) is not None:
            raise Exception(
                "Error: Duplicate Method Name '%s' found" % method_name
        ) 
        self.symtab.scope_stack.push_scope()
        params = []
        for param in node.Params:
            #we need to add params to the current scope as well as we need to put it into the class symbol
            params.append(self.visit(param))
        #we need to work on the body of the constructor/method yet
        if blockVisit:
            self.visit(node.body , blockVisit)
        method_symb = MethodSymbol(method_name , params , type_symbol , node.body , mainFunction)
        self.symtab.scope_stack.pop_scope()
        self.symtab.insert(method_symb)
        return
            
    def visit_Block(self, node , visitBlock = False):
        for child in node.statements:
            self.visit(child , visitBlock)
                     
    def visit_Assign(self, node , visitBlock = False):
        
        # takes care of all assignment statements
        objCreationCons = self.visit(node.right , visitBlock)
        #print('objCreationCons is', objCreationCons)
        #left hand side can be either a variable or an object attribute
        self.visit(node.left , visitBlock)
        #we verified if both sides already exists and if valid.
        
        
        #different possibilities:
        """
        left hand side: variable (builtIn | object) , attribute
        right hand side : variable (builtIn | object) , attribute , literal , expression (can be handled later), object creation
        """
        if isinstance(objCreationCons , ClassSymbol):
            #print('we got the object creation' , objCreationCons)
            if isinstance(node.left , Var):
                left_var_symbol = self.symtab.lookup(node.left.value)
                #print('checking if they are equal or not')
                #print('*'+objCreationCons.getName()+'*')
                #print('*'+left_var_symbol.getClass().getName()+'*')
                
                
                if str(objCreationCons.getName()) !=str(left_var_symbol.getClass().getName()):
                    raise Exception('invalid constructor')
        
    def visit_ObjectAttribute(self , node , visitBlock = False):
        #we need to check if the object exists
        """
        we checked if the object exists. 
        we also checked if the attribute exists
        """
        var_name = node.objectName.value
        var_symbol = self.symtab.lookup(var_name)
        
        if var_symbol is None or not isinstance(var_symbol, ObjVarSymbol):
            raise Exception("Error: Object '%s' not found" % var_name)
        #so we made sure object actually exists.
        #now we need to check if the attribute exists
        if visitBlock:
            className = var_symbol.getClass().getName()
            #print('classname is ', className)
            class_symb = self.symtab.lookup(className)
            attributes = class_symb.getAttributes()
            #print(attributes)
            if node.attributeName.value not in attributes:
                raise Exception("Error: Attribute '%s' not found in class '%s'" % (node.attributeName.value , className))
               
    def visit_ObjectCreation(self, node , visitBlock = False):
        if visitBlock:
            className = node.className.value
            classSymb = self.symtab.lookup(className)
            if classSymb is None or not isinstance(classSymb, ClassSymbol):
                raise Exception("Error: Class '%s' not found" % className)
            #we also need to check if the constructor arguments matches with the original ones
            constructor = classSymb.getConstructors()
            params = constructor[className].getParams()
            arguments = node.arguments
            if len(arguments)!=len(params):
                raise Exception('invalid number of arguments for the constructor %s' % className)
            return classSymb
        return None
        
    def visit_Var(self, node , visitBlock = False):
        var_name = node.value
        var_symbol = self.symtab.lookup(var_name)
        #print('var symbol is' , var_symbol)
        if var_symbol is None:
            raise Exception(
                "Error: Symbol(identifier) not found '%s'" % var_name
        )
            
    def visit_MethodCall(self, node , visitBlock = False):
        #we need to check if the method exists
        #we also need to check if the parameter list matches with the argument lists
        #we need to deal with instance methods and static methods separately
        #so if are dealing with instance methods , we need the methods in the other classes too.
        #for that we need to make sure that other classes are parsed before our class
        #but that cannot be assured for every class. 
        #so this means , we have to parse all the field declarations and method declarations before we start parsing the methods.
        
        #right now we will just make sure that object exists in case of instance  methods
        #and we will also ensure that the method exists in case of static methods.
        if node.object is None:
            #this is a method call within the same class
            method_name = node.name.value
            method_symb = self.symtab.lookup(method_name)
            if method_symb is None or not isinstance(method_symb, MethodSymbol):
                raise Exception("Error: Method '%s' not found" % method_name)
            for i in node.arguments:
                self.visit(i, visitBlock)
            if visitBlock:
                
                params = method_symb.getParams()
                arguments = node.arguments
                #print('here are the params', params)
                #print('here are the arguments', arguments)
                #print(method_symb.body)
                if len(arguments)!=len(params):
                    raise Exception('invalid number of arguments')
            
        else:
            #this is a method calal outside the same class
            obj_name = node.object.value
            obj_symb = self.symtab.lookup(obj_name)
            if obj_symb is None or not isinstance(obj_symb, ObjVarSymbol):
                raise Exception("Error: Object '%s' not found" % obj_name)
            for i in node.arguments:
                self.visit(i, visitBlock)
            node.object = Var(node.object.token , obj_symb.getClass())
        
    def visit_Num(self, node, visitBlock = False):
        return node.value
    
    def visit_NoOp(self, node, visitBlock = False):
        pass
    
    def visit_UnaryOp(self, node , visitBlock = False):
        if isinstance(node.expr , ObjectCreation):
            raise Exception('unary operator has an invalid operand "new %s()"' %node.expr.className.value)
        self.visit(node.expr)

    def visit_BinOp(self, node, visitBlock = False):
        if isinstance(node.left , ObjectCreation) :
            raise Exception('binary operator has an invalid operand "new %s()"' %node.left.className.value)
        if isinstance(node.right , ObjectCreation):
            raise Exception('binary operator has an invalid operand "new %s()"' %node.right.className.value)
        self.visit(node.left , visitBlock)
        self.visit(node.right , visitBlock)




###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

class Interpreter(NodeVisitor):
    def __init__(self, tree, mainClass , symbolTable):
        self.tree = tree
        self.mainClass = mainClass
        self.GLOBAL_MEMORY = {}
        self.symtab = symbolTable
        self.scopeStack = self.symtab.initializeMemoryScopeStack()
        self.scopeStack.push_scope()

    def visit_Program(self, node , visitBlock = False):
        self.symtab._init_builtins()
        self.scopeStack.push_scope()
        self.symtab.insertMemory(self.mainClass.name, self.symtab._classes[self.mainClass.name])
        self.scopeStack.current_scope().print_scope()
        Functions = mainClass.getMethods()
        mainFunction = None
        for name in Functions:
            if name=="main":
                mainFunction = Functions[name]
        #print(mainFunction.body)
        self.visit(mainFunction.body)
        self.symtab.memoryScopeStack.current_scope().print_scope()


    def visit_Block(self, node, visitBlock = False):
        """
        we have to deal with assignment statments
        we have to deal with declaration statments
        we have to deal with method calls
        we have to deal with object creation
        """
        print('we are into the visit_Block')
        for statement in node.statements:
            self.visit(statement)
    
    def visit_VariableDeclaration(self , node, visitBlock = False):
        print('we are into variable declaration')
        #print('type is ' , node.type)
        default = 0
        if isinstance(node.type , ObjectType):
            default = 'null'
        else:
            default = 0
        for variable in node.variables:
            self.visit_VarDecl(variable, default)
            #print(variable)
    

    def visit_VarDecl(self, node,defaultValue ,  visitBlock = False):
        var_name = node.left.value
        print(var_name)
        if(node.right is not None):
            var_value = self.visit(node.right)
            self.symtab.insertMemory(var_name ,var_value)
        if(node.right is None):
            self.symtab.insertMemory(var_name , defaultValue)
            
    
    
    def visit_ObjectCreation(self , node , visitBlock = False):
        print('we are into object creation')
        className = node.className.value
        classInfo = self.symtab.getClassInfo(className)
        attributes = classInfo.getAttributes()
        objConstructor = classInfo.getConstructors()[className]
        params = objConstructor.getParams()
        newObject = {}
        for i in attributes:
            newObject[i] = self.visit(attributes[i].getValue())
        print('before constructor:' , newObject)
        self.scopeStack.push_scope()
        for pos , param in enumerate(params):
            self.symtab.insertMemory(param.getName(), self.visit(node.arguments[pos]))
        self.symtab.insertMemory('this' , ObjVarSymbol('this', className , newObject))
        
        self.visit(objConstructor.body)
        self.scopeStack.current_scope().print_scope()
        print('after constructor: ', newObject)
        #print(newObject) 
        self.scopeStack.pop_scope()
        return newObject
    
    def visit_ObjectAttribute(self , node , visitBlock= False):
        obj = self.symtab.lookUpMemory(node.objectName.value)
        return obj[node.attributeName.value]
        
        
    def visit_Assign(self, node, visitBlock = False):
        if isinstance(node.left , Var):
            var_name = node.left.value
            var_value = self.visit(node.right)
            
            if self.symtab.lookUpMemory(var_name) is None:
                self.scopeStack.current_scope().symbols["this"].getValue()[var_name] = var_value
            else:
                self.symtab.insertMemory(var_name ,var_value)
        elif isinstance(node.left , ObjectAttribute):
            var_value = self.visit(node.right)
            obj = self.symtab.lookUpMemory(node.left.objectName.value)
            obj[node.left.attributeName.value] = var_value
        
    def visit_MethodCall(self , node , visitBlock = False):
        #this can be instance method call or static method call
        print('we are inside method call!')
        if node.object is None:
            #static method call
            method_name = node.name.value
            className = list(self.scopeStack.current_scope().symbols.keys())[0]
            classSymb = self.symtab._classInfo[className]
            methods = classSymb.getMethods()
            method = methods[method_name]
            """ print('class name is : ', method)
            print(node.name.value)
            print(node.arguments) """
            self.scopeStack.push_scope()
            self.symtab.insertMemory(className, self.symtab._classes[className])
            for i in range(len(node.arguments)):
                self.symtab.insertMemory(method.getParams()[i].getName(), self.visit(node.arguments[i]))
            self.scopeStack.current_scope().print_scope()
            self.visit(method.body)
            self.scopeStack.current_scope().print_scope()
            self.scopeStack.pop_scope()
        else:
            #instance method call
            obj = self.symtab.lookUpMemory(node.object.value)
            method_name = node.name.value
            className = node.object.type.getName()
            classSymb = self.symtab._classInfo[className]
            methods = classSymb.getMethods()
            method = methods[method_name]
            print(method)
            params = method.getParams()
            self.scopeStack.push_scope()
            self.symtab.insertMemory(className, self.symtab._classes[className])
            for i in range(len(params)):
                self.symtab.insertMemory(params[i].getName(), self.visit(node.arguments[i]))
            self.scopeStack.current_scope().print_scope()
            self.visit(method.body)
            self.scopeStack.current_scope().print_scope()
            self.scopeStack.pop_scope()
        pass

    def visit_Type(self, node, visitBlock = False):
        # Do nothing
        pass

    def visit_BinOp(self, node,visitBlock = False):
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

    def visit_Num(self, node, visitBlock = False):
        return node.value

    def visit_UnaryOp(self, node, visitBlock = False):
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Var(self, node, visitBlock = False):
        var_name = node.value
        #var_value = self.symtab.lookUpMemory(var_name)
        var_value = None
        if self.symtab.lookUpMemory(var_name) is None:
            var_value = self.scopeStack.current_scope().symbols["this"].getValue()[var_name]
        else:
            var_value = self.symtab.lookUpMemory(var_name)
        return var_value

    def visit_NoOp(self, node, visitBlock = False):
        pass


    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)


text = open('hello.txt', 'r').read()

lexer = Lexer(text)
parser = Parser(lexer)
tree = parser.parse()

semantic_analyzer = SemanticAnalyzer()
semantic_analyzer.visit(tree , False)
classInformation = semantic_analyzer.symtab._classInfo
mainFound = False
mainClass = None
for i in classInformation:
    if mainFound:
        if classInformation[i].containsMain:
            raise Exception('Multiple Main functions in the file')
    else:
        if classInformation[i].containsMain:
            mainFound = True
            mainClass = classInformation[i]

if not mainFound:
    print('finishing execution as no "main" function found')

interpreter = Interpreter(tree , mainClass , semantic_analyzer.symtab)
result = interpreter.interpret()


print('')
""" print('Run-time GLOBAL_MEMORY contents:')
for k, v in sorted(interpreter.GLOBAL_MEMORY.items()):
    print('{} = {}'.format(k, v)) """
    