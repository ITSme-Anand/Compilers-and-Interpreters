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


class Block(AST):
    def __init__(self, statement_nodes):
        self.statements = statement_nodes
        

class Assignment_statement(AST):
    def __init__(self, statements):
        self.statements = statements


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value