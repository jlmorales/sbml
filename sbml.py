class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class NumberNode(Node):
    def __init__(self, v):
        if('.' in v):
            self.value = float(v)
        else:
            self.value = int(v)

    def evaluate(self):
        return self.value

class StringNode(Node):
    def __init__(self, v):
        self.value = v

    def evaluate(self):
        return self.value

class BopNode(Node):
    def __init__(self, op, v1, v2):
        #if number and number check rules if rules broken raise Syntax Error else assign v1 v2 op
        #if string and string check rules..
        #if list and list check rules..
        #else raise semantic error
        if(type(v1.evaluate()) in [float, int] and type(v2.evaluate()) in [float, int]):
            self.numberRules(op,v1,v2)
            self.v1, self.v2, self.op = v1, v2, op
        elif(type(v1.evaluate()) is str and type (v2.evaluate()) is str):
            self.StringRules(op,v1,v2)
            self.v1, self.v2, self.op = v1, v2, op
        else:
            print("SEMANTIC ERROR")
            raise SyntaxError

    def evaluate(self):
        if (self.op == '+'):
            if(type(self.v1.evaluate()) is str):
                v1 , v2 = self.v1.evaluate(), self.v2.evaluate()
                v1 , v2 = v1[1:len(v1) - 1], v2[1:len(v2) - 1]
                return '"' + v1 + v2 + '"'
            return self.v1.evaluate() + self.v2.evaluate()
        elif (self.op == '-'):
            return self.v1.evaluate() - self.v2.evaluate()
        elif (self.op == '*'):
            return self.v1.evaluate() * self.v2.evaluate()
        elif (self.op == '/'):
            return self.v1.evaluate() / self.v2.evaluate()
        elif (self.op == 'mod'):
            return self.v1.evaluate() % self.v2.evaluate()
        elif (self.op == 'div'):
            return self.v1.evaluate() // self.v2.evaluate()    

    def numberRules(self, op, v1, v2):
        if op == '/' and v2.evaluate() == 0:
            print("SEMANTIC ERROR")
            raise SyntaxError
        if op == 'mod' and (type(v1.evaluate()) is float or type(v2.evaluate()) is float):
            print("SEMANTIC ERROR")
            raise SyntaxError
        if op == 'div' and (type(v1.evaluate()) is float or type(v2.evaluate()) is float):
            print("SEMANTIC ERROR")
            raise SyntaxError

    def StringRules(self, op, v1, v2):
        if not op in ['+']:
            print("SEMANTIC ERROR")
            raise SyntaxError


tokens = (
    'LPAREN', 'RPAREN',
    'NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','REMAINDER','QDIVIDE',
    'STRING'
    )

# Tokens
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_REMAINDER = 'mod'
t_QDIVIDE = 'div'

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = StringNode(t.value)
    return t

def t_NUMBER(t):
    r'-?\d*(\d\.|\.\d)\d* | \d+'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Ignored characters
t_ignore = " \t"

def t_error(t):
    print(t)
    t.lexer.skip(1)
    
# Build the lexer
import ply.lex as lex
parser = lex.lex()

# Parsing rules
precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE',"REMAINDER", 'QDIVIDE'),
    ('nonassoc','UMINUS'),
    )

def p_statement_expr(t):
    'statement : expression'
    print(t[1].evaluate())

def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression REMAINDER expression
                  | expression QDIVIDE expression'''
        
    t[0] = BopNode(t[2], t[1], t[3])
    
def p_expression_uminus(t):
    'factor : MINUS factor %prec UMINUS'
    t[0] = NumberNode(str(-t[2].evaluate()))

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_factor_number(t):
    'factor : NUMBER'
    t[0] = t[1]

def p_factor_string(t):
    'expression : STRING'
    t[0] = t[1]

def p_expression_factor(t):
    '''expression : factor'''
    t[0] = t[1]

def p_error(t):
    print("SYNTAX ERROR")
    parser.restart()

import ply.yacc as yacc
parser = yacc.yacc()

import fileinput
for line in fileinput.input():
    yacc.parse(line)

# while 1:
#     try:
#         s = input('input > ')   # Use raw_input on Python 2
#     except EOFError:
#         break
#     yacc.parse(s)