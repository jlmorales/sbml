#Jose Morales 109306481
class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class BoolNode(Node):
    def __init__(self, v):
        if(v == 'true'):
            self.value = True
        else:
            self.value = False

    def evaluate(self):
        return self.value

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
        self.value = v[1:len(v) - 1]

    def evaluate(self):
        return self.value
     
class ListNode(Node):
    def __init__(self, v):
        self.value = v

    def evaluate(self):
        return [n.evaluate() for n in self.value]

class TupleNode(Node):
    def __init__(self, v):
        self.value = v

    def evaluate(self):
        return tuple([n.evaluate() for n in self.value])

class SopNode(Node):
    def __init__(self, op, v):
        self.v, self.op = v , op
       
    def evaluate(self):
        v = self.v.evaluate()
        self.semanticCheck(self.op, v)
        if( self.op == 'not'):
            return not v
        elif(self.op == '-'):
            return -v
        elif(self.op == '+'):
            return v
    def boolRules(self, op, v):
        if op not in ['not']:
            printerror()
    def semanticCheck(self, op, v):
        if(type(v) is bool):
            if op not in ['not']:
                printerror()
        elif(type(v) in [int, float]):
            if op not in ['-', '+']:
                printerror
        else:
            printerror()

class BopNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        #type check
        v1 = self.v1.evaluate()
        v2 = self.v2.evaluate()
        self.typeCheck(self.op,v1,v2)
        if (self.op == '+'):
            return v1 + v2
        elif (self.op == '-'):
            return v1 - v2
        elif (self.op == '*'):
            return v1 * v2
        elif (self.op == '/'):
            return v1 / v2
        elif (self.op == 'mod'):
            return v1 % v2
        elif (self.op == 'div'):
            return v1 // v2 
        elif (self.op == '**'):
            return v1 ** v2
        elif (self.op == '=='):
            return v1 == v2
        elif (self.op == '>='):
            return v1 >= v2 
        elif (self.op == '<='):
            return v1 <= v2
        elif (self.op == '<>'):
            return v1 != v2
        elif (self.op == '<'):
            return v1 < v2
        elif (self.op == '>'):
            return v1 > v2         
        elif (self.op == 'andalso'):
            return v1 and v2
        elif (self.op == 'orelse'):
            return v1 or v2   
        elif (self.op == 'in'):
            return v1 in v2
        elif(self.op == '::'):
            return [v1] + v2
        elif(self.op == '#'):
            v1 -= 1
            if(v1 < 0 or v1 >= len(v2)):
                printerror()
            else:
                return v2[v1] 
        elif(self.op == 'index'):
            if(v2 < 0 ):
                printerror()
            else:
                return v1[v2]
            

    def numberRules(self, op, v1, v2):
        if op not in ['+', '-','/', '*', '**', 'div','mod', '==','>=', '<=', '<>', '<', '>']:
            printerror()
        if op == '/' and v2 == 0:
            printerror()
        if op == 'mod' and (type(v1) is float or type(v2) is float):
            printerror()
        if op == 'div' and (type(v1) is float or type(v2) is float):
            printerror()

    def StringRules(self, op, v1, v2):
        if not op in ['+','==','>=', '<=', '<>', '<', '>', 'in']:
            printerror()

    def boolRules(self, op, v1, v2):
        if not op in ['andalso', 'orelse']:
            printerror()
    
    def listRules(self, op, v1, v2):
        if not op in ['+','::']:
            printerror()
    
    def typeCheck(self, op, v1, v2):
        if(type(v1) in [float, int] and type(v2) in [float, int]):
            self.numberRules(op,v1,v2)
        elif(type(v1) is str and type (v2) is str):
            self.StringRules(op,v1,v2)
        elif(type(v1) is bool and type (v2) is bool):
            self.boolRules(op,v1,v2)
        elif(type(v1) is list and type (v2) is list):
            self.listRules(op,v1,v2)
        elif(type(v1) is list and type(v2) is int):
            if not op in ['index']:
                printerror()
        elif(type(v1) is str and type(v2) is int):
            if not op in ['index']:
                printerror()
        elif(type(v1) in [float, int, str, bool] and type (v2) is list):
            if not op in ['::', 'in']:
                printerror()
        elif(type(v1) is int and type (v2) is tuple and op == '#'):
            if not op in ['#']:
                printerror()
        else:
            printerror()

def printerror():
    # print("SEMANTIC ERROR")
    # lexer.lexpos = len(lexer.lexdata)
    raise SyntaxError

reserved = {
    'div' : 'QDIVIDE',
    'mod' : 'REMAINDER',
    'andalso' : 'AND',
    'orelse' : 'OR',
    'not' : 'NOT',
    'in' : 'IN',
    'true' : 'TRUE',
    'false' : 'FALSE',
}

tokens = [
    'LPAREN', 'RPAREN',
    'LBRACK', 'RBRACK',
    'COMMA',
    'SEMICOLON',
    'CONS', 'TINDEX',
    'NUMBER', 'NUMBER_2',
    'PLUS','MINUS','TIMES','DIVIDE','EXPONENT',
    'EQ', 'GEQ', 'LEQ', 'NEQ', 'LT', 'GT',
    'STRING', 'STRING_2'
] + list(reserved.values())

# Tokens
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

t_LBRACK  = r'\['
t_RBRACK  = r'\]'
t_SEMICOLON  = r';'

t_CONS = r'::'
t_TINDEX = r'\#'

t_COMMA  = r','

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_EXPONENT   = r'\*\*'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'

t_EQ = r'=='
t_GEQ = r'>='
t_LEQ = r'<='
t_NEQ = r'<>'
t_LT = r'<'
t_GT = r'>'

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reserved.get(t.value,'ID')    # Check for reserved words
     if(t.type == 'ID'):
         t.lexer.skip(1)
     elif(t.value in ['true', 'false']):
        t.value = BoolNode(t.value)
        return t
    #  elif(t.value in ['orelse']):
    #     t.value = 'OR'
    #     return t
    #  elif(t.value in ['andalso']):
    #     t.value = 'AND'
    #     return t
    #  elif(t.value in ['not']):
    #     t.value = 'NOT'
    #     return t
     else:
         return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = StringNode(t.value)
    return t

def t_STRING_2(t):
    r'\'([^\\\n]|(\\.))*?\''
    t.value = StringNode(t.value)
    return t

def t_NUMBER_2(t):
    r'(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        # print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        # print("Integer value too large %d", t.value)
        t.value = 0
    return t



def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Ignored characters
t_ignore = " \t"
# t_ignore = "\n[ ]*\n"

def t_error(t):
    # print(t)
    # t.lexer.skip(1)
    raise Exception

# Build the lexer
import ply.lex as lex
lexer = lex.lex(debug=0)

# Parsing rules
precedence = (
    ('left','OR'),
    ('left','AND'),
    ('left','NOT'),
    ('left','EQ', 'GEQ', 'LEQ', 'NEQ', 'LT', 'GT' ),
    ('right', 'CONS'),
    ('left','IN'),
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE',"REMAINDER", 'QDIVIDE'),
    ('right','EXPONENT'),
    ('left','LBRACK'),
    ('left','LPAREN'),
    ('nonassoc','UMINUS'),
    )

def p_statement_expr(t):
    'statement : expression SEMICOLON'
    t[0] = t[1]

def p_expression_uminus(t):
    '''expression : MINUS expression %prec UMINUS
                  | PLUS expression %prec UMINUS'''
    # t[0] = NumberNode(str(-t[2].evaluate()))
    t[0] = SopNode(t[1],t[2])

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

#LISTS
def p_expression_list(t):
    '''expression : LBRACK expression_list RBRACK'''
    t[0] = ListNode(t[2])
def p_expression_list_empty(t):
    '''expression : LBRACK  RBRACK'''
    t[0] = ListNode([])

def p_expression_list_index(t):
    '''expression : expression LBRACK expression RBRACK'''
    t[0] = BopNode('index', t[1], t[3])

#tuples
def p_expression_tuple(t):
    '''expression : LPAREN expression_list COMMA expression RPAREN'''
    temp = t[2] + [t[4]]
    # temp = tuple(temp)
    t[0] = TupleNode(temp)

def p_expression_tuple_empty(t):
    '''expression : LPAREN RPAREN'''
    t[0] = TupleNode([])

def p_expression_tuple_index(t):
    '''expression : TINDEX expression expression %prec LPAREN'''
    t[0] = BopNode(t[1], t[2], t[3])

#expression list
def p_expression_list_1(t):
    '''expression_list : expression '''
    t[0] = [t[1]]

def p_expression_list_2(t):
    '''expression_list : expression_list COMMA expression '''
    t[0] = t[1] + [t[3]]

def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression REMAINDER expression
                  | expression QDIVIDE expression
                  | expression EXPONENT expression
                  | expression EQ expression
                  | expression GEQ expression
                  | expression LEQ expression
                  | expression NEQ expression
                  | expression LT expression
                  | expression GT expression'''
        
    t[0] = BopNode(t[2], t[1], t[3])

def p_expression_binop_list(t):
    '''expression : expression IN expression
                  | expression CONS expression'''
    t[0] = BopNode(t[2], t[1], t[3])

def p_expression_sinop(t):
    'expression : NOT expression %prec NOT'
    t[0] = SopNode(t[1], t[2])

def p_expression_binop_bool(t):
    '''expression : expression AND expression
                  | expression OR expression'''
    t[0] = BopNode(t[2], t[1], t[3])

def p_factor_number(t):
    'factor : NUMBER'
    t[0] = t[1]

def p_factor_number2(t):
    'factor : NUMBER_2'
    t[0] = t[1]

def p_factor_string(t):
    'expression : STRING'
    t[0] = t[1]

def p_factor_string2(t):
    'expression : STRING_2'
    t[0] = t[1] 

def p_expression_factor(t):
    '''expression : factor'''
    t[0] = t[1]

#boolean
def p_expression_true_false(t):
    '''bool : TRUE
            | FALSE'''
    t[0] = t[1]

def p_expression_bool(t):
    '''expression : bool'''
    t[0] = t[1]


def p_error(t):
    #print("SYNTAX ERROR")
    #lexer.lexpos = len(lexer.lexdata)
    #parser.restart()
    raise Exception

import ply.yacc as yacc
parser = yacc.yacc(debug=0)


# while 1:
#     t = 1
#     try:
#         s = input('input > ')   # Use raw_input on Python 2
#         ast = yacc.parse(s)
#     except Exception:
#         print('SYNTAX ERROR')
#         t = 0

#     if(t==1):
#         try:
#             out = ast.evaluate()
#             if type(out) is str:
#                 print("'" + str(out) + "'")
#             else:
#                 print(out)
#         except Exception:
#             print('SEMANTIC ERROR')



import sys


if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
print(sys.argv[1])
fd = open(sys.argv[1], 'r')
code = ""

with open(sys.argv[1], 'r') as f_in:
    lines = [line.rstrip() for line in f_in] # All lines including the blank ones
    lines = [line for line in lines if line] # Non-blank lines

for line in lines:
    t = 1
    try:
        line = line.strip()
        ast = yacc.parse(line)
    except Exception:
        print('SYNTAX ERROR')
        t = 0
    if(t==1):
        try:
            out = ast.evaluate()
            if type(out) is str:
                print("'" + str(out) + "'")
            else:
                print(out)
        except Exception:
            print('SEMANTIC ERROR')


