import re

token_regex = re.compile(r"\s*(?P<INT>\d+)|(?P<PRINT>print)|(?P<INCREMENT>\++)|(?P<DECREMENT>--)|\b(?P<TYPESTRING>String)\b|\b(?P<TYPEINT>int)\b|(?P<PLUS>\+)|(?P<MINUS>-)|(?P<MULTIPLY>\*)|(?P<DIVIDE>/)|(?P<LPAREN>\()|(?P<RPAREN>\))|(?P<LBRACE>{)|(?P<RBRACE>})|(?P<COMMENT>#.*(\n|$))|(?P<IF>if)|(?P<ELSE>else)|(?P<LESS>\<)|(?P<GREATER>\>)|(?P<WHILE>while)|(?P<DO>do)|(?P<FOR>for)|\b(?P<IN>in)\b|(?P<RANGE>range)|(?P<STRING>\"[^\"]*\")|(?P<TRUE>true)|(?P<FALSE>false)|(?P<EQ>==)|(?P<AND>and)|(?P<OR>or)|(?P<NOT>not)|(?P<TERNARY>\?)|(?P<COLON>:)|(?P<ASSIGN>=)|(?P<VARIABLE>[a-zA-Z_][a-zA-Z0-9_]*)")

INT = "INT"
PRINT = "PRINT"
TYPEINT = "TYPEINT"
TYPESTRING = "TYPESTRING"
INCREMENT = "INCREMENT"
DECREMENT = "DECREMENT"
PLUS = "PLUS"
MINUS = "MINUS"
MULTIPLY = "MULTIPLY"
DIVIDE = "DIVIDE"
LPAREN = "LPAREN"
RPAREN = "RPAREN"
LBRACE = "LBRACE"
RBRACE = "RBRACE"
COMMENT = "COMMENT"
IF = "IF"
ELSE = "ELSE"
LESS = "LESS"
GREATER = "GREATER"
WHILE = "WHILE"
DO = "DO"
FOR = "FOR"
IN = "IN"
RANGE = "RANGE"
STRING = "STRING"
TRUE = "TRUE"
FALSE = "FALSE"
EQ = "EQ"
AND = "AND"
OR = "OR"
NOT = "NOT"
TERNARY = "TERNARY"
COLON = "COLON"
ASSIGN = "ASSIGN"
VARIABLE = "VARIABLE"

class Error:
    def __init__(self, error_name, details):
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        return result

class IllegalCharError(Error):
    def __init__(self, details):
        super().__init__('Illegal Character', details)

def lex(expression):
    tokens = []
    for match in token_regex.finditer(expression):
        token_type = match.lastgroup
        token_value = match.group(token_type)
        
        if token_type == INT:
            tokens.append((int(token_value)))
        elif token_type == STRING:
            tokens.append(((token_value)))
        elif token_type == PRINT:
            tokens.append(('print'))
        elif token_type == TYPEINT:
            tokens.append((token_value))
        elif token_type == TYPESTRING:
            tokens.append((token_value))
        elif token_type == TRUE:
            tokens.append((True))
        elif token_type == FALSE:
            tokens.append((False))
        elif token_type == TERNARY:
            tokens.append(('?'))
        elif token_type == COLON:
            tokens.append((':'))
        elif token_type == LESS:
            tokens.append(('<'))
        elif token_type == GREATER:
            tokens.append(('>'))
        elif token_type == ASSIGN:
            tokens.append((token_value))
        elif token_type == VARIABLE:
            tokens.append(((token_value)))
        elif token_type != COMMENT:
            tokens.append((token_value))
        else:
            error = IllegalCharError(f"Invalid character: '{token_value}'")
            raise error
    return tokens