import re

#defines a regex pattern to match various types of tokens
token_regex = re.compile(r"\s*(?P<INT>\d+)|(?P<PRINT>print)|(?P<INCREMENT>\++)|(?P<DECREMENT>--)|\b(?P<TYPESTRING>String)\b|\b(?P<TYPEINT>int)\b|(?P<PLUS>\+)|(?P<MINUS>-)|(?P<MULTIPLY>\*)|(?P<DIVIDE>/)|(?P<LPAREN>\()|(?P<RPAREN>\))|(?P<LBRACE>{)|(?P<RBRACE>})|(?P<COMMENT>#.*(\n|$))|(?P<IF>if)|(?P<ELSE>else)|(?P<LESS>\<)|(?P<GREATER>\>)|(?P<WHILE>while)|(?P<DO>do)|(?P<FOR>for)|\b(?P<IN>in)\b|(?P<RANGE>range)|(?P<STRING>\"[^\"]*\")|(?P<TRUE>true)|(?P<FALSE>false)|(?P<EQ>==)|(?P<AND>and)|(?P<OR>or)|(?P<NOT>not)|(?P<TERNARY>\?)|(?P<COLON>:)|(?P<SEMICOLON>;)|(?P<ASSIGN>=)|(?P<VARIABLE>[a-zA-Z_][a-zA-Z0-9_]*)")

#define as constants for ease of use / show token type
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
SEMICOLON = "SEMICOLON"

#Generic error handler
class Error:
    def __init__(self, error_name, details):
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        return result

#Inherits error handler, handles illegal chars
class IllegalCharError(Error):
    def __init__(self, details):
        super().__init__('Illegal Character', details)

#takes an input expression and returns a list of tokens.
def lex(expression):
    tokens = []
    #iterates over the matches found
    for match in token_regex.finditer(expression):
        token_type = match.lastgroup
        token_value = match.group(token_type)
        
        #determines the type of each token based on the regex pattern
        if token_type == INT:
            tokens.append((token_value))
        elif token_type == STRING:
            tokens.append((token_value))
        elif token_type == PRINT:
            tokens.append(('print'))
        elif token_type == TYPEINT:
            tokens.append((token_value))
        elif token_type == TYPESTRING:
            tokens.append((token_value))
        elif token_type == TRUE:
            tokens.append(('true'))
        elif token_type == FALSE:
            tokens.append(('false'))
        elif token_type == TERNARY:
            tokens.append(('?'))
        elif token_type == COLON:
            tokens.append((':'))
        elif token_type == SEMICOLON:
            tokens.append((';'))
        elif token_type == LESS:
            tokens.append(('<'))
        elif token_type == GREATER:
            tokens.append(('>'))
        elif token_type == ASSIGN:
            tokens.append((token_value))
        elif token_type == VARIABLE:
            tokens.append(((token_value)))
        elif token_type != COMMENT:
            #appends the token to the list of tokens
            tokens.append((token_value))
        else:
            #If invalid character encountered raise IllegalCharError
            error = IllegalCharError(f"Invalid character: '{token_value}'")
            raise error
    return tokens