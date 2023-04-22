import re

token_regex = re.compile(r"\s*(?P<INT>\d+)|(?P<PLUS>\+)|(?P<MINUS>-)|(?P<MULTIPLY>\*)|(?P<DIVIDE>/)|(?P<LPAREN>\()|(?P<RPAREN>\))|(?P<LBRACE>{)|(?P<RBRACE>})|(?P<COMMENT>#.*)")

INT = "INT"
PLUS = "PLUS"
MINUS = "MINUS"
MULTIPLY = "MULTIPLY"
DIVIDE = "DIVIDE"
LPAREN = "LPAREN"
RPAREN = "RPAREN"
LBRACE = "LBRACE"
RBRACE = "RBRACE"
COMMENT = "COMMENT"

def lex(expression):
    tokens = []
    for match in token_regex.finditer(expression):
        token_type = match.lastgroup
        token_value = match.group(token_type)
        
        if token_type == INT:
            tokens.append((INT, int(token_value)))
        elif token_type != COMMENT:
            tokens.append((token_type, token_value))
    
    return tokens