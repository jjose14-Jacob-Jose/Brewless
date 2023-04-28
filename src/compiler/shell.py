import brewlessLexer

code = '''
if (a == b) {
  print("a and b are equal")
} else {
  print("a and b are not equal")
}

while (i < 10) {
  print(i)
  i = i + 1
}

for i in range(5):
  print(i)

for i in range(2, 5):
  print(i)

for (int i = 0; i < 5; i++) {
  print(i)
}
'''

while True:
    text = input('brewless > ')
    tokens = brewlessLexer.lex(text)
    print(tokens)

