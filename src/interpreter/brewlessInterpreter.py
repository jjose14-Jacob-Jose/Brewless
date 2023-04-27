from pyswip import Prolog

# prolog = Prolog()
# prolog.consult("assignSemantics.pl")
# P= "[begin, var, z, ; , var, x, ;, z, :=, x, end, .]"
# for result in prolog.query("program_eval( [begin, var, z, ; , var, x, ;, z, :=, x, end, .], 2, 3, Z)"):
#     print(result["P"], result["Z"])

# prolog = Prolog()
# prolog.consult("sample.pl")
# name = "alice"
# queryString = ("father(" + name + ", Z)")
# for result in prolog.query(queryString):
#     print(result["Z"])

# program(P, [begin, var, z, ; , var, x, ;, z, :=, x, end, .], []), program_eval(P, 2, 3, Z).



# prolog = Prolog()
# prolog.consult("assignSemantics.pl")
# input_Program= "[begin, var, z, ; , var, x, ;, z, :=, x, end, .]"
# queryString = "program( P, "+ input_Program + ", [])."
# print("queryString: ", queryString)
# for result in prolog.query(queryString):
#     print(result["P"])
    
#     parseTree = result["P"]
#     print("parsetree: " + parseTree)
#     x = "2"
#     y = "3"
#     queryTwo = "program_eval("+parseTree+", " + x + ", " + y + ", Z)."
#     print("queryTwo: ", queryTwo)

#     for resultTwo in prolog.query(queryTwo):
#             print(result["Z"])


prolog = Prolog()
prolog.consult("assignSemantics.pl")
input_Program= "[begin, var, z, ; , var, x, ;, z, :=, x, end, .]"
queryString = "execute_program( " + input_Program  + ", Z)."
print("queryString: ", queryString)
resultOne = prolog.query(queryString)
for result in resultOne:
    print(result["Z"])

