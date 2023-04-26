from pyswip import Prolog

prolog = Prolog()
prolog.consult("test_data/assignSemantics.pl")
P= "[begin, var, z, ; , var, x, ;, z, :=, x, end, .]"
for result in prolog.query("program_eval(", P, ", 2, 3, Z)"):
    print(result["P"], result["Z"])