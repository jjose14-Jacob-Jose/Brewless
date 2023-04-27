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


# prolog = Prolog()
# prolog.consult("assignSemantics.pl")
# input_Program= "[begin, var, z, ; , var, x, ;, z, :=, x, end, .]"
# queryString = "execute_program( " + input_Program  + ", Z)."
# print("queryString: ", queryString)
# resultOne = prolog.query(queryString)
# for result in resultOne:
#     print(result["Z"])

import sys

MSG_MISSING_COMMAND_LINE_ARGUMENTS = "Please enter 3 commandline arguments in the format <prolog_file_name> <prolog_query> <output_variable>."
MSG_ERROR_WHILE_RUNNING_PROLOG = "There was an error while executing the program. Please check the program's syntax and calling parameters. "


def getCommandLineArgumentsAndCompileTokens():
    # Get the command line arguments
    listCommandLineArguments = sys.argv

    try:
        # Prolog 'filepath', 'predicate name' and 'ouput variable name' must be mentioned in this sequence. 
        prologFileName = listCommandLineArguments[1]
        prologCommandOrQuery = listCommandLineArguments[2]
        prologOutputVariable = listCommandLineArguments[3]

        return compileTokens(prologFileName, prologCommandOrQuery, prologOutputVariable)
    except:
        print(MSG_MISSING_COMMAND_LINE_ARGUMENTS)


def compileTokens(prologFilePath, prologCommandOrQuery, prologOutputVariable):
    try:
        # Create a Prolog object.
        prolog = Prolog()

        # Consult the Prolog file.
        prolog.consult(prologFilePath)

        # Call the Prolog predicate.
        listPrologResults = prolog.query(prologCommandOrQuery)

        # Return Prolog query result.
        # Prolog query result is list had multiple instances of same value. So only first value chose.
        return getFirstResult(listPrologResults, prologOutputVariable)
        # return getAllResult(listPrologResults, prologOutputVariable)

    except:
        print(MSG_ERROR_WHILE_RUNNING_PROLOG)


# This method only returns first result of Prolog query.
def getFirstResult(listPrologResults, prologOutputVariable):
    if len(listPrologResults) > 0:
        return listPrologResults[0][prologOutputVariable]


# This method only returns all result of Prolog query.    
def getAllResult(listPrologResults, prologOutputVariable):
    listResults = []
    for prologResult in listPrologResults:
        listResults.append(prologResult[prologOutputVariable])
    return listResults


def main():
    print(getCommandLineArgumentsAndCompileTokens())


if __name__ == "__main__":
    main()
