from pyswip import Prolog

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
        # Prolog query result had multiple instances of same value. Set 'flagGetAllResults' as 'false' to get only first value.
        flagGetAllResults = False
        return getPrologQuery(listPrologResults, prologOutputVariable, flagGetAllResults)
        # return getAllResult(listPrologResults, prologOutputVariable)

    except:
        print(MSG_ERROR_WHILE_RUNNING_PROLOG)

# This method only returns results of Prolog query.
def getPrologQuery(listPrologResults, prologOutputVariable, flagGetAllResults):
    list_results = []
    for prologResult in listPrologResults:
        list_results.append(prologResult[prologOutputVariable])

        # Return only first result if 'flagGetAllResults' is false.
        if not flagGetAllResults:
            break
    return list_results


def main():
    print(getCommandLineArgumentsAndCompileTokens())


if __name__ == "__main__":
    main()


# To run this class from Ubuntu Terminal:
#     python3 brewlessInterpreter.py assignSemantics.pl "execute_program([begin, var, z, ; , var, x, ;, z, :=, x, end, .], Z)." "Z"
