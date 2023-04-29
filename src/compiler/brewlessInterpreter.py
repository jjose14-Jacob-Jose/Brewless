from pyswip import Prolog

import sys
import brewlessLexer

MSG_MISSING_COMMAND_LINE_ARGUMENTS = "Please enter 3 commandline arguments in the format <prolog_file_name> <prolog_query> <output_variable>."
MSG_ERROR_WHILE_RUNNING_PROLOG = "There was an error while executing the program. Please check the program's syntax and calling parameters. "

def runBrewless():
    # Get the command line arguments
    listCommandLineArguments = sys.argv

    try:
        # Read contents of Brewless program to a String.
        brewlessFileName = listCommandLineArguments[1]
        stringFileContents = ""
        with open(brewlessFileName, 'r') as file:
            for fileLine in file:
                stringFileContents += fileLine

        tokens = brewlessLexer.lex(stringFileContents)

        # Create a Prolog predicate query String.
        stringPrologQuery = 'compile_and_run_brewless(' + '\''.join(tokens)
        stringPrologQuery = stringPrologQuery + ")."

        compileTokens("brewlessDCGParserWrapper.pl", stringPrologQuery, False)

    except:
        print(MSG_MISSING_COMMAND_LINE_ARGUMENTS)


def testbrewless():
    # Get the command line arguments
    # listCommandLineArguments = sys.argv

    try:
        # Read contents of Brewless program to a String.
        # brewlessFileName = listCommandLineArguments[1]
        brewlessFilename = "sample2.brewless"
        stringFileContents = ""
        with open(brewlessFilename, 'r') as file:
            for fileLine in file:
                stringFileContents += fileLine

        # Create a Prolog predicate query String.
        tokens = brewlessLexer.lex(stringFileContents)

        # Create a Prolog predicate query String.
        stringPrologQuery = 'compile_and_run_brewless(' + '\''.join(tokens)
        stringPrologQuery = stringPrologQuery + ")."

        compileTokens("brewlessDCGParserWrapper.pl", stringPrologQuery, False)

    except:
        print(MSG_MISSING_COMMAND_LINE_ARGUMENTS)


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
    testbrewless()


if __name__ == "__main__":
    main()


# To run this class from Ubuntu Terminal:
#     python3 brewlessInterpreter.py assignSemantics.pl "execute_program([begin, var, z, ; , var, x, ;, z, :=, x, end, .], Z)." "Z"
