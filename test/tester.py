#!/usr/bin/env python
# coding: utf-8

import glob
import os.path
import os
import sys

# well... cat a file, don't have internet to search
# for a better solution
def catFile( filename ):
    f = open( filename )
    for line in f:
        print( line )
    f.close()

# Sometimes the text editor used to specify the text
# output add an extra line at the end of the file.
# Get rid of it.
def trimLastEmptyLine( lst ):
    if lst[-1] == []:
        return lst[0:-1]
    return lst

def trimEndLine( line ):
    while line[-1] == '\r' or line[-1] == '\n':
        line = line[0:-1]
    return line

# Grossly compare two files (don't have internet
# can't search better)
def diffFile( fileName1, fileName2 ):
    file1 = open(fileName1)
    file2 = open(fileName2)

    lst1 = trimLastEmptyLine(list(file1))
    lst2 = trimLastEmptyLine(list(file2))

    lst1 = map( trimEndLine, lst1 )
    lst2 = map( trimEndLine, lst2 )

    file1.close()
    file2.close()

    if len(lst1) != len(lst2):
        return True

    for (line1, line2) in zip(lst1, lst2):
        if line1 != line2:
            return True

    return False

# Run a test case
def testRunner( verbose, runAction, okPredicate, path ):
    tempFilename = "tmpTestFile"

    if verbose:
        print(path)

    rezCode = runAction(path, tempFilename )

    if okPredicate(rezCode):
        print(
          ("\n======================================\n" 
          +"Failed valid {0}\n"
          +"======================================")
          .format(path)
          )
        catFile( path )
    else:
        sys.stdout.write( "." )

    rezFile = path + ".txtout"
    if os.path.exists(rezFile) and diffFile(tempFilename, rezFile):
        print(
          ("\n======================================\n" 
          +"Wrong results {0}\n"
          +"======================================")
          .format(path)
          )
        catFile( path )
        print("=================== Waited:") 
        catFile( rezFile )
        print("=================== but got:")
        catFile( tempFilename )

    # Might not be created in syntax test cases.
    if os.path.exists(tempFilename):
        os.remove(tempFilename)

def execRunner( path, tempFilename ):
    runnable = "../../dist/build/Webrexp/webrexp -q -f "
    return os.system(runnable + path + " > " + tempFilename)

# Run... all the valid test cases
def runAllValidTests(verbose):
    for filename in glob.glob("*.webrexp"):
        testRunner(verbose, execRunner, lambda retCode: retCode != 0, filename)

def runAllWrongTests(verbose):
    for filename in glob.glob("mustfail/*.webrexp"):
        testRunner(verbose, execRunner, lambda retCode: retCode == 0, filename)

def syntaxRunner( path, tempFilename ):
    runnable = "../dist/build/Webrexp/webrexp --dot -q -f "
    return os.system(runnable + path + " > /dev/null" )
    
def runSyntaxValid(verbose):
    for filename in glob.glob("puresyntax/valid/*.webrexp"):
        testRunner(verbose, syntaxRunner, lambda retCode: retCode != 0, filename)
    
def runSyntaxInvalid(verbose):
    for filename in glob.glob("puresyntax/invalid/*.webrexp"):
        testRunner(verbose, syntaxRunner, lambda retCode: retCode == 0, filename)

if __name__ == "__main__":
    verbose = True
    print(">> Syntax")
    runSyntaxValid(verbose)
    runSyntaxInvalid(verbose)

    print("\n>> Executable")
    os.chdir( "executable" )
    runAllValidTests(verbose)
    runAllWrongTests(verbose)


