#!/usr/bin/env python
# coding: utf-8

import glob
import os.path
import os

runnable = "../../dist/build/Webrexp/webrexp -q -f "
tempFilename = "tmpTestFile"

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
    if lst[-1] == "":
        return lst[0:-1]
    return lst

# Grossly compare two files (don't have internet
# can't search better)
def diffFile( fileName1, fileName2 ):
    file1 = open(fileName1)
    file2 = open(fileName2)

    lst1 = trimLastEmptyLine(list(file1))
    lst2 = trimLastEmptyLine(list(file2))

    file1.close()
    file2.close()

    if len(lst1) != len(lst2):
        return True

    for (line1, line2) in zip(lst1, lst2):
    	if line1 != line2:
            return True

    return False

# Run a test case
def testRunner( path ):
    rezCode = os.system(runnable + path + " > " + tempFilename)

    if rezCode != 0:
    	print(
          ("======================================\n" 
          +"Failed valid {0}\n"
          +"======================================")
          .format(path)
          )
        catFile( path )
    else:
    	print( "." )

    rezFile = path + ".txtout"
    if os.path.exists(rezFile) and diffFile(tempFilename, rezFile):
    	print(
          ("======================================\n" 
          +"Wrong results {0}\n"
          +"======================================")
          .format(path)
          )
        catFile( path )
        print("=================== Waited:") 
        catFile( rezFile )
        print("=================== but got:")
        catFile( tempFilename )


        

# Run... all the text cases
def runAllTests():
    for filename in glob.glob("*.webrexp"):
        testRunner(filename)

if __name__ == "__main__":
    runAllTests()

