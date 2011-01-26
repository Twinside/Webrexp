WebRexp
=======
Query language designed to extract information from a bunch of HTML files.
The idea is to be able to extract information from a bunch of files linked
between each others, using a syntax similar to regexp.

Using
-----
For tutorial and examples, please see the project's [wiki](https://github.com/Twinside/Webrexp/wiki/Webrexp). 
As a teasing, here is some samples for a tiny command line for listing a RSS stream's title :

    webrexp '"http://someurl.com" > item title {.}'

And another one to dump all the images from a webpage

    webrexp '"http://someurl.com" > img {.}'

Building
--------
To build the webrexp project you must have GHC (Glasgow Haskell Compiler) installed and some cabal package. To know which package your are missing, in the source folder type :

    make conf

A list of missing package should be shown, then you can

    cabal install packagename

for every missing package and finally type `make`to build. Or you can download the binary. Binary are simpler. So fucking simpler.

