WebRexp
=======
Query language designed to extract information from a bunch of HTML files.
The idea is to be able to extract information from a bunch of files linked
between each others, using a syntax similar to regexp.

Design
------

    rootpage = http://feoaijfoieajfe.com
    next = rootpage (> (div.comic-pane img {.}; div.nav-next a))+
    
    next = rootpage (> (div.comic-pane img {@width = 3; .}; div.nav-next a))+
    
    
    next = rootpage > div | span |[4] ..

 * {} : output action
 * [] : index selection bracket
 * > : link following operator, descend
 * * : 0 or more times previous expresiion
 * + : 1 or more times previous expresiion
 * . : class accessing operator
 * # : id/name accessing operator, used?
 * ! : unicity guarentee, avoid cycling
 * ^ : select previous element in dom
 * | : Select next element in dom
 * < : Select upper element
 
