WebRexp
=======
Query language designed to extract information from a bunch of HTML files.
The idea is to be able to extract information from a bunch of files linked
between each others, using a syntax similar to regexp.

How to use
----------
Now the use of webrexp is pretty simple, you just use the following command :

    webrexp "expression"


Webrexp writing
---------------

A webrexp describe a path within a graph (by extension tree) followed by the engine.
At each moment, the engine manipulate a list of graph node (thinks tags if you prefer)
or a list of string.

The documentation is still incomplete as some other functionalities hasn't been tested
yet.

### Walking in a web page

A webrexp basically perform searches in the dom, a bit like jQuery selectors.
For example if you find the following element in your webrexp :

    div a img

It means to find from the current nodes (it can be many), a `div` element, then
from all the found div element, find all `a`, and finally within all the `a` elements
found, find all the images. This way you can walk around easily. Moreover you can
refine easily some element

    div.some-class

Will find some div element with the attribute `class` equal to "some-class". You
can also find by "name"

    div#someId

which will find div element with the attribute `id` equal to someId. And you can
even combine elements :

    div.some-class#someId

### Accessing a web page

The first thing to do is to access a web page, the first idea is to use a string,
with an URI in it, and follow it

    "http://somewebsiteIWant.com" >

The `>` operator is the "Dereference" operator, it will try to follow any link
or graph element. If you give it a string, it will try to download a string and
parse it. If you give it a tag, it will try to find an `href` attribute and follow
it.

Some pattern is to find some links and follow them

    div.nav-next a >

For example, the previous request will search a div element of class `nav-next`, all
the links within (hopefully just one :)) and follow the link. Now the analysed node
is the root of the pointed document.

### Filtering content

** /!\ This part has not been debugged/tested yet **

You might want to further refine your currently selected node, and checking
some attributes of it, for this you can use action.

    div img { @alt /= "" }

the actions are between `{}`, everything that's between `{}` can exprimate some
comparison. You can directly query attributes and basic expression syntax is
provided. You can chain comparison with `;`

    div img { @alt /= ""; width > 10; height < 2000 }

### Dumping content

** This part is still experimental **

Dumping content is a simple action called `.`

    img {.}

As all actions, it's put into `{}`, you can still filter before dumping :

    img {@alt /= ""; .}

If the selected nodes have an `src` attribute, it's what is dumped, otherwise
some text approximation is made and displayed on screen.

TODO : make some string formating inside the expression.

### Branching

You might want to find/dump several elements in some webpage, placed at different
place in the document, for that you can use branches

    "http://somesite.com" > (head title {.}; img {.})

Here two elements are dumped, first the title of the document, then all the
images of the document. When branches are encountered, the current state is kept
and is reset after all `;`. So here `head` will be searched in the `html` node,
and `img` to. Each branch is independent, only the last one is kept for further
processing.

If one of the branch fail, the following branches are not executed.

### Repeating

You might want to repeat some path into your documents often, if you want
to dump data spread across many web pages for instance. Let's write an
expression which dump all title of the pages linked to the current one
recursively.

    "http://somesite.com" > (head title {.}; a >)*

We use a branch to dump the title of the page, then we find
all links of the current page and follow them. After that the
`*` repeat the expression while there is no error, or if there
is no node left. The stared expression is always valid, even
if no full execution occur. You can use the `+` variant which
says that at least one valid execution must occur.

    "http://somesite.com" > (head title {.}; a >)+

Real examples
-------------
Here some real-world examples used to dump some... comics

    webrexp '"http://www.someComic.com/chapterone/" > (div.comicpane img {.}; div.nav-next a >)*'
    webrexp '"http://www.someOtherComic.com/" > div.nav-first a > (div#comic img {.}; div.nav-next a >)*'

TODO : tweak threadDelay to better control server hammering.

Webrexp Reference
=================
Command line ref
----------------

    Webrexp
      -o FILE   --output=FILE  output FILE
      -f FILE   --file=FILE    input FILE, use - for stdin
      -v        --verbose      Display many information
      -q        --quiet        Remain practically silent
      -h        --help         Display help (this screen)
      -d Delay  --delay=Delay  Time to wait between HTTP request (ms)


Expression grammar
------------------

    webrexp ::= exprPath (';' exprPath)*
    
    noderange ::= [0-9]
                | [0-9] '-' [0-9]
    
    
    ranges ::= noderange
             | ranges ',' noderange
    
    webident ::= [a-zA-Z0-9-_]+
    
    webref ::= webident
             | webref '.' webident
             | webref '@' webident
             | webref '#' webident
        
    
    attribute ::= '@' webident

    stringLiteral ::= '"' .* '"' // in fact, it's a regular escaped string
                                 // literal, so you can \t and \n as you want.
    
    actionTerm ::= attribute
                 | '(' actionExpr ')'
                 | stringLiteral
                 | [0-9]+
                 | '.'
    
    actionExpr ::= actionTerm
                 | actionExpr '&' actionExpr
                 | actionExpr '|' actionExpr
    
                 | actionExpr '=' actionExpr
                 | actionExpr '/=' actionExpr
                 | actionExpr '<' actionExpr
                 | actionExpr '>' actionExpr
                 | actionExpr '<=' actionExpr
                 | actionExpr '>=' actionExpr
    
                 | actionExpr '+' actionExpr
                 | actionExpr '-' actionExpr
    
                 | actionExpr '*' actionExpr
                 | actionExpr '/' actionExpr
    
    actionList ::= actionExpr (';' actionExpr)*
    
    exprPath ::= expr+
    
    expr ::= expterm
           | expr '*'
           | expr '+'
           | expr '!'
    
    expterm ::= '(' webrexp ')'
              | '{' actionList '}'
              | '[' ranges ']'
              | '>'
              | '^'
              | '|'
              | '<'
              | stringLiteral
              | webref
            
