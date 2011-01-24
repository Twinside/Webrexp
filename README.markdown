WebRexp
=======
Query language designed to extract information from a bunch of HTML files.
The idea is to be able to extract information from a bunch of files linked
between each others, using a syntax similar to regexp.

How to use
----------
Now the use of webrexp is pretty simple, you just use the following command :

    webrexp *expression*


Webrexp writing
---------------

A webrexp describe a path within a graph (by extension tree) followed by the engine.
At each moment, the engine manipulate a list of graph node (thinks tags if you prefer)
or a list of string.


### Walking in a webpage

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

### Accessing a webpage

The first thing to do is to access a webpage, the first idea is to use a string,
with an URI in it, and follow it

    "http://somewebsiteIWant.com" >

The `>` operator is the "Dereference" operator, it will try to follow any link
or graph element. If you give it a string, it will try to download a string and
parse it. If you give it a tag, it will try to find an `href` attribute and follow
it.

Some pattern is to find some links and follow them

    div.nav-next a >

For exemple, the previous request will search a div element of class nav-next, all
the links within (hopefully just one :)) and follow the link. Now the analysed node
is the root of the pointed document.

### Filtering content

** /!\ This part has not been debugged/tested yet **

You might want to further refine your currently selected node, and checking
some attributes of it, for this you can use action.

    div img { @alt != "" }

the actions are between `{  }`, everything that's between {} can exprimate some
comparaison. You can directly query attributes and basic expression syntax is
provided. You can chain comparison with `;`

    div img { @alt != ""; width > 10; height < 2000 }

### Dumping content

Webrexp Reference
=================

Design
------

    rootpage = http://feoaijfoieajfe.com
    next = rootpage ((div.comic-pane img {.}; div.nav-next a) >)+
    
    next = rootpage > ((div.comic-pane img {@width = 3; .}; div.nav-next a) >)+
    
    
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
 

Expression grammar
------------------

