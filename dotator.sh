#!/bin/sh

for i in `ls test/*.webrexp`;
do
    dist/build/Webrexp/webrexp -f $i --dot > temp
    dot -Tpdf -o "${i}.pdf" temp
done

rm temp

