#!/bin/sh

for i in `ls test/*.webrexp`;
do
    runhaskell webrexp2dot.hs $i > temp
    dot -Tpdf -o "${i}.pdf" temp
done

rm temp

