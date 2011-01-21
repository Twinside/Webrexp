
build:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs --user configure

clean:
	runhaskell Setup.hs clean

run:
	runhaskell parserTest.hs

test:
	runhaskell downloadTest.hs -package bytestring -package network -package http

doc:
	runhaskell Setup.hs haddock --with-haddock=${HOME}/.cabal/bin/haddock

opendoc:
	open dist/doc/html/Webrexp/webrexp/index.html

