
build:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs --user configure

clean:
	runhaskell Setup.hs clean

run:
	runhaskell parserTest.hs
