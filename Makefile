ifeq ($(shell uname),WindowsNT)

HADDOCKPATH		:=${APPDATA}/cabal/bin/haddock

else

HADDOCKPATH		:=${HOME}/.cabal/bin/haddock

endif

build:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs --user configure

clean:
	runhaskell Setup.hs clean

run:
	cd debug; ./debug.sh 2>&1 | tee out.txt; cd ..

doc:
	runhaskell Setup.hs haddock --with-haddock=${HADDOCKPATH}

opendoc:
	open dist/doc/html/Webrexp/webrexp/index.html

lint:
	hlint .

