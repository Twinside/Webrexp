
build:
	runhaskell Setup.hs build
	# cp dist/build/Webrexp/Webrexp.exe webrexp.exe

conf:
	runhaskell Setup.hs --user configure

clean:
	runhaskell Setup.hs clean

run:
	cd debug; ./debug.sh 2>&1 | tee out.txt; cd ..

doc:
	runhaskell Setup.hs haddock --with-haddock=${HOME}/.cabal/bin/haddock

opendoc:
	open dist/doc/html/Webrexp/webrexp/index.html

