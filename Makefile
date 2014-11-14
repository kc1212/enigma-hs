WFLAGS=-Wall

all:
	ghc --make $(WFLAGS) Main.hs
	ghc Tests.hs
clean:
	rm -rf *.o *.hi Main
