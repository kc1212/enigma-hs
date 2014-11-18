WFLAGS=-Wall

all:
	ghc --make $(WFLAGS) Main.hs -o enigma
	ghc Tests.hs -o tests
	./tests
clean:
	rm -rf *.o *.hi Main
