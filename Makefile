
all: Main.hs
	ghc --make Main.hs -o icfp

clean:
	rm *.hi *.o icfp
