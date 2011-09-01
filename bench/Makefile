bm.html: bm.csv
	criterion-to-html bm.csv

bm.csv: bench
	./bench -u bm.csv

bench: Util.hs Dense.hs Random.hs Sparse.hs Bench.hs Main.hs
	ghc -O2 --make Main.hs -o bench

prof: lgl
	./lgl +RTS -p
	mv lgl.prof prof
	cat prof

lgl: Util.hs Dense.hs Random.hs Sparse.hs Profile.hs
	ghc -rtsopts -prof -auto-all -O2 -o lgl Profile.hs 

.PHONY: clean
clean:
	-rm frozen/*.o
	-rm frozen/*.hi
	-rm *.o
	-rm *.hi
	-rm *.html
	-rm *.csv
	-rm bench
	-rm lgl
	-rm prof
