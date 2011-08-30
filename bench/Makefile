bm.html: bm.csv
	criterion-to-html bm.csv

bm.csv: bench
	./bench -u bm.csv

bench: Util.hs Dense.hs Random.hs Sparse.hs Main.hs
	ghc -O2 --make Main.hs -o bench

.PHONY: clean
clean:
	-rm *.o
	-rm *.hi
	-rm *.html
	-rm *.csv
	-rm bench

.PHONY: pull
pull:
	cabal install criterion criterion-to-html
