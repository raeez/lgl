bm.html: bm.csv
	criterion-to-html bm.csv

bm.csv: run
	./run -u bm.csv

run: Util.hs Dense.hs Main.hs
	ghc -O2 --make Main.hs -o run

.PHONY: clean
clean:
	rm *.o
	rm *.hi
	rm *.html
	rm *.csv
	rm run

.PHONY: pull
pull:
	cabal install criterion criterion-to-html
