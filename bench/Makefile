out.html: out.csv
	criterion-to-html dense.csv

out.csv: run
	./run -u run.csv

run: Util.hs Dense.hs Main.hs
	ghc -O2 --make Main.hs -o run

.PHONY: clean
clean:
	rm *.o
	rm *.hi
	rm *.html
	rm *.csv
	rm run
