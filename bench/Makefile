out.html: out.csv
	criterion-to-html dense.csv

out.csv: dense
	./dense -u dense.csv

dense: Dense.hs Util.hs
	ghc -O2 --make dense.hs

.PHONY: clean
clean:
	rm *.o
	rm *.hi
	rm *.html
	rm *.csv
	rm dense
