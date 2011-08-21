out.html: out.csv
	criterion-to-html out.csv

out.csv: rand
	./rand -u out.csv

rand: rand.hs
	ghc -O --make rand.hs
