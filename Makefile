dist/bm.html: dist/bm.csv
	criterion-to-html dist/bm.csv

dist/bm.csv: dist/bench
	./dist/bench -u dist/bm.csv

dist/bench: Util/Util.hs Util/Dense.hs Util/Random.hs Util/Sparse.hs Util/Suite.hs Util/Bench.hs
	ghc -O2 --make Util/Bench.hs -o dist/bench

.PHONY: prof
prof: dist/prof
	cat dist/prof

dist/prof: dist/lgl
	./dist/lgl +RTS -p
	mv lgl.prof dist/prof

dist/lgl: Util/Util.hs Util/Dense.hs Util/Random.hs Util/Sparse.hs Util/Profile.hs
	ghc -rtsopts -prof -auto-all -O2 -o dist/lgl Util/Profile.hs 

.PHONY: test
test: dist/test
	./dist/test +RTS -K100000000000 -RTS

dist/test: Util/Util.hs Util/Dense.hs Util/Random.hs Util/Sparse.hs Util/Test.hs
	ghc -rtsopts --make Util/Test.hs -o dist/test

.PHONY: clean
clean:
	-rm Frozen/*.o
	-rm Frozen/*.hi
	-rm Data/Graph/*.hi
	-rm Data/Graph/*.o
	-rm Data/Graph/Linear/*.hi
	-rm Data/Graph/Linear/*.o
	-rm Data/Graph/Linear/Query/*.hi
	-rm Data/Graph/Linear/Query/*.o
	-rm Data/Graph/Linear/Representation/*.hi
	-rm Data/Graph/Linear/Representation/*.o
	-rm *.o
	-rm *.hi
	-rm *.html
	-rm *.csv
	-rm dist/*.o
	-rm dist/*.hi
	-rm dist/*.html
	-rm dist/*.csv
	-rm dist/bench
	-rm dist/lgl
	-rm dist/prof
	-rm dist/test
