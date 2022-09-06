.PHONY: tests

tests: tests/RSQLite.Rout.save tests/duckdb.Rout.save

%.Rout.save : %.R
	R CMD BATCH --vanilla $< $@
