.PHONY: tests build check roxygenize

tests: tests/RSQLite.Rout.save tests/duckdb.Rout.save

%.Rout.save : %.R
	R CMD BATCH --vanilla $< $@

roxygenize :
	R -e "roxygen2::roxygenize()"

build : roxygenize
	cd .. && R CMD build $(CURDIR)

check : build
	cd .. && R CMD check --as-cran $(CURDIR)_*.tar.gz
