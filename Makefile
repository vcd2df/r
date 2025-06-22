all:	test
	git commit -a -m "Makefile autocommit"
	git push

test:
	R CMD build .
	R CMD check --as-cran vcd2df_1.0.1.tar.gz 
	rm -rf *.Rcheck 
	rm -rf ..Rcheck
	
