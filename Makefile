all:
	R CMD build .
	R CMD check --as-cran vcd2df_1.0.0.tar.gz 
	rm -rf *.Rcheck 
	rm -rf ..Rcheck
	git commit -a -m "Makefile autocommit"
	git push
