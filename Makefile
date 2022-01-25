PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: rd check1

rd:
	Rscript -e 'library(methods);devtools::document()'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd", encoding="UTF-8")'

build1:
	Rscript -e 'devtools::build()'

build2:
	Rscript -e 'devtools::build(vignettes = FALSE)'

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check1: rd build1
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

crancheck: rd build1
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz", args="--as-cran")'

debug: rd build1
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

vignettes:
	Rscript -e 'usethis::use_vignette("$(PKGNAME)")'
	
clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

clean2:
	cd ..;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz

create:
	cd ..;\
	Rscript -e 'usethis:::create_package("$(PKGSRC)")'

license:
	Rscript -e 'usethis::use_gpl3_license("$(PKGNAME)")'

bignore:
	Rscript -e 'usethis::use_build_ignore(glob2rx("inst/extdata/*.png"), escape = FALSE)'
	Rscript -e 'usethis::use_build_ignore(c("Makefile", "README.md", "README.Rmd", "CONDUCT.md", ".Rproj.user", ".Rproj"))'

gignore:
	Rscript -e 'usethis::use_git_ignore(c(".DS_Store", ".RData", ".Rhistory", ".Rproj.user"))'
