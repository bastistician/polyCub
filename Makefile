################################################################################
## Useful rules to build, check and install an R source package
##
## Copyright (C) 2012,2014-2019,2022,2024-2025 Sebastian Meyer
################################################################################

## define variable for R to enable 'make check R=R-devel'
R := R

## package name and version (taken from DESCRIPTION)
PKG := $(strip $(shell grep "^Package:" DESCRIPTION | cut -f 2 -d ":"))
VERSION := $(strip $(shell grep "^Version:" DESCRIPTION | cut -f 2 -d ":"))

## non-commercial use of gpclib granted when building the package vignette
## and checking locally
export R_GPCLIBPERMIT := true

## roxygenise (update NAMESPACE and Rd files)
document:
	$R --no-restore --no-save --no-init-file -s -e "roxygen2::roxygenise()"

## build vignettes
vignettes:
	NOT_CRAN=true $R --vanilla -s -e 'tools::buildVignettes(dir = ".")'

## build the package
build: document
	NOT_CRAN=true $R CMD build .

## package installation
install: build
	$R CMD INSTALL ${PKG}_${VERSION}.tar.gz


## auxiliary functions ("canned recipes") for check rules
define check-report-warnings-in-examples
cd ${PKG}.Rcheck; \
nwarn=`grep -c "^Warning" ${PKG}-Ex.Rout`; \
if [ $$nwarn -gt 0 ]; then echo "\n\tWARNING: $$nwarn" \
	"warning(s) thrown when running examples,\n" \
	"\t         see file ${PKG}.Rcheck/${PKG}-Ex.Rout\n"; fi
endef

## standard --as-cran check with remote checks disabled
check: build
	_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE _R_CHECK_EXAMPLE_TIMING_THRESHOLD_=2 $R CMD check --as-cran --timings ${PKG}_${VERSION}.tar.gz
## further option: --use-gct (for better detection of memory bugs/segfaults)
	@$(check-report-warnings-in-examples)

## use codetools::checkUsagePackage
checkUsage: install
	echo "library('${PKG}'); library('codetools'); \
	checkUsagePackage('${PKG}', all = TRUE, \
            suppressParamAssigns = TRUE, suppressParamUnused = TRUE)" \
	| $R -s --no-save --no-restore

## manuals
manual.pdf: DESCRIPTION document
	$R CMD Rd2pdf --batch --force --output="$@" .

manual.html: DESCRIPTION document
	echo "tools::pkg2HTML(dir = '.', out = '$@', toc_entry = 'name')" \
	  | $R -s --no-save --no-restore
	xdg-open "$@"

## generate HTML page from NEWS.Rd
# NEWS.html: inst/NEWS.Rd
# 	$R --vanilla -s -e \
# 	'tools::Rd2HTML("$<", out="$@", stylesheet="https://CRAN.R-project.org/web/CRAN_web.css")'
# 	[ `uname -s` = "Darwin" ] && open "$@" || xdg-open "$@"

## generate HTML page from NEWS.md
NEWS.html: NEWS.md
	pandoc --quiet --from=commonmark --to=html --output="$@" --standalone --css "https://CRAN.R-project.org/web/CRAN_web.css" "$<"
	[ `uname -s` = "Darwin" ] && open "$@" || xdg-open "$@"

## report code coverage
covr:
	$R --vanilla -s -e \
	'covr::report(covr::package_coverage(type="all"), file="/tmp/covr.html", browse=TRUE)'

## spell check
spelling:
	$R --vanilla -s -e 'spelling::spell_check_package()'
	codespell -S '*~' examples inst NEWS.md README.md R src tests vignettes

## cleanup
clean:
	cd src; rm -f *.o *.so *.dll symbols.rds
	rm -f ./*/.Rhistory

## almost all targets are "phony"
.PHONY: document vignettes build install check checkUsage covr spelling clean
