################################################################################
## Useful rules to build, check and install an R source package
##
## Copyright (C) 2012,2014-2018 Sebastian Meyer
################################################################################

## define variable for R to enable 'make check R=R-devel'
R := R

## package name and version (taken from DESCRIPTION)
PKG := $(strip $(shell grep "^Package:" DESCRIPTION | cut -f 2 -d ":"))
VERSION := $(strip $(shell grep "^Version:" DESCRIPTION | cut -f 2 -d ":"))

## roxygenise (update NAMESPACE and Rd files)
document:
	$R --no-restore --no-save --no-init-file --slave -e "devtools::document()"

## build the package
build: document
	$R CMD build .

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
	| $R --slave --no-save --no-restore

## make pdf manual
manual.pdf: DESCRIPTION document
	$R CMD Rd2pdf --batch --force --output="$@" .

## generate HTML page from NEWS.Rd
# NEWS.html: inst/NEWS.Rd
# 	$R --vanilla --slave -e \
# 	'tools::Rd2HTML("$<", out="$@", stylesheet="https://CRAN.R-project.org/web/CRAN_web.css")'
# 	[ `uname -s` = "Darwin" ] && open "$@" || xdg-open "$@"

## generate HTML page from NEWS.md
NEWS.html: NEWS.md
	pandoc --quiet --from=commonmark --to=html --output="$@" --standalone --css "https://CRAN.R-project.org/web/CRAN_web.css" "$<"
	[ `uname -s` = "Darwin" ] && open "$@" || xdg-open "$@"

## report code coverage
covr:
	$R --vanilla --slave -e \
	'covr::report(covr::package_coverage(type="all"), file="/tmp/covr.html", browse=TRUE)'

## spell check
spelling:
	$R --vanilla --slave -e \
	'spelling::spell_check_package(lang = "en_US")'

## cleanup
clean:
	cd pkg/src; rm -f *.o *.so *.dll symbols.rds
	rm -f ./*/.Rhistory

## almost all targets are "phony"
.PHONY: document build install check checkUsage covr spelling clean
