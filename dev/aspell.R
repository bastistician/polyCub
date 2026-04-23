#!/usr/bin/Rscript
## Experimenting with the aspell_package* functions from 'utils'
library("utils"); if (interactive()) apropos("^aspell_package")

DIR      <- if (basename(getwd()) == "dev") ".." else "."
CONTROL  <- c("--lang=en_US")
PKGNAMES <- c("gpclib", "maptools", "mvtnorm", "polyCub",
              "sp", "spatstat", "statmod", "rgeos")
WORDS    <- c("Sommariva", "Vianello",
              "antiderivative", "cubature", "integrand", "isotropy",
              "bivariate", "coercible", "vectorized") # also in "en_stats"
## aspell() requires a serialized dictionary file:
dict <- tempfile(fileext = ".rds")
saveRDS(WORDS, dict)


aspell_package_R_files(DIR, control = CONTROL)
## FIXME: should consistently quote symbols in messages

aspell_package_Rd_files(DIR, control = CONTROL, dictionaries = dict)

##aspell_package_vignettes(DIR, control = CONTROL, dictionaries = dict)
## FIXME: tools::vignetteEngine("litedown::vignette")$aspell
##        sets inappropriate control="-H -t", "--add-filter=markdown" has
##        no effect, `code` is not ignored (even with -M),
##        and line numbers are wrong.
## Alternative: use built-in markdown filter (requires 'commonmark' and 'xml2')
## FIXME: finds weird "htm"/"vignett", but should simply skip yaml keys.
wordlist_vignette <- tempfile()
writeLines(c("Abramowitz", "Stegun", "SV", "integrands", "runtimes",
             PKGNAMES), # cannot ignore all bold text
           wordlist_vignette)
aspell(file.path(DIR, "vignettes", "polyCub.Rmd"), filter = "md",
       control = c(CONTROL, "--add-filter=tex", # for $math$ and %\Vignette*
                   paste0("--add-wordlists=", wordlist_vignette)),
       dictionaries = c(dict))

## DESCRIPTION is also covered by R CMD check --as-cran if also
##     _R_CHECK_CRAN_INCOMING_USE_ASPELL_=TRUE
## which could be configured via .aspell/defaults.R
## but we do not want to bundle such a developer tool.
utils:::aspell_package_description(DIR, control = CONTROL, dictionaries = dict,
                                   ignore = "'[^']+'") # quoted words

## NEWS.md is currently not covered by the aspell_package* functions
wordlist_NEWS <- tempfile()
writeLines(c("CRAN", "GEOS", "Hedevang", "Quintero",
             PKGNAMES), # cannot ignore all bold text
           wordlist_NEWS)
aspell(file.path(DIR, "NEWS.md"), filter = "md",
       control = c(CONTROL, paste0("--add-wordlists=", wordlist_NEWS)),
       dictionaries = dict)


## Much simpler:
##     spelling::spell_check_package(DIR)
## But: no exact location, not linked, and does not ignore *~ files
## Could use wordlist file from Sys.getenv("SPELLING_WORDLIST").
