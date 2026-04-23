#!/usr/bin/Rscript
### generate internal R/sysdata.rda
### Time-stamp: <[sysdata.R] 2014-08-22 14:20 (CEST) by SM>


### cache nodes & weights of Gauss-Legendre quadrature rule for degrees 1:61

.NWGL <- lapply(seq_len(61L), function (n)
                unname(statmod::gauss.quad(n = n, kind = "legendre")))
attr(.NWGL, "statmodVersion") <- packageVersion("statmod")


### save

FILE <- "../R/sysdata.rda"
save(list = c(".NWGL"), file = FILE, version = 2)
## try to improve compression
tools::resaveRdaFiles(FILE)
