if (!requireNamespace("statmod")) q("no")
library("polyCub")

## statmod::gauss.quad() gives cached Gauss-Legendre nodes/weights
new.NWGL <- lapply(
    X = seq_len(61L),
    FUN = function (n)
    unname(statmod::gauss.quad(n = n, kind = "legendre"))
)
stopifnot(all.equal(new.NWGL, polyCub:::.NWGL, check.attributes = FALSE))

## polyCub.SV() can fetch nodes and weights from 'statmod'
diamond <- list(list(x = c(1,2,1,0), y = c(1,2,3,2)))
nw <- polyCub.SV(diamond, f = NULL, nGQ = 83)  # nGQ > 61
stopifnot(is.list(nw))

## polyCub.SV() can reduce nodes with zero weight
rectangle <- list(list(x = c(-1,1,1,-1), y = c(1,1,2,2)))
##nw0 <- polyCub.SV(rectangle, f = NULL, nGQ = 3, engine = "C")[[1]]  # 0s
nw <- polyCub.SV(rectangle, f = NULL, nGQ = 3, engine = "C+reduce")[[1]]
stopifnot(nw$weights != 0)
##f <- function (s) 1  # => calculate area (= 2)
stopifnot(all.equal(sum(nw$weights), 2))
