context("polyCub_iso C-routine (API)")

## CAVE (as of R-3.4.0 with testthat 1.0.2):
## During R CMD check, tools:::.runPackageTests() sets R_TESTS=startup.Rs,
## a file which is created in the parent directory "tests", see
## file.path(R.home("share"), "R", "tests-startup.R")
## for its contents. However, testthat tests are run with the working directory
## set to here, so auxiliary R sessions initiated here would fail when trying
## to source() the R_TESTS file on startup, see the system Rprofile file
## file.path(R.home("library"), "base", "R", "Rprofile")
## for what happens. Solution: unset R_TESTS (or set to "") for sub-R processes.

## function to call an R CMD with environment variables
## 'env' specified as a named character vector
Rcmd <- function (args, env = character(), ...) {
    stopifnot(is.vector(env, mode = "character"),
              !is.null(names(env)))
    if (.Platform$OS.type == "windows") {
        if (length(env)) {
            ## the 'env' argument of system2() is not supported on Windows
            setenv <- function (envs) {
                old <- Sys.getenv(names(envs), unset = NA, names = TRUE)
                set <- !is.na(envs)
                if (any(set)) do.call(Sys.setenv, as.list(envs[set]))
                if (any(!set)) Sys.unsetenv(names(envs)[!set])
                invisible(old)
            }
            oldenv <- setenv(env)
            on.exit(setenv(oldenv))
        }
        system2(command = file.path(R.home("bin"), "Rcmd.exe"),
                args = args, ...)
    } else {
        system2(command = file.path(R.home("bin"), "R"),
                args = c("CMD", args),
                env = paste(names(env), env, sep = "="), ...)
    }
}

message("compiling polyiso_powerlaw.c using R CMD SHLIB")
shlib_error <- Rcmd(args = c("SHLIB", "--clean", "polyiso_powerlaw.c"),
                    env = c("PKG_CPPFLAGS" = paste0("-I", system.file("include", package="polyCub")),
                            "R_TESTS" = ""))
if (shlib_error)
    skip("failed to build the shared object/DLL for the polyCub_iso example")

## load shared object/DLL
myDLL <- paste0("polyiso_powerlaw", .Platform$dynlib.ext)
loadNamespace("polyCub")
dyn.load(myDLL)

## R function calling C_polyiso_powerlaw
polyiso_powerlaw <- function (xypoly, logpars, center,
                              subdivisions = 100L, rel.tol = .Machine$double.eps^0.25,
                              abs.tol = rel.tol, stop.on.error = TRUE)
{
    .C("C_polyiso_powerlaw",
       as.double(xypoly$x), as.double(xypoly$y), as.integer(length(xypoly$x)),
       as.double(logpars),
       as.double(center[1L]), as.double(center[2L]),
       as.integer(subdivisions), as.double(abs.tol), as.double(rel.tol),
       as.integer(stop.on.error),
       value = double(1L), abserr = double(1L), neval = integer(1L))[c("value", "abserr", "neval")]
}


## example polygon and function parameters
set.seed(1)
xy <- list(x = stats::rnorm(10), y = stats::rnorm(10))
hidx <- grDevices::chull(xy)
xypoly <- lapply(xy, "[", rev(hidx))  # anticlockwise vertex order
logpars <- log(c(0.5, 1))

(res <- polyiso_powerlaw(xypoly, logpars, center = c(0,0)))


## compare with R implementation
intrfr.powerlaw <- function (R, logpars)
{
    sigma <- exp(logpars[[1L]])
    d <- exp(logpars[[2L]])
    if (d == 1) {
        R - sigma * log(R/sigma + 1)
    } else if (d == 2) {
        log(R/sigma + 1) - R/(R+sigma)
    } else {
        (R*(R+sigma)^(1-d) - ((R+sigma)^(2-d) - sigma^(2-d))/(2-d)) / (1-d)
    }
}
(orig <- polyCub:::polyCub1.iso(xypoly, intrfr.powerlaw, logpars, center = c(0,0)))


test_that("C and R implementations give equal results", {
    expect_equal(res$value, orig[1])
    expect_equal(res$abserr, orig[2])
})

## microbenchmark::microbenchmark(
##     polyCub:::polyCub1.iso(xypoly, intrfr.powerlaw, logpars, center = c(0,0)), # 250 mus
##     polyiso_powerlaw(xypoly, logpars, center = c(0,0)), times = 1000)          #  50 mus

dyn.unload(myDLL)
file.remove(myDLL)
