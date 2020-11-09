library("polyCub")

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

## test compilation with #include <polyCubAPI.h> for the polyCub_iso C-routine
message("compiling polyiso_powerlaw.c using R CMD SHLIB")
shlib_error <- Rcmd(
    args = c("SHLIB", "--clean", "polyiso_powerlaw.c"),
    env = c("PKG_CPPFLAGS" = paste0(
                "-I", system.file("include", package="polyCub")
            ),
            "R_TESTS" = "")
)
if (shlib_error) {
    warning("failed to build the shared object/DLL for the polyCub_iso example")
    q("no")
}

## load shared object/DLL
myDLL <- paste0("polyiso_powerlaw", .Platform$dynlib.ext)
loadNamespace("polyCub")
dyn.load(myDLL)

## R function calling C_polyiso_powerlaw
polyiso_powerlaw <- function (xypoly, logpars, center,
                              subdivisions = 100L,
                              rel.tol = .Machine$double.eps^0.25,
                              abs.tol = rel.tol, stop.on.error = TRUE)
{
    .C("C_polyiso_powerlaw",
       as.double(xypoly$x), as.double(xypoly$y), as.integer(length(xypoly$x)),
       as.double(logpars),
       as.double(center[1L]), as.double(center[2L]),
       as.integer(subdivisions), as.double(abs.tol), as.double(rel.tol),
       as.integer(stop.on.error),
       value = double(1L), abserr = double(1L), neval = integer(1L)
       )[c("value", "abserr", "neval")]
}


## example polygon and function parameters
diamond <- list(x = c(1,2,1,0), y = c(1,2,3,2))
logpars <- log(c(0.5, 1))
center <- c(0.5,2.5)  # lies on an edge (to cover that case as well)

(res <- polyiso_powerlaw(xypoly = diamond,
                         logpars = logpars,
                         center = center))


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
(orig <- polyCub:::polyCub1.iso(poly = diamond,
                                intrfr = intrfr.powerlaw,
                                logpars = logpars,
                                center = center))

stopifnot(all.equal(res$value, orig[1L]))
stopifnot(all.equal(res$abserr, orig[2L]))

## microbenchmark::microbenchmark(
##     polyCub:::polyCub1.iso(diamond, intrfr.powerlaw, logpars, center=center),
##     polyiso_powerlaw(diamond, logpars, center=center),
##     times = 1000)
## ## 150 mus vs. 20 mus

dyn.unload(myDLL)
file.remove(myDLL)
