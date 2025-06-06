#' @tags globals
#' @tags listenv
#' @tags detritus-files
#' @tags mirai_multisession

library(future.mirai)
library(listenv)

plan(mirai_multisession)

message("*** Tricky use cases related to globals ...")

message("- Globals with the same name as 'base' objects ...")

## 'col' is masked by 'base::col' (Issue #55)

col <- 3
x %<-% { stopifnot(is.numeric(col)); col }
print(x)
stopifnot(x == col)

message("- flapply(x, FUN = base::vector, ...) ...")

flapply <- function(x, FUN, ...) {
  res <- listenv()
  for (ii in seq_along(x)) {
    res[[ii]] %<-% FUN(x[[ii]], ...)
  }
  names(res) <- names(x)

  ## Make sure 'x', 'FUN' and 'ii' are truly
  ## exported to the future environment
  rm(list = c("x", "FUN", "ii"))

  as.list(res)
}

x <- list(a = "integer", b = "numeric", c = "character", c = "list")
str(list(x = x))

y0 <- lapply(x, FUN = base::vector, length = 2L)
str(list(y0 = y0))

y <- flapply(x, FUN = base::vector, length = 2L)
str(list(y = y))
stopifnot(identical(y, y0))


message("- flapply(x, FUN = future:::hpaste, ...) ...")

x <- list(a = c("hello", b = 1:100))
str(list(x = x))

y0 <- lapply(x, FUN = future:::hpaste, collapse = "; ", maxHead = 3L)
str(list(y0 = y0))

y <- flapply(x, FUN = future:::hpaste, collapse = "; ", maxHead = 3L)
str(list(y = y))
stopifnot(identical(y, y0))


message("- flapply(x, FUN = listenv::listenv, ...) ...")

x <- list()

y <- listenv()
y$A <- 3L
x$a <- y

y <- listenv()
y$A <- 3L
y$B <- c("hello", b = 1:100)
x$b <- y

print(x)

y0 <- lapply(x, FUN = listenv::mapping)
str(list(y0 = y0))

y <- flapply(x, FUN = listenv::mapping)
str(list(y = y))
stopifnot(identical(y, y0))


message("*** Tricky use cases related to globals ... DONE")

plan(sequential)

