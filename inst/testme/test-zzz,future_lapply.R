#' @tags future.apply
#' @tags listenv
#' @tags detritus-files
#' @tags mirai_multisession

# Run this test with debug on mostly to increase test coverage
options(future.debug = TRUE)

library(future.mirai)
library(listenv)

if (requireNamespace("future.apply", quietly = TRUE)) {
  future_lapply <- future.apply::future_lapply
  
  strategies <- c("mirai_multisession")
  
  message("*** future_lapply() ...")
  
  message("- future_lapply(x, FUN = vector, ...) ...")
  
  x <- list(a = "integer", c = "character", c = "list")
  str(list(x = x))
  
  y0 <- lapply(x, FUN = vector, length = 2L)
  str(list(y0 = y0))
  
  for (strategy in strategies) {
    mdebugf("- plan('%s') ...", strategy)
    plan(strategy)
    stopifnot(nbrOfWorkers() < Inf)
    
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = vector, length = 2L,
                         future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
    
    plan(sequential)
  }
  
  
  message("- future_lapply(x, FUN = base::vector, ...) ...")
  
  x <- list(a = "integer", c = "character", c = "list")
  str(list(x = x))
  
  y0 <- lapply(x, FUN = base::vector, length = 2L)
  str(list(y0 = y0))
  
  for (strategy in strategies) {
    mdebugf("- plan('%s') ...", strategy)
    plan(strategy)
    stopifnot(nbrOfWorkers() < Inf)
  
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = base::vector, length = 2L,
                         future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }

    plan(sequential)
  }
  
  message("- future_lapply(x, FUN = future:::hpaste, ...) ...")
  
  x <- list(a = c("hello", b = 1:100))
  str(list(x = x))
  
  y0 <- lapply(x, FUN = future:::hpaste, collapse = "; ", maxHead = 3L)
  str(list(y0 = y0))
  
  for (strategy in strategies) {
    mdebugf("- plan('%s') ...", strategy)
    plan(strategy)
    stopifnot(nbrOfWorkers() < Inf)
  
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = future:::hpaste, collapse = "; ",
                         maxHead = 3L, future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
    
    plan(sequential)
  }
  
  
  message("- future_lapply(x, FUN = listenv::listenv, ...) ...")
  
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
  
  for (strategy in strategies) {
    mdebugf("- plan('%s') ...", strategy)
    plan(strategy)
    stopifnot(nbrOfWorkers() < Inf)
  
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN = listenv::mapping, future.scheduling = scheduling)
      str(list(y = y))
      stopifnot(identical(y, y0))
    }
    
    plan(sequential)
  }
  
  
  message("- future_lapply(x, FUN, ...) for large length(x) ...")
  a <- 3.14
  x <- 1:1e6
  
  y <- future_lapply(x, FUN = function(z) sqrt(z + a))
  y <- unlist(y, use.names = FALSE)
  
  stopifnot(all.equal(y, sqrt(x + a)))
  
  
  message("- future_lapply() with global in non-attached package ...")
  library("tools")
  my_ext <- function(x) file_ext(x)
  y_truth <- lapply("abc.txt", FUN = my_ext)
  
  for (strategy in strategies) {
    plan(strategy)
    
    y <- future_lapply("abc.txt", FUN = my_ext)
    stopifnot(identical(y, y_truth))
    
    plan(sequential)
  }
  
  message("*** future_lapply() ... DONE")
}
