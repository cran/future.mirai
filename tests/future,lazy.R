source("incl/start.R")

message("*** Futures - lazy ...")

strategies <- c("mirai_cluster", "mirai_multisession")

for (strategy in strategies) {
  mprintf("- plan('%s') ...", strategy)
  plan(strategy)

  a <- 42
  f <- future(2 * a, lazy = TRUE)
  a <- 21
  v <- value(f)
  stopifnot(v == 84)

  a <- 42
  v %<-% { 2 * a } %lazy% TRUE
  a <- 21
  stopifnot(v == 84)

  mprintf("- plan('%s') ... DONE", strategy)
} ## for (strategy ...)

message("*** Futures - lazy ... DONE")

source("incl/end.R")
