source("incl/start.R")

message("*** Futures - labels ...")

strategies <- c("mirai_cluster", "mirai_multisession")

for (strategy in strategies) {
  mprintf("- plan('%s') ...", strategy)
  plan(strategy)

  for (label in list(NULL, sprintf("strategy_%s", strategy))) {
    fcn <- get(strategy, mode = "function")
    stopifnot(inherits(fcn, strategy))
    f <- fcn(42, label = label)
    print(f)
    stopifnot(identical(f$label, label))
    v <- value(f)
    stopifnot(v == 42)

    f <- future(42, label = label)
    print(f)
    stopifnot(identical(f$label, label))
    v <- value(f)
    stopifnot(v == 42)

    v %<-% { 42 } %label% label
    f <- futureOf(v)
    print(f)
    stopifnot(identical(f$label, label))
    stopifnot(v == 42)

  } ## for (label ...)

  mprintf("- plan('%s') ... DONE", strategy)
} ## for (strategy ...)

message("*** Futures - labels ... DONE")

source("incl/end.R")
