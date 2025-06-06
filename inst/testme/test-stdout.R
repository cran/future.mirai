#' @tags relays
#' @tags detritus-files
#' @tags mirai_multisession

library(future.mirai)

message("*** Standard output ...")

truth_rows <- utils::capture.output({
  print(1:50)
  str(1:50)
  cat(letters, sep = "-")
  cat(1:6, collapse = "\n")
  write.table(datasets::iris[1:10,], sep = "\t")
})
truth <- paste0(paste(truth_rows, collapse = "\n"), "\n")
print(truth)

message("mirai_multisession ...")
plan(mirai_multisession)

for (stdout in c(TRUE, FALSE, NA)) {
  message(sprintf("- stdout = %s", stdout))
  
  f <- future({
    print(1:50)
    str(1:50)
    cat(letters, sep = "-")
    cat(1:6, collapse = "\n")
    write.table(datasets::iris[1:10,], sep = "\t")
    42L
  }, stdout = stdout)
  r <- result(f)
  str(r)
  stopifnot(value(f) == 42L)

  if (is.na(stdout)) {
    stopifnot(is.null(r$stdout))
  } else if (stdout) {
    print(r)
    stopifnot(identical(r$stdout, truth))
  } else {
    stopifnot(is.null(r$stdout))
  }

  v %<-% {
    print(1:50)
    str(1:50)
    cat(letters, sep = "-")
    cat(1:6, collapse = "\n")
    write.table(datasets::iris[1:10,], sep = "\t")
    42L
  } %stdout% stdout
  out <- utils::capture.output(y <- v)
  stopifnot(y == 42L)

  if (is.na(stdout) || !stdout) {
    stopifnot(out == "")
  } else {
    print(out)
    stopifnot(identical(out, truth_rows))
  }
} ## for (stdout ...)

message("mirai_multisession ... DONE")

plan(sequential)

message("*** Standard output ... DONE")

