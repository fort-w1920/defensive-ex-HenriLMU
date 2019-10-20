lag <- function(x, n = 1L) {
  assert_vector(x, strict = TRUE)
  stopifnot(!is.list(x))
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

