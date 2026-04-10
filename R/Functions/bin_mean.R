bin_mean <- function (vec, every, na.rm = TRUE) {
  n <- length(vec)
  x <- .colMeans(vec, every, n %/% every, na.rm)
  r <- n %% every
  if (r) x <- c(x, mean.default(vec[(n - r + 1):n], na.rm = na.rm))
  x
}
