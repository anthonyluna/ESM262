ricker <- function(A0, J0, u, y0, a, b, c11, c12, c21, c22) {
  i <- 1
  timeseries <- data.frame(
    "i" = i,
    "xn" = A0 + J0,
    "yn" = y0
  )
  while (i < 1000) {
    if (i == 1) {
      Jn <- a * A0 * exp(-c11 * A0 - c12 * y0)
      An <- (1 - u) * J0
      yn <- b * y0 * exp(-c21 * J0 - c22 * y0)
      xn <- A0 + J0
      new_row <- c(i, xn, yn)
    } else {
      Jnn <- a * An * exp(-c11 * An - c12 * yn)
      Ann <- (1 - u) * Jn
      ynn <- b * yn * exp(-c21 * Jn - c22 * yn)
      xnn <- An + Jn

      Jn <- Jnn
      An <- Ann
      yn <- ynn
      xn <- xnn

      new_row <- c(i, xn, yn)
    }

    timeseries <- rbind(timeseries, new_row, make.row.names = FALSE)


    i <- i + 1
  }
  return(timeseries)
}