
#' Compute Population Variance
#'
#' This function calculates the population variance of a numeric vector, with an option to handle missing values.
#'
#' @param x A numeric vector.
#' @param na.rm A logical value indicating whether `NA` values should be removed before the computation.
#' The default is `TRUE`.
#'
#' @return A numeric value representing the population variance of the input vector.
#'
#' @details The function adjusts the sample variance (calculated by `var()`) by multiplying it by \((n-1)/n\),
#' where \(n\) is the length of the input vector, to derive the population variance.
#'
#' @examples
#' # Example usage
#' data <- c(1, 2, 3, 4, 5)
#' variance(data)
#'
#' data_with_na <- c(1, 2, 3, NA, 5)
#' variance(data_with_na, na.rm = TRUE)
#' variance(data_with_na, na.rm = FALSE)
#'
#' @export
#'
variance <- function(x, na.rm = TRUE) {
  n <- if (na.rm) sum(!is.na(x)) else length(x)
  y <- stats::var(x, na.rm = na.rm) * (n - 1) / n

  return(y)
}
