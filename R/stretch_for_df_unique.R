#' @title From lower triangular to full
#'
#' @description
#' The output from politicR::prox_by_bill and boot_prox is the
#' lower triangular part of a proximity matrix, from top to bottom, from left to right.
#' This function transforms it into a vector, of a full proximity matrix,
#' from top to bottom, from left to right.
#'
#' @param x a vectror. Lower tringualar matrix as a vector.
#' @param diag_to_insert The diagonal to insert. By default is 1 to all elements.
#'
#' @return A vector as a full proximity matrix from top to bottom, from left to right.
#'
stretch_for_df_unique <- function(x,
                                  diag_to_insert = NULL) {

  #-------------------------------------------
  # Discover the lenght

  # (n*n-n)/2 = length(res$t0)
  # using the quadratic formula:
  quad <- function(a, b, c)
  {
    a <- as.complex(a)
    answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
                (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
    if(all(Im(answer) == 0)) answer <- Re(answer)
    if(answer[1] == answer[2]) return(answer[1])
    answer
  }

  n <- quad(a = 1, b = -1, c = -2*length(x))
  # keep only the positive answer
  n <- n[n > 0]

  #-------------------------------------------

  matrix_empty <- matrix(0, nrow = n, ncol = n)

  matrix_empty[lower.tri(matrix_empty, diag = F)] <- x

  # fill the up and down part
  matrix_empty %<>%
    Matrix::forceSymmetric(uplo="L")

  if (is.null(diag_to_insert)) {
    # fill the diag with 100
    diag(matrix_empty) <- 1
  } else{
    diag(matrix_empty) <- diag_to_insert
  }

  y <- matrix_empty %>% as.vector()

  return(y)

}


