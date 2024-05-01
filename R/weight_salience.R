
#' @title Weighted average of a vector
#'
#' @description
#' With just two inputs, it creates a weighted average with normalization.
#' USed to weight bills by their importance.
#' At the end, it's normalized for the weights to be independent of the scale used.
#'
#' @param vec_prox A vector with the proximity between 2 parties over a set of bills.
#' @param vec_media_index A vector with the respective importance of those bills.
#'
#' @export
#'
#' @return The proximity of the parties weighted by the importance.
#'
#' @examples
#' # The proximities between two parties over 5 bills
#' vec_proximity <- c(0.1, 0.5, 0.6, 0.4, 0.9)
#'
#' # the media index of each bill
#' vec_media_indexes <- c(5, 40, 30, 70, 10)
#'
#' weight_salience(vec_proximity,
#'                 vec_media_indexes)
#'
#'
#'

weight_salience <- function(vec_prox,
                            vec_media_index) {

  # vector of proximity and the vector of weights must have equal sizes
  stopifnot(length(vec_prox) == length(vec_media_index))

  # Delete the NA's from vec_prox and their corresponding elements in vec_media_index
  vec_media_index <- vec_media_index[!is.na(vec_prox)]
  vec_prox <- vec_prox[!is.na(vec_prox)]

  # Fill the NAs in vec_media_index by the weights average.
  # If there are no news available, a weight is not given.
  av <- vec_media_index %>% mean(na.rm = T)

  # In case they are all NAs
  if (is.nan(av)) {
    av <- 1
  }

  vec_media_index[is.na(vec_media_index)] <- av

  # check if they are still the same length
  stopifnot(length(vec_prox) == length(vec_media_index))

  # the weighted average:
  x <- mean(vec_prox * (vec_media_index) )

  # the normalization
  x <- x * (length(vec_media_index) )/sum(vec_media_index)

  return(x)
}



