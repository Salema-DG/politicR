
#' @title Weight bills by their importance.
#'
#' @description
#' With just two inputs, it creates a weighted average with normalization.
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

  # they must have equal sizes
  stopifnot(length(vec_prox) == length(vec_media_index))

  # delete the NA's from vec_prox and their corresponding elements in vec_media_index
  vec_media_index <- vec_media_index[!is.na(vec_prox)]
  vec_prox <- vec_prox[!is.na(vec_prox)]


  # fill the NAs in vec_media_index by their respective av.
  av <- vec_media_index %>% mean(na.rm = T)

  # in case they are all NAs
  if (is.nan(av)) {
    av <- 1
  }

  vec_media_index[is.na(vec_media_index)] <- av

  stopifnot(length(vec_prox) == length(vec_media_index))

  # the weighted average:
  x <- mean(vec_prox * (vec_media_index/100) )

  # the normalization
  x <- x * (length(vec_media_index) * 100)/sum(vec_media_index)

  return(x)
}



