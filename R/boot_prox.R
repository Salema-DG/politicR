#' @title Bootstap Funtion for Party Proximity
#'
#' @description
#' This function takes the output from politicR::prox_by_bill and agreagates all the bills.
#' The function can make the average proximity, and number of bills of two parties.
#' But the main feature is perforing this agregation while disregarding some bills using "indeces".
#' That allows the function to be used in the bootspraping function boot::boot.
#'
#' @param data A tibble where each line is a bill, and has a proximity vector nested,
#' @param indices The indexes to be retured. Necessary for the bootstrap.
#' @param type Can either be "mean" (default) or "length" for the number of bills.
#' @param na_sub Logical values. If 2 parties never meet each other, substitute the output NA to -1. Only for type = "mean".
#'
#' @return A vector with party proximities or number of bills voted by party combination. Only the lower triangular part of a proximity matrix, from top to bottom, from left to right.
#'
#' @export
#'
#' @examples
#' #ta bem
#'

boot_prox <- function(data,
                      indices = NULL,
                      type = "mean",
                      na_sub = FALSE
) {

  # if the value is not suplied
  if (is.null(indices)) {
    indices <- 1:nrow(data)
  }

  ## this allows replacement in the monte carlo
  d <- data[indices,] # choose the laws

  # Add all the vectors
  # X <- d$distance %>%
  #   purrr::reduce(`+`)
  #
  # # mean proximity:
  # X <- X/nrow(d)
  # X <- c(as.matrix(X))

  vector <-
    d$distance %>%
    #map(c) %>% #transform the tibbles into multiple vectors
    unlist() %>% # transform into only one vector
    unname() # remove the names

  # number of parties combinations
  np <- d$distance[[1]] %>% length()

  # number of bills proposed (sample from DGP)
  nl <- d %>% nrow()

  #reached the "rotated list"
  # where each elemtn of the list is a party-party combination vector
  # in that vector, each elemtn is a bill proximity

  if (type == "mean") {
    x <- 1:np %>% purrr::map(~{
      vector[seq(.x, np*nl, by = np)]
    }) %>%
      purrr::map(mean, na.rm = T) %>%
      unlist()
  }

  if (type == "length") {
    x <- 1:np %>% purrr::map(~{
      vector[seq(.x, np*nl, by = np)]
    }) %>%
      # delete the NAs
      purrr::map(~{
        .x[!is.na(.x)]
      }) %>%
      purrr::map(length) %>%
      unlist()
  }

  if (na_sub == T) {
    # substitute the NA's by -1?
    x[is.na(x)] <- -1
  }


  return(x)

}




