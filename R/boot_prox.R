#' @title Bootstap Funtion for Party Proximity
#'
#' @description
#' Function to use in the bootstraping.
#'
#' @param data A tibble where each line is a bill, and has a proximity vector nested,
#' @param indices The indexes to be retured. Necessary for the bootstrap.
#'
#' @return A vector with the
#'
#' @export
#'
#' @examples
#'
#'

boot_prox <- function(data,
                      indices
) {

  d <- data[indices,] # choose the laws

  # Add all the vectors
  # X <- d$distance %>%
  #   purrr::reduce(`+`)
  #
  # # mean proximity:
  # X <- X/nrow(d)
  # X <- c(as.matrix(X))

  vector <-
    d %>%
    map(c) %>% #transform the tibbles into multiple vectors
    unlist() %>% # transform into only one vector
    unname() # remove the names

  # number of parties
  np <- d[[1]] %>% nrow()

  # number of bills proposed
  nl <- d %>% length()

  #reached the "rotated list"
  # where each elemtn of the list is a party-party combination vector
  # in that vector, each elemtn is a bill proximity

  x <- 1:np %>% map(~{
    vector[seq(.x, np*nl, by = np)]
  }) %>%
    map(mean, na.rm = T)


  return(x)

}




