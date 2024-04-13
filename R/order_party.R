
#' @title Order the parties
#'
#' @description
#' This function orders a factor variable containing parties according to their
#' Left-right spectrum.
#'
#' @param data a tibble
#' @param name Name of the column with party info
#'
#' @export
#'
#' @examples
#' #data(ar_data)
#' #ar_data %>% order_party(partido)
#'
#'

order_party <- function(data,
                        name) {

  #-------------------------------#
  # the order, from left to right #
  #-------------------------------#

  #create the vector
  order_all_parties <-
    c("CDU", "PCP", "PEV", "ID", "MDP/CDE",
      "MDP", "BE", "UDP", "L", "PSN", "PAN",
      "UEDS", "PS", "ASDI", "PRD", "PSD",
      "IL", "CDS", "CDS+", "PPM", "CH")

  # apply tha order
  data %<>%
    dplyr::mutate({{name}} := {{ name }} %>% forcats::fct_relevel(order_all_parties))

  return(data)

}

