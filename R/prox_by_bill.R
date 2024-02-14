
#' @title Create a proximity matrix for each bill ####
#'
#' @description
#' This function transforms a tidy dataset with roll-call data into party proximity by each vote.
#' It outputs a nested tibble or list that shows the Euclidean distance between all party by bill.
#' If "list" (the default) the function returns a list of matrices.
#' If type = "tibble", the function returns a tibble where each
#' line has a vector that is the lower triangular part of a proximity matrix for a bill.
#' The lower tringular part is organized from top to bottom, from left to right.
#' The function assigns a value of 1 to an MP that votes in favor, 0 to against and 0.5 to abstentions and misses.
#' Note: for the boot::boot function, a dataset must be provided.
#'
#' @param data A tibble or data frame
#' @param type A string. Can be "list" or "tibble".
#' @param order Organization of parties in the prox matrix. "leftright", the default, organizes parties from left to right. "alphabetical" is also possible.
#'
#' @return A tibble or a list, where each element is a party proximity to a given bill.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data("ar_data")
#' ar_data |>
#'   filter(legis %in% c(14, 15)) %>%
#'   prox_by_bill(type = "tibble")
#'

prox_by_bill <- function(data,
                         type = "tibble",
                         order = "leftright") {



  if (order == "alphabetical") {
    # the order must be changed to political order.
    data %<>%
      dplyr::arrange(id_vot, partido)
  }

  if (order == "leftright") {
    #order of all parties
    order_all_parties <- c("CDU",
                           "PCP",
                           "PEV",
                           "ID",
                           "MDP/CDE",
                           "MDP",
                           "BE",
                           "UDP",
                           "L",
                           "PAN",
                           "UEDS",
                           "PS",
                           "ASDI",
                           "PSN",
                           "PRD",
                           "PSD",
                           "IL",
                           "CDS",
                           "PPM",
                           "CH")

    order <- dplyr::intersect(
      order_all_parties,
      data$partido %>% base::unique())

    data %<>%
      dplyr::arrange(id_vot,
              factor(partido, levels = order))

  }



  # Because of voting freedom, this chunk of code creates an index of voting direction
  #   each vote in favor counts as a 1, against as a 0 and abstention 0.5
  # This chunk returns one observation per party per bill vote.
  data %<>%
    dplyr::mutate(voto = dplyr::case_when(
      voto == "favor" ~ 1 * (n_votos/n_dep),
      voto %in% c("ausencia", "abstencao") ~ 0.5 * (n_votos/n_dep),
      voto == "contra" ~ 0 * (n_votos/n_dep)
    )) %>%
    dplyr::group_by(id_vot, partido) %>%
    dplyr::summarize(voto = sum(voto)) %>%
    dplyr::ungroup()

  #----------------------------------------------------------------------------#
  # Filling the dataset with NAs when parties are not included in a vote
  # The goal is to have every possible party relationship represented,
  # If missing, we want an NA, not a missing.

  # vector with all the bills proposed (unique ID of the vote)
  un_id_vot <- data %>%
    dplyr::distinct(id_vot) %>%
    dplyr::pull()

  # vec with all the parties present in the given data
  un_party <- data$partido %>%
    unique()

  list_missing_parties <- data %>%
    dplyr::select(id_vot, partido, voto) %>%
    split(.$id_vot) %>%
    purrr::map(dplyr::pull, partido) %>%
    purrr::map(~{base::setdiff(un_party, .x)})

  df_missing_parties <-
    list_missing_parties %>%
    purrr::map2(names(list_missing_parties), ~{
      tibble::tibble(
        id_vot = .y,
        partido = .x,
        voto = NA # tibble will expand this column to the size of "partido"
      )
    }) %>%
    purrr::reduce(dplyr::bind_rows)

  # add df_missing_parties to the data
  data %<>%
    dplyr::bind_rows(
      df_missing_parties
    )

  # for some reason, it loses the ordering
  data %<>%
    dplyr::arrange(id_vot,
            factor(partido, levels = order))

  if (type == "list") {
    stop() # not yet finish
    data %>%
      dplyr::arrange(id_vot, partido) %>%
      split(.$id_vot)

    # map(select, voto) %>%
    # map(dist, method = "euclidean", diag = T, upper = T) %>% #here, calcualte eucledian distance
    # map(as.matrix) %>%
    # map(as_tibble)
  }

  if (type == "tibble") {

    nested_tibble <- data %>%
      dplyr::group_by(id_vot) %>% # Nesting a grouped data frame nests all variables apart from the group vars
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(distance = purrr::map(.x = data,
                                          .f = ~{

                                            vec <- .x %>%
                                              dplyr::select(voto) %>%
                                              # This function computes the distance automatically.
                                              stats::dist(method = "euclidean"#, diag = T, upper = T
                                              ) %>%
                                              c()

                                            vec <- 1 - vec # transform distance into proximity

                                            vec #%>%
                                              #tibble::as_tibble()
                                          })) %>%
      dplyr::select(-data)
  }

  # A test
  # Do all have the same size?
  ntf <- nested_tibble %>%
    dplyr::mutate(n = length(distance)) %>%
    dplyr::pull(n) %>%
    unlist() %>%
    unique() %>%
    length()

  stopifnot(ntf == 1)

  return(nested_tibble)

}
