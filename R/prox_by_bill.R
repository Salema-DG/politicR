
#' @title Create a proximity matrix for each bill ####
#'
#' @description
#' Eucledian distance. Ina 2d pane, it's the absolute difference.
#' If "list" (the default) the function returns a list of matrices, where each
#' elements is a bill. If "tibble", the function returns a tibble where each
#' line has another tibble as matrix.
#' Note: for the boot::boot function, a dataset must be provided.
#' But the object returned it streched. The function prox_matrix(2 for now) to transform it into a matrix.
#'
#' @param data A tibble or data frame
#' @param type A string. Can be "list" or "tibble".
#'
#' @return A tibble or a matrix, where each element is a vote containing the party proximity per bill. But the object is streched.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data("ar_data")
#' ar_data |>
#'   filter(legis == 15) %>%
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
      arrange(id_vot,
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

  # for some reason, it loses the ordering
  data %<>%
    arrange(id_vot,
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

                              vec %>%
                                tibble::as_tibble()
                            })) %>%
      dplyr::select(-data)
  }

  # Do all have the same size?
  ntf <- nested_tibble %>%
    dplyr::mutate(n = purrr::map(distance, nrow)) %>%
    dplyr::pull(n) %>%
    unlist() %>%
    unique() %>%
    length()

  stopifnot(ntf == 1)

  return(nested_tibble)

}
