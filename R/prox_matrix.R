
#' @title Build a Proximity Matrix
#'
#' @description
#' Build a proximity matrix of the parties.
#' The function considers agreements to count as "1" and disagreements to count as "-1",
#' while abstentions count as 0. Then, an average is performed.
#' If no input is provided, the function will use all available votes in the dataset.
#'
#' @param dataset A tibble
#' @param ... Parties to consider in the function.
#' @param legislature A numeric vector with legislatures to plot.
#' @param commission A numeric vector with the ID of the commissions crosswalk.
#' @param type Type of law. A character vector.
#' @param stage A character vector with the stage in which the vote was taken.
#' @param vec_parties It does the same as "...", but the input is a numeric vector. Useful for the shiny app.
#' @param initiated_by A character vector with the parties that proposed the law to parliament.
#' @param min_n_votes The relationship between two parties is only shown if it meets this minimum requirement.
#' @param unanimous If 0, show only non unanimous. If one show only unanimous.
#' @param plot_plotly The output is a plotly, not a ggplot matrix.
#' @param exclude_plot Return the matrix, not a plot.
#'
#' @return A matrix graph.
#'
#' @export
#'
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' data(ar_data)
#'
#' # ar_data |> prox_matrix()
#' ar_data |> prox_matrix(legislature = 15)
#'

prox_matrix <- function(dataset,
                        ... ,
                        legislature = c(1:20),
                        #start = "1900/01/01",
                        #end = "3000/01/01",
                        commission = c(1:11),
                        type = c("law",
                                 NA,
                                 "resolution",
                                 "constitutional",
                                 "correction",
                                 "inquiry",
                                 "recommendation",
                                 "validation",
                                 "statute"),
                        stage = c("general",
                                  "final",
                                  "other",
                                  "specific",
                                  "recommendation"),
                        vec_parties = c("CDU",
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
                                        "CH"),
                        initiated_by = c('Government and others',
                                         "CDU",
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
                                         "CH"),
                        min_n_votes = 0,
                        unanimous = c(0,1),
                        plot_plotly = TRUE,
                        exclude_plot = F
) {

  # for the global vars
  bill_type <- data <- dismatch <- id_com_cross <-
  id_vot <- idv <- idv2 <- legis <- match_approve <- match_disapprove <-
  max_conf_interval <- mean_dismatch_diff_max <- mean_dismatch_diff_min <-
  min_conf_interval <- n <- n_dep <- n_votes <- n_votes_against <- n_votes_for <-
  n_votes_for_against_0 <- n_votes_for_against_1 <- n_votos <- partido <- partido2 <-
  perc_dismatch <- perc_match <- perc_match_against <- perc_match_for <-
  perc_match_for_against_0 <- perc_match_for_against_1 <- proponente <-
  total_dep <- unanime <- vote_stage <- voto <- votos_legis_partido <- NULL

  # Necessary columns
  df_temp <- dataset %>%
    dplyr::select(id_vot,
           partido,
           voto,
           n_votos,
           legis,
           n_dep,
           total_dep,
           data,
           id_com_cross,
           bill_type,
           vote_stage,
           proponente,
           votos_legis_partido)

  # Restrict the dataset from the chosen restrictions
  df_temp <-
    dataset %>%
    #the legislature number
    dplyr::filter(legis %in% legislature) %>%
    # the time frame
    # FROM OLDER APP VERSION - WE HAVE REMOVED THE OPTION TO CHOOSE THE DATES
    #filter(data %within% (lubridate::ymd(start) %--% ymd(end))) %>%
    #commission
    dplyr::filter(purrr::map_lgl(id_com_cross, ~ any(.x %in% commission))) %>%
    #bill type
    dplyr::filter(bill_type %in% type) %>%
    #voting stage
    dplyr::filter(vote_stage %in% stage) %>%
    # minimum number of votes
    dplyr::filter(votos_legis_partido >= min_n_votes) %>%
    # partido que iniciou a proposta
    dplyr::filter(purrr::map_lgl(proponente, ~ any(.x %in% initiated_by))) %>%
    # inclusão de votações unânimes
    dplyr::filter(unanime %in% unanimous)


  # lastly, restrict to certain parties

  #parties to keep
  my_parties <- rlang::enexprs(...) #defuse the input

  if(length(my_parties) != 0){#apply only if values are supplied

    df_temp %<>% dplyr::filter(partido %in% my_parties)

  }

  # for the shiny its also useful to select the parties like this

  df_temp %<>%
    dplyr::filter(partido %in% vec_parties)

  #Check if df_temp exists/has values
  #If not, the matrix output should be a NULL value
  if(df_temp %>% nrow != 0){

    #-----------------
    # Classify the Vote
    #------------------


    # because of liberdade de voto, develop a party index (indice direção de voto), where:
    df_temp %<>%
      dplyr::mutate(idv = dplyr::case_when(
        voto == "favor" ~ 1*(n_votos/n_dep),
        voto %in% c("ausencia", "abstencao") ~ 0.5*(n_votos/n_dep),
        voto == "contra" ~ 0*(n_votos/n_dep)
      ))

    df_temp %<>% dplyr::count(id_vot,
                       partido,
                       wt = idv,
                       name = "idv")

    #this creates a dataset with all possible combinations from 2 vectors
    df_unique <- tidyr::expand_grid(id_vot = unique(df_temp$id_vot),
                             partido = unique(df_temp$partido),
                             partido2 = unique(df_temp$partido))


    # add the vote of partido in a given bill
    df_unique %<>%
      dplyr::left_join(df_temp %>%
                  dplyr::select(id_vot, partido, idv),
                by = c("id_vot", "partido"))

    df_unique %<>%
      dplyr::left_join(df_temp %>%
                  dplyr::select(id_vot, partido, idv) %>%
                  dplyr::rename(partido2 = partido,
                         idv2 = idv),
                by = c("id_vot", "partido2"))

    df_unique %<>%
      #the absolute value of the difference
      dplyr::mutate(dismatch = abs(idv - idv2) %>% as.numeric(),
             match_approve = dplyr::case_when(idv == 1 ~ idv2,
                                       TRUE ~ NA),
             match_disapprove = dplyr::case_when(idv == 0 ~ 1 - idv2,
                                          TRUE ~ NA)
      )

    #and then aggregate, in a percentage
    # we have to remove the NA to be able not to account if a party disappears in a middle of a legis
    df_matrix <- df_unique %>%
      tidyr::drop_na(dismatch) %>%
      dplyr::group_by(partido,
               partido2) %>%
      dplyr::reframe(perc_dismatch = mean(dismatch),
              n_votes = dplyr::n(),
              mean_dismatch_diff_min = dplyr::case_when( # need to be more than 1 and unique
                length(dismatch) > 1 & length(unique(dismatch)) > 1 ~ (((t.test(dismatch))$conf.int)[1]), # devia ser infinito
                TRUE ~ -1000
              ),
              mean_dismatch_diff_max = dplyr::case_when( # need to be more than 1 and unique
                length(dismatch) > 1 & length(unique(dismatch)) > 1 ~ (((t.test(dismatch))$conf.int)[2]), # devia ser infinito
                TRUE ~ 1000
              )) %>%
      dplyr::ungroup() %>%

      #second join to take the voting direction into account
      dplyr::left_join(df_unique %>%
                         tidyr::drop_na(dismatch) %>%
                         dplyr::filter(idv %in% c(0,1)) %>%
                         dplyr::group_by(partido,
                           partido2,
                           idv) %>%
                         dplyr::reframe(perc_match_for = sum(match_approve, na.rm = TRUE)/dplyr::n(),
                          perc_match_against = sum(match_disapprove, na.rm = TRUE)/dplyr::n(),
                          n_votes_for_against = dplyr::n()) %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(perc_match_for_against = dplyr::case_when(perc_match_for == 0|is.na(perc_match_for) ~ perc_match_against,
                                                            perc_match_against == 0|is.na(perc_match_against) ~ perc_match_for)) %>%
                         dplyr::select(!c(perc_match_for, perc_match_against)) %>%
                  tidyr::pivot_wider(names_from = 'idv',
                              #names_prefix = 'idv',
                              values_from = c('perc_match_for_against', 'n_votes_for_against'))) %>%
      dplyr::mutate(perc_match = (1-perc_dismatch)*100,
             min_conf_interval = (1 - mean_dismatch_diff_max)*100,
             max_conf_interval = (1 - mean_dismatch_diff_min)*100,
             perc_match_for = perc_match_for_against_1*100,
             perc_match_against = perc_match_for_against_0*100,
             n_votes_for = n_votes_for_against_1,
             n_votes_against = n_votes_for_against_0
      )


    #options on names
    df_matrix %<>%
      dplyr::mutate(dplyr::across(c(partido, partido2),              ~{
        dplyr::case_when( #all the parties that led to BE:
          .x %in% c("MDP/CDE") ~ "MDP",
          TRUE ~ .x
        )}))


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
      df_matrix$partido %>% base::unique())

    p2 <- df_matrix %>%
      dplyr::mutate(perc_match = perc_match %>% round(2),
             min_conf_interval = min_conf_interval %>% round(2),
             max_conf_interval = max_conf_interval %>% round(2),
             perc_match_for = perc_match_for %>% round(2),
             perc_match_against = perc_match_against %>% round(2)
      ) %>%

      dplyr::filter(partido != "Independente" & partido2 != "Independente") %>%
      dplyr::mutate(partido = partido %>% forcats::fct_relevel(order),
             partido2 = partido2 %>% forcats::fct_relevel(rev(order))) %>%
      dplyr::select(partido,
             partido2,
             perc_match,
             n_votes,
             min_conf_interval,
             max_conf_interval,
             perc_match_for,
             perc_match_against,
             n_votes_for,
             n_votes_against)
  }


  else{p2 = tibble::tibble(partido = 'Not available')}

 if (exclude_plot == F) {

  matrix_chart =
    p2 %>%
    ggplot2::ggplot(ggplot2::aes(x = partido,
               y = partido2,
               fill = perc_match,
               text = paste0(partido,
                             #if(partido != partido2) #tentativa de resolver 'PS and PS'
                             #  {paste0(" and ", partido2, "<br>")},
                             #else
                             #{""},
                             " and ", partido2, "<br>",
                             'N. votes: ', n_votes, "<br>",
                             "Min. conf. interval: ", min_conf_interval, '%',  '<br>',
                             "Max. conf. interval: ", max_conf_interval, '%',  '<br>')
    )
    ) +
    ggplot2::geom_tile(color = "white",
              linetype = 1,
              lwd = 1) +
    ggplot2::coord_fixed() + #fix as a square
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(perc_match), "%")),
              color = "white",
              size = ggplot2::rel(3.5),
              fontface = 'bold') +
    ggplot2::scale_x_discrete(limits = NULL, expand = ggplot2::expansion(add = 0, mult = c(.01, .01))) +
    ggplot2::scale_y_discrete(limits = NULL, expand = ggplot2::expansion(add = 0, mult = c(.01, .01))) +
    ggplot2::scale_fill_gradientn(colours = RColorBrewer::brewer.pal(4,"BrBG"),
                                  na.value = "grey20") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = ggplot2::rel(1))) #Trying to reduce distance from y axis to graph


  if(plot_plotly == TRUE){
    matrix_chart %<>% plotly::ggplotly(tooltip = c("text")) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }

  return(matrix_chart)

 }else{
   return(p2)
 }

}
