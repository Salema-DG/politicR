
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
#' @param plot Return the dataset p2, not a plot.
#' @param inference Use the bootstrapping inference to build confidence intervals.
#' @param n_boot Number of re-sampling for the bootstrap.
#' @param seed_for_boot Seed for the bootstap
#' @param app Give a dataset ready to plot
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
                        plot = TRUE,
                        inference = F,
                        n_boot = 1000,
                        seed_for_boot = 12345,
                        app = F,
                        weight_by_salience = FALSE
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

  if(app == T){
    p2 = dataset
  }

  else {
  #----------------------------------------------------------------------------#
  # Prepare the data: apply the user choices ####
  #----------------------------------------------------------------------------#

  # Necessary columns for the votes.
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
           votos_legis_partido,
           media_index)

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


  # lastly, restrict to certain parties. But we allow for parties to be chosen
  # with elypsis (...) and as a character vecor

  my_parties <- rlang::enexprs(...) #defuse the input
  if(length(my_parties) != 0){#apply only if values are supplied
    df_temp %<>% dplyr::filter(partido %in% my_parties)
  }

  df_temp %<>%
    dplyr::filter(partido %in% vec_parties)


  if (nrow(df_temp) == 0) {
    p2 = tibble::tibble(partido = 'Not available')
  } else{ # the next section only occurs if there is data

  #----------------------------------------------------------------------------#
  # Calculate the Proximity ####
  #----------------------------------------------------------------------------#

    # create df_unique, a dataset that has all the party combinations
    # It must be ordered from left to right.
    ordered_parties <- df_temp %>%
      dplyr::distinct(partido) %>%
      dplyr::arrange(factor(partido, levels = vec_parties))

    # Create all the combinations (event with themselves)
    df_unique <- tidyr::expand_grid(partido = ordered_parties$partido,
                                    partido2 = ordered_parties$partido)

    # This is the dataset I want to fill to them feed to ggplot.

    # Nest, in each bill, a vector that is the proximity combination of each party combination.
    # It's the lower triangular part of a prox matrix, from top to bottom, left to right.
    # It must be a nested tibble, cannot be a list, because of the boot function.
    df_nest_bill <- df_temp %>%
      prox_by_bill()

    if (weight_by_salience == F) {
      # Calculate the party proximity
      perc_match_vec <- df_nest_bill %>%
        boot_prox()
    }else{
      # Calculate the party proximity
      perc_match_vec <- df_nest_bill %>%
        left_join(df_temp %>%
                    dplyr::distinct(id_vot,
                           media_index),
                  by = "id_vot") %>%
        boot_prox(type = "salience",
                  vec_salience = media_index)
    }



    # Calculate the amount of votes
    n_votes_vec <- df_nest_bill %>%
      boot_prox(type = "length")

    # I must now stretch these vectors to go from the lower triangular part of the matrix
    # to the full matrix. The proximity matrix is symmetric

    # The diagonal will get 1's automatically
    df_unique$perc_match <- perc_match_vec %>% stretch_for_df_unique()

    # For the quantity of bills, I must choose the diagonal for n_botes
    n_for_diag <- df_temp %>%
      dplyr::count(partido) %>%
      dplyr::arrange(factor(partido, levels = vec_parties)) %>%
      dplyr::pull(n)

    df_unique$n_votes <- n_votes_vec %>% stretch_for_df_unique(diag_to_insert = n_for_diag)

    # All the parties that never meet get NA's
    df_unique[df_unique == "NaN"] <- NA

    # turn the proximity combos below min_n_votes into NA
    df_unique %<>%
      dplyr::mutate(perc_match = dplyr::case_when(
        n_votes < min_n_votes ~ NA,
        TRUE ~ perc_match
      ))

    if (inference == TRUE) {

      #-----------#
      # Inference #
      #-----------#

      # Inference using bootstrap at the level of each bill
      set.seed(seed_for_boot)
      res <- boot::boot(data = df_nest_bill,
                        statistic = boot_prox,
                        R = n_boot,
                        stype = "i",
                        sim = "ordinary", # I must explore what this means
                        na_sub = TRUE # option of the boot_prox function
      )

      # separate the -1 from the rest
      na_vec <- rep(NA, length(res$t0))

      # positions of non -1
      pos <- which(res$t0 != -1)

      # The indexes of the ones that don't have a -1
      list_ci <- pos %>%
        purrr::map(~{
          a <- boot::boot.ci(res, type = "bca", index = .x)
          a$bca[4:5] #return
        })

      ci_vec <- list_ci %>% unlist()
      # get the upper and lower thresholds of the CI
      low_ci_vec <- ci_vec[seq(1, length(list_ci)*2, by = 2)]
      high_ci_vec <- ci_vec[seq(2, length(list_ci)*2, by = 2)]

      # add the -1's
      na_vec[pos] <- low_ci_vec
      low_ci_vec <- na_vec

      na_vec[pos] <- high_ci_vec
      high_ci_vec <- na_vec

      # add to df_unique
      df_unique$low_ci <- low_ci_vec %>% stretch_for_df_unique()
      df_unique$high_ci <- high_ci_vec %>% stretch_for_df_unique()

      # in percentages
      df_unique %<>%
        dplyr::mutate(low_ci = (low_ci*100) %>% round(2),
                      high_ci = (high_ci*100) %>% round(2))


    }

    # Final toutches

    # Change the names of some parties
    df_unique %<>%
      dplyr::mutate(dplyr::across(c(partido, partido2),
                                  ~{
        dplyr::case_when( #all the parties that led to BE:
          .x %in% c("MDP/CDE") ~ "MDP",
          TRUE ~ .x
        )}))


    p2 <- df_unique %>%
      dplyr::mutate(perc_match = (perc_match*100) %>% round(2)

      ) %>%
      dplyr::filter(partido != "Independente" & partido2 != "Independente") %>%
      dplyr::mutate(partido = partido %>% forcats::fct_relevel(ordered_parties$partido),
                    partido2 = partido2 %>% forcats::fct_relevel(rev(ordered_parties$partido)))



  }
  }

 if (plot == T) {

   if (inference == T) {
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
                                                  "Min. conf. interval: ", low_ci, '%',  '<br>',
                                                  "Max. conf. interval: ", high_ci, '%',  '<br>')
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
   }else{
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
                                                  'N. votes: ', n_votes, "<br>")
       )
       ) +
       ggplot2::geom_tile(color = "white",
                          linetype = 1,
                          lwd = 1) +
       ggplot2::coord_fixed() + #fix as a square
       ggplot2::scale_fill_gradientn(colours = RColorBrewer::brewer.pal(4,"BrBG"),
                                     na.value = "grey20") +
       ggplot2::geom_text(ggplot2::aes(label = paste0(round(perc_match), "%")),
                          color = "white",
                          size = ggplot2::rel(3.5),
                          fontface = 'bold') +
       ggplot2::scale_x_discrete(limits = NULL, expand = ggplot2::expansion(add = 0, mult = c(.01, .01))) +
       ggplot2::scale_y_discrete(limits = NULL, expand = ggplot2::expansion(add = 0, mult = c(.01, .01))) +
       ggplot2::theme_minimal() +
       ggplot2::theme(legend.position = "none",
                      axis.title = ggplot2::element_blank(),
                      axis.text = ggplot2::element_text(size = ggplot2::rel(1))) #Trying to reduce distance from y axis to graph

   }



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
