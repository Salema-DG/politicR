
#' @title Count the number of News that talk about a bill.
#'
#' @description
#' This function takes the entities in the title of bills and counts how many
#' times they appear in the news.
#' The function returns both an extensive margin and an intensive plus extensive margin columns.
#' extensive margin: does a certain entity appear in a bill title?
#' extensive and intensive margin: how many times does a certain entity appear in a bill title?
#' The function can parallelize the procedure.
#' But the number of cores must be set outside using
#' plan(multisession, workers = x) and with the furrr package loaded.
#'
#' @param data Tibble with entities in bills and their dates
#' @param var_entity Name of the variable with the entities. Not a string.
#' @param var_date Name of the variable with the dates. Not a string.
#' @param data_media Data with the media news
#' @param var_news Name of the variable of the news to use. It can refer to texts, titles, subtitles, etc. Not a string.
#' @param days_treshold How many days should be given as an interval. The dates of the news are noisy, thus this should be high.
#'
#' @export
#' @importFrom lubridate %within%
#'
#' @return data with 2 columns, one for extensive and other for extensive plus intensive margin.
#'
#' @examples
#' # df_gpt %>%
#' #   media_bill_counts(entity,
#' #                     date,
#' #                     df_desarquivo,
#' #                     text)
#'
#'

media_bill_count = function(data,
                            var_entity,
                            var_date,
                            data_media,
                            var_news,
                            days_treshold = 365){

  list_match <- data %>%
    dplyr::pull({{var_entity}}) %>% # vector with the entities

    furrr::future_map2( (data %>% dplyr::pull({{var_date}})), # vector with the dates
                        ~{

                          df_temp <- data_media %>%
                            dplyr::filter(.y %within%
                                            interval(lubridate::ymd(date) - days_treshold,
                                                     lubridate::ymd(date) + days_treshold) == T)

                          # extensive margin: does a certain entity appear in a bill title?
                          n_ext <- .x %>%
                            # dividir .x em um vetor
                            purrr::map(function(entity){

                              stringr::str_detect((df_temp %>% dplyr::pull({{var_news}})),
                                                  entity) %>%
                                sum()

                            }) %>%
                            unlist() %>%
                            sum()

                          # extensive and intensive margin: how many times does a certain entity appear in a bill title?
                          n_ext_int <- .x %>%
                            # dividir .x em um vetor
                            purrr::map(function(entity){

                              stringr::str_count((df_temp %>% dplyr::pull({{var_news}})),
                                                 entity) %>%
                                sum()

                            }) %>%
                            unlist() %>%
                            sum()

                          c(n_ext, n_ext_int)

                        },
                        .progress = TRUE)

  # add the new name
  data %<>% dplyr::mutate(ext_margin = list_match %>% purrr::map(`[[`, 1) %>% unlist(),
                          ext_int_margin = list_match %>% purrr::map(`[[`, 2) %>% unlist())

  return(data)
}




