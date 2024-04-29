
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
#' @param var_news_id A unique ID idintifying the news article saved by Arquivo.pt
#' @param var_news_title Name of the varaible with the news titles
#' @param var_media_date Name of the variable with the news time stamp or date.
#' @param ratio_entity_presence How many (in share) of the bill's entities must be in the same article to be considered a match.
#'
#' @export
#' @importFrom lubridate %within%
#'
#' @return data with 2 columns, one for extensive and other for extensive plus intensive margin. Plus a vector with all the id of the bills mentioned.
#'
#' @examples
#' # df_gpt %>%
#' #   media_bill_counts(entity,
#' #                     date,
#' #                     df_desarquivo,
#' #                     data,
#' #                     text,
#' #                     titulos)
#'
#'

media_bill_count = function(data,
                            var_entity,
                            var_date,
                            data_media,
                            var_media_date,
                            var_news,
                            var_news_title,
                            var_news_id,
                            days_treshold = 365,
                            ratio_entity_presence = 0.9){

  list_match <- data %>%
    dplyr::pull({{var_entity}}) %>% # vector with the entities

    furrr::future_map2( (data %>% dplyr::pull({{var_date}})), # vector with the dates
                        ~{

                          df_temp <- data_media %>%
                            dplyr::filter(.y %within%
                                            interval(lubridate::ymd({{var_media_date}}) - days_treshold,
                                                     lubridate::ymd({{var_media_date}}) + days_treshold) == T)

                          vec_news <- df_temp %>% dplyr::pull({{var_news}})
                          vec_news_title <- df_temp %>% dplyr::pull({{var_news_title}})


                          # Does a certain entity appear in a bill title?
                          list_entity_presence <- .x %>%
                            # dividir .x em um vetor
                            purrr::map(function(entity){

                              # order the
                              stringr::str_detect(vec_news,
                                                  entity)

                            })

                          # how many entities does each article contain
                          vec_n_entity_in_news <- list_entity_presence %>%
                            as.data.frame() %>%
                            t() %>%
                            tibble::as_tibble(.name_repair = "minimal") %>%
                            purrr::map(sum) %>%
                            unlist()

                          # for a certain news article to be considered, we need to make sure that the number of apperances
                          # use the inverse of the tangent function to
                          # have an evolution of the match, a threshold for each bill
                          t <- atan(sum(.x)/2)/pi*2*ratio_entity_presence

                          vec_use_news <- vec_n_entity_in_news/sum(.x) > t

                          # update the news vectors
                          vec_news <- vec_news[vec_use_news]
                          vec_news_title <- vec_news_title[vec_use_news]

                          # the ids of the news used
                          vec_news_id <- df_temp %>% dplyr::pull({{var_news_id}})
                          vec_news_id <- vec_news_id[vec_use_news]

                          # extensive margin: does the law appear?
                          n_ext <- sum(vec_use_news)

                          # extensive and intensive margin: how many times does a certain entity appear in a bill title?
                          n_ext_int <- .x %>%
                            # dividir .x em um vetor
                            purrr::map(function(entity){

                              stringr::str_count(vec_news,
                                                 entity) %>%
                                sum()

                            }) %>%
                            unlist() %>%
                            sum()

                          # of the already selected laws, what are the ones mentioned in the title
                          list_title_presence <- .x %>%
                            purrr::map(function(entity) {

                              stringr::str_detect(vec_news_title,
                                                  entity) %>%
                                sum()
                            }) %>%
                            as.data.frame() %>%
                            t() %>%
                            tibble::as_tibble(.name_repair = "minimal") %>%
                            purrr::map(sum) %>%
                            unlist()

                          n_title_ext <- sum(list_title_presence > 0)

                          list(c(n_ext, n_ext_int, n_title_ext),
                               vec_news_id)


                        },
                        .progress = TRUE)

  # add the new name
  data %<>% dplyr::mutate(ext_margin = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 1) %>% unlist(),
                          ext_int_margin = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 2) %>% unlist(),
                          title = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 3) %>% unlist(),
                          match_id = list_match %>% purrr::map(`[[`, 2))

  return(data)
}




