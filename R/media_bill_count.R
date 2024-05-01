
#' @title Match bills to Online News Articles
#'
#' @description
#' This function takes the entities in the proposed title of bills and
#' matches them with online news articles.
#' For a news article to be classified as a match it needs to contain a y
#' percentage of the entities mentioned in the bill title.
#' That y is calculated with a arctan function, to assure that the more
#' entities a bill contains, the bigger the necessary percentage.
#'
#' Moreover, the function returns both an extensive margin and an intensive plus extensive margin columns.
#' The extensive margin covers if a news article covered the bill.
#' The extensive plus intensive margin covers how many times are all entities mentioned in all eligible articles.
#'
#' This function can parallelize the procedure.
#' But the number of cores must be set outside using
#' plan(multisession, workers = x) and with the furrr package loaded.
#'
#' @param data Tibble with entities in bills and their dates
#' @param var_entity Name of the variable with the entities. Not a string.
#' @param var_date Name of the variable with the dates. Not a string.
#' @param data_media Data with the media news
#' @param var_news Name of the variable of the news to use. It can refer to texts, titles, subtitles, etc. Not a string.
#' @param days_treshold How many days should be given as an interval. The dates of the news are noisy, thus this should be high.
#' @param var_news_id A unique ID identifying the news article saved by Arquivo.pt
#' @param var_news_title Name of the variable with the news titles
#' @param var_media_date Name of the variable with the news time stamp or date.
#' @param ratio_entity_presence The "asymptote" of the arctan function. How many (in share) of the bill's entities must be in the same article to be considered a match.
#'
#' @export
#' @importFrom lubridate %within%
#'
#' @return data with 6 columns. "ext_margin" gives the number of news matched. "ext_int_margin" gives the extensive plus intensive margin. title gives the number of news that mention some entity in the title. match_id_news and match_id_title give a vector with the IDs of the matched news. n_time_eligible gives the number of news that could have been matched.
#'
#' @examples
#' # df_gpt %>%
#' # filter(year(date) == 2016) %>%
#' #   media_bill_count(var_entity = entities,
#' #                    var_date = date,
#' #                    data_media = desarquivo,
#' #                    var_media_date = date,
#' #                    var_news = text,
#' #                    var_news_title = title,
#' #                    var_news_id = id,
#' #                    days_treshold = 250)
#' ratio <- 0.5
#' atan(1:15/2)/pi*2*0.5
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
                            ratio_entity_presence = 0.5){


  # Start the loop for each bill's vector of entities
  list_match <- data %>%
    dplyr::pull({{var_entity}}) %>% # Each element has several entities

    furrr::future_map2( (data %>% dplyr::pull({{var_date}})), # vector of dates
                        ~{
                          # restrict the news to match to an intreval
                          df_temp <- data_media %>%
                            dplyr::filter(
                              (lubridate::ymd(.y) %within%
                                 lubridate::interval((lubridate::ymd({{var_media_date}}) - days_treshold),
                                                     (lubridate::ymd({{var_media_date}}) + days_treshold)))
                              == T)

                          if (nrow(df_temp) == 0) {
                            list(c(NA, NA, NA, 0),
                                 NA,
                                 NA)
                          } else{


                          # keep the eligible news' texts and titles in a vector
                          vec_news <- df_temp %>% pull({{var_news}})
                          vec_news_title <- df_temp %>% pull({{var_news_title}})

                          # number of eligible news
                          n_eligible <- length(vec_news)

                          # For each entity inside .x:
                          # Does it appear in a given news?
                          list_entity_presence <- .x %>%
                            # dividir .x em um vetor
                            purrr::map(function(entity){

                              # order the
                              stringr::str_detect(vec_news,
                                                  entity)

                            })

                          # the number of vectors in the list must be
                          # equal to the number of entities in that bill
                          stopifnot(length(list_entity_presence) == length(.x))

                          # now, we must count how many entities does
                          # each news article contain
                          vec_n_entity_in_news <- list_entity_presence %>%
                            as.data.frame(col.names = paste0("entity_", 1:length(.x))) %>%
                            t() %>%
                            tibble::as_tibble(.name_repair = "minimal") %>%
                            purrr::map(sum) %>%
                            unlist()

                          # for a certain news article to be considered,
                          # i require a minimum proportion.
                          # That proportion should increase with the number of entities
                          # Use the inverse of the tangent function:
                          t <- atan(length(.x)/2)/pi*2*ratio_entity_presence

                          # check if the number of entities in a given news article
                          # is above the required threshold:
                          vec_use_news <- vec_n_entity_in_news/length(.x) > t

                          # Keep only the news articles that fulfill
                          # the minimum requisite
                          vec_news <- vec_news[vec_use_news]
                          vec_news_title <- vec_news_title[vec_use_news]

                          # the ids of the news used
                          vec_news_id <- df_temp %>% dplyr::pull({{var_news_id}})
                          vec_news_id <- vec_news_id[vec_use_news]

                          # Now, calculate the number of matches
                          # given the already selectedc laws

                          # extensive margin: does the law appear?
                          n_ext <- length(vec_news_id)


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

                          # The ones mentioned in the title
                          list_title_presence <- .x %>%
                            purrr::map(function(entity) {

                              stringr::str_detect(vec_news_title,
                                                  entity)
                            }) %>%
                            as.data.frame(col.names = paste0("entity_", 1:length(.x))) %>%
                            t() %>%
                            tibble::as_tibble(.name_repair = "minimal") %>%
                            purrr::map(sum) %>%
                            unlist()

                          vec_keep_title <- list_title_presence > 0
                          n_title_ext <- sum(vec_keep_title)

                          vec_title_id <- vec_news_id[vec_keep_title]


                          # Return a list
                          list(c(n_ext, n_ext_int, n_title_ext, n_eligible),
                               vec_news_id,
                               vec_title_id)
                          }

                        },
                        .progress = TRUE)

  # add the new name
  data %<>% dplyr::mutate(ext_margin = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 1) %>% unlist(),
                          ext_int_margin = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 2) %>% unlist(),
                          title = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 3) %>% unlist(),
                          match_id_news = list_match %>% purrr::map(`[[`, 2),
                          match_id_title = list_match %>% purrr::map(`[[`, 3),
                          n_time_eligible = list_match %>% purrr::map(`[[`, 1) %>% purrr::map(`[[`, 4) %>% unlist())


  return(data)
}




