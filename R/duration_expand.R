
#' @title Expand the dates to daily data
#'
#' @description
#' This file...
#'
#'
#' @param group Group for which to expand
#'
#'
#'
#'

duration_expand <- function(data,
                            start,
                            end,
                            group) {

  # by turning into numeric, it turns into days since jan 1 1970
  data %<>%
    dplyr::mutate(dur_inicial = {{start}} %>% as.numeric(),
           dur_final = {{end}} %>% as.numeric())

  #Expand the data to contain every day for every party
  df_expanded <- seq(min(data$dur_inicial),
                     max(data$dur_final),
                     by = 1) %>%
    expand.grid(unique(data %>% dplyr::pull({{group}}))) %>%
    dplyr::rename(duration = Var1,
           {{group}} := Var2)

  # join by party and match the number in df_expanded to the time frame in df
  by <- dplyr::join_by(partido, dplyr::between(duration, dur_inicial, dur_final,  bounds = "[]"))

  df1 <- df_expanded %>%
    dplyr::left_join(data,
              by = by)

  # set the percentages and deputies of when they were not elected as 0
  df1[is.na(df1)] <- 0

  df1 %<>%
    dplyr::mutate(day = duration %>% as.Date(origin = "1970/01/01"))

  return(df1)

}
