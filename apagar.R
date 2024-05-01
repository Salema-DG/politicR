

library(devtools)
library(tidyverse)
#library(lubridate)
load_all()

df <- readRDS("C:/Users/msacc/Dropbox/R-direct/Project_data_AR/01_Dados/1_data/2_final_data/df_1st_vote.rds")
df_gpt <- readRDS("C:/Users/msacc/Dropbox/R-direct/Project_data_AR/14_Arquivo/01_data/df_gpt.rds")
desarquivo <- read_csv("C:/Users/msacc/Dropbox/R-direct/Project_data_AR/01_Dados/1_data/2_final_data/desarquivo.final_news.csv")

# only select required columns
desarquivo %<>%
  mutate(date = as_date(timestamp)) %>%
  select('id' = `_id`,
         title,
         text,
         date,
         url,
         website)


df_gpt %<>%
  inner_join(df %>%
               select(id_ini,
                      data) %>%
               distinct(),
             by = join_by('id_ini')) %>%
  rename('date' = data)

df_gpt %>%
  filter(year(date) == 2016) %>%
  slice(1:10)




load_all()



temp <- df_gpt %>%
  filter(year(date) == 2016) %>%
  media_bill_count(var_entity = entities,
                   var_date = date,
                   data_media = desarquivo,
                   var_media_date = date,
                   var_news = text,
                   var_news_title = title,
                   var_news_id = id,
                   days_treshold = 250)

temp2 <- desarquivo %>%
  dplyr::filter( (as.Date("2016-01-15") %within%
                    interval( (lubridate::ymd(date) - 200),
                              (lubridate::ymd(date) + 200) )) == T)
















