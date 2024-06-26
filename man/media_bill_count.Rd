% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/media_bill_count.R
\name{media_bill_count}
\alias{media_bill_count}
\title{Match bills to Online News Articles}
\usage{
media_bill_count(
  data,
  var_entity,
  var_date,
  data_media,
  var_media_date,
  var_news,
  var_news_title,
  var_news_id,
  days_treshold = 365,
  ratio_entity_presence = 0.5
)
}
\arguments{
\item{data}{Tibble with entities in bills and their dates}

\item{var_entity}{Name of the variable with the entities. Not a string.}

\item{var_date}{Name of the variable with the dates. Not a string.}

\item{data_media}{Data with the media news}

\item{var_media_date}{Name of the variable with the news time stamp or date.}

\item{var_news}{Name of the variable of the news to use. It can refer to texts, titles, subtitles, etc. Not a string.}

\item{var_news_title}{Name of the variable with the news titles}

\item{var_news_id}{A unique ID identifying the news article saved by Arquivo.pt}

\item{days_treshold}{How many days should be given as an interval. The dates of the news are noisy, thus this should be high.}

\item{ratio_entity_presence}{The "asymptote" of the arctan function. How many (in share) of the bill's entities must be in the same article to be considered a match.}
}
\value{
data with 6 columns. "ext_margin" gives the number of news matched. "ext_int_margin" gives the extensive plus intensive margin. title gives the number of news that mention some entity in the title. match_id_news and match_id_title give a vector with the IDs of the matched news. n_time_eligible gives the number of news that could have been matched.
}
\description{
This function takes the entities in the proposed title of bills and
matches them with online news articles.
For a news article to be classified as a match it needs to contain a y
percentage of the entities mentioned in the bill title.
That y is calculated with a arctan function, to assure that the more
entities a bill contains, the bigger the necessary percentage.

Moreover, the function returns both an extensive margin and an intensive plus extensive margin columns.
The extensive margin covers if a news article covered the bill.
The extensive plus intensive margin covers how many times are all entities mentioned in all eligible articles.

This function can parallelize the procedure.
But the number of cores must be set outside using
plan(multisession, workers = x) and with the furrr package loaded.
}
\examples{
# df_gpt \%>\%
# filter(year(date) == 2016) \%>\%
#   media_bill_count(var_entity = entities,
#                    var_date = date,
#                    data_media = desarquivo,
#                    var_media_date = date,
#                    var_news = text,
#                    var_news_title = title,
#                    var_news_id = id,
#                    days_treshold = 250)
ratio <- 0.5
atan(1:15/2)/pi*2*0.5

}
