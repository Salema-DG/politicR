

order_party <- function(data,
                        name) {

  #-------------------------------#
  # the order, from left to right #
  #-------------------------------#

  #create the vector
  order_all_parties <-
    c("CDU", "PCP", "PEV", "ID", "MDP/CDE",
      "MDP", "BE", "UDP", "L", "PSN", "PAN",
      "UEDS", "PS", "ASDI", "PRD", "PSD",
      "IL", "CDS", "CDS+", "PPM", "CH")

  # apply tha order
  data %<>%
    mutate("{name}" := {{ name }} %>% forcats::fct_relevel(order_all_parties))

  return(data)

}

