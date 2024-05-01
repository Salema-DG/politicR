
#' @title Calculate p-values from a lm()
#'
#' @description
#' This function calculates the p-values from a lm() object.
#' The function can extract the p-values but also allows
#' for the correction of those p-values, given any multiple testing method
#' available in the function p-adjust.
#' Type ?p.adjust.methods for an overvierw.
#'
#' @param lm_object An estimated lm() object
#' @param correction_method String. A correction method to pass to p.adjust()
#'
#' @return A named vector with the adjusted p-values.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' ## load an example dataset
#' #data(mtcars)
#'#
#' ## estimate a linear model
#' #m <- lm(mpg ~ as.factor(cyl) + hp + wt + am, data = mtcars)
#'#
#' ## extract the non-corrected p-values
#' #p_values(m)
#'#
#' ## extract p-values corrected with the Holm metghod.
#' #m |> p_values(correction = "holm")
#'

p_values <- function(lm_object,
                     correction = NULL,
                     rounded_to = 5) {

  #extract the p-values.
  # They are stored in summary(lm)
  p <- summary(lm_object)$coefficients[, 4]

  if (is.null(correction)) {
    return(p)
  } else{
    # adjust the p-value
    adj_p <- p %>%
      stats::p.adjust(correction) %>%
      round(rounded_to)

    return(adj_p)
  }

}


