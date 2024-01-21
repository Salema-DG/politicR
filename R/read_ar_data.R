
#' @title Read AR Data
#'
#' @description
#' Read cleaned data with party votes from the Portuguese parliament.
#'
#' @param name Name of the dataset to load. A string.
#' @param path Path to the folder "Project_data_AR", if the function is not ran within the project.
#'
#' @return A dataset or a list of available datasets, if "name" is left in blank.
#'
#' @export
#'
#' @examples
#' # to list available data
#' #read_ar_data()
#' read_ar_data(path = "../Project_data_AR")
#'
#' # to load data
#' # df <- read_ar_data("df_1st_vote")
#'
#'
read_ar_data <- function(name = NULL,
                         path = NULL) {



  if (is.null(path)) {
    path <- "01_Dados/1_data/2_final_data"
  } else{
    path <- paste0(path, "/", "01_Dados/1_data/2_final_data")
  }

  if (is.null(name)) {
    return(list.files(path))
  } else{
    return(readRDS(paste0(path, "/", name, ".rds")))
  }
}




