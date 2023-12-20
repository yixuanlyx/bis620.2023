#' This function does exploratory analysis of the dataset
#' A table demonstrating the demographic information is created
#' @param dl a list of tables of the Panitumumab dataset
#' @return a summary table of the demographic information, including age, sex, weight, KRAS type
#' @importFrom table1 table1
#' @export

summary_table <- function(dl){
  table1(~ .-SUBJID-DTH-DTHDY | ATRT, data=dl)
}
