#' This function does data processing.
#' @param dl an object inherited from list.
#' @return full dataset.
#' @importFrom dplyr left_join select filter
#' @export

data_process <- function(dl) {
  demo <- dl$adsl |> select(SUBJID, DTHDY, DTH,ATRT, AGE, SEX, B_WEIGHT, DIAGTYPE) |>
    filter(DIAGTYPE == "Colon")
  demo$SEX <- factor(demo$SEX, levels = unique(demo$SEX), exclude = NULL)
  demo$ATRT <- factor(demo$ATRT, levels = unique(demo$ATRT),exclude = NULL)
  bio <- dl$biomark |>
    select(SUBJID,BMMTNM1,BMMTR1,BMMTNM2,BMMTR2,BMMTNM3,BMMTR3,BMMTNM15,BMMTR15 )
  bio <- replace(bio, bio == "", "Unknown")
  demo1 <- left_join(demo, bio, by = "SUBJID")
  demo1$marker <- apply(demo1[, c("BMMTR1", "BMMTR2", "BMMTR3", "BMMTR15")], 1, function(row) {
    if ("Mutant" %in% row) {
      "Mutant"
    } else if (sum(row == "Wild-type") > sum(row %in% c("Unknown", "Failure"))) {
      "Wild-type"
    } else {
      "Unknown"
    }
  })
  ad <- dl$adae |> select(SUBJID, AESEVCD) |> group_by(SUBJID) |>
    summarize(AESEVCD = max(AESEVCD))
  demo2 <- left_join(demo1, ad, by = "SUBJID")
  demo2$AESEVCD <- ifelse(is.na(demo2$AESEVCD), 'No Event', demo2$AESEVCD)
  demo2$AESEVCD <- factor(demo2$AESEVCD, levels = c('No Event', '1','2','3','4'),  exclude = NULL)
  final <- demo2 |> select(SUBJID, DTHDY, DTH,ATRT, AGE, SEX, B_WEIGHT, marker, AESEVCD)
  final$marker <- factor(final$marker, levels = c('Unknown', 'Mutant','Wild-type'),  exclude = NULL)


}
