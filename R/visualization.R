#' Plot number of sex
#'
#' This function Create a plot of number of total females and males
#' @param final an object inherited from list. It is
#' assumed to have a `adsl` list name
#' @return a plot of number of females and males.
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_col geom_text theme_bw xlab ylab
#' facet_wrap theme element_text cut_width
#' @importFrom dplyr inner_join group_by summarize count
#' @importFrom gridExtra grid.arrange
#' @export

plot <- function(final){
  summary_sex <- final |> select(SUBJID,ATRT, SEX) |>
    group_by(ATRT) |> summarize(
      Female = sum(SEX=="Female"),
      Male = sum(SEX == "Male"))
  plot_sex <- summary_sex |>
    pivot_longer(-ATRT) |> ggplot(aes(x = name, y = value))+
    geom_col() +
    theme_bw() +
    facet_wrap("ATRT")+
    xlab("Sex") + ylab("count")
  summary_age <- final |> group_by(ATRT) |> count(cut_width(AGE, 10))
  plot_age <- summary_age |> ggplot(aes(x = `cut_width(AGE, 10)`, y = n))+
    geom_col() +
    theme_bw() +
    facet_wrap("ATRT")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Age") + ylab("count")
  summary_weight <- final |> group_by(ATRT) |> count(cut_width(B_WEIGHT, 20))
  plot_weight <- summary_weight |> ggplot(aes(x = `cut_width(B_WEIGHT, 20)`, y = n))+
    geom_col() +
    theme_bw() +
    facet_wrap("ATRT")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Weight") + ylab("count")
  summary_marker <- final |> select(SUBJID,ATRT, marker) |>
    group_by(ATRT) |> summarize(
      Mutant = sum(marker=="Mutant"),
      Wild_type = sum(marker == "Wild-type"),
      Unknown = sum(marker == "Unknown"))
  plot_marker <- summary_marker |>
    pivot_longer(-ATRT) |> ggplot(aes(x = name, y = value))+
    geom_col() +
    theme_bw() +
    facet_wrap("ATRT")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("marker") + ylab("count")
  summary_event <- final |> select(SUBJID,ATRT, AESEVCD) |>
    group_by(ATRT) |> summarize(
      Grade1 = sum(AESEVCD=="1"),
      Grade2 = sum(AESEVCD == "2"),
      Grade3 = sum(AESEVCD == "3"),
      Grade4 = sum(AESEVCD == "4"),
      NoEvent = sum(AESEVCD == "No Event"))
  plot_event <- summary_event |>
    pivot_longer(-ATRT) |> ggplot(aes(x = name, y = value))+
    geom_col() +
    theme_bw() +
    facet_wrap("ATRT")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Adverse event") + ylab("count")
  grid.arrange(plot_sex, plot_age, plot_weight, plot_marker, plot_event, ncol=2)

}
