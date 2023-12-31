library(dplyr)
library(duckdb)
library(tidyr)
library(purrr)
library(DBI)
library(DT)
library(ggplot2)
library(ctrialsgov)

# Create the connection to a database and "studies" and "sponsors" tables.
con <- dbConnect(
  duckdb(
    file.path("..", "..", "..", "ctgov.duckdb"),
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
ctgov_load_duckdb_file(file.path("..", "..", "..", "ctgov-derived.duckdb"))


studies <- tbl(con, "studies")
sponsors <- tbl(con, "sponsors")
conditions <- tbl(con, "conditions")


cond <- conditions %>% select(nct_id, name)
spons <- sponsors %>% select(nct_id, lead_or_collaborator, name)
studies <- left_join(left_join(studies, cond, by = "nct_id"),
                     spons, by = "nct_id")



#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds <- kwds[kwds != ""]
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else {
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram <- function(x) {
  x$phase[is.na(x$phase)] <- "NA"
  x <- x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  ggplot(x, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials <- function(d) {
  # Get all of the unique dates.
  all_dates <- d |>
    pivot_longer(cols = everything()) |> # |>select(-name)
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  within_date <- function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count <-
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}


plot_concurrent_studies <- function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}

# Create a histogram of the conditions returned by a brief title keyword search
# @param x the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.
plot_condition_histogram <- function(x) {
  x$name.x[is.na(x$name.x)] <- "NA"
  x <- x |>
    select(name.x) |>
    group_by(name.x) |>
    summarize(n = n()) %>%
    arrange(desc(n))

  ggplot(x %>% head(30), aes(x = name.x, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Top 30 Conditions") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
