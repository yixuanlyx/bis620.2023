

#' @title Print "hello world"
#' @description Print the hello world function to the console with
#' parameter way who to way hello to.
#' @param who Who to say hello to. (Default is "world")
#' @examples
#' hello("people in BIS620")
#' @export


hello <- function(who = "world") {
  sprintf("Hello, %s!", who)
}
