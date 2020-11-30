#' Convert (potentially) nested .json to tibble
#'
#' @param f File name (.json)
#'
#' @return Flattened tibble

json_to_tibble <- function(f) {
  library(magrittr)

  if (!file.exists(f)) {
    stop("File does not exist.")
  }

  json_tibble <- jsonlite::fromJSON(f) %>%
    tibble::as_tibble() %>%
    jsonlite::flatten() %>%
    tibble::as_tibble()

  return(json_tibble)
}
