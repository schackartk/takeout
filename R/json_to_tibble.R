#' Convert (potentially) nested .json to tibble
#'
#' @param f File name (.json)
#'
#' @return Flattened tibble

json_to_tibble <- function(f) {
  if (!file.exists(f)) {
    stop("File does not exist.")
  }

  raw_json <- jsonlite::fromJSON(f)
  raw_tibble <- tibble::as_tibble(raw_json)
  flat_json <- jsonlite::flatten(raw_tibble)
  flat_tibble <- tibble::as_tibble(flat_json)

  return(json_tibble)
}
