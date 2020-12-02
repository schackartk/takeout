#' Convert (potentially) nested .json to tibble
#'
#' Read in a .json file, unnest and flatten it, and return as a tibble.
#'
#' @param f Name of a .json file (string).
#'
#' @return Tibble
#'
#' @export

json_to_tibble <- function(f) {
  if (!file.exists(f)) {
    stop(paste0("'", f,"' does not exist in current working directory ('",
                getwd(),"')."))
  }

  raw_json <- jsonlite::fromJSON(f)
  raw_tibble <- tibble::as_tibble(raw_json)
  flat_json <- jsonlite::flatten(raw_tibble)
  tibble::as_tibble(flat_json)
}
