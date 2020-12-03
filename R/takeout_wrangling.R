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

#' Clean up Location History data
#'
#' Rename columns, convert latitude and longitude notation.
#'
#' @param df Location history dataframe as output from json_to_tibble()
#'
#' @return Tibble
#'
#' @export

clean_locations <- function(df) {

  # df typically has 4 columns, but only 3 are critical

  if (ncol(df) < 4) {
    warning("Location history dataframe has fewer columns than expected.")
  }

  miss_col <- ""

  if (!any("locations.timestampMs" == colnames(df))) {
    miss_col <- "timestamp"
  }
  if (!any("locations.latitudeE7" == colnames(df))) {
    miss_col <- "latitude"
  }
  if (!any("locations.longitudeE7" == colnames(df))) {
    miss_col <- "longitude"
  }

  if(miss_col != "") {
    stop(paste0("Location history dataframe has missing column: '",
                miss_col, "'." ))
  }

  df_renamed <- dplyr::rename(df,
                              "timestamp" = "locations.timestampMs",
                              "latitude" = "locations.latitudeE7",
                              "longitude" = "locations.longitudeE7",
                              "accuracy" = "locations.accuracy")

  df_scaled <- stats::na.omit(df_renamed)

  df_scaled$timestamp <- as.numeric(df_scaled$timestamp) * 10^-3
  df_scaled$timestamp <-
    as.POSIXct(as.numeric(as.character(df_scaled$timestamp)),
               origin = "1970-01-01")
  df_scaled$latitude <- df_scaled$latitude * 10^-7
  df_scaled$longitude <- df_scaled$longitude * 10^-7

  df_scaled

}
