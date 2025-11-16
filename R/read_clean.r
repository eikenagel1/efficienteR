#' Clean + read CSV file
#' @param path Path to CSV
#' @return A cleaned tibble
#' @export
read_clean <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}
