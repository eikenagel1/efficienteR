#' Quick CSV/TSV reader
#'
#' Read delimited files with sensible defaults and return a tibble.
#'
#' @param path Character scalar or vector of file paths.
#' @param delim Delimiter; inferred from extension when NULL.
#' @param col_types Optional readr-style column spec.
#' @param na Character vector of strings to interpret as NA.
#' @param guess_max Max lines to inspect for type guessing.
#' @param ... Passed through to vroom::vroom().
#' @return A tibble (data frame).
#' @examples
#' quick_read_csv(here::here("data-raw","example.csv"))
#' @export
quick_read_csv <- function(path, delim = NULL, col_types = NULL, na = c("", "NA"), guess_max = 10000, ...) {
  stopifnot(is.character(path))
  if (length(path) == 1L && !file.exists(path)) {
    stop("File not found: ", path)
  }
  if (is.null(delim)) {
    ext <- tolower(tools::file_ext(path[1]))
    delim <- if (ext %in% c("tsv", "tab")) "\t" else ","
  }
  vroom::vroom(
    file = path,
    delim = delim,
    col_types = col_types,
    na = na,
    guess_max = guess_max,
    progress = interactive(),
    ...
  )
}
