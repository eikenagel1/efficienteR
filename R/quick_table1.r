#' Quick Table 1 (one group)
#' @description Descriptive stats for a set of variables (no grouping).
#'   Numeric: n, missing, mean, sd, median, IQR, min, max
#'   Categorical: level counts and percents.
#' @param df Data frame
#' @param ... Columns to summarize (unquoted). If none, all columns.
#' @param digits Rounding for numeric summaries
#' @return Tibble
#' @export
quick_table1 <- function(df, ..., digits = 2) {
  cols <- rlang::quos(...)
  dat  <- if (length(cols)) dplyr::select(df, !!!cols) else df

  num_vars <- names(dat)[vapply(dat, is.numeric, logical(1))]
  cat_vars <- setdiff(names(dat), num_vars)

  num_tbl <- NULL
  if (length(num_vars)) {
    num_tbl <- purrr::map_dfr(num_vars, function(v) {
      x <- dat[[v]]
      tibble::tibble(
        variable = v,
        type = "numeric",
        n = sum(!is.na(x)),
        missing = sum(is.na(x)),
        mean = round(mean(x, na.rm = TRUE), digits),
        sd = round(stats::sd(x, na.rm = TRUE), digits),
        median = round(stats::median(x, na.rm = TRUE), digits),
        iqr = round(stats::IQR(x, na.rm = TRUE), digits),
        min = round(min(x, na.rm = TRUE), digits),
        max = round(max(x, na.rm = TRUE), digits)
      )
    })
  }

  cat_tbl <- NULL
  if (length(cat_vars)) {
    cat_tbl <- purrr::map_dfr(cat_vars, function(v) {
      x <- dat[[v]]
      x <- forcats::fct_explicit_na(as.factor(x), na_level = "(Missing)")
      tab <- as.data.frame(prop.table(table(x)) * 100)
      names(tab) <- c("level", "percent")
      tab$count <- as.integer(table(x)[tab$level])
      tab$variable <- v
      tab$type <- "categorical"
      dplyr::relocate(tab, variable, type)
    }) |>
      dplyr::mutate(percent = round(percent, digits))
  }

  dplyr::bind_rows(num_tbl, cat_tbl) |>
    dplyr::arrange(variable, dplyr::desc(type))
}
