#' Quick paired visual for two timepoints per subject
#' @description Draws lines connecting subject values across exactly two timepoints.
#' @param df Data frame
#' @param id Subject identifier (unquoted)
#' @param time Timepoint column (unquoted). Should have exactly 2 levels.
#' @param value Numeric column (unquoted)
#' @param color Optional grouping for color (unquoted), default NULL
#' @param alpha Line transparency
#' @return ggplot
#' @export
quick_visual_2_timepoints <- function(df, id, time, value, color = NULL, alpha = 0.4) {
  idsym   <- rlang::ensym(id)
  timesym <- rlang::ensym(time)
  valsym  <- rlang::ensym(value)

  pdat <- df |>
    dplyr::select(!!idsym, !!timesym, !!valsym, {{ color }}) |>
    dplyr::filter(!is.na(!!valsym)) |>
    dplyr::mutate(
      !!timesym := forcats::as_factor(!!timesym),
      !!timesym := forcats::fct_inorder(!!timesym)
    )

  if (length(levels(pdat[[rlang::as_name(timesym)]])) != 2) {
    stop("`time` must have exactly two levels in the plotting data.")
  }

  aes_base <- ggplot2::aes(x = !!timesym, y = !!valsym, group = !!idsym)
  aes_col  <- if (rlang::quo_is_null(rlang::enquo(color))) aes_base else
    modifyList(aes_base, ggplot2::aes(color = {{ color }}))

  ggplot2::ggplot(pdat, aes_col) +
    ggplot2::geom_line(alpha = alpha) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = NULL, y = rlang::as_name(valsym)) +
    ggplot2::theme_minimal()
}
