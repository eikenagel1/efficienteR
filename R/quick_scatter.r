#' Quick scatter with identity line
#' @param df A data frame
#' @param x,y Columns (unquoted)
#' @param alpha Point transparency
#' @param add_lm Add a linear fit line?
#' @export
quick_scatter <- function(df, x, y, alpha = 0.6, add_lm = TRUE) {
  p <- ggplot2::ggplot(df, ggplot2::aes({{ x }}, {{ y }})) +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::geom_abline(linetype = 2)
  if (isTRUE(add_lm)) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)
  }
  p + ggplot2::labs(x = rlang::as_name(rlang::ensym(x)),
                    y = rlang::as_name(rlang::ensym(y)))
}
