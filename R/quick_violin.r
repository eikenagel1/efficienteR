#' Quick violin with median + IQR
#' @param df A data frame
#' @param x Grouping column (unquoted)
#' @param y Numeric column (unquoted)
#' @export
quick_violin <- function(df, x, y) {
  ggplot2::ggplot(df, ggplot2::aes({{ x }}, {{ y }})) +
    ggplot2::geom_violin(trim = FALSE) +
    ggplot2::stat_summary(fun = stats::median, geom = "point", size = 2) +
    ggplot2::stat_summary(fun.data = ~ data.frame(y = stats::median(.),
                                                  ymin = stats::quantile(., 0.25, na.rm = TRUE),
                                                  ymax = stats::quantile(., 0.75, na.rm = TRUE)),
                          geom = "errorbar", width = 0.15)
}
