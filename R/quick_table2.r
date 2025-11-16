#' Quick Table 2 (two-group comparison)
#' @description Compares variables between two groups with p-values.
#'   Numeric: default Wilcoxon rank-sum (no normality assumption).
#'   Categorical: chi-squared, falls back to Fisher if needed.
#' @param df Data frame
#' @param group Two-level grouping column (unquoted)
#' @param ... Columns to compare (unquoted). If none, all except group.
#' @param method "wilcox" (default) or "t" for numeric tests
#' @param digits Rounding of summaries
#' @return Tibble with group summaries and p-values
#' @export
quick_table2 <- function(df, group, ..., method = "wilcox", digits = 2) {
  gsym <- rlang::ensym(group)
  gvec <- df[[rlang::as_name(gsym)]]
  if (length(unique(stats::na.omit(gvec))) != 2)
    stop("`group` must have exactly two levels (non-missing).")

  cols <- rlang::quos(...)
  dat  <- if (length(cols)) dplyr::select(df, !!gsym, !!!cols) else df
  dat  <- dplyr::rename(dat, .grp = !!gsym) |> dplyr::mutate(.grp = as.factor(.grp))

  num_vars <- names(dat)[vapply(dat, is.numeric, logical(1)) & names(dat) != ".grp"]
  cat_vars <- setdiff(setdiff(names(dat), num_vars), ".grp")

  # numeric
  num_out <- NULL
  if (length(num_vars)) {
    num_out <- purrr::map_dfr(num_vars, function(v) {
      x <- dat[[v]]
      s <- dat |>
        dplyr::group_by(.grp) |>
        dplyr::summarise(
          n = sum(!is.na(.data[[v]])),
          mean = mean(.data[[v]], na.rm = TRUE),
          sd = stats::sd(.data[[v]], na.rm = TRUE),
          median = stats::median(.data[[v]], na.rm = TRUE),
          iqr = stats::IQR(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        )
      s$mean <- round(s$mean, digits); s$sd <- round(s$sd, digits)
      s$median <- round(s$median, digits); s$iqr <- round(s$iqr, digits)

      # test
      if (method == "t") {
        tt <- stats::t.test(dat[[v]] ~ dat$.grp)
        p  <- broom::tidy(tt)$p.value
      } else {
        wt <- stats::wilcox.test(dat[[v]] ~ dat$.grp, exact = FALSE)
        p  <- broom::tidy(wt)$p.value
      }

      tibble::tibble(
        variable = v, type = "numeric",
        group1 = as.character(levels(dat$.grp)[1]),
        group2 = as.character(levels(dat$.grp)[2]),
        n1 = s$n[1], mean1 = s$mean[1], sd1 = s$sd[1], median1 = s$median[1], iqr1 = s$iqr[1],
        n2 = s$n[2], mean2 = s$mean[2], sd2 = s$sd[2], median2 = s$median[2], iqr2 = s$iqr[2],
        p = signif(p, 3)
      )
    })
  }

  # categorical
  cat_out <- NULL
  if (length(cat_vars)) {
    cat_out <- purrr::map_dfr(cat_vars, function(v) {
      x <- dat |>
        dplyr::mutate(var = forcats::fct_explicit_na(as.factor(.data[[v]]), "(Missing)"))
      tab <- xtabs(~ var + .grp, data = x)

      # prefer chi-squared; fall back to Fisher if any expected < 5
      test <- try(stats::chisq.test(tab), silent = TRUE)
      if (inherits(test, "try-error") || any(test$expected < 5)) {
        test <- stats::fisher.test(tab)
      }
      p <- broom::tidy(test)$p.value

      # group-wise %
      prop <- prop.table(tab, 2) * 100
      dplyr::as_tibble(as.data.frame.matrix(prop), rownames = "level") |>
        dplyr::mutate(
          variable = v, type = "categorical",
          p = signif(p, 3)
        ) |>
        dplyr::relocate(variable, type, level)
    })
    cat_out <- dplyr::mutate(cat_out, dplyr::across(where(is.numeric), ~ round(., digits)))
  }

  dplyr::bind_rows(num_out, cat_out)
}
