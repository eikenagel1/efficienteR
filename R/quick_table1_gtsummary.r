#' Quick Table 1 using gtsummary (Medical Journal Standard)
#' @description Creates a publication-ready Table 1 for medical papers using gtsummary.
#'   For continuous variables:
#'   - Tests normality using Shapiro-Wilk test
#'   - Reports mean ± SD if normally distributed (p > 0.05)
#'   - Reports median [IQR] if not normally distributed (p ≤ 0.05)
#'   For categorical variables:
#'   - Reports N (%)
#'   Can stratify by a grouping variable with automatic statistical tests.
#' @param df Data frame
#' @param by Optional grouping variable (unquoted column name) for stratification
#' @param ... Columns to summarize (unquoted). If none, all columns except 'by'.
#' @param normality_test Logical, whether to test normality (default TRUE).
#'   If TRUE, uses Shapiro-Wilk for n < 5000, Anderson-Darling for larger samples.
#' @param alpha Significance level for normality test (default 0.05)
#' @param add_p Logical, add p-values for group comparisons (default TRUE if 'by' is specified)
#' @param add_overall Logical, add overall column when stratifying (default TRUE)
#' @return gtsummary tbl_summary object
#' @export
#' @examples
#' \dontrun{
#' # Basic table
#' quick_table1_gtsummary(mtcars)
#'
#' # Stratified by group with p-values
#' quick_table1_gtsummary(mtcars, by = am, mpg, cyl, disp, hp)
#'
#' # All variables stratified
#' quick_table1_gtsummary(mtcars, by = vs)
#' }
quick_table1_gtsummary <- function(df,
                                   by = NULL,
                                   ...,
                                   normality_test = TRUE,
                                   alpha = 0.05,
                                   add_p = !missing(by),
                                   add_overall = TRUE) {

  # Check if gtsummary is available
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop("Package 'gtsummary' is required. Install it with: install.packages('gtsummary')")
  }

  # Capture by variable
  by_var <- rlang::enquo(by)
  by_name <- if (!rlang::quo_is_null(by_var)) {
    rlang::as_name(by_var)
  } else {
    NULL
  }

  # Capture columns to summarize
  cols <- rlang::quos(...)
  dat <- if (length(cols)) {
    dplyr::select(df, !!!cols)
  } else if (!is.null(by_name)) {
    dplyr::select(df, -dplyr::all_of(by_name))
  } else {
    df
  }

  # Add by variable back if specified
  if (!is.null(by_name)) {
    dat[[by_name]] <- df[[by_name]]
  }

  # Determine which numeric variables are normally distributed
  num_vars <- names(dat)[vapply(dat, is.numeric, logical(1))]

  statistic_list <- list()
  type_list <- list()

  if (length(num_vars) > 0 && normality_test) {
    for (var in num_vars) {
      x <- dat[[var]]
      x_clean <- x[!is.na(x)]

      if (length(x_clean) < 3) {
        # Not enough data for normality test, use median
        statistic_list[[var]] <- "{median} [{p25}, {p75}]"
        type_list[[var]] <- "continuous"
        next
      }

      # Choose appropriate normality test
      is_normal <- tryCatch({
        if (length(x_clean) < 5000) {
          # Shapiro-Wilk test for smaller samples
          test_result <- stats::shapiro.test(x_clean)
        } else {
          # Anderson-Darling test for larger samples (if nortest available)
          if (requireNamespace("nortest", quietly = TRUE)) {
            test_result <- nortest::ad.test(x_clean)
          } else {
            # Fall back to visual inspection via skewness
            skew <- (mean(x_clean) - stats::median(x_clean)) / stats::sd(x_clean)
            test_result <- list(p.value = ifelse(abs(skew) < 0.5, 0.1, 0.01))
          }
        }
        test_result$p.value > alpha
      }, error = function(e) {
        # If test fails, assume non-normal
        FALSE
      })

      if (is_normal) {
        # Normal distribution: mean ± SD
        statistic_list[[var]] <- "{mean} ± {sd}"
        type_list[[var]] <- "continuous"
      } else {
        # Non-normal distribution: median [IQR]
        statistic_list[[var]] <- "{median} [{p25}, {p75}]"
        type_list[[var]] <- "continuous"
      }
    }
  } else if (length(num_vars) > 0) {
    # If not testing normality, default to mean ± SD
    for (var in num_vars) {
      statistic_list[[var]] <- "{mean} ± {sd}"
      type_list[[var]] <- "continuous"
    }
  }

  # Create base table
  if (!is.null(by_name)) {
    tbl <- dat |>
      gtsummary::tbl_summary(
        by = dplyr::all_of(by_name),
        statistic = if (length(statistic_list) > 0) statistic_list else NULL,
        type = if (length(type_list) > 0) type_list else NULL,
        missing = "ifany",
        missing_text = "Missing"
      )

    # Add overall column if requested
    if (add_overall) {
      tbl <- tbl |> gtsummary::add_overall()
    }

    # Add p-values if requested
    if (add_p) {
      tbl <- tbl |> gtsummary::add_p()
    }
  } else {
    tbl <- dat |>
      gtsummary::tbl_summary(
        statistic = if (length(statistic_list) > 0) statistic_list else NULL,
        type = if (length(type_list) > 0) type_list else NULL,
        missing = "ifany",
        missing_text = "Missing"
      )
  }

  # Apply bold labels for better readability
  tbl <- tbl |> gtsummary::bold_labels()

  return(tbl)
}
