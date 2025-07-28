#' Visualise and analyse callose‑count data (≥ 2 treatment groups)
#'
#' Draws a box‑plot, checks distributional assumptions, and chooses the
#' correct significance test: Welch *t* / Wilcoxon for **2** groups, or
#' one‑way ANOVA + Tukey **vs.** Kruskal–Wallis + Dunn for **≥ 3** groups.
#' Progress messages are written to the Console; graphics can be shown or
#' suppressed via the `show_plots` switch.
#'
#' ## Run‑time dependencies
#' * **ggplot2** – plotting & file export
#' * **dplyr**   – tidy helpers (example only)
#' * **car**     – `leveneTest()`
#' * **multcomp** – `glht()` + `mcp()` for Tukey contrasts
#' * **FSA**     – `dunnTest()` for non‑parametric post‑hoc
#'
#' Declare these in your **DESCRIPTION** under **Imports:** and omit any
#' `library()` calls inside the package code.
#'
#' @param df   A `data.frame` containing the response and grouping columns.
#' @param group_col  Character, column that defines treatment groups.
#'                   *Default*: `"plant"`.
#' @param count_col  Character, column with callose counts.
#'                   *Default*: `"callose_count"`.
#' @param boxplot_filename Optional `.png` path; if non‑`NULL`, the box‑plot is
#'        exported with `ggplot2::ggsave()`.  *Default*: `NULL`.
#' @param alpha  Numeric, α‑level for assumption tests.  *Default*: `0.05`.
#' @param show_plots Logical, draw the box‑plot + histogram to the active
#'        device?  *Default*: `TRUE`.
#' @param verbose    Logical, print informative messages?  *Default*: `TRUE`.
#'
#' @return (Invisibly) a named list with
#' \describe{
#'   \item{`plot`}{`ggplot` object of the box‑plot.}
#'   \item{`shapiro`}{Shapiro–Wilk test (`htest`).}
#'   \item{`levene`}{Levene test (`anova`) — only if ≥ 3 groups.}
#'   \item{`test_result`}{Main significance test.}
#'   \item{`posthoc`}{Post‑hoc comparisons — only if ≥ 3 groups.}
#' }
#'
#' @import ggplot2
#' @importFrom car leveneTest
#' @importFrom multcomp glht mcp
#' @importFrom FSA dunnTest
#' @importFrom stats aov kruskal.test lm resid shapiro.test t.test wilcox.test
#' @importFrom graphics hist
#' @examples
#' if (requireNamespace("palmerpenguins", quietly = TRUE)) {
#'   library(dplyr)
#'   dat <- palmerpenguins::penguins %>%
#'     filter(!is.na(flipper_length_mm)) %>%
#'     select(species, flipper_length_mm)
#'
#'   res <- visualize_and_analyze_callose(dat,
#'                                        group_col  = "species",
#'                                        count_col  = "flipper_length_mm")
#'   # Console: progress + ANOVA/Tukey summaries
#'   res$test_result
#'   summary(res$posthoc)
#'   # Re‑draw the box‑plot later
#'   print(res$plot)
#' }
#' @export
visualize_and_analyze_callose <- function(df,
                                          group_col        = "plant",
                                          count_col        = "callose_count",
                                          boxplot_filename = NULL,
                                          alpha            = 0.05,
                                          show_plots       = TRUE,
                                          verbose          = TRUE) {

  ## ------------ helper for conditional messaging -----------------------
  vcat <- function(..., .verbose = TRUE) {
    if (isTRUE(.verbose)) message(..., appendLF = TRUE)
  }

  ## ------------ basic checks & prep ------------------------------------
  if (!all(c(group_col, count_col) %in% names(df)))
    stop("Columns '", group_col, "' and/or '", count_col,
         "' not found in 'df'.")

  df[[group_col]] <- as.factor(df[[group_col]])
  k <- nlevels(df[[group_col]])

  vcat("Groups detected: ", k, " (", paste(levels(df[[group_col]]),
                                           collapse = ", "), ").", .verbose = verbose)

  ## ------------ box‑plot ------------------------------------------------
  vcat("Building box‑plot …", .verbose = verbose)
  box <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[group_col]],
                 y = .data[[count_col]],
                 fill = .data[[group_col]])) +
    ggplot2::geom_boxplot(width = .5, linewidth = .9) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = group_col, y = count_col) +
    ggplot2::scale_fill_brewer(palette = "Set2")

  if (isTRUE(show_plots)) print(box)

  if (!is.null(boxplot_filename)) {
    vcat("Saving box‑plot → ", boxplot_filename, .verbose = verbose)
    ggplot2::ggsave(filename = boxplot_filename, plot = box,
                    width = 5, height = 5, dpi = 300, units = "in")
  }

  ## ------------ histogram ----------------------------------------------
  if (isTRUE(show_plots)) {
    vcat("Drawing histogram …", .verbose = verbose)
    graphics::hist(df[[count_col]],
                   main = "Histogram of Callose Counts",
                   xlab = count_col)
  }

  ## ------------ choose test route --------------------------------------
  if (k == 2) {                          # ---------- two groups ----------
    vcat("Testing normality on raw data …", .verbose = verbose)
    shapiro_res <- stats::shapiro.test(df[[count_col]])

    if (isTRUE(verbose))
      print(shapiro_res)

    if (shapiro_res$p.value > alpha) {
      vcat("Normal ⇒ Welch's two‑sample t‑test", .verbose = verbose)
      test_main <- stats::t.test(
        stats::as.formula(paste(count_col, "~", group_col)),
        data = df)
    } else {
      vcat("Non‑normal ⇒ Wilcoxon rank‑sum test", .verbose = verbose)
      test_main <- stats::wilcox.test(
        stats::as.formula(paste(count_col, "~", group_col)),
        data = df, exact = FALSE)
    }

    if (isTRUE(verbose)) print(test_main)

    out <- list(plot = box,
                shapiro = shapiro_res,
                test_result = test_main)

  } else if (k > 2) {                    # ---------- ≥ three groups ------
    vcat("Fitting linear model …", .verbose = verbose)
    fit        <- stats::lm(
      stats::as.formula(paste(count_col, "~", group_col)),
      data = df)
    resid_vals <- stats::resid(fit)

    vcat("Testing residual normality …", .verbose = verbose)
    shapiro_res <- stats::shapiro.test(resid_vals)
    if (isTRUE(verbose)) print(shapiro_res)

    vcat("Testing homogeneity (Levene) …", .verbose = verbose)
    levene_res <- car::leveneTest(
      stats::as.formula(paste(count_col, "~", group_col)),
      data = df, center = median)
    if (isTRUE(verbose)) print(levene_res)

    normal   <- shapiro_res$p.value > alpha
    homosked <- levene_res[[3]][1]   > alpha  # p‑value in col 3, row 1

    if (normal && homosked) {         # --- parametric path --------------
      vcat("Assumptions met ⇒ one‑way ANOVA + Tukey HSD", .verbose = verbose)
      test_main <- stats::aov(fit)

      # Build named list programmatically: list(species = "Tukey")
      tukey_list <- setNames(list("Tukey"), group_col)
      linf       <- do.call(multcomp::mcp, tukey_list)
      posthoc    <- multcomp::glht(test_main, linfct = linf)

    } else {                           # --- non‑parametric path ----------
      vcat("Assumptions violated ⇒ Kruskal–Wallis + Dunn", .verbose = verbose)
      test_main <- stats::kruskal.test(
        stats::as.formula(paste(count_col, "~", group_col)),
        data = df)
      posthoc   <- FSA::dunnTest(
        stats::as.formula(paste(count_col, "~", group_col)),
        data   = df,
        method = "bonferroni")
    }

    if (isTRUE(verbose)) {
      print(test_main)
      vcat("--- Post‑hoc comparisons ---", .verbose = TRUE)
      print(summary(posthoc))
    }

    out <- list(plot        = box,
                shapiro     = shapiro_res,
                levene      = levene_res,
                test_result = test_main,
                posthoc     = posthoc)

  } else {
    stop("Grouping variable '", group_col, "' must have ≥ 2 levels.")
  }

  invisible(out)
}
