#' Visualize and Analyze Callose Count Data
#'
#' This function creates visualizations (boxplot and histogram) of callose count data
#' and performs statistical analysis to compare two plant groups. The function first
#' checks if the callose counts follow a normal distribution using the Shapiro-Wilk test.
#' If they do, a Welch two-sample t-test is performed. If not, a non-parametric
#' Wilcoxon rank-sum test is used.
#'
#' @param df A dataframe containing summary callose count data, such as the output from \code{summarize_callose_data()}.
#' @param group_col The name of the grouping variable (typically "plant").
#' @param count_col The name of the column containing callose counts (typically "callose_count").
#' @param boxplot_filename Optional. File path to save the generated boxplot (e.g., "count_box_plot.png").
#'
#' @return A list containing:
#' \item{plot}{The ggplot2 object of the boxplot.}
#' \item{shapiro}{The result of the Shapiro-Wilk normality test.}
#' \item{test_result}{The result of the t-test or Wilcoxon test.}
#'
#' @examples
#' \dontrun{
#' analysis_result <- visualize_and_analyze_callose(
#'   df = full_summary,
#'   group_col = "plant",
#'   count_col = "callose_count",
#'   boxplot_filename = "count_box_plot.png"
#' )
#' }
#'
#' @export
visualize_and_analyze_callose <- function(df,
                                          group_col = "plant",
                                          count_col = "callose_count",
                                          boxplot_filename = "count_box_plot.png") {
  require(ggplot2)
  require(dplyr)
  
  # ---- Boxplot ----
  cat("Generating boxplot...\n")
  df[[group_col]] <- as.factor(df[[group_col]])
  
  plot <- ggplot(data = df, aes_string(x = group_col, y = count_col, fill = group_col)) +
    geom_boxplot(width = 0.5, linewidth = 0.9) +
    theme_bw(24) +
    labs(x = "Plant Number", y = "Mean callose deposit \ncount") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.4),
      legend.position = "none"
    ) +
    scale_fill_manual(values = c("#D81B60", "#004D40"))
  
  print(plot)
  
  # ---- Save boxplot ----
  if (!is.null(boxplot_filename)) {
    cat(paste("Saving boxplot to:", boxplot_filename, "\n"))
    ggsave(filename = boxplot_filename, plot = plot, width = 5, height = 5, dpi = 300, units = "in", device = 'png')
  }
  
  # ---- Histogram ----
  cat("Displaying histogram of callose counts...\n")
  hist(df[[count_col]], main = "Histogram of Callose Counts", xlab = "Callose Count")
  
  # ---- Shapiro-Wilk test ----
  cat("Running Shapiro-Wilk test for normality...\n")
  shapiro_result <- shapiro.test(df[[count_col]])
  print(shapiro_result)
  
  # ---- Statistical test ----
  unique_groups <- length(unique(df[[group_col]]))
  
  if (unique_groups != 2) {
    stop("This function currently supports only two groups for statistical comparison.")
  }
  
  if (shapiro_result$p.value > 0.05) {
    cat("Normality assumption met (p >", round(shapiro_result$p.value, 4), "). Running Welch's t-test...\n")
    test_result <- t.test(as.formula(paste(count_col, "~", group_col)), data = df)
  } else {
    cat("Normality assumption NOT met (p <", round(shapiro_result$p.value, 4), "). Running Wilcoxon rank-sum test...\n")
    test_result <- wilcox.test(as.formula(paste(count_col, "~", group_col)), data = df, exact = FALSE)
  }
  
  print(test_result)
  
  # ---- Return results ----
  return(list(
    plot = plot,
    shapiro = shapiro_result,
    test_result = test_result
  ))
}
