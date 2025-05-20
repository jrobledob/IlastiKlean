#' Summarize Callose Data by Group
#'
#' This function creates summary statistics for callose detections in Ilastik output files.
#' It counts callose objects and computes their average size per image, while accounting
#' for images with zero callose objects by referencing a full image list.
#'
#' @param df A cleaned data frame output by \code{read_and_clean_csvs()}, with all objects from all Ilastik images.
#' @param class_col The name of the column containing predicted class labels (default is "Predicted_Class").
#' @param size_col The name of the column containing object size values (default is "Size_in_pixels").
#' @param target_class The class name to summarize (e.g., "Callose").
#' @param group_vars A character vector of grouping variables (e.g., \code{c("plant", "sample", "pic_number")}).
#' @param output_csv Optional. If specified, the full summary will be written to this file path as a .csv file.
#'
#' @return A data frame summarizing counts and mean sizes of the target class objects per group.
#' If \code{output_csv} is provided, the result is also written to disk.
#'
#' @examples
#' \dontrun{
#' summary_df <- summarize_callose_data(
#'   df = objects_with_file_names,
#'   target_class = "Callose",
#'   group_vars = c("plant", "sample", "pic_number"),
#'   output_csv = "summary_output.csv"
#' )
#' }
#'
#' @export
summarize_callose_data <- function(df,
                                   class_col = "Predicted_Class",
                                   size_col = "Size_in_pixels",
                                   target_class = "Callose",
                                   group_vars = c("plant", "sample", "pic_number"),
                                   output_csv = NULL) {
  require(dplyr)
  require(tidyr)
  require(readr)
  
  # Ensure size column is numeric
  df[[size_col]] <- as.numeric(df[[size_col]])
  
  # Create reference dataset with all group combinations
  n_reference <- df %>% count(across(all_of(group_vars)))
  
  # Filter for target class (e.g., Callose)
  class_df <- df[df[[class_col]] == target_class, ]
  
  # Summarize class-specific data
  callose_summary <- class_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      callose_count = n(),
      mean_size = mean(.data[[size_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join to ensure inclusion of all reference image groups
  full_summary <- n_reference %>%
    left_join(callose_summary, by = group_vars) %>%
    replace_na(list(callose_count = 0, mean_size = 0))
  
  # Optionally write to .csv
  if (!is.null(output_csv)) {
    write_csv(full_summary, output_csv)
  }
  
  return(full_summary)
}
