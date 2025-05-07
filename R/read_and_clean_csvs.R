#' Read and combine CSV files with cleaned column names
#'
#' @description
#' Reads all CSV files in the directory given by `path`, coerces every column
#' to character, binds them into one tibble, adds a `filename` column (just the
#' file name, no path), and replaces spaces in column names with underscores.
#'
#' @param path Character. Directory containing the CSV files. Defaults to `"."`.
#' @param pattern Character. A regular expression to match files. Defaults to `"\\.csv$"`.
#'
#' @return A tibble with all rows from every CSV, all columns as character,
#'   with an extra `filename` column (base name only) and clean column names.
#'
#' @importFrom readr read_csv cols
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # CSVs in the working directory
#' df1 <- read_and_clean_csvs(path = "./")
#'
#' # CSVs in some other folder
#' df2 <- read_and_clean_csvs(path = "data/")
#' }
read_and_clean_csvs <- function(path = ".", pattern = "\\.csv$") {
  # 1. list with full paths
  files <- list.files(path = path,
                      pattern = pattern,
                      full.names = TRUE)

  if (length(files) == 0) {
    warning("No CSV files found with pattern '", pattern,
            "' in path '", path, "'.")
    return(tibble::tibble())
  }

  # 2. read each, tag with basename
  combined <- purrr::map_df(files, function(flnm) {
    df <- readr::read_csv(flnm, col_types = readr::cols(.default = "c"))
    df$filename <- basename(flnm)
    df
  })

  # 3. clean column names
  names(combined) <- gsub(" ", "_", names(combined))

  combined
}

