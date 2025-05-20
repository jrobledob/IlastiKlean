#' Generate and save synthetic plant datasets
#'
#' This function simulates measurement data for two plants, each with a given
#' number of samples and pictures per sample.  For plant 1 it draws classes from
#' `c("Callose", "Artifact", "Background")` and for plant 2 from
#' `c("Callose", "Callose", "Background", "Callose")`.  All datasets are saved
#' as CSV files in `wor_dir` and returned as a named list.
#'
#' @param samples Integer. Number of samples per plant.
#' @param pictures Integer. Number of pictures per sample.
#' @param wor_dir Character. Path to directory where CSV files will be written.
#' @return A named list of `data.frame`s, one element per plant/sample/picture,
#'   named like `dataset.ch00_plant1_sample1_pic1`.
#' @examples
#' \dontrun{
#' ## Create 3 samples × 3 pictures and save CSVs in "./out"
#' ds <- generate_datasets(samples = 3,
#'                         pictures = 3,
#'                         wor_dir = "./out")
#' }
#' @export
sim_Ilastik_output <- function(samples, pictures, wor_dir= "./") {
  # internal helper: simulate one data.frame for a sample/picture
  df_function1 <- function(class) {
    # create 3 objects with random measurements
    data.frame(
      object_id               = seq_len(3),
      `Predicted Class`       = sample(class, 3, replace = TRUE),
      `Size in pixels`        = sample(10:10000, 3),
      `Probability of Callose`= runif(3, 0, 1),
      `Variance of Intensity_0` = runif(3, 1, 1000),
      `Maximum intensity_0`   = sample(1:255, 3),
      `Center of the object_0`= runif(3, 1, 1044),
      check.names = FALSE
    )
  }

  # class definitions for each plant
  classes1 <- c("Callose", "Artifact", "Background")
  classes2 <- c("Callose", "Callose", "Background", "Callose")

  # ensure output directory exists
  if (!dir.exists(wor_dir)) {
    dir.create(wor_dir, recursive = TRUE)
  }

  # container for all generated datasets
  datasets_list <- list()

  # loop over plants 1 and 2
  for (plant in 1:2) {
    # pick the correct class vector
    class_vec <- if (plant == 1) classes1 else classes2

    # loop over samples and pictures
    for (i in seq_len(samples)) {
      for (j in seq_len(pictures)) {
        # build the object name
        nm <- sprintf("dataset.ch00_plant%d_sample%d_pic%d", plant, i, j)

        # simulate the data
        df <- df_function1(class_vec)

        # save in the return list
        datasets_list[[nm]] <- df

        # write out as CSV
        write.csv(df,
                  file = file.path(wor_dir, paste0(nm, ".csv")),
                  row.names = FALSE)
      }
    }
  }

  # return all data.frames
  datasets_list
}
