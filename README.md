# KleanLastic <img src="https://img.shields.io/badge/R-package-blue" align="right"/>

**KleanLastic** is an R package that provides a streamlined toolkit for simulating, cleaning, analyzing, and visualizing image-based object classification data from pipelines such as Ilastik. It is especially designed for plant biologists and researchers who need a quick and reliable way to handle callose (or similar object) quantification across experimental groups.

------------------------------------------------------------------------

## 📦 Installation

You can install the development version of KleanLastic directly from GitHub:

``` r
# If you don't have devtools installed:
install.packages("devtools")

# Install KleanLastic
devtools::install_github("jrobledob/KleanLastic")
```

------------------------------------------------------------------------

## 🚀 Features

-   **`sim_Ilastik_output()`** Generate synthetic Ilastik-like CSV datasets for simulation and pipeline testing.

-   **`read_and_clean_csvs()`** Read and clean multiple Ilastik output CSV files into a single tidy tibble, adding filename metadata.

-   **Filename Parsing & Metadata Extraction** Extract experimental info (e.g., plant, sample, picture number) encoded in filenames for downstream grouping.

-   **`summarize_callose_data()`** Group and summarize target-class object counts (e.g., callose) and sizes across samples/images.

-   **`visualize_and_analyze_callose()`** Visualize group differences using boxplots and histograms, with automatic normality testing and appropriate statistical comparisons (Welch's t-test or Wilcoxon).

------------------------------------------------------------------------

## 🧪 Example Workflow

``` r
# Load package
library(KleanLastic)

# 1. Simulate synthetic CSV files (3 samples × 3 images per sample)
sim_Ilastik_output(samples = 3, pictures = 3, wor_dir = "./output")

# 2. Read and combine the CSV files
df <- read_and_clean_csvs(path = "./output")

# 3. Clean filename strings (if needed)
library(dplyr)
library(stringr)
df <- df %>%
  mutate(filename = str_replace_all(filename, "dataset.ch00_", "")) %>%
  mutate(filename = str_replace_all(filename, ".csv", ""))

# 4. Split filename into metadata
library(tidyr)
df <- df %>%
  separate_wider_delim(filename, delim = "_",
                       names = c("plant", "sample", "pic_number"),
                       too_many = "merge")

# 5. Summarize callose data
summary_df <- summarize_callose_data(
  df = df,
  target_class = "Callose",
  group_vars = c("plant", "sample", "pic_number"),
  output_csv = "summary_output.csv"
)

# 6. Visualize and analyze
result <- visualize_and_analyze_callose(
  df = summary_df,
  group_col = "plant",
  count_col = "callose_count",
  boxplot_filename = "callose_boxplot.png"
)
```

------------------------------------------------------------------------

## 📊 Output

-   Cleaned and merged tibble with all CSV contents
-   Group-wise summary of callose object counts and mean sizes
-   Boxplot comparing callose counts across groups
-   Histogram of callose distributions
-   Shapiro-Wilk test for normality
-   Automatic t-test or Wilcoxon rank-sum test

------------------------------------------------------------------------

## 👨‍🔬 Who is this for?

-   Researchers analyzing microscopy outputs from Ilastik or similar segmentation software
-   Beginners looking for a simplified analysis pipeline
-   Advanced users wanting a modifiable framework for batch quantification and comparison

------------------------------------------------------------------------

## 📋 Authors

-   Jacobo Robledo (creator and maintainer)
-   Amit Levy
-   Stacy Welker

------------------------------------------------------------------------

## 📄 License

MIT License. See `LICENSE` file for details.

------------------------------------------------------------------------

## 🐛 Issues & Feedback

Please submit bug reports, suggestions, or feature requests via the [Issues page](https://github.com/jrobledob/KleanLastic/issues).

------------------------------------------------------------------------

