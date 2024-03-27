#' ---
#' output: reprex::reprex_document
#' ---

library(arrow)

data <- readRDS("problem.rds")

arrow::write_dataset(
    data, 
    path = "~/problem", 
    partitioning = c("publication_year", "page"), 
    format = "parquet"
)
