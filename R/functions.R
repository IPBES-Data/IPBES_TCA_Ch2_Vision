#' Get Corpus
#'
#' This function opens a dataset from a specified path and returns a corpus object.
#'
#' @param path The path to the dataset.
#' @importFrom arrow open_dataset
#' @return A corpus object.
#' @examples
#' get_corpus("data/corpus/")
get_corpus <- function(
    path = file.path("data", "corpus"),
    unify_schemas = FALSE) {
    con <- arrow::open_dataset(
        sources = path,
        unify_schemas = unify_schemas
    )
    return(con)
}


#' Test Corpus
#'
#' This function tests the parquet files from a specified directory and prints a message for each file.
#' The function does not return anything, the essential are the disgnostig messages
#' @importFrom arrow read_parquet
#' @param path The path to the directory containing the parquet files.
#' @return NULL
#' @export
#'
#' @examples
#' test_corpus()
#'
#' @keywords internal
test_corpus <- function(
    path = file.path("data", "corpus")) {
    p <- list.files(
        path = "data/corpus",
        pattern = ".parquet$",
        recursive = TRUE,
        full.names = TRUE
    )
    oldOpt <- options(warn = 1)
    try(
        invisible(
            lapply(
                p,
                function(x) {
                    message(x)
                    read_parquet(x)
                    invisible(NULL)
                }
            )
        )
    )
    options(oldOpt)
}

serialize_arrow <- function(data) {
    data |>
        mutate(
            topics = purrr::map_chr(topics, ~ serialize(.x, NULL) |> base64enc::base64encode()),
            author = purrr::map_chr(author, ~ serialize(.x, NULL) |> base64enc::base64encode())
        )
}

unserialize_arrow <- function(data) {
    data |>
        mutate(
            topics = purrr::map(topics, ~ .x |>
                base64enc::base64decode() |>
                unserialize()),
            author = purrr::map(author, ~ .x |>
                base64enc::base64decode() |>
                unserialize())
        )
}
