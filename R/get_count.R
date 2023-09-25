get_count <- function(
    search_term,
    dois = NULL
) {
    if (length(dois) != length(unique(dois))) {
        stop("\n Duplicate DOIs are not supported!")
    }
    count <- as.list(dois) |>
        sapply(
            FUN = function(doi) {
                openalexR::oa_query(search = search_term, filter = c(doi = doi)) |>
                    openalexR::oa_request(count_only = TRUE) |>
                    unlist()
            }
        ) |>
        t() |>
        as.data.frame() |>
        dplyr::select(count) |>
        unlist()
    names(count) <- dois
    return(count)
}
