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

#' Assess Search Term
#'
#' This function assesses the search term by counting the number of occurrences in a given text corpus.
#'
#' @param st The search term to be assessed. Each line should be one sub-term. Usually, these are combined by `OR`.
#' @param remove A regular expression pattern to remove from the search term.
#' @param excl_others Logical indicating whether to exclude other search terms from the count.
#' @param mc.cores The number of CPU cores to use for parallel processing.
#'
#' @return A data frame with the search term and the corresponding count.
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom openalexR oa_fetch
#'
#' @md
#'
#' @examples
#' assess_search_term(list("climate OR", "change"))
#'
#' @keywords internal
assess_search_term <- function(
    st = NULL,
    AND_term = NULL,
    remove = " OR$",
    excl_others = FALSE,
    verbose = FALSE,
    mc.cores = 8) {
    st <- gsub(pattern = remove, replacement = "", st)
    result <- data.frame(
        term = st,
        count = pbapply::pblapply(
            st,
            function(x) {
                if (excl_others) {
                    excl <- st[!(st %in% x)]
                    searchterm <- paste0("(", x, ") NOT (", paste0(excl, collapse = " OR "), ")")
                } else {
                    searchterm <- x
                }

                if (is.null(AND_term)) {
                    searchterm <- IPBES.R::compact(searchterm)
                } else {
                    searchterm <- IPBES.R::compact(paste0("(", AND_term, ") AND (", searchterm, ")"))
                }

                openalexR::oa_fetch(
                    title_and_abstract.search = searchterm,
                    output = "list",
                    count_only = TRUE,
                    verbose = verbose
                )$count
            }
        ) |>
            unlist()
    )
    return(result)
}


#' Reduce the length of a search term based on assessment results
#'
#' This function takes a search term and reduces its length based on the assessment results.
#' It iteratively removes terms from the search term until a valid count is obtained.
#'
#' @param search_term The original search term to be reduced.
#' @param AND_term An additional search term to be included in the search query with `AND` bbut which will
#'   **not** be shortened.
#' @param verbose A logical value indicating whether to display additional information during the process.
#'
#' @return A list containing:assessment
#'   - **search_term**: reduced search term based on the assessment results.
#'   - **cut_off**: the index of the last term in the reduced search term returned as `search_term`.
#'   - **assessment**: a data frame with the search term and the corresponding count resulting from
#'     the assessment (`assess_search_term()`).
#'   - **rel_cut**: the relative reduction in the number of search results.
#'
#'
#' @import dplyr
#' @importFrom openalexR oa_fetch
#' @importFrom dplyr arrange
#'
#' @examples
#' # Example 1: Reduce search term length
#' search_term <- c("climate change", "biodiversity loss", "land degradation", "pollution")
#' reduced_term <- reduce_search_term_length(search_term)
#' print(reduced_term)
#'
#' # Example 2: Reduce search term length with additional term
#' search_term <- c("climate change", "biodiversity loss", "land degradation", "pollution")
#' AND_term <- "sustainable development"
#' reduced_term <- reduce_search_term_length(search_term, AND_term)
#' print(reduced_term)
reduce_search_term_length <- function(
    search_term,
    AND_term = NULL,
    verbose = FALSE,
    progress = TRUE) {
    st_assessment <- assess_search_term(
        st = search_term,
        remove = " OR$",
        excl_others = FALSE,
        mc.cores = 8
    ) |>
        dplyr::arrange(desc(count)) |>
        dplyr::mutate(
            p = count / sum(count)
        )

    sts <- st_assessment$term
    cut_off <- length(sts)
    count <- NULL

    repeat {
        if (progress) {
            message("Cut-off: ", cut_off)
            # browser()
        }
        st <- sts[1:cut_off] |>
            paste(collapse = " OR ") |>
            gsub(
                pattern = " OR$",
                replacement = ""
            )
        if (is.null(AND_term)) {
            st <- IPBES.R::compact(st)
        } else {
            st <- IPBES.R::compact(paste0("(", AND_term, ") AND (", st, ")"))
        }

        try(
            count <- openalexR::oa_fetch(
                title_and_abstract.search = st,
                count_only = TRUE,
                output = "list",
                verbose = verbose
            )$count,
            silent = FALSE
        )

        if (is.null(count)) {
            cut_off <- cut_off - 1
            if (cut_off == 0) {
                break
            }
        } else {
            break
        }
    }

    result <- list(
        search_term = sts[1:cut_off],
        cut_off = cut_off,
        assessment = tibble::as_tibble(st_assessment),
        rel_excluded = 1 - (sum(st_assessment$count[1:cut_off]) / sum(st_assessment$count)),
        final_count = count
    )

    return(result)
}




# x <- reduce_search_term_length(
#     search_term = readLines("input/tca_corpus/search terms/case.txt"),
#     AND_term = params$s_1_tca_corpus
# )
