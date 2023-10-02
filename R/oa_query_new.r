
oa_query_new <- function(
    filter = NULL, multiple_id = FALSE, identifier = NULL,
    entity = if (is.null(identifier)) NULL else id_type(identifier[[1]]),
    options = NULL, search = NULL, group_by = NULL, endpoint = "https://api.openalex.org",
    verbose = FALSE, ...) {
  
  entity <- match.arg(entity, oa_entities())
  filter <- c(filter, list(...))
  if (length(filter) > 0 || multiple_id) {
    null_locations <- vapply(filter, is.null, logical(1))
    filter[null_locations] <- NULL
    filter <- lapply(filter, asl)
    flt_ready <- mapply(append_flt, filter, names(filter))
    flt_ready <- paste0(flt_ready, collapse = ",")
  } else {
    flt_ready <- list()
  }
  if (!is.null(options$select)) {
    options$select <- paste(options$select, collapse = ",")
  }
  if (is.null(identifier) || multiple_id) {
    if (length(filter) == 0 && is.null(search) && is.null(options$sample)) {
      message("Identifier is missing, please specify filter or search argument.")
      return()
    }
    path <- entity
    query <- c(list(
      filter = flt_ready, search = search,
      group_by = group_by
    ), options)
  } else {
    path <- paste(entity, identifier, sep = "/")
    query <- NULL
  }
  query_url <- httr::modify_url(endpoint, path = path, query = query)
  if (verbose) {
    message("Requesting url: ", query_url)
  }
  query_url
}
