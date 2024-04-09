fts_search  <- function(
    search_term,
    conjunctive = TRUE,
    duckdb = params$duckdb_fn){

        con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = duckdb, read_only = TRUE)
        on.exit(try( duckdb::dbDisconnect(con, shutdown = TRUE))
        #
        dbExecute(con, "INSTALL fts")
        dbExecute(con, "LOAD fts")

        input_table <- "tca_corpus"
        input_id <- "id"
        input_values <- "'display_name', 'ab'"

        sql <- paste0(
            "SELECT ",
            "    fts_main_tca_corpus.match_bm25(",
            "       id, ",
            "       '", search_term, "',",
            "       conjunctive := ", as.numeric(conjunctive),
            "    ) AS score, ",
            "    id, ",
            "    publication_year ",
            "FROM ",
            "    tca_corpus ",
            "WHERE  ",
            "    score IS NOT NULL ",
            "ORDER BY ",
            "    score DESC;"
        )

        dbGetQuery(con, sql)
        #
}