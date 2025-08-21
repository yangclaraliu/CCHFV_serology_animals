make_metadata <- function(df, n_examples = 3) {
  data.frame(
    variable = names(df),
    class = sapply(df, function(x) class(x)[1]),
    n_missing = sapply(df, function(x) sum(is.na(x))),
    n_unique  = sapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE)),
    examples = sapply(df, function(x) {
      # take first n unique non-missing values
      vals <- unique(na.omit(x))[1:n_examples]
      paste(vals, collapse = ", ")
    }),
    stringsAsFactors = FALSE
  )
}

df_meta <- make_metadata(df)
