# compute means of all numeric columns in df
# output: a data.frame
col_means <- function(df) {
  
  df <- data.frame(df)
  
  numeric <- vapply(df, is.numeric, logical(1))

  numeric_cols <- df[, numeric]
  
  numeric_cols <- data.frame(numeric_cols)
  
  data.frame(lapply(numeric_cols, mean))

}


