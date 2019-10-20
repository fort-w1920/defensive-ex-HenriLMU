# compute means of all numeric columns in df
# output: a data.frame
col_means <- function(df, na.rm = FALSE) {
  
  # control na.rm input 
  assert_logical(na.rm)

  
  # Vektoren, die keine Listen sind, können einfach über die Mean-Funktion
  # berechnet werde
  if (is.vector(df) && !is.list(df)) {
    assert_numeric(df)
    
    return(mean(df))
  
  } else {
    
    # change (e.g. list) to dataframe
    df <- as.data.frame(df)
    
    # Überprüfe, ob eine Dimension 'null' ist
    if (any(dim(df) == 0)) {
      warning("There 're no data in your dataframe")
      return(data.frame())
    }
     
    if (dim(df)[2] == 1) {
      
      df_dim1 <- as.data.frame(mean(df[,1]))
      names(df_dim1) <- names(df)
      
      if (is.na(df_dim1)) return(data.frame())
      else return(df_dim1)
      
    } else {
      numeric <- vapply(df, is.numeric, logical(1))
      numeric_cols <- data.frame(df[, numeric])
      if (any(dim(numeric_cols)) == 0) return(data.frame())
      numeric_col_means <- data.frame(lapply(numeric_cols,
                                             # lapply with mean specification:
                                             function(x) mean(x, na.rm = na.rm)))
      names(numeric_col_means) <- names(df[, numeric, drop = na.rm])
      
      return(numeric_col_means)
    }
  }
}



