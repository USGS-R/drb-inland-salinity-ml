recursive_fun<- function(x, df, col1, col2){
  #' @description Recursively collect associated values in vectors of col 2 based on val on col 1 
  #' @param x subseg_seg object
  #' @param df dataframe to loop up 
  #' @param col1 the name of first col (as str) of the value x
  #' @param col2 the name of second col (as str) of the source segs
  #' @example recursive_fun(x = df[[col1]][x],  df = df, col1 = 'col1', col2 = 'col2')))
  #' @value list of all df[[col2]] values recursively collected from x 
  
  # get row number where df$col1 is x 
  row_num <- which(df[[col1]] == x)

  # Vector to store the upstream segments
  upstream_segs <- x
  
  #Get the from_segs and recursively loop over them
  from_segs <- df[[col2]][row_num][[1]]
  for (i in 1:length(from_segs)){
    if(!is.na(from_segs[i])){
      upstream_segs <- unique(c(upstream_segs, Recall(x = from_segs[i],df = df, col1 = col1, col2 = col2)))
    }
  }
return(upstream_segs)
    
}

