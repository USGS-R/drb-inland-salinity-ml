recursive_fun<- function(x, df, col1, col2){
  #' @x subseg_seg object
  #' @df dataframe to loop up 
  #' @col1 the name of first col (as str) of the value x
  #' @col2 the name of second col (as str) of the source segs
  
  # get row number where df$col1 is x 
  row_num <- which(df[[col1]] == x)

  # Vector to store the upstream segments - Setting all as character to make consistent across all fun output
  upstream_segs <- as.character(x)
  
  #Get the from_segs and recursively loop over them
  from_segs <- df[[col2]][row_num][[1]]
  for (i in 1:length(from_segs)){
    if(!is.na(from_segs[i])){
      upstream_segs <- unique(c(upstream_segs, from_segs[i], Recall(x = from_segs[i],df = df, col1 = col1, col2 = col2)))
    }
  }
  #this returns nothing, otherwise the last element in the vector is NA. should return a characters
return(upstream_segs)
    
}

