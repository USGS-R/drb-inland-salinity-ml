recursive_fun<- function(x, df, col1, col2){
  #' @x subseg_seg object
  #' @df dataframe to loop up 
  #' @col1 the name of first col (as str) of the value x
  #' @col2 the name of second col (as str) of the source segs
  
  # setting all as character to make consistent across all fun output
  x <- as.character(x)
  # get row number where df$col1 is x 
  row_num <- which(df[[col1]] == x)
  
  #get the from_segs and recursively loop over them
  val <- df[[col2]][row_num][[1]]
  for (i in 1:length(val)){
    if(!is.na(val[i])){
      return(unique(c(x, val, Recall(x = val[i],df = df, col1 = col1, col2 = col2))))
    }else{
      #this returns nothing, otherwise the last element in the vector is NA. should return a characters
      return(x)
    }
  }
}

