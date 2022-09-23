generate_credentials <- function(dummy_var = NULL){
  #' @description generates AWS credentials
  #' @param dummy_var variable that is not used in the function. Allows the user
  #' to force a target dependency before generating the credentials.
  #' @return always returns 0

  if (!file.exists('~/saml2aws-files/pass.txt')){
    stop('please add pass.txt file to ~/saml2aws-files. See README.')
  }
  pass <- read_table('~/saml2aws-files/pass.txt', col_names = FALSE, show_col_types = FALSE) %>%
    as.character()
  system(paste0("~/saml2aws-files/saml2aws login --skip-prompt --role='arn:aws:iam::807615458658:role/adfs-wma-developer' --profile='dev' --force --session-duration=28800 --credentials-file='./credentials' --quiet --password='", pass, "'")) %>%
    suppressWarnings()
  rm(pass)
  
  return(0)
}