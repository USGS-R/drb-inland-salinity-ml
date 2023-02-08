generate_credentials <- function(dummy_var = NULL){
  #' @description generates AWS credentials
  #' @param dummy_var variable that is not used in the function. Allows the user
  #' to force a target dependency before generating the credentials.
  #' @return always returns 0
  
  if (file.exists('./credentials')){
    #check if the file was generated within the last 10 minutes
    cred <- read_table(file = './credentials', skip = 1, col_names = FALSE, show_col_types = FALSE)
    #get expiration time, which is 8 hrs from the current time.
    c_time <- cred$X3[cred$X1 == 'x_security_token_expires']
    #convert to UTC and subtract 8 hrs to get to time file was generated
    c_time <- as_datetime(c_time) - hours(8)
    #compare to system time in UTC
    s_time <- as_datetime(Sys.time(), "UTC")

    if((s_time - c_time) > minutes(10)){
      #generate new credentials file
      if (!file.exists('~/saml2aws-files/pass.txt')){
        stop('please add pass.txt file to ~/saml2aws-files. See README.')
      }
      pass <- read_table('~/saml2aws-files/pass.txt', col_names = FALSE, show_col_types = FALSE) %>%
        as.character()
      system(paste0("~/saml2aws-files/saml2aws login --skip-prompt --role='arn:aws:iam::807615458658:role/adfs-wma-developer' --profile='dev' --force --session-duration=28800 --credentials-file='./credentials' --quiet --password='", pass, "'")) %>%
        suppressWarnings()
      rm(pass)
    }
  }
  
  return(0)
}