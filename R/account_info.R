############################################################## 
#' Get account information
#'
#' Get details for one of the accounts owned by the user
#' 
#' @param token The personal access token. There is a link on your OANDA fxTrade account profile page titled 'Manage API Access' (My Account -> My Services -> Manage API Access). From there, you can generate a personal access token to use with the OANDA API, as well as revoke a token you may currently have.
#' 
#' @param accountId The account id to fetch the list of info for. Can be retrieved with a call of \code{account_list}.
#' 
#' @param accountType The subdomain for the request is dependent on the environment you wish to obtain access tokens for. Possible choices are Practice or Trade
#' 
#' @examples account_info('my_token_1234', 'my_account_1234')
#' 
#' @export
#' 

account_info <- function(token, accountId, accountType = c("Trade", "Practice")){
  library("httr")
  
  # Check arguments
  accountType = match.arg(accountType)
  stopifnot(is.character(token))
  
  # Create URL
  url <- ifelse(accountType == "Practice", "https://api-fxpractice.oanda.com/v1/accounts", "https://api-fxtrade.oanda.com/v1/accounts")
  url <- paste0(url, "/", accountId)
  
  # Send Request
  get_request <- GET(url, add_headers(Authorization = paste0("Bearer ", token)))
  if(get_request$status_code != 200)
    stop(paste0("Request failed with code ", get_request$status_code," and error message:\n", content(get_request)$message))
  
  # Create dataframe
  results <- unlist(content(get_request))
  results_df <- data.frame(matrix(results, ncol = length(results), byrow = TRUE), 
                           stringsAsFactors = FALSE)
  colnames(results_df) <- names(results)
  
  # Format df
  for(i in c(3:10))
    results_df[,i] = as.numeric(results_df[,i])
  
  return(results_df)
}