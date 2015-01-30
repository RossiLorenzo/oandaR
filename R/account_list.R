############################################################## 
#' Get accounts for a user
#'
#' Get a list of accounts owned by the user
#' 
#' @param token The personal access token. There is a link on your OANDA fxTrade account profile page titled 'Manage API Access' (My Account -> My Services -> Manage API Access). From there, you can generate a personal access token to use with the OANDA API, as well as revoke a token you may currently have.
#' 
#' @param username The name of the user. Note: This is only required on the sandbox, on our production systems your access token will identify you.
#' 
#' @param accountType The subdomain for the request is dependent on the environment you wish to obtain access tokens for. Possible choices are Practice or Trade
#' 
#' @return A data frame with all the following fields: \emph{accountId}, \emph{accountName}, \emph{accountCurrency}, \emph{marginRate} 
#' 
#' @examples account_list('my_token_1234', 'Practice')
#' 
#' @export
#' 

account_list <- function(token, username = NULL, accountType = c("Trade", "Practice")){
  library("httr")
  
  # Check arguments
  accountType = match.arg(accountType)
  stopifnot(is.character(token), is.character(username) | is.null(username))

  # Create URL
  url <- ifelse(accountType == "Practice", "https://api-fxpractice.oanda.com/v1/accounts", "https://api-fxtrade.oanda.com/v1/accounts")
  if(!is.null(username))
    url <- paste0(url, "?username=", URLencode(username))

  # Send Request
  get_request <- GET(url, add_headers(Authorization = paste0("Bearer ", token)))
  if(get_request$status_code != 200)
    stop(paste0("Request failed with code ", get_request$status_code," and error message:\n", content(get_request)$message))
  
  # Create dataframe
  results <- content(get_request)[[1]]
  results_df <- data.frame(matrix(unlist(results), ncol = length(results[[1]]), byrow = TRUE), 
                           stringsAsFactors = FALSE)
  colnames(results_df) <- names(results[[1]])
  
  # Format df
  if("marginRate" %in% colnames(results_df))
    results_df[,"marginRate"] = as.numeric(results_df[,"marginRate"])
  if("halted" %in% colnames(results_df))
    results_df[,"halted"] = as.logical(results_df[,"halted"])
  
  return(results_df)
}