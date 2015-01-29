############################################################## 
#' Get current prices
#'
#' Fetch live prices for specified instruments that are available on the OANDA platform.
#' 
#' @param token The personal access token. There is a link on your OANDA fxTrade account profile page titled 'Manage API Access' (My Account -> My Services -> Manage API Access). From there, you can generate a personal access token to use with the OANDA API, as well as revoke a token you may currently have.
#' 
#' @param instruments A vector of instruments to fetch prices for. Values should be one of the available instrument from the \code{instrument_list} response.
#' 
#' @param accountType The subdomain for the request is dependent on the environment you wish to obtain access tokens for. Possible choices are Practice or Trade
#' 
#' @return A data frame with \emph{time}, \emph{bid} and \emph{ask} for all the instruments specified.
#' 
#' @examples current_prices('my_token_1234', c('EUR_USD', 'USD_JPY'))
#' 
#' @export
#' 

# TO ADD
# @param since When specified, only prices that occurred after the specified timestamp are returned.

current_prices <- function(token, instruments, accountType = c("Trade", "Practice")){ #, since = NULL
  library("httr")
  # Check arguments
  stopifnot(is.character(token), is.character(instruments)) #, class(since) == "Date" | is.null(since)
  # Create URL
  base_url <- ifelse(accountType == "Practice", "https://api-fxpractice.oanda.com/v1/prices?", "https://api-fxtrade.oanda.com/v1/prices?")
  url <- paste0(base_url, "instruments=", paste(instruments, collapse = "%2C"))
  # Send Request
  get_request <- GET(url, add_headers(Authorization = paste0("Bearer ", token)))
  if(get_request$status_code != 200)
    stop(paste0("Request failed with code ", get_request$status_code," and error message:\n", content(get_request)$message))
  # Format results
  results <- content(get_request)[[1]]
  results_df <- data.frame(matrix(unlist(results), ncol = length(results[[1]]), byrow = TRUE), 
                           stringsAsFactors = FALSE)
  colnames(results_df) <- names(results[[1]])
  return(results_df)
}