############################################################## 
#' Get an instrument list
#'
#' Get a list of tradeable instruments (currency pairs, CFDs, and commodities) that are available for trading with the account specified.
#' 
#' @param token The personal access token. There is a link on your OANDA fxTrade account profile page titled 'Manage API Access' (My Account -> My Services -> Manage API Access). From there, you can generate a personal access token to use with the OANDA API, as well as revoke a token you may currently have.
#' 
#' @param accountId The account id to fetch the list of tradeable instruments for.
#' 
#' @param accountType The subdomain for the request is dependent on the environment you wish to obtain access tokens for. Possible choices are Practice or Trade
#' 
#' @param fields A vector of instrument fields that are to be returned in the response. Please see the Value section below for a list of valid values. If the fields option is not specified, all fields will be returned
#' 
#' @param instruments A vector of instruments that are to be returned in the response. If the instruments option is not specified, all instruments will be returned
#' 
#' @return A data frame with all or some of the following fields:\cr 
#' \emph{instrument}: Name of the instrument. This value should be used to fetch prices and create orders and trades.\cr 
#' \emph{displayName}: Display name for the end user.\cr 
#' \emph{pip}: Value of 1 pip for the instrument. More on pip.\cr 
#' \emph{maxTradeUnits}: The maximum number of units that can be traded for the instrument.\cr 
#' \emph{precision}: The smallest unit of measurement to express the change in value between the instrument pair.\cr 
#' \emph{maxTrailingStop}: The maximum trailing stop value (in pips) that can be set when trading the instrument.\cr 
#' \emph{minTrailingStop}: The minimum trailing stop value (in pips) that can be set when trading the instrument.\cr 
#' \emph{marginRate}: The margin requirement for the instrument. A 3\% margin rate will be represented as 0.03.\cr
#' \emph{halted}: The current trading status of the instrument. True if trading is halted, false if trading is active. \cr 
#' 
#' @examples instrument_list('my_token_1234', 'my_account_1234', 'Practice', c('pip', 'precision'), c('AUD_CAD', 'AUD_CHF'))
#' 
#' @export
#' 

instrument_list <- function(token, accountId, accountType = c("Practice", "Trade"), fields = NULL, instruments = NULL){
  library("httr")
  # Check arguments
  accountType <- match.arg(accountType)
  stopifnot(is.character(token), is.character(fields) | is.null(fields), is.character(instruments) | is.null(instruments))
  allowed_fields <- c("instrument", "displayName", "pip", "maxTradeUnits", "precision", "maxTrailingStop", "minTrailingStop", "marginRate", "halted")
  if(is.null(fields))
    fields = allowed_fields
  not_allowed_fields <- fields %in% allowed_fields
  if(any(!not_allowed_fields))
    stop(paste0("The following fields are not valid: ", paste(fields[!not_allowed_fields], collapse = ", ")))
  # Create URL
  base_url <- ifelse(accountType == "Practice", "https://api-fxpractice.oanda.com/v1/instruments?", "https://api-fxtrade.oanda.com/v1/instruments?")
  url <- paste0(base_url, "accountId=", accountId)
  if(!is.null(instruments))
    url <- paste0(url, "&instruments=", paste(instruments, collapse = "%2C"))
  url <- paste0(url, "&fields=", paste(fields, collapse = "%2C"))
  # Send Request
  get_request <- GET(url, add_headers(Authorization = paste0("Bearer ", token)))
  if(get_request$status_code != 200)
    stop(paste0("Request failed with code ", get_request$status_code," and error message:\n", content(get_request)$message))
  # Format results
  results <- content(get_request)[[1]]
  results_df <- data.frame(matrix(unlist(results), ncol = length(results[[1]]), byrow = TRUE), 
                           stringsAsFactors = FALSE)
  colnames(results_df) <- names(results[[1]])
  return(results)
}