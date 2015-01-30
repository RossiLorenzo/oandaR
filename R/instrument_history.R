############################################################## 
#' Get instrument history
#'
#' Get historical information on an instrument
#' 
#' @param token The personal access token. There is a link on your OANDA fxTrade account profile page titled 'Manage API Access' (My Account -> My Services -> Manage API Access). From there, you can generate a personal access token to use with the OANDA API, as well as revoke a token you may currently have.
#' 
#' @param instrument Name of the instrument to retrieve history for. The instrument should be one of the available instrument from the \code{instrument_list} response.
#' 
#' @param granularity The time range represented by each candlestick. The value specified will determine the alignment of the first candlestick. Default is 5 seconds.
#' 
#' @param count The number of candles to return in the response. Count willl be ignored if both the start and end parameters are also specified. If not specified, count will default to 500. The maximum acceptable value for count is 5000.
#' 
#' @param start The start timestamp for the range of candles requested. It must be a string in the following format: YYYY-MM-DD HH:MM:SS (UTC).
#' 
#' @param end The end timestamp for the range of candles requested. It must be a string in the following format: YYYY-MM-DD HH:MM:SS (UTC).
#' 
#' @param candleFormat Candlesticks representation (about candestick representation). This can be one of the following: 'midpoint' - Midpoint based candlesticks. 'bidask' - Bid/Ask based candlesticks The default for candleFormat is 'bidask' if the candleFormat parameter is not specified.
#' 
#' @param includeFirst If includeFirst is set to TRUE, the candlestick covered by the start timestamp will be returned. If it is set to FALSE, this candlestick will not be returned. This field exists so clients may easily ensure that they can poll for all candles more recent than their last received candle. The default for includeFirst is TRUE if the includeFirst parameter is not specified.
#' 
#' @param dailyAlignment The hour of day used to align candles with hourly, daily, weekly, or monthly granularity. The value specified is interpreted as an hour in the timezone set through the alignmentTimezone parameter and must be an integer between 0 and 23. The default for dailyAlignment is 21 when Eastern Daylight Time is in effect and 22 when Eastern Standard Time is in effect.
#' 
#' @param alignmentTimezone The timezone to be used for the dailyAlignment parameter. This parameter does NOT affect the returned timestamp, the start or end parameters, these will always be in UTC. The timezone format used is defined by the IANA Time Zone Database, a full list of the timezones supported by the REST API can be found here: \link{http://developer.oanda.com/docs/timezones.txt}. The default for alignmentTimezone is 'America/New_York' if the alignmentTimezone parameter is not specified.
#' 
#' @param weeklyAlignment The day of the week used to align candles with weekly granularity. The value specified will be used as the start/end day when calculating the weekly candles. Valid values are: 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'. The default for weeklyAlignment is 'Friday' if the weeklyAlignment parameter is not specified.
#' 
#' @param accountType The subdomain for the request is dependent on the environment you wish to obtain access tokens for. Possible choices are Practice or Trade
#' 
#' @return A data frame with \emph{time}, \emph{openBid}, \emph{openAsk}, \emph{highBid}, \emph{highAsk}, \emph{lowBid}, \emph{lowAsk}, \emph{closeBid}, \emph{closeAsk}, \emph{volume} and \emph{complete}
#' 
#' @note Valid values for the granularity are:\cr
#' \bold{Seconds}: 'S5', 'S10', 'S15', 'S30' - 5,10,15,30 seconds\cr
#' \bold{Minutes}: 'M1', 'M2', 'M3', 'M4', 'M5', 'M10', 'M15', 'M30', 1,2,3,4,5,10,15,30 minutes\cr
#' \bold{Hours}: 'H1', 'H2', 'H3', 'H4', 'H6', 'H8', 'H12' - 1,2,3,4,6,8,12 hours\cr
#' \bold{Days}: 'D' - 1 day\cr
#' \bold{Weeks}: 'W' - 1 week\cr
#' \bold{Months}: 'M' - 1 month\cr
#' 
#' @examples instrument_history(token, 'EUR_USD', accountType = 'Practice', start = '2015-01-01 12:00:00', end = '2015-01-29 12:00:00', granularity = 'M10')
#' 
#' @export

instrument_history <- function(token, instrument, 
                               granularity = c("S5", "S10", "S15", "S30", "M1", "M2", "M3", "M4", "M5", "M10", "M15", "M30", "H1", "H2", "H3", "H4", "H6", "H8", "H12", "D", "W", "M"),
                               count = 500, start = NULL, end = NULL, candleFormat = c("bidask" ,"midpoint"), 
                               includeFirst = TRUE, dailyAlignment = 21, alignmentTimezone = "America/New_York", 
                               weeklyAlignment = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), 
                               accountType = c("Trade", "Practice")){
  
  library("httr")
  
  # Check arguments
  granularity = match.arg(granularity)
  candleFormat = match.arg(candleFormat)
  weeklyAlignment = match.arg(weeklyAlignment)
  accountType = match.arg(accountType)
  stopifnot(is.character(token), is.character(instrument), 
            is.numeric(count), length(count) == 1, count >= 1, count <= 5000, count%%1==0, 
            is.character(start) | is.null(start), is.character(end) | is.null(end), 
            is.logical(includeFirst), length(includeFirst) == 1, 
            is.numeric(dailyAlignment), length(dailyAlignment) == 1, dailyAlignment >= 0, dailyAlignment <= 23, dailyAlignment%%1 == 0,
            is.character(alignmentTimezone), length(alignmentTimezone) == 1)
  if(!is.null(start)){
    start = strptime(start, tz = "GMT", format("%Y-%m-%d %H:%M:%S"))
    if(is.na(start))
      stop("The start date should be in the following format: YYYY-MM-DD HH:MM:SS. i.e: '2015-01-29 14:15:00'")
  }
  if(!is.null(end)){
    end = strptime(end, tz = "GMT", format("%Y-%m-%d %H:%M:%S"))
    if(is.na(end))
      stop("The end date should be in the following format: YYYY-MM-DD HH:MM:SS. i.e: '2015-01-29 14:45:00'")
  }
  if(!is.null(start) & !is.null(end))
    if(as.numeric(end - start) <= 0)
      stop("The end date should be bigger than the start date")
  
  # Create URL
  base_url <- ifelse(accountType == "Practice", "https://api-fxpractice.oanda.com/v1/candles?", "https://api-fxtrade.oanda.com/v1/candles?")
  url <- paste0(base_url, "instrument=", instrument)
  url <- paste0(url, "&granularity=", granularity)
  url <- paste0(url, "&candleFormat=", candleFormat)
  url <- paste0(url, "&alignmentTimezone=", gsub("/", "%2F", alignmentTimezone))
  url <- paste0(url, "&weeklyAlignment=", weeklyAlignment)
  url <- paste0(url, "&accountType=", accountType)
  if(!is.null(start))
    url <- paste0(url, "&start=", gsub(":", "%3A", format(start ,"%Y-%m-%dT%H:%M:%S.000000Z")))
  if(!is.null(end))
    url <- paste0(url, "&end=", gsub(":", "%3A", format(end ,"%Y-%m-%dT%H:%M:%S.000000Z")))
  if(is.null(start) | is.null(end))
    url <- paste0(url, "&count=", count)
  
  # Send Request
  get_request <- GET(url, add_headers(Authorization = paste0("Bearer ", token)))
  if(get_request$status_code != 200)
    stop(paste0("Request failed with code ", get_request$status_code," and error message:\n", content(get_request)$message))
  
  # Create results df
  results <- content(get_request)[[3]]
  results_df <- data.frame(matrix(unlist(results), ncol = length(results[[1]]), byrow = TRUE), 
                           stringsAsFactors = FALSE)
  colnames(results_df) <- names(results[[1]])
  
  # Format df
  results_df$time = strptime(results_df$time, tz = "GMT", format("%Y-%m-%dT%H:%M:%S.000000Z"))
  for(i in 2:10)
    results_df[,i] = as.numeric(results_df[,i])
  results_df$complete = as.logical(results_df$complete)
  
  results = new("oanda_result", url = url, type = "historical", result = results_df)
  return(results)
}