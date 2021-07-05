#' Get IPO date
#'
#' @description Get IPO date from fmp cloud.
#' @param ticker Stock ticker
#' @param api_key Fmp cloud API key.
#' @return Stock IPO date
#' @import httr
#' @examples
#' get_ticker_changes("AAPL)
#' @export
get_ipo_date <- function(ticker, api_key) {
  url <- paste0("https://financialmodelingprep.com/api/v4/company-outlook")
  p <- content(GET(url, query = list(symbol = ticker, apikey = api_key)))
  if ("error" %in% names(p)) {
    return("2004-01-01")
  } else {
    return(p$profile$ipoDate)
  }
}

