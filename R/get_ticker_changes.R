#' Get ticker changes
#'
#' @description Get ticker changes from the web and link every change to ticker.
#' @param ticker Stock ticker
#' @return Stock changes
#' @import httr
#' @import rvest
#' @import data.table
#' @examples
#' get_ticker_changes("AAPL)
#' @export
get_ticker_changes <- function(ticker) {
  p <- POST('https://www.quantumonline.com/search.cfm',
            body = list(
              tickersymbol = ticker,
              sopt = 'symbol',
              '1.0.1' = 'Search'
            ))
  changes <- content(p)
  changes <- html_elements(changes, xpath = "//*[contains(text(),'Previous Ticker')]")
  changes <- html_text(changes)
  changes <- gsub('.*Symbol:', '', changes)
  changes <- trimws(changes)
  date <- as.Date(str_extract(changes, '\\d+/\\d+/\\d+'), '%m/%d/%Y')
  tickers <- str_extract(changes, '\\w+')
  changes <- data.table(ticker = ticker, date = date, ticker_change = tickers)
  return(changes)
}
