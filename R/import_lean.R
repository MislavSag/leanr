#' Import local market data
#'
#' @description Import market data with lean structure from dictionary.
#' @param path Path to local market data.
#' @param tickers Stocks to import.
#' @return Market data organized as panel data
#' @import data.table
#' @importFrom lubridate ymd_hm
#' @export
import_lean <- function(path, tickers = NA) {

  # solve No visible binding for global variable
  symbol <- datetime <- `.` <- high <- low <- volume <- NULL

  # chosse ticker to import
  if (all(is.na(tickers))) {
    market_data_files <- list.files(path, full.names = TRUE)
  } else {
    tickers_pattern <- paste0("^", tolower(tickers), ".zip", collapse = "|")
    market_data_files <- list.files(path, full.names = TRUE, pattern = tickers_pattern)
  }

  # import data
  market_data <- lapply(market_data_files, function(x) {
    y <- fread(cmd = paste0('unzip -p ', x),
               col.names = c('datetime', "open", "high", "low", "close", "volume"))
    y[, symbol := toupper(gsub(".*/|\\.zip", "", x))]
  })
  market_data <- rbindlist(market_data)

  # choose columns
  market_data <- market_data[, .(symbol, datetime, open, high, low, close, volume)]
  ohlc <- c('open', 'high', 'low', 'close')

  # quantcoonect format to ussual format
  market_data[, (ohlc) := lapply(.SD, function(x) x / 10000), .SDcols = ohlc]
  market_data[, datetime := ymd_hm(datetime, tz = "EST")]
  setorderv(market_data, c("symbol", "datetime"))
  return(market_data)
}
