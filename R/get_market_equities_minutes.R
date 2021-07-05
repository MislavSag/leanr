#' Import local market data with minute frequency
#'
#' @description Import market data with lean structure from dictionary with minute frequency.
#' @param path Path to local market data.
#' @param tickers Stocks to import.
#' @return Market data organized as panel data.
#' @import data.table
#' @export
get_market_equities_minutes <- function(path, tickers = NA) {

  # solve No visible binding for global variable
  symbol <- datetime <- `.` <- high <- low <- volume <- NULL

  # chosse ticker to import
  if (all(is.na(tickers))) {
    market_data_files <- list.files(path, full.names = TRUE)
  } else {
    market_data_files <- paste0(path, "/", tolower(tickers))
  }

  # import data
  market_data <- lapply(market_data_files, function(x) {
    zip_files <- list.files(x, full.names = TRUE)
    daily_data <- lapply(zip_files, function(y) {
      data_ <- fread(cmd = paste0('unzip -p ', y),
                     col.names = c('datetime', "open", "high", "low", "close", "volume"))
      data_[, symbol := toupper(gsub(".*/", "", x))]
      data_[, date := as.Date(gsub(".*/|_trade.*", "", y), format = "%Y%m%d")]
    })
    daily_data <- rbindlist(daily_data)
    market_data <- daily_data[, .(symbol, datetime, date, open, high, low, close, volume)]
    ohlc <- c('open', 'high', 'low', 'close')

    # quantcoonect format to ussual format
    market_data[, (ohlc) := lapply(.SD, function(x) x / 10000), .SDcols = ohlc]
    market_data[, time := dhms(datetime / 1000)]
    market_data[, datetime := as.POSIXct(paste(date, time))]
    setorderv(market_data, c("symbol", "datetime"))
    market_data[, `:=`(time = NULL, date = NULL)]
  })
  market_data <- rbindlist(market_data)
  setorderv(market_data, c("symbol", "datetime"))
  return(market_data)
}

#' Convert seconds to hour:minute:seconds
#'
#' @description Convert seconds from midnight to hour:minute:seconds time format.
#' @param t Seconds from midnight.
#' @return Hour, minute, second format of seconds.
#' @export
dhms <- function(t){
  paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
               ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
               ,formatC(t %% 60, width = 2, format = "d", flag = "0")
               ,sep = ":")
}

# x <- get_market_equities_minutes("D:/market_data/equity/usa/minute", "STI")
