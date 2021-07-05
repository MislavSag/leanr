#' Get factor files
#'
#' @description Get factor files for Lean engine.
#' @param ticker Stock ticker
#' @param ohlcv Data table with OHLCV columns
#' @param api_key FMP cloud API key
#' @param save_dir Dir path to save factor files
#' @return Stock IPO date
#' @import httr
#' @import fmpcloudr
#' @import data.table
#' @importFrom zoo na.locf
#' @examples
#' data("aapl")
#' create_factor_file("AAPL", aapl, api_key = Sys.getenv("APIKEY"), save_dir = "./')
#' @export
create_factor_file <- function(ticker, ohlcv, api_key, save_dir) {

  # solve No visible binding for global variable
  symbol <- adjDividend <- `.` <- NULL

  fmpc_set_token(api_key)

  # daily market data df should be the input
  # ohlcv$date <- as.Date(ohlcv$datetime)
  ohlcv <- as.data.table(ohlcv)
  df <- ohlcv[symbol == ticker, .(date, close)]

  # add splits data
  splits <- as.data.table(get_stock_split_factor(ticker))
  # if (ticker == "WST") {
  #
  #   splits$date <- splits$date + 1
  #   splits$date <- splits$date + 1
  #   }
  if (all(is.na(splits))) {
    df[, split_factor := NA]
  } else {
    df <- splits[df, on = "date"]
    df$ratio <- NULL
  }

  # get dividends
  dividends <- fmpc_security_dividends(ticker, startDate = '2004-01-01')
  dividends <- as.data.table(dividends)
  if (all(is.na(dividends))) {
    df[, dividend := NA]
  } else {
    if (!("dividend" %in% names(dividends))) {
      dividends[, dividend := adjDividend]
    }
    dividends <- dividends[, dividend := ifelse(is.na(dividend) & adjDividend > 0, adjDividend, dividend)]
    dividends <- dividends[, .(date, dividend)]
    df <- dividends[df, on = "date"]
  }

  # if no splits and dividends return factor files with start and end dates
  if (length(dividends) == 0 & nrow(splits) == 0) {
    factor_file <- data.table(date = c("20040102", "20500101"),
                              price_factor = c(1, 1),
                              split_factor = c(1, 1),
                              lag_close = c(df$close[1], 0))
    # save
    fwrite(factor_file, file.path(save_dir, paste0(tolower(ticker), ".csv")), col.names = FALSE, row.names = FALSE)
    return(factor_file)
  }

  # clean data
  df[, `:=`(lag_close = shift(close),
            date = shift(date))]
  df <- df[!is.na(date)]

  # keep oly dividend ot split days
  factor_file <- df[(!is.na(dividend) & dividend != 0) | !is.na(split_factor), .(date, dividend, split_factor, lag_close)]
  if (nrow(factor_file) == 0) {
    factor_file <- data.table(date = c("20040102", "20500101"),
                              price_factor = c(1, 1),
                              split_factor = c(1, 1),
                              lag_close = c(df$close[1], 0))
    # save
    fwrite(factor_file, file.path(save_dir, paste0(tolower(ticker), ".csv")), col.names = FALSE, row.names = FALSE)
    return(factor_file)
  }
  factor_file <- unique(factor_file)
  # factor_file <- factor_file[date < as.Date("2018-06-01")] # TO COMPARE WITH QC
  factor_file[, split_factor := na.locf(split_factor, rev = TRUE, na.rm = FALSE)]
  factor_file <- factor_file[is.na(split_factor), split_factor := 1]

  # add row to the end
  factor_file <- rbind(factor_file, data.table(date = as.Date("2050-01-01"), dividend = NA, lag_close = 0, split_factor = 1))

  # calculate price factor
  price_factor <- vector("numeric", nrow(factor_file))
  lag_close <- factor_file$lag_close
  split_factor <- factor_file$split_factor
  dividend <- factor_file$dividend
  for (i in nrow(factor_file):1) {
    if (i == nrow(factor_file)) {
      price_factor[i] <- (lag_close[i-1] - dividend[i-1]) / lag_close[i-1] * 1
    } else if (i == 1) {
      price_factor[i] <- NA
    } else if (is.na(dividend[i-1])) {
      price_factor[i] <- price_factor[i+1]
    } else {
      price_factor[i] <- ((lag_close[i-1] - dividend[i-1]) / lag_close[i-1]) * price_factor[i+1]
    }
  }
  price_factor <- c(price_factor[-1], NA)
  price_factor[is.na(price_factor)] <- 1

  # add factor to factor file
  factor_file[, price_factor := price_factor]
  factor_file[, date := format.Date(date, "%Y%m%d")]
  factor_file <- factor_file[, .(date, price_factor, split_factor, lag_close)]

  # save
  fwrite(factor_file, file.path(save_dir, paste0(tolower(ticker), ".csv")), col.names = FALSE, row.names = FALSE)

  return(factor_file)
}
