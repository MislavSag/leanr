#' Get split history
#'
#' @description Get split data from the web and calculate split factors.
#' @param ticker Stock ticker
#' @return Stock splits
#' @import httr
#' @import rvest
#' @import stringr
#' @examples
#' get_stock_split("AAPL)
#' @export
get_stock_split_factor <- function(ticker) {
  splits <- content(GET("https://www.splithistory.com/", query = list(symbol = ticker)))
  splits <- html_nodes(splits, xpath = "//table[@width='208' and @style='font-family: Arial; font-size: 12px']")
  splits <- html_table(splits, header = TRUE)[[1]]
  splits$date <- as.Date(splits$Date, "%m/%d/%Y")
  ratio1 <- as.numeric(str_extract(splits$Ratio, "\\d+"))
  ratio2 <- as.numeric(str_extract(splits$Ratio, "\\d+$"))
  splits$ratio <- ratio2 / ratio1
  splits <- splits[splits$date > as.Date("2004-01-01"), ]
  splits <- splits[order(splits$date), ]
  splits <- splits[splits$ratio != 1, ]
  if (length(splits) == 0) {
      return(NA)
    } else {
      splits$split_factor <- rev(cumprod(rev(splits$ratio)))
    }
  return(splits[, c('date', 'ratio', 'split_factor')])
}
