#' AAPL market data
#'
#' Market data for AAPL with hour frequency for period from 2004-01-01 to 2021-05-01.
#' Data was generated through FMP cloud API.
#'
#' @docType data
#'
#' @usage data(aapl)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{symbol}{Stock symbol}
#'  \item{datetime}{Datetime of the bar}
#'  \item{open}{Open price}
#'  \item{high}{High price}
#'  \item{low}{Low price}
#'  \item{close}{Close price}
#'  \item{volume}{Volume}
#' }
#' @references Data was collected using FMP cloud API
#' @keywords datasets
#' @examples
#'
#' data(aapl)
#' head(aapl)
#'
"aapl"
