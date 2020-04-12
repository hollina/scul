#' Cigarette sales and retail prices by state
#'
#' Data from Abadie, Diamond, and Hainmuelle (2010)
#'
#' @docType data
#'
#' @usage data(cigarette_sales)
#'
#' @format Each row is a year, each column is either cigarette sales or retail cigarette price by state. State is indicated by suffix with FIPS code.
#'
#' @keywords datasets
#'
#' @references Abadie, Alberto, Alexis Diamond, and Jens Hainmueller. 2010. “Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California’s Tobacco Control Program.” Journal of the American Statistical Association 105 (490): 493–505. https://doi.org/10.1198/jasa.2009.ap08746.
#'

#' @examples
#' data(cigarette_sales)
#' mean(cigarette_sales$year)
#' mean(cigarette_sales$cigsale_6)
"cigarette_sales"
