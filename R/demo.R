#' double
#'
#' @param num The number to be doubles
#' @return The value for 'num', doubled
#' @export
#'
#' @examples
#' double(2)
#'
double <- function(num){

  chk_number(num)

  result <- num * 2
  result
}
