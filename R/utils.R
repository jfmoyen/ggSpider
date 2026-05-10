##### Utils #####
#' @title has_attribute()
#' @description Check whtther an object has a specific attribute
#' @param x the object to text
#' @param which the attribute
#' @return Boolean.
#' @details That's really a simple function, what?
#' @examples
#' \dontrun{
#' if(interactive()){
#'  has_attribute(mtcars,"names") ##TRUE
#'  has_attribute(mtcars,"dim") ##FALSE
#'  }
#' }
#' @rdname has_attribute
#' @export

has_attribute <- function(x, which){

  which %in% names(attributes(x))
}
