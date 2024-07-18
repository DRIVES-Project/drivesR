
#' Fahrenheit to Celsius conversion
#'
#' Convert degrees Fahrenheit to degrees Celsius
#' @param F_temp The temperature in degrees Farenheit
#' @return The temperature in degrees Celsius
#' @export
#'
#' @examples
#' temp1 <- F_to_C(50);
#' temp2 <- F_to_C(c(50,63,23))
F_to_C <- function(F_temp){
  C_temp <- (F_temp - 32) * 5/9;
  return(C_temp);
}

#' Celsus to Fahrenheit conversion
#'
#'Convert degrees Celsius to degrees Fahrenheit
#' @param C_temp The temperature in degrees Celsius
#'
#' @return The temperature in degrees Fahrenheit
#' @export
#'
#' @examples
#' temp1 <- C_to_F(22);
#' temp2 <- C_to_F( c(-2, 12, 23));
C_to_F <- function(C_temp){
  F_temp <- (C_temp * 9/5) + 32;
  return(F_temp);
}
