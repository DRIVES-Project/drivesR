
#' Convert length from feet to meters
#'
#' @param ft_len Length in feet
#'
#' @return Length in meters
#' @export
#'
#' @examples
#' ft_len(8);
#' ft_len(c(8,9,10))
ft_to_m <- function(ft_len){
 return(ft_len * 0.3048) 
}

#' Convert length from meters to feet
#'
#' @param m_len Length in meters
#'
#' @return Length in feet.
#' @export
#'
#' @examples
#' m_to_ft(5);
#' m_to_ft(c(5,6,7))
m_to_ft <- function(m_len){
  return(m_len / 0.3048) 
}


#' Convert weight from pounds to kg
#'
#' @param lb_wt Weight in pounds.
#'
#' @return Weight in kg.
#' @export
#'
#' @examples
#' lb_to_kg(1);
#' lb_to_kg(c(1,2,3))
lb_to_kg <- function(lb_wt){
  kg_wt = lb_wt *  0.4535924
  return(kg_wt)
}

#' Convert weight from kg to lb.
#'
#' @param kg_wt Weight in kg.
#'
#' @return Weight in lb.
#' @export
#'
#' @examples
#' kg_to_lb(1)
#' kg_to_lb(c(1,2,3))
kg_to_lb <- function(kg_wt){
  lb_wt = kg_wt * 2.204623
  return(lb_wt)
}

#' Convert area from Acres to hectares
#'
#' @param acres a value or numeric vector of area in acres
#'
#' @return a value or numeric vector of the same area in hectares
#' @export
#'
#' @examples
acre_to_hectare <- function(acres){
  haPerAcre = 0.4046856
  hectares = acres * haPerAcre
  return(hectares)
}

#' Convert any unit per acre area to the same unit per hectare area
#'
#' @param  unitacre A value or numeric vectors in a per-acre unit, e.g., seeds/acre.
#'
#' @return A value or numeric  vector in the equivalent per-hectare unit, e.g., seeds/ha
#' @export
#'
#' @examples
unitacre_to_unithectare <- function(unitacre){
  acresPerHa = 2.471054
  unithectare = unitacre*acresPerHa
  return(unithectare)
}

lbacre_to_kghectare <- function(lbacre){
  acresPerHa = 2.471054
  kgPerLb = 0.4535924
  kgha = lbacre * kgPerLb * acresPerHa
  return(kgha)
}
#TODO----------

# bushel_to_kg <- function(bushels, bushelwt = "USDA"){
#   
# }
# 
# 
# bushelPerAcre_to_kgPerHectare <- function(bua, testwt = "USDA"){
#   
# }
# 
# getDryWt <- function(inputwt, percent_moisture){
#   
# }
# 


