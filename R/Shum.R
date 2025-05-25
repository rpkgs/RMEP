#' calculate specific humidity for MEP model input
#'
#' @param TA Air temperature(unit: deg C)
#' @param RH Relative humidity(unit: percent)
#' @param PA Atmospheric pressure(unit: kPa)
#' @return specific humidity(unit:kg/kg)
#' @importFrom humidity SVP WVP2
#' @examples
#' Shum(TA = 20, PA = 101, RH = 50)
#' @export
Shum <- function(TA, PA, RH) {
  Tk <- C2K(TA)
  Es <- SVP(Tk)
  E <- WVP2(RH, Es)
  qs <- SH(E, PA * 1000) # specific humidity in kg/kg
  return(qs)
}

#' calculate saturated specific humidity for MEP model input
#' @param  Ts surface temperature(unit: deg C)
#' @return saturated specific humidity(unit:kg/kg)
#' @importFrom humidity SVP.ClaCla
#' @examples
#' SShum(Ts = 20)
#' @export
SShum <- function(Ts) {
  t_temp <- C2K(Ts)
  Es <- SVP.ClaCla(t_temp) # calculate saturated vapor pressure Es
  Sqs <- SH(Es * 100) # calculate saturated specific humidity
  return(Sqs)
}
