#' Variables for MEP model analysis in Vignettes.
#'
#' A dataset containing the time series with variables of almost 1170.
#' datademo.
#'
#' @format A data frame with 1170 rows and 13 variables:
#' \describe{
#'   \item{year}{year of 2016}
#'   \item{month}{June}
#'   \item{day}{1st to 30th}
#'   \item{hour}{0 to 23, in hour}
#'   \item{minute}{0 and 30, in minute}
#'   \item{H}{observed sensible heat, in W/m2}
#'   \item{LE}{observed latent heat, in W/m2}
#'   \item{TA}{air temperature, in deg C}
#'   \item{TS}{surface temperature, in deg C}
#'   \item{NETRAD}{net radiation, in W/m2}
#'   \item{RH}{Relative humidity, in percent}
#'   \item{G}{observed ground heat, in W/m2}
#'   \item{PA}{atmospheric pressure, in Kpa}
#'   ...
#' }
"datademo"

#' Variable for MEP model example.
#' @name timeseries
#' 
#' A dataframe includes 3 variables timeseries.
#'
#' @format A dataframe includes 3 variables
#' \describe{
#'   \item{time}{time series}
#'   \item{MEP}{MEP estimations, in W/m2}
#'   \item{obs}{observations, in W/m2}
#'   ...
#' }
"timeseries"


#' Variables for MEP model analysis in Vignettes.
#' @name variables
#' 
#' @details 
#' - `lat`: latitude, in deg
#' - `lon`: longitude, in deg
#' - `Ts` : surface temperature, in deg C
#' - `qs` : specific humidity, in kg/kg
#' - `sm`  : soil miosture, in kg/m2
#'
#' - `I`  : surface thermal inertia, in tiu
#' - `Rn` : net radaition, in W/m2
#' - `RnL`: net Long-wave radaition, in W/m2
#' - `MEP`: MEP estimations, in W/m2
#' - `obs`: ET observations, in W/m2
NULL


#' Variables for MEP model analysis in Vignettes.
#'
#' Surface temperature in 2000/01
#' Ts.
#'
#' @format A matrix in Netcdf format
#' \describe{
#'   \item{Ts}{surface temperature, in deg C}
#'   ...
#' }
"Ts"

#' Variables for MEP model analysis in Vignettes.
#'
#' Distance above the surface
#' z.
#'
#' @format A matrix in Netcdf format
#' \describe{
#'   \item{z}{Distance above the surface, in m}
#'   ...
#' }
"z"

#' Variables for MEP model analysis in Vignettes.
#'
#' Distance above the surface
#' type.
#'
#' @format A matrix in Netcdf format
#' \describe{
#'   \item{type}{surface type, 1 or 2 or 3}
#'   ...
#' }
"type"

#' Variables for MEP model analysis in Vignettes.
#'
#' Global annual average ET during 1978-2018
#' ETannual.
#'
#' @format A matrix in Netcdf format
#' \describe{
#'   \item{ETannual}{annaul ET, in W/m2}
#'   ...
#' }
"ETannual"
