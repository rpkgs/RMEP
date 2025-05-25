#' Calculate evapotranspiration and energy fluxes with MEP method
#'
#' Calculate actual evapotranspiration, potential evapotranspiration, senseble heat flux and ground heat flux based on the maximum entropy production(MEP) model.
#' @param Rn net radiation (unit:W/m2)
#' @param RnL net long-wave radiation (unit:W/m2)
#' @param Ts surface temperature (unit:Celsius)
#' @param qs specific humidity (unit:kg/kg)
#' @param I media thermal inertial (units:TIU),default is 800 TIU
#' @param z theoretical height above surface (unit:m), constant value and default is 2.5 m
#' @param type 1 for bare soil surface or short canopy, 2 for dense canopy and 3 for Water-snow-ice surface
#' @return A list includes latent heat flux(EMEP,W/m2),sensible heat flux(HMEP,W/m2),ground heat flux(GMEP,W/m2) and evapotranspiration(ETMEP,mm/day)
#' @importFrom pracma ones zeros isempty fzero
#' 
#' @examples
#' RMEP(Rn = 200, RnL = 100, qs = 0.003, Ts = 20, type = 1)
#' RMEP(Rn = 300, RnL = 100, qs = 0.004, Ts = 25, I = 800, z = 8, type = 1)
#' @export
RMEP <- function(Rn, RnL, qs, Ts, I = 800, z = 2.5, type = 1) {
  # Parameters
  rhoa <- 1.18 # column vector of air density (kg*m^-3)
  I <- I # thermal inertia of the material(TIU)
  z <- z # lowest height of MOST applied (m)
  T0 <- 273.15 # freezing point
  Ts <- Ts + T0 # Ts in K
  Tr <- 300 # reference temperature
  Cp <- 1006 # specific heat of air
  Rv <- 461.5 # gas constant of water vapor
  a <- 1 # coefficient of water version
  g <- 9.81 # gravitational acceleration
  k <- 0.4 # Von Karman constant
  Lv <- 2.5E06 # vaporization heat of water vapor
  Liv <- 2.83E06 # vaporization heat of ice
  alpha <- 1 # MOST constant
  beta <- 4.7 # MOST constant
  r1 <- 15 # MOST constant
  r2 <- 9 # MOST constant
  C1 <- matrix(c(sqrt(3) / alpha, 2 / (1 + 2 * alpha)), ncol = 2, byrow = FALSE) # MEP constant
  C2 <- matrix(c(r2 / 2, 2 * beta), ncol = 2, byrow = FALSE) # MEP constant
  e16 <- 1.0 / 6 # exponent

  # Calculate Variables
  stab <- ones(1, length(Rn)) # Create vector of stability
  stab[which(Rn < 0)] <- 0 # Calculate Variables
  I0 <- zeros(1, length(Rn)) # Create vector of I0
  unsta <- which(stab == 1) # Unstable condition
  sta <- which(stab == 0) # Stable condition

  if (1 - isempty(unsta)) {
    I0[unsta] <- rhoa[unsta] * Cp * sqrt(C1[, 1] * k * z) * (C2[, 1] * k * z * g / (rhoa[unsta] * Cp * Tr))^(e16)
  }

  if (1 - isempty(sta)) {
    I0[sta] <- rhoa[sta] * Cp * sqrt(C1[, 2] * k * z) * (C2[, 2] * k * z * g / (rhoa[sta] * Cp * Tr))^(e16)
  }
  ice_num <- which(Ts < T0) # ice/snow condition (only used in water-snow-ice version type = 3)
  Lv_vec <- ones(1, length(Ts)) * Lv # create Lv vector
  Lv_vec[ice_num] <- Liv
  sg <- sqrt(a) * Lv_vec^2 * qs / (Cp * Rv * Ts^2) # sigma computation
  B <- 6 * (sqrt(1 + 11 / 36 * sg) - 1) # B computation
  Bsr <- B / sg # B/sigma
  IdI0 <- I / I0 # I/I0 Stable condition
  # grab effective data (by skipping NaN values)
  id_1 <- which(is.na(Rn) == 0)
  id_2 <- which(is.na(RnL) == 0)
  id_3 <- which(is.na(qs) == 0)
  id_4 <- which(is.na(Ts) == 0)
  id_5 <- which(is.na(rhoa) == 0)
  id_6 <- which(is.na(I) == 0)
  id_7 <- which(is.infinite(IdI0) == 0)
  id_8 <- which(is.na(IdI0) == 0)

  inds <- intersect(id_1, id_2)
  inds <- intersect(inds, id_3)
  inds <- intersect(inds, id_4)
  inds <- intersect(inds, id_5)
  inds <- intersect(inds, id_6)
  inds <- intersect(inds, id_7)
  inds <- intersect(inds, id_8)

  # MEP calculation
  HMEP <- zeros(1, length(inds))
  EMEP <- zeros(1, length(inds))
  GMEP <- zeros(1, length(inds))

  if (type == 1) { 
    # Bare Soil
    for (i in c(1:length(inds))) {
      j = inds[i]
      temp <- fzero(function(H) {
        abs(H)^(e16) * (B[j] + 1) * H + Bsr[j] * IdI0[j] * H - abs(H)^(e16) * Rn[j]
      }, 0.5 * Rn[j])[1]
      
      HMEP[j] <- temp[[1]]
      EMEP[j] <- B[j] * HMEP[[j]]
      GMEP[j] <- Rn[j] - EMEP[j] - HMEP[[j]]
    }
  } else if (type == 2) { 
    # Canopy
    for (i in c(1:length(inds))) {
      j = inds[i]
      temp <- fzero(function(H) {
        (B[j] + 1) * H - Rn[j]
      }, 0.5 * Rn[j])[1]
      
      HMEP[j] <- temp[[1]]
      EMEP[j] <- Rn[j] - HMEP[[j]]
      GMEP[j] <- 0
    }
  } else { 
    # Water-snow-ice (type == 3)
    for (i in c(1:length(inds))) {
      j = inds[i]
      temp <- fzero(function(H) {
        abs(H)^(e16) * (B[j] + 1) * H + Bsr[j] * IdI0[j] * H - abs(H)^(e16) * Rn[j]
      }, 0.5 * Rn[j])[1]
      HMEP[j] <- temp[[1]]
      EMEP[j] <- B[j] * HMEP[j]
      GMEP[j] <- RnL[j] - HMEP[j] - EMEP[j]
    }
  }
  return(list(ETMEP = EMEP * 0.0352653, HMEP = HMEP, EMEP = EMEP, GMEP = GMEP, inds = inds)) # 1W/m2=0.0352653 mm/day
}


#' Calculate potential ET
#'
#' Calculate energy fluxes and potential evapotranspiration based on the MEP model
#' @param Rn net radiation(unit:W/m2)
#' @param RnL net long-wave radiation(unit:W/m2)
#' @param Ts surface temperature(unit:Celsius)
#' @param I media thermal inertial (units:TIU),default is 600 TIU
#' @param z theoretical height above surface (unit:m),constant value and default is 2.5 m
#' @param type 1 for bare soil surface or short canopy, 2 for dense canopy and 3 for Water-snow-ice surface
#' @return A list concludes latent heat flux(EMEP,W/m2),sensible heat flux(HMEP,W/m2),ground heat flux(GMEP,W/m2) and potential ET(PETMEP,W/m2)
#' @importFrom pracma ones zeros isempty fzero
#' @importFrom humidity C2K SVP.ClaCla SH
#' @examples
#' RMEPPET(Rn = 200, RnL = 100, Ts = 20, type = 1)
#' RMEPPET(Rn = 300, RnL = 100, Ts = 25, I = 800, z = 8, type = 1)
#' @export
RMEPPET <- function(Rn, RnL, Ts, I = 800, z = 2.5, type = 1) {
  t_temp <- C2K(Ts)
  Es <- SVP.ClaCla(t_temp) # calculating saturation vapor pressure Es
  SShums <- SH(Es * 100, p = 101325) # calculating saturated specific humidity
  output <- RMEP(Rn <- Rn, RnL <- RnL, qs <- SShums, Ts <- Ts, type = type)
  print("RMEPPET completed!")
  PETMEP <- output$ETMEP
  HMEP <- output$HMEP
  EMEP <- output$EMEP
  GMEP <- output$GMEP
  inds <- output$inds
  return(list(PETMEP = PETMEP, HMEP = HMEP, EMEP = EMEP, GMEP = GMEP, inds = inds))
}




#' Implementing MEP model with NetCDF format data
#'
#' Calculate actual evapotranspiration by the MEP model with data of NetCDF format. The dimensions of all inputs must be consistent.
#' @param Rn net radiation (unit:W/m2)
#' @param RnL net long-wave radiation (unit:W/m2)
#' @param Ts surface temperature (unit:Celsius)
#' @param qs specific humidity (unit:kg/kg)
#' @param I media thermal inertial (units:TIU)
#' @param z theoretical height above surface (unit:m)
#' @param type 1 for bare soil surface or short canopy, 2 for dense canopy and 3 for Water-snow-ice surface
#' @return Latent heat flux (unit:W/m2)
#' @importFrom pracma ones zeros isempty fzero
#' @examples
#' RMEP_nc(Rn, RnL, qs, Ts, I, z, type)
#' @export
RMEP_nc <- function(Rn, RnL, qs, Ts, I, z, type) {
  MEPLE <- array(NA, dim(Rn))
  MEPLEcal <- function(Rn, RnL, qs, Ts, I, z, type) {
    if (is.na(Rn) | is.na(RnL) | is.na(Ts) | is.na(qs) | is.na(z) | is.na(type)) {
      return(NA)
    }
    return(RMEP(Rn, RnL, qs, Ts, I, z, type)$EMEP)
  }

  for (ii in 1:length(Rn)) {
    MEPLE[ii] <- MEPLEcal(Rn[ii], RnL[ii], qs[ii], Ts[ii], I[ii], z[ii], type[ii])
  }
  return(MEPLE)
}
