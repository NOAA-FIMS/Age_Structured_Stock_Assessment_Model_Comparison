#' @export
convertSRparms <- function(R0, h, phi, sigmaR, mean2med, model){
  BC <- ifelse(mean2med == TRUE, exp(-0.5 * sigmaR^2), exp(0.5 * sigmaR^2))
  if (model==1) {
    S0BC <- (BC * 0.8 * R0 * h * phi - 0.2 * phi * R0 * (1 - h)) / (h - 0.2)
    R0BC <-  S0BC / phi
    Rnew <- BC * 0.8 * R0 * h * 0.2 * S0BC / (0.2 * phi * R0 * (1 - h) + (h - 0.2) * 0.2 * S0BC)
    hBC <- Rnew / R0BC
  }

  if (model==2) {
    S0BC <- R0 * phi * (1 + log(BC) / h)
    R0BC <- S0BC / phi
    Rnew <- BC * 0.2 *S0BC / phi * exp(h * (1 - 0.2 * S0BC/ (R0 * phi)))
    hBC <- 1.25*log(5*Rnew /R0BC)
  }

  return(list(S0BC = S0BC, R0BC = R0BC, hBC = hBC))
}
