#' Calculation of omega i (internal)
#' @param idx index
#' @param Ai matrix
#' @param dSprod matrix with products of delta S
calc_omega_i <- function(idx, Ai, dSprod){

  Ai.idx <- match(names(idx), rownames(Ai)) #order the matrix as idx
  Ai_perm <- Ai[Ai.idx, Ai.idx]

  omega_vect_i <- dSprod[1:length(idx), 1:length(idx)] * Ai_perm
  omega_vect_i <- sum(omega_vect_i)

  return(omega_vect_i)

}
