#' Calculation of permutation-adjusted network smoothing index
#' @param X0 input matrix
#' @param A adjancency matrix
#' @param k number of permutations
#' @param eps numeric value
#' @param mc.cores number of cores
#' @param ... additional parameteres of network_propagation
#' @return \code{data.frame} with X0, Xs, S, p and Sp
#' @import parallel
#' @export
#'
calc_Sp <- function(X0, A, eps=1, k=99, mc.cores=1, ...){

  all_X0 <- c(list(X0), lapply(1:k, function(x) matrix(as.numeric(X0), ncol=1, dimnames = list(sample(rownames(X0), nrow(X0))))))
  all_X0 <- lapply(all_X0, function(x) x[match(rownames(A), rownames(x)), , drop=F ])

  cat("network propagation\n")
  if(mc.cores==1){
    all_Xs <- lapply(all_X0, function(x) network_propagation(x, A, ...)$Ft)
  }else{
    all_Xs <- parallel::mclapply(all_X0, function(x) network_propagation(x, A, ...)$Ft, mc.cores=mc.cores)
  }

  cat("calculation of Sp\n")
  all_S <- lapply(1:(k+1), function(i) nsi(all_X0[[i]], all_Xs[[i]], eps=eps))

  all_S_only <- lapply(all_S, function(x) x[, 3, drop=FALSE])

  est_p <- calc_p(all_S_only)

  Sp <- all_S[[1]]
  Sp$p <- as.numeric(est_p)
  Sp$Sp <- Sp$S * -log10(Sp$p)

  return(Sp)

}
