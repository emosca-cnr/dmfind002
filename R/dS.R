#' Delta Network Smoothing Index
#' @param X0 input matrix
#' @param Xs smoothed matrix
#' @param classes vector of classes, values must be equal to 1 or 2
#' @param eps numeric value
#' @return named vector of delta smoothing indexes
#'
dS <- function(X0, Xs, classes, eps=1){


  nsi_2 <- nsi(X0[, classes==2, drop=F], Xs[, classes==2, drop=F], eps=eps)
  nsi_1 <- nsi(X0[, classes==1, drop=F], Xs[, classes==1, drop=F], eps=eps)

  d_21_nsi <- nsi_2$S - nsi_1$S
  names(d_21_nsi) <- rownames(nsi_2)

  dS_df <- data.frame(c1=nsi_1, c2=nsi_2,  dX0=nsi_2$X0 - nsi_1$X0, dXs=nsi_2$Xs - nsi_1$Xs, dS=d_21_nsi, row.names = rownames(nsi_2), stringsAsFactors = FALSE)

  return(dS_df)

}
