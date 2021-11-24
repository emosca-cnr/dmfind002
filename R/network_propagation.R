#' Network propagation
#' @param F0 vector or matrix composed of column vectors with initial distribution of information
#' @param A normalized adjacency matrix
#' @param alpha numeric, the smothing factor
#' @param nMax numeric, maximum number of iterations
#' @param eps numeric, the iteration will stop when the maximum difference between matrix Ft between two consecutive iteraction is smaller than \code{eps}
#' @param final.smooth TRUE/FALSE, whether to do the final step of smoothing
#' @param all.steps, TRUE/FALSE, whether to store all steps
#' @param verbose, TRUE/FALSE
#' @return a list with:
#' \itemize{
#' \item{\code{Ft}}{ the smoothed matrix;}
#' \item{\code{eps}}{ see above;}
#' \item{\code{max.abs.diff}}{ max(abs(F_t) - abs(F_{t-1}));}
#' \item{\code{Ft.all}}{ transient Ft matrices.}
#' }
network_propagation <- function(F0, A, alpha=0.7, nMax=1e4, eps=1e-6, final.smooth=FALSE, all.steps=FALSE, verbose=FALSE){

  Ft <- F0
  Fprev <- F0

  if(all.steps){
    Ft.all <- list()
    Ft.all[[1]] <- F0
  }

  F0 <- (1 - alpha) * F0
  A <- alpha * A

  for(i in 2:nMax){

    if(i %% 5 == 0 & verbose)
      cat(i, " ")

    Ft <- A %*%  Fprev + F0

    if(all.steps)
      Ft.all[[i]] <- Ft

    max.abs.diff <- max(abs(Ft-Fprev))
    Fprev <- Ft

    if(max.abs.diff < eps){
      if(final.smooth)
        Ft <- A %*%  Fprev
      break
    }

  }

  if(verbose)
    cat('\n')

  if(all.steps){

    return(list(Ft=Ft, eps=eps, max.abs.diff=max.abs.diff, Ft.all=Ft.all))
  }else{

    return(list(Ft=Ft, eps=eps, max.abs.diff=max.abs.diff))
  }
}



