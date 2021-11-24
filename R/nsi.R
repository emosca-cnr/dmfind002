#' Network Smoothing Index
#' @param x input matrix/vector
#' @param y smoothed matrix/vector
#' @param eps numeric value
#' @return data.frame with X0, Xs, and S
#'
nsi <- function(x, y, eps=1){

  if(!(identical(rownames(x), rownames(y))))
    stop('rownames(x) and rownames(y) are not identicail\n')


  if(ncol(x)==1 & ncol(y)==1){

    X0 <- x
    Xs <- y

    out <- data.frame(
      X0 = X0,
      Xs = Xs,
      row.names = rownames(x)
    )

  }else{

    if(!(identical(colnames(x), colnames(y))))
      stop('colnames(x) and colnames(y) are not identicail\n')

      X0 <- rowMeans(x)
      Xs <- rowMeans(y)

    out <- data.frame(
      X0 = X0,
      Xs = Xs,
      row.names = rownames(x)
    )

  }

  colnames(out) <- c('X0', 'Xs')

  out$S <-  out$Xs / (out$X0 + eps)

  return(out)
}
