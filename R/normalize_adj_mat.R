#' Symmetric Normalization of Adjancency Matrix
#' @details This function applies the following normalization: a_ij' = a_ij / sqrt(d_i d_j), where d_i is the degree of vertex i
#' @export
#' @param A adjacency matrix
#' @return normalized adjacency matrix
normalize_adj_mat <- function(A){

  #dii = degree(i)
  #aij' = aij / sqrt(dii * dij)

  ArowSums <- sqrt(rowSums(A))
  ArowSumsMat <- matrix(rep(ArowSums, dim(A)[1]), dim(A)[1], dim(A)[2])
  AcolSumsMat <- matrix(rep(ArowSums, dim(A)[1]), dim(A)[1], dim(A)[2], byrow=TRUE)
  Anorm <- A / ArowSumsMat / AcolSumsMat
  rownames(Anorm) <- rownames(A)
  colnames(Anorm) <- colnames(A)

  return(Anorm)
}
