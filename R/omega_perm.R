#' Calculation of permuted omega function
#' @param edge_list edge list
#' @param dS scores vector; it must have the same names and size of the vertices of G
#' @param idx vector of randomly resampled dS scores; the names must be the same as dS's


omega_perm <- function(idx, edge_list, dS) {


#   lp <- rep(0, n)
#
#   for (i in 1:n){
#
#     Gi <- igraph::induced.subgraph(G, match(names(temp)[1:i], V(G)$name)) # note that the vertex order is not the same as in temp
#
#     Ai <- as.matrix(get.adjacency(Gi)) # extract local adjacency matrix
#
#     idx.sorted <- sort(idx[match(rownames(Ai), names(idx))]) #order the matrix as idx
#     Ai.idx <- match(names(idx.sorted), rownames(Ai))
#     Ai <- Ai[Ai.idx, Ai.idx]
#
#     lp[i] <- t(temp[1:i]) %*% Ai %*% temp[1:i] # lp computation
#   }


  #Gi <- igraph::induced.subgraph(G, match(names(dS), V(G)$name)) # note that the vertex order is not the same as in temp
  #Ai <- as.matrix(get.adjacency(Gi)) # extract local adjacency matrix

  #assign numeric ids to labels
  ids <- as.numeric(as.factor(names(dS)))
  n <- length(dS)

  #only edges between the considered vertices
  el <- edge_list[edge_list[, 1] %in% names(dS) & edge_list[, 2] %in% names(dS), , drop=F]

  if(nrow(el)>0){

    el_ids <- matrix(0, nrow = nrow(el), ncol=ncol(el))
    el_ids[, 1] <- ids[match(el[, 1], names(dS))]
    el_ids[, 2] <- ids[match(el[, 2], names(dS))]
    Ai <- matrix(0, n, n, dimnames = list(names(dS)[order(ids)]))
    Ai[el_ids] <- 1
    #ensure that is simmettric
    if(!isSymmetric(Ai)){
      Ai <- sign(Ai + t(Ai))
    }

  #for x 2:l, create vectors of idx referring only to elements in dS, sorted by idx
  idx_2_n <- lapply(2:length(dS), function(x) sort(idx[names(idx) %in% names(dS)[1:x]]))

  omega_vect <- dS %*% t(dS) #cross product of correct values

  omega_vect <- c(0, unlist(lapply(idx_2_n, calc_omega_i, Ai=Ai, dSprod=omega_vect)))

  }else{
    omega_vect <- dS
    omega_vect[] <- 0
  }

  return(omega_vect)
}
