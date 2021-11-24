#' Calculation of omega function
#' @param edge_list edge list
#' @param dS scores vector; it must have the same names and size of the vertices of G


omega <- function(edge_list, dS) {

  #   omega_vect <- rep(0, n)
  #
  #   for (i in 1:n){
  #
  #     Gi <- igraph::induced.subgraph(G, match(names(temp)[1:i], V(G)$name)) # note that the vertex order is not the same as in temp
  #
  #     Ai <- as.matrix(get.adjacency(Gi)) # extract local adjacency matrix
  #     idx.norm <- match(names(temp[1:i]), rownames(Ai)) # appropriate setting of Ai and temp names
  #     Ai <- Ai[idx.norm, idx.norm]
  #
  #     #implementation 1
  #     omega_vect[i] <- t(temp[1:i]) %*% Ai %*% temp[1:i] # omega_vect computation
  #
  #   }

  #implementation 2
  #Gi <- igraph::induced.subgraph(edge_list, match(names(dS), V(G)$name)) # note that the vertex order is not the same as in dS
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
    idx.norm <- match(names(dS), rownames(Ai)) # appropriate setting of Ai and dS names
    Ai <- Ai[idx.norm, idx.norm]

    omega_vect <- dS %*% t(dS)
    omega_vect <- omega_vect * Ai
    omega_vect[upper.tri(omega_vect)] <- 0
    omega_vect <- 2*cumsum(rowSums(omega_vect))

  }else{
    omega_vect <- dS
    omega_vect[] <- 0
  }

  return(omega_vect)
}
