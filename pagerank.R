library(lsa)

PageRank <- function(A, d = 0.85, iter){
    A <- t(A)
    p <- nrow(A)
    # sink has no outgoing links,
    # replace with 
    sinks <- which(colSums(A)==0)
    fix.sinks <- rep(1, p)
    for (i in sinks) {
        A[,i] <- fix.sinks
    }
    # no self loops
    diag(A) <- 0
    deg <- colSums(A)
    M <- sweep(A, 2, deg, FUN = '/')
    M[is.na(M)] <- 0 # This is when we should correct for sinks
    e <- c(rep(1,p))
    # create list of initial ranks
    # initial ranks are equal to 1/(p)
    prev.ranks <- c(rep(1/p, p))
    # empty list to hold updated rank 
    new.ranks <- c(rep(0,p))
    # b is defined in the document
    b <- ((1-d)/p)*e
    # Iterate until convergence
    j = 0
    while (j < iter) {
        # w_j is defined in the document
        w_j <- M%*%prev.ranks
        new.ranks <- (d*w_j)+b
        prev.ranks <- new.ranks
        new.ranks <- c(rep(0,p))
        j = j+1
    }
    return(prev.ranks)
}

get_document_adjacency <- function(doc_mat, thresh=0.9){
    da <- cosine(doc_mat)
    diag(da) <- 0
    matrix(as.numeric(da>thresh), nrow=dim(da)[1])
}