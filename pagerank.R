library(lsa)

PageRank <- function(A, d = 0.85, iter){
    
    # remove self-loops 
    diag(A) <- 0
    
    # find number of vertices p
    p <- nrow(A)
    # create list of initial ranks
    # initial ranks are equal to 1/(p)
    prev.ranks <- c(rep(1/p, p))
    # empty list to hold updated rank 
    new.ranks <- c(rep(0,p))
    # Iterate until convergence
    t = 0
    while (t < iter) {
        # iterate over all vertices U in the graph
        for (U in 1:p) {
            # get vertices V which U has incoming edges from
            incoming <- which(A[,U] == 1)
            # iterate over incoming vertices
            for (V in incoming) {
                # get number of outgoing edges from each vertex V
                # in incoming and update ranks
                outgoing <- length(which(A[V,] == 1))
                update <- prev.ranks[V]/outgoing
                # update
                new.ranks[U] = new.ranks[U] + update
            }
            # include damping factor
            new.ranks[U] <- ((1-d)/p) + d*new.ranks[U]
        }
        prev.ranks <- new.ranks
        new.ranks <- c(rep(0,p))
        t = t+1
    }
    return(prev.ranks)
}

get_document_adjacency <- function(doc_mat, thresh=0.9){
    da <- cosine(doc_mat)
    matrix(as.numeric(da>thresh), nrow=dim(da)[1])
}