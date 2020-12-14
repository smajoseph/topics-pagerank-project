library(lsa)
source("~/tmp/topics-proj/scrape_html.R")

url <- "http://www.york.ie"

# build document matrix 
doc_mat <- get_document_matrix_from_url(url)

## get the adjacency matrix 
get_document_adjacency <- function(doc_mat, thresh=0.9){
    da <- cosine(doc_mat)
    da[da>thresh]
}

####### Final output will be of the form #######################################
##                                                                            ##
##         [1, 0, ..., 0, 1,                                                  ##
##          0, 1, ..., 0, 1,                                                  ##
##          .  .       .  .                                                   ## 
##          .  .       .  .                                                   ##
##          .  .       .  .                                                   ##
##          1, 0, ..., 1, 0]                                                  ##
##                                in {0,1}^(p x p)                            ##
##                                                                            ##
################################################################################

################################################################################
##                                                                            ##
##             MAIN STUFF                                                     ##
##                                                                            ##
################################################################################



## rank document with PR alg here

# TODO: hook in angyalka's algorithm 

# then we recover the plaintext sentences by keeping a ledger of col_num <-> sentence 

# TODO: write function to pretty print the top N sentences 