'
main code for blog summarization
you may have to change the paths to .R files & embeds .txt

embeds can be downloaded from:
http://nlp.stanford.edu/data/glove.6B.zip

use the 50 dimension set
glove.6B.50d.txt
'

source("~/tmp/topics-proj/scrape_html.R")
source("~/tmp/topics-proj/pagerank.R")


print_topn <- function(ledger, rank, n){
    for (i in 1:n){
        j <- which.max(rank)
        sent <- values(ledger[[as.character(j)]], keys="raw")
        print(paste(paste(i, ':', sep=''), sent, paste("(", round(rank[j], 5), ")", sep=''), sep=' '))
        rank[j] <- 0
    }
}

# wiki 
url <- "https://en.wikipedia.org/wiki/Hayabusa2"

# podcast transcript
url <- "https://www.wnycstudios.org/podcasts/anthropocene-reviewed/episodes/anthropocene-reviewed-john-green-plague"

# web md
url <- "https://www.webmd.com/heart-disease/guide/heart-disease-diagnosis#1"

# build document matrix 
document <- get_document_from_url(url)

## get the adjacency matrix 
mat <- get_document_adjacency(document$matrix, thresh=0.9)

## get the pagerank
rank <- PageRank(mat, iter=10000)

# then we recover the plaintext sentences by keeping a ledger of col_num <-> sentence 
ledger <- document$ledger

# display top n

print_topn(ledger, rank, 5)