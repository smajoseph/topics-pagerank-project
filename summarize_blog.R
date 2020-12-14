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

# ex: url <- "http://www.cp.fart/"
url <- ""

# build document matrix 
document <- get_document_from_url(url)

## get the adjacency matrix 
mat <- get_document_adjacency(document$matrix, thresh=0.9)

## get the pagerank
rank <- PageRank(mat)

# then we recover the plaintext sentences by keeping a ledger of col_num <-> sentence 
ledger <- document$ledger

# TODO: write function to pretty print the top N sentences 