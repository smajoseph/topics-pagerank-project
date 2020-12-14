library(rvest)
library(stringr)
library(hash)
library(text2vec)
library(quanteda)
library(readtext)
library(spacyr)
library(magrittr)
library(Rcrawler)
options(expressions = 5e5)

## in the terminal: $> pip3 install spacy;
##                  $> python3 -m spacy download en_core-web_sm

spacy_initialize(
    model="en_core_web_sm",
    python_executable = "/home/joe/anaconda3/bin/python3"
    )

load_embeddings <- function(path){
    
    embeds <- hash()
    f <- file(path, "r", blocking=F)
    raw_embeds <- readLines(f)
    
    ct <- 1
    for (em in raw_embeds){
        
        embed <- unlist(strsplit(em, ' ')) 
        embeds[embed[1]] <- as.numeric(embed[-1])
        ct <- ct + 1 
    }
    
    close(f)
    embeds
    
}

get_sentence_vector <- function(tokens){
    avg_vec <- 0
    for (t in tokens){
        avg_vec <- avg_vec + t
    }
    return(array(avg_vec/length(tokens)))
}

embed_path <- "~/tmp/topics-proj/glove.6B.50d.txt"
print(paste("Loading word embeddings from", embed_path, "...", sep=" "))
embeds <- load_embeddings(embed_path)
print("Done.")

build_document <- function(para_list){
    
    sent_dict <- hash()
    sent_list <- c()
    
    oldw <- getOption("warn")
    options(warn = -1)
    
    for (p in para_list){
        sent_list[length(sent_list) + 1 ] <- unlist(strsplit(str_to_lower(p), '\\.'))
    }
    
    options(warn = oldw)
    
    key <- 1
    for (s in sent_list){
        
        token_df <- spacy_parse(
            s,
            pos=F,
            tag=F,
            entity=F,
            additional_attributes = c("is_punct", "is_space", "is_bracket", "is_digit", "is_ascii")
            )
        tokens <- token_df[(token_df$is_space==FALSE &
                                token_df$is_punct==FALSE &
                                token_df$is_digit==FALSE & 
                                token_df$is_ascii==TRUE & 
                                token_df$is_bracket==FALSE),]$token
        
        vectors <- c()
        
        token_ct <- 1
        for (t in tokens){
            
            vec <- tryCatch(array(values(embeds[t])), error=function(e){})
            
            if (typeof(vec) != "NULL"){
                vectors[[token_ct]] <- vec
                token_ct <- token_ct + 1
            }
        }
        
        sentence_vec <- get_sentence_vector(vectors)
        
        if (sum(is.nan(sentence_vec)) == 0){
            sent <- hash(keys=c("raw", "vecs"))
            sent[["raw"]] <- s
            sent[['sentence_vector']] <- sentence_vec
            sent_dict[as.character(key)] <- sent
            key <- key + 1
        }
    }
    sent_dict
}

get_document_matrix <- function(sentence_dict){
    p <- length(keys(sentence_dict))
    doc <- matrix(0,p,50)
    
    for (s in keys(sentence_dict)){
        doc[as.numeric(s),] <- sentence_dict[[s]]$sentence_vector
    }
    doc
}

get_document_matrix_from_url <- function(url){
    
    para_list <- unlist(ContentScraper(Url = url, XpathPatterns=c("//p"), ManyPerPattern=TRUE))
    document <- build_document(para_list)
    
    list(matrix=get_document_matrix(document), document=document)
}