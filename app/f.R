setwd("c:/R/TextAnalysisWithR")
input.dir <- "data/plainText"
files.v <- dir(input.dir, "\\.txt$")

# Function to print a vector of file names in user
# friendly format
show.files <- function(file.name.v){
    for(i in 1:length(file.name.v)){
        cat(i, file.name.v[i], "\n", sep=" ")
    }
}

show.files(files.v)

make.file.word.v.l <- function(files.v, input.dir){
    #set up an empty container
    text.word.vector.l <- list()
    # loop over the files
    for(i in 1:length(files.v)){
        # read the file in (notice that it is here that we need to know the input
        # directory
        text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                       what="character", sep="\n")
        #convert to single string
        text.v <- paste(text.v, collapse=" ")
        #lowercase and split on non-word characters
        text.lower.v <- tolower(text.v)
        text.words.v <- strsplit(text.lower.v, "\\W")
        text.words.v <- unlist(text.words.v)
        #remove the blanks
        text.words.v <- text.words.v[which(text.words.v!="")]
        #use the index id from the files.v vector as the "name" in the list
        text.word.vector.l[[files.v[i]]] <- text.words.v
    }
    return(text.word.vector.l)
    
}


my.corpus.l <- make.file.word.v.l(files.v, input.dir)
positions.v <- which(my.corpus.l[[1]][]=="gutenberg")