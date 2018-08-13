#' Convert the corpus to prepare for ldatuning
#'
#' @param doc Referring to the object created by the essay function
#' @return Text number, Token Types, Sentence numbers, and Title. An abbreviated version of summary in quanteda
#' @export
#' @examples
#'features <- tm_dfm(doc)

tm_dfm <- function(doc = doc){
        doc %>%
        quanteda::dfm(., tolower=T, remove_punct = T, remove = stopwords('en')) %>%
        quanteda::convert(., to = "tm")
}

