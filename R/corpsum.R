#' Return the summary statistics on text found in an .xlsx file.
#' This requires the previous essay function to be written like this:
#' doc <- essay(data_dir, file, 1)
#' @param doc A character object returned from the essay function
#' @return Text number, Token Types, Sentence numbers, and Title. An abbreviated version of summary in quanteda
#' @export
#' @examples
#' corpsum(doc)



corpsum <- function(doc = doc){
        summ <- summary(corpus(doc, text_field = "section3"))
        print(summ)
        rm(summ)
}


