#' Create a corpus object
#'
#' @param a The file directory holding the data set
#' @param b The file in question
#' @param d The case number
#' @return The corpus object
#' @export
#' @examples
#' doc <- essay(data_dir, file, 1)


essay <- function(a = data_dir, b = file, d = 0){
        composition <- read.xlsx(paste0(a, b)) %>%
                .[d, c(d,23)] %>%
                quanteda::corpus(., text_field = "section3")
        return(composition)
}
