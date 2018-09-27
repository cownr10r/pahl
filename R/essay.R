#' Create a corpus object
#'
#' @param a The file directory holding the data set
#' @param b The file in question
#' @param d The case number
#' @param e The row number for texts
#' @return The corpus object
#' @export
#' @examples
#' doc <- essay(data_dir, file, 1)


thisessay <- function(a = data_dir, b = file, d = 0, e = 0){
        composition <- openxlsx::read.xlsx(paste0(a, b)) %>%
                .[d,c(d,e)] %>%
                quanteda::corpus(., text_field = "section3")
        return(composition)
}
