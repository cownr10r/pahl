#' Save the Output
#'
#' @param save_dir The directory path.
#' @param file_name The name of the file (without extension or dot), in quotes.
#' @return saved evidence in an RDS file to capture artifacts from the probabilistic event.
#' @export
#' @examples
#' filesave(data_dir, "case1")


filesave <- function(save_dir = in_quotes, file_name = in_quotes) {
         	case <- list(topics, school, work, family)
         	saveRDS(case, paste0(save_dir, file_name, d, ".RDS"))
         }
