#' This will save an instance of Gibbs topic models from the ldamodel function
#'
#' @param a topics object from ldamodel (from the general environment)
#' @param b The data directory
#' @param d The Case number
#' @return Text number, Token Types, Sentence numbers, and Title. An abbreviated version of summary in quanteda
#' @export
#' @examples
#' mod <- ldamodel(features, 8)
#' model <- mod$gibbs
#' topics <- mod$words
#'
#' savet(topics, data_dir, 1)


savet <- function(a = topics, b = data_dir, d = case){
        archivo <- b %>% append(.,case) %>% append(., ".tsv") %>% paste0(., sep = "", collapse = "")
        write.table(a, file= archivo, quote=TRUE, sep='\t')
}
