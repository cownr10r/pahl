#' Run the tuning algorithm results and the plot
#'
#' @param a The features object
#' @return The "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014" algorithms
#' @export
#' @examples
#' tune(features)


tune <- function(a = features){
        a
        result <- ldatuning::FindTopicsNumber(
                a,
                topics = seq(from = 2, to = 20, by = 1),
                metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                method = "Gibbs",
                control = list(seed = 77),
                mc.cores = 2L,
                verbose = TRUE
        )
        return(ldatuning::FindTopicsNumber_plot(result))
}
