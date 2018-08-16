#' This will return a topics model generated through Gibbs sampling
#'
#' @param a An object named 'features' from tm_dfm
#' @param b The kappa number
#' @return A topic model from "topicmodels" with Gibbs Sampling
#' @export
#' @examples
#' mod <- ldamodel(features, 8)
#' model <- mod$gibbs
#' topics <- mod$words


ldamodel <- function(a = features, b = 0){
        SEED = sample(1:1000000, 1)
        gibbs <- topicmodels::LDA(a, b, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000))
        words <- terms(gibbs,30) %>% data.frame(., stringsAsFactors=F)
        kappa <- b
        seeds <- gibbs@control@seed
        result <- list(gibbs=gibbs, words = words, seed = seeds, kappa = kappa, text = text)
        return(result)
}
