#' Record the Probabilistic Event
#'
#' @param case_num The case number
#' @param model The model generated from ldamodel function
#' @param concept The thematic concept (viz. 'school', 'family', 'work')
#' @param doc The document in question
#' @param topic_num The topic number from the reviewed topics list.
#' @param math The number that needs to be subtracted from the beta sum, based on KWIC
#' @param ommitted_terms vocabulary terms rooted from the summary beta values based on human inspection.
#' @return An artifact qualifying as the record of the probabilistic event
#' @export
#' @examples
#' output <- betas(d = 1, model = model, concept = 'work', doc = doc, topic_num = 3, math = 0)

betas <- function(d = 0, model = model$Gibbs, concept = 'school', doc, topic_num = 0, math = 0, omitted_terms = 0){
topoi <- tidytext::tidy(model, matrix = "beta") %>%
dplyr::filter(., topic == topic_num)
topoi$topic <- NULL
topoi
ifelse(concept == 'school', beta_list <- dplyr::filter(topoi, grepl("school|college|cse|c.s.e|h.n.d.|H.N.D.|h.n.c.|o.n.d.|training|naval college|career|equivalent|leave|leaving|level*|university|exam|course|guilds|qualifications|grade|sheppey|comprehensive", term)),
ifelse(concept == 'work', beta_list <- dplyr::filter(topoi, grepl("job|apprentice*|factory|technician|foreman|tool-making|dole|unemploy*|r.a.f|electric*|airport|tudor|bp|firm|gatwick|medway|intelligence|corps|rochester|plumbing|ltd|grundig|elliots|chatham|heating|landscaper|corps|milkman|artillery|sargeant|police|sewing|shop|officer|fitter|turner|junior|navy|packer|insurance|navy|naval|architect|shell|fire|lorry|employ*|manager|technological|magazine|garage|seam*|retire*|carpet|mill|shorthand|typi*|Woolworths|mechanic|clerk|abbots|business|promotion|assistant|teacher|army|secretar*|clerical|hotel|interview|bank|dockyard|salon|builder|nurse|nursing|blind|attendant|keresley|cook|groom|unemployed|dole|barclays|engineer*|receptionist|supermarket|library|librarian|ministry|defense|hairdress*|consultant|w.r.n.s|accountant|service|royal|airforce|geologist", term)),
ifelse(concept == 'family', beta_list <- dplyr::filter(topoi, grepl("wife|marriage|marry|companion|married|remarry|remarried|child|children|husband|son|daughter|family|girl|boy|kid|mother|divorce|baby|babies|father|grandfather|grandmother|grandparent|grandparents|her|she|family", term)),
this <- dplyr::filter(topoi, grepl("blah", term)))))
beta_list <- data.frame(beta_list)
sum_this <- sum(beta_list$beta)

document <- quanteda::tokens(doc, what = "word")

dic <- quanteda::dictionary(list(school = c('school','college','cse','c.s.e', 'h.n.d.','H.N.D.','h.n.c.','o.n.d.','training','naval college','career', 'equivalent','level','levels','university','exam','exams','levels','course',  'guilds','qualifications','grade','sheppey','comprehensive'),
work = c('job','jobs','apprentice*','interviews','factory','technician','foreman','tool-making','dole','unemploy*','r.a.f','electric*','airport','tudor','bp','firm','gatwick','medway','intelligence','corps','rochester','plumbing','ltd','grundig','elliots','chatham','heating','landscaper','corps','milkman','artillery','sargeant','police','sewing','shop','officer','fitter','turner','junior','navy','packer','insurance','navy','naval','architect','shell','fire','lorry','employ*','manager','technological','magazine','garage','seam*','retire*','carpet','mill','shorthand','typi*','Woolworths','mechanic','clerk','abbots','business','promotion','assistant','teacher','army','secretar*','clerical','hotel','interview','bank','dockyard','salon','builder','nurse','nursing','blind','attendant','keresley','cook','groom','unemployed','dole','barclays','engineer*','receptionist','supermarket','library','librarian','ministry','defense','hairdress*','consultant','w.r.n.s','accountant','service','royal','airforce','geologist'),
family = c('wife','companion','marry','married','child','husband','marriage','son','daughter','famil','girl','boy','kid','mother','remarry','remarried','divorce','baby','babies','housewife','father','grandfather','grandmother','divorced','grandparent','her','she','family')))

units <- quanteda::tokens_lookup(document,dic) %>%
		dfm(.)
context <- quanteda::kwic(document,dic[concept], 8)

subtraction <- sum_this - math

topic_num <- topic_num

result1 <- list(case_num = d, concept = concept, topic_number = topic_num, beta_list = beta_list, sum = sum_this, dfm = units, context = context, subtracting_betas = math, final_betas = subtraction, ommitted_terms = omitted_terms) %>% return(.)

result2 <- list(case_num = d, topic_number = topic_num, concept = concept, beta_list = beta_list, sum = sum_this, dfm = units, context = context) %>% return(.)

ifelse(math == 0, return(result2), return(result1))
}
