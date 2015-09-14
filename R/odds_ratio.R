odds_ratio <- function(data, alpha = 0.05) {
	   
  results <- cbind(data, "odds_ratio" = NA, "ci_low" = NA, "ci_upp" = NA) 
  results[1, "odds_ratio"] <- 1
  cases_unexposed <- data[1, "cases"]
  controls_unexposed <- data[1, "controls"]
  
  for (i in 2:nrow(data)) {

    cases_exposed <- data[i, "cases"]
    controls_exposed <- data[i, "controls"]
    totExposed <- cases_exposed + controls_exposed
    totUnexposed <- cases_unexposed + controls_unexposed
    
    probDiseaseGivenExposed <- cases_exposed/totExposed
    probDiseaseGivenUnexposed <- cases_unexposed/totUnexposed
    probControlGivenExposed <- controls_exposed/totExposed
    probControlGivenUnexposed <- controls_unexposed/totUnexposed
    
    odds_ratio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
		  (probControlGivenExposed*probDiseaseGivenUnexposed)
     
    confidenceLevel <- (1 - alpha)*100
    sigma <- sqrt((1/cases_exposed)+(1/controls_exposed)+
		  (1/cases_unexposed)+(1/controls_unexposed))
     
    z <- qnorm(1 - (alpha/2))
    ci <- odds_ratio * c(exp(-z * sigma), exp( z * sigma))
    
    results[i, c("odds_ratio", "ci_low", "ci_upp")] <- c(odds_ratio, ci[1], ci[2])
    
  }
  results
}
