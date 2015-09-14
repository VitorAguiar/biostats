relative_risk <- function(data, alpha = 0.05, nrows = 2) {
	
  for (i in 1:nrow(data)) {

    DiseaseUnexposed <- data[nrows,1]
    ControlUnexposed <- data[nrows,2]

  if (i < nrows) {
    
    DiseaseExposed <- data[i,1]
    ControlExposed <- data[i,2]
    totExposed <- DiseaseExposed + ControlExposed
    totUnexposed <- DiseaseUnexposed + ControlUnexposed
    
    probDiseaseGivenExposed <- DiseaseExposed/totExposed
    probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
    
    relativeRisk <- probDiseaseGivenExposed/probDiseaseGivenUnexposed
    print(paste0("category = ", rownames(data)[i], 
		 ", relative risk = ", relativeRisk))
    
    confidenceLevel <- (1 - alpha)*100
    sigma <- sqrt((1/DiseaseExposed) - (1/totExposed) + 
		  (1/DiseaseUnexposed) - (1/totUnexposed))
    
    z <- qnorm(1 - (alpha/2))
    ci <- relativeRisk * c(exp(-z * sigma), exp(z * sigma))
    print(paste0("category = ", rownames(data)[i], ", ", confidenceLevel,
		 "% confidence interval = [", ci[1],", ", ci[2],"]"))
    }
  }
}
