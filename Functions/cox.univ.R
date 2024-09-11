########################################
#' Function to compute univariate Cox
#' Karine Thai - 2024/02/20
########################################
 
cox.univ <- function(df, par, time, stat) { 
  #' @param df dataframe containing parameters of interest, column time for time-to-event, and column status 0 (lost to follow-up) or 1 (relapse)
  #' @param par column names of parameters to test in Cox regression
  #' @param time column name of parameter time
  #' @param stat column name of parameter status
  
  library(survival)
  
  # Extract time and status
  time <- df[,time]
  status <- df[,stat]
  
  # Initialize a data frame to store results
  res <- data.frame(Parameter = character(), Hazard_Ratio = numeric(), Lower_ci = numeric(), Upper_ci = numeric(), p_value = numeric(), row.names = NULL)
  
  # Iterate over each parameter column
  for (p in par) {
    cov <- df[, p]
    
    # Perform Cox regression
    fit <- coxph(Surv(time, status) ~ cov, data = df)
    
    # Extract hazard ratio and p-value
    hr <- exp(fit$coefficients)
    ci <- confint(fit, level = 0.95)
    lower_ci <- exp(ci[, 1])
    upper_ci <- exp(ci[, 2])
    
    pvals <- summary(fit)$coefficients[1, "Pr(>|z|)"]
    
    # Store results in the result data frame
    res <- rbind(res, data.frame(Parameter = p, Hazard_Ratio = hr, Lower_ci = lower_ci, Upper_ci = upper_ci, p_value = pvals))
  
  }
  res[,'p.adj']=p.adjust(res[,'p_value'], method='fdr', n=length(res[,'p_value']))
  # Return the result
  return(res)
}
