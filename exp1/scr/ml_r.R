ml_r <- function(data = NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

#function for multilevel reliability estimates (1: items, 2: vignettes, 3: participants)

# Compute multilevel reliability coefficients using psych::multilevel.reliability
ml_r_results <- multilevel.reliability(data,
                                      grp = "ID",           # participants
                                      Time = "vignette",    # vignettes
                                      items = "item_label", # number of items
                                      values = "value",
                                      alpha = TRUE,  # Cronbach's alpha
                                      icc = TRUE,    # intraclass correlations
                                      lmer = FALSE,  # variance components via lmer
                                      long = TRUE, all=TRUE)

# Examine the reliability results
print(ml_r_results)
# The output includes generalizability coefficients:
#  - e.g., RkF (reliability of averages across all items and occasions),
#  - RkR (reliability of person mean across trials),
#  - RkRn (consistency of between-person differences over time),
#  - Rc (generalizability of change across trials),
#  - Rcn (generalizability of within-person item variability),
#  - R1R (reliability of a single trial measurement).

}
