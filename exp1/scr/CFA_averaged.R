CFA_averaged <- function(data=NULL,vignettes=NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  if(is.null(vignettes)){
    stop("vignettes is NULL. Please provide 2-3 strings.")
  }

  N <- length(unique(data$ID))
#  vignettes <- c("E","D","R")
#  vignettes <- c("F","M")
  # make into loop that puts these into a list
  AVE<-list()
  for (v in 1:length(vignettes)) {
    tmp <- dplyr::filter(data,vignette==vignettes[v])
    tmp_w<-pivot_wider(tmp, names_from = item_label, values_from = value,id_expand = FALSE,id_cols = c('ID'))
    tmp_w<-data.frame(tmp_w)
    rownames(tmp_w) <- tmp_w$ID
    tmp_w <- select(tmp_w, -ID)
#    colnames(tmp_w)<-questions$ID
    cm <- cor(tmp_w, method = "pearson")
    AVE[[v]]<-cm
    #par <- fa.parallel(tmp_w, fa="fa",cor="cor",fm="minres")
    #par <- fa.parallel(cm, fa="fa",cor="cor",fm="minres",plot = TRUE, n.obs=18*2)
    #  KMO(cm)
    fac <- principal(cm,nfactors = length(vignettes),n.obs = N)
    #print(vignettes[v])
    #print(fac$Structure,digits = 3,cut = 0.3,short = TRUE)
  }

  AVE_T<-list()
  for (k in 1:length(vignettes)) {
    AVE_T[[k]]<-fisherz(AVE[[k]])
  # AVE_T[[2]]<-fisherz(AVE[[2]])
  # AVE_T[[3]]<-fisherz(AVE[[3]])
  }
  if(length(vignettes) == 2){
    MEAN_T <- (AVE_T[[1]] + AVE_T[[2]]) / 2
  } else if (length(vignettes) == 3){
    # If there are three vignettes, average them
    # This is the case for EDR, FM, and PEP
    MEAN_T <- (AVE_T[[1]] + AVE_T[[2]]+ AVE_T[[3]]) / 3

    if(length(AVE_T) != 3){
      stop("Expected exactly three vignettes for averaging.")
    }
  } else {
    stop("vignettes must be of length 2 or 3.")
  }

  MEAN_cm <- fisherz2r(MEAN_T)
  diag(MEAN_cm) <- 1 # put the diagonal back to 1

  fac <- principal(MEAN_cm,nfactors = length(vignettes),n.obs = N) #????
  print(fac$Structure,digits = 3,cut = 0.1,short = TRUE)

  return(fac)

}

