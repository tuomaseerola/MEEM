CFA_items <- function(data = NULL,vignette=c(1,2,3),items=NULL,vignette_name="EDR",bonusquestion="GEMIAC"){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }



if(bonusquestion=="GEMIAC"){
  data <- dplyr::select(data,-Emotion,-Emotion2)
}
  if(bonusquestion=="HAAS"){
    data <- dplyr::select(data,-HAAS_1,-HAAS_2,-HAAS_3,-HAAS_4,-HAAS_5,-HAAS_6,-HAAS_7,-HAAS_8,-HAAS_9,-HAAS_10,-HAAS_11,-HAAS_12)
  }

  # exception for modelling all episodes,
  if(vignette_name=="ALL"){
    tmp2 <- data # do nothing, keep all episodes in
  } else {
    tmp2 <- dplyr::filter(data,itemCategory==vignette_name) # ?? sus
  }

  TMP3<-NULL
  for (k in 1:length(vignette)) {
    TMP3 <- rbind(TMP3,dplyr::filter(tmp2,VigNro==vignette[k])) # OK
  }


  tmp_w <- pivot_wider(TMP3, names_from = item_label, values_from = value,id_expand = FALSE,id_cols = c('ProlificID','VigNro'))
  tmp_w<-data.frame(tmp_w)
  rownames(tmp_w) <- 1:nrow(tmp_w)

  # keep ProlificID and VigNro for now!
  ratings <- tmp_w

#_  tmp_w <- dplyr::select(tmp_w, -ProlificID)
#_  ratings <- dplyr::select(tmp_w,-VigNro)

  #fit2 <- lavaan::cfa(model, data = ratings,meanstructure = TRUE)
  #print(fit2)
  #fit.meas <- lavaan::fitMeasures(fit,c("tli","chisq","df","pvalue", "cfi", "rmsea", "srmr"))
  #print(knitr::kable(fit.meas,digits=2))
  #ds <- semTools::discriminantValidity(object = fit2)
  #print(ds)

#  results<-list(fit.meas)
  return(ratings)

}
