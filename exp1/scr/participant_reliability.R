participant_reliability <- function(data = NULL,THRESHOLD = FALSE){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  if(THRESHOLD==FALSE){
    THRESHOLD <- -1 # correlation threshold for removing participants (66 obs, with alpha 0.05 from auxiliary_stuff.R) # THIS ACTUALLY WORKS PRETTY WELL !!!!!!
  }

  # Option 1: across vignettes
  tmp_w <- pivot_wider(data, names_from = item, values_from = value,id_expand = FALSE,id_cols = c('ID'))
  IDs <- paste(tmp_w$ID)
  tmp_w<-dplyr::select(tmp_w,-ID)

  # Option 2: preserve vignettes
  #tmp_w <- pivot_wider(df, names_from = itemID, values_from = value,id_expand = FALSE,id_cols = c('ID','vignette'))
  #IDs <- paste(tmp_w$ID)
  #tmp_w<-dplyr::select(tmp_w,-ID)

  participants <- data.frame(t(tmp_w)) # to flip these to participants
  names(participants) <- IDs
  a <- suppressMessages(suppressWarnings(psych::alpha(participants, check.keys = FALSE, warnings = FALSE)))
  print(paste("Alpha:",round(a$total$raw_alpha,2))) #
  print("Eliminate the worst participants (r below THRESHOLD)")
  eliminate_IDs <- rownames(a$item.stats)[a$item.stats$raw.r < THRESHOLD]
  if(length(eliminate_IDs) == 0){
    print("No participants to eliminate")
    return(data)
  }
  else if(length(eliminate_IDs) > 0){
    print(paste("Number of participants to eliminate:",length(eliminate_IDs)))
    print(eliminate_IDs)
    participants_trimmed <- participants[, !names(participants) %in% eliminate_IDs]
    a <- suppressMessages(suppressWarnings(psych::alpha(participants_trimmed, check.keys = FALSE, warnings = FALSE)))
    print(paste("Alpha:",round(a$total$raw_alpha,2))) #
    data <- dplyr::filter(data, !ID %in% eliminate_IDs) # remove the participants with low
    print(paste("N after trimming:",length(unique(data$ID))))
  }

  return(data)

}
