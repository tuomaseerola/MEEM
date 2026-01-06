participant_reliability <- function(data = NULL,THRESHOLD = FALSE,verbose = FALSE){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  if(THRESHOLD==FALSE){
    THRESHOLD <- -1 # correlation threshold for removing participants (66 obs, with alpha 0.05 from auxiliary_stuff.R) # THIS ACTUALLY WORKS PRETTY WELL !!!!!!
  }

  # Option 1: across vignettes

  removed_participants <- NULL
  alphas_before<-NULL
  alphas_after<-NULL


  for (k in 1:12){
    if(verbose){
      print(paste("------------ Vignette:",k))
    }
    d <- dplyr::filter(data, VigNro == k)

    tmp_w <- pivot_wider(d, names_from = item_label, values_from = value,id_expand = FALSE,id_cols = c('ProlificID'))
    IDs <- paste(tmp_w$ProlificID)
    tmp_w<-dplyr::select(tmp_w,-ProlificID)

    participants <- data.frame(t(tmp_w)) # to flip these to participants
    names(participants) <- IDs
    a <- suppressMessages(suppressWarnings(psych::alpha(participants, check.keys = FALSE, warnings = FALSE)))
    if(verbose){
      print(paste("Alpha:",round(a$total$raw_alpha,2)))
    }
    alphas_before<-c(alphas_before,a$total$raw_alpha)
    if(verbose){
      print("Eliminate the worst participants (r below THRESHOLD)")
    }
    eliminate_IDs <- rownames(a$item.stats)[a$item.stats$raw.r < THRESHOLD]

      if(length(eliminate_IDs) == 0){
        if(verbose){
          print("No participants to eliminate")
        }
        alphas_after<-c(alphas_after,a$total$raw_alpha)
        removed_participants<-c(removed_participants,eliminate_IDs)
    #  return(data)
    }
      else if(length(eliminate_IDs) > 0){
        if(verbose){
          print(paste("Number of participants to eliminate:",length(eliminate_IDs)))
          print(eliminate_IDs)
        }
        participants_trimmed <- participants[, !names(participants) %in% eliminate_IDs]
        a <- suppressMessages(suppressWarnings(psych::alpha(participants_trimmed, check.keys = FALSE, warnings = FALSE)))
        alphas_after<-c(alphas_after,a$total$raw_alpha)
        if(verbose){
          print(paste("Alpha:",round(a$total$raw_alpha,2)))
        }
        data <- dplyr::filter(data, !ProlificID %in% eliminate_IDs) # remove the participants with low
        if(verbose){
          print(paste("N after trimming:",length(unique(data$ProlificID))))
        }
        removed_participants<-c(removed_participants,eliminate_IDs)
    }
  }

    vals <- list(removed_participants=removed_participants,alphas_before=alphas_before,alphas_after=alphas_after)
  return(vals)

}
