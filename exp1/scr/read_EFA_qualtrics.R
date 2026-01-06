read_EFA_qualtrics <- function(filename=NULL,subconstructs=NULL,controlQ=NULL){

  print(filename)
  print(subconstructs)
  print(controlQ)

  tmp <- read.csv(filename, header = TRUE, na.strings = "")
  names(tmp) <- gsub("\\.", "", names(tmp)) # remove full stops from the column names
  tmp <- tmp[3:nrow(tmp), ]     # delete two first rows (metadata)

  for (i in 1:length(subconstructs)) {
    tmp <- tmp %>% mutate(across(starts_with(paste0(subconstructs[i],"_")), as.numeric)) # items into numeric
  }
  #Ensure only consenting participants to remove NA
  tmp <- filter(tmp,Consent==2)
  # convert into numeric
  tmp <- tmp %>% mutate(across(starts_with("Prog"), as.numeric))
  tmp <- tmp %>% mutate(across(starts_with("Dur"), as.numeric))
  tmp <- tmp %>% mutate(across(starts_with("Age"), ~ as.numeric(as.character(.))))
  tmp <- tmp %>% mutate(across(matches("^(Gender|OMSI|Music.)"), as.numeric)) # items into numeric
  # rename some columns
  names(tmp)[names(tmp)=="Age"]<- "Age"
  names(tmp)[names(tmp)=="Durationinseconds"]<- "Duration"

  print(paste("Number of participants in the data:", nrow(tmp)))
#  print(paste("Number of unique IDs:", length(unique(tmp$Prolific_ID))))
  ## Filtering
  tmp2 <- filter(tmp,Progress==100)
  tmp2 <- filter(tmp2,DistributionChannel!="preview")
  tmp2 <- filter(tmp2,Age!=99)
  print(paste("Number of participants after filter incomplete/preview/99 age:", nrow(tmp2)))
  # Eliminate unnecessary columns
  d <- dplyr::select(tmp2, -any_of(c('StartDate', 'EndDate', 'Status', 'Progress', 'Duration', 'Finished', 'RecordedDate', 'DistributionChannel', 'UserLanguage', 'ResponseId','Consent')))
  d <- dplyr::select(d, -any_of(c("IPAddress", "RecipientFirstName","RecipientLastName","RecipientEmail","LocationLatitude","LocationLongitude","ExternalReference","ID")))

#### Explicit
if((length(d$OMSI10)>0)==TRUE){
  d$OMSI <- d$OMSI10
  d <- select(d,-c('OMSI10'))
}

  d$Musiccheck <- case_when(d$Musiccheck == 1 ~ "Yes", d$Musiccheck == 2 ~ "No")
  d$Gender <- case_when(d$Gender == 1 ~ "Male", d$Gender == 2 ~ "Female", d$Gender == 3 ~ "Non-binary", d$Gender == 4 ~ "Other", d$Gender == 5 ~ "Prefer not to say")
  d$OMSI <- case_when(d$OMSI == 1 ~ "Nonmusician", d$OMSI == 2 ~ "Music-loving Nonmusician", d$OMSI == 3 ~ "Amateur musician", d$OMSI == 4 ~ "Serious amateur musician", d$OMSI == 5 ~ "Semiprofessional musician", d$OMSI == 6 ~ "Professional musician")

  # Add check whether c_id exists ----
    # Except for EDR, this wasn't set, so set it
  if(subconstructs[1]=="E" & subconstructs[2]=="D"){
    d$XX <- 1
    controlQ <- "XX"
  }
  if(length(controlQ) > 0 & sum(names(d)==controlQ) > 0){
    c_id<-which(names(d)==controlQ)
    inattentive <- d[,c_id]!=1
    print(paste("Inattentive in total:",sum(inattentive)))
    print("The IDs of the inattentive participants are:")
    print(d$PROLIFIC_PID[inattentive])
    d <- d[!inattentive,] # remove inattentive participants
    d<- dplyr::select(d, -all_of(controlQ)) # remove control question
    if(controlQ=="C_16"){
      names(d)[which(names(d)=="C_17")] <- "C_16"
    }
  }

# rename profilic code to ID
  #d$ID <- d[,which(str_detect(tolower(names(d)),"prolific"))[1]]
  #d <- select(d,-c(which(str_detect(tolower(names(d)),"prolific"))))

  prolific_cols <- which(str_detect(tolower(names(d)),"prolific"))
  #print(names(d)[prolific_cols])

  if(length(prolific_cols) > 0) {
    # If prolific column exists, use it for ID
    d$ID <- d[, prolific_cols[1]]
    d <- select(d, -any_of(prolific_cols))  # Remove the prolific column
  } else {
    # If no prolific column exists, create a warning or default ID
    warning("No prolific column found. Creating sequential ID.")
    d$ID <- 1:nrow(d)
  }

  # update the labels of the items
  # e.g. B{nro} => A{nro} # DANGER
  # e.g. C{nro} => G{nro}
  # e.g. I{nro} => B{nro}
  # e.g. S{nro} => L{nro}
  # e.g. U{nro} => C{nro}
  # e.g. X{nro} => I{nro} # DANGER
  # e.g. P{nro} => X{nro}

  # Display at least the diagnostics, N
  print(paste("Number of accepted participants:", nrow(d)))

  return(d)

}

