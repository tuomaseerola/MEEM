read_EFA_qualtrics <- function(filename=NULL,bonusquestion="GEMIAC",verbose = FALSE){

  if(verbose){
    print(filename)
  }
  require(readr)
  col_names <- names(readr::read_csv(filename, n_max = 0,show_col_types = FALSE))

  if(bonusquestion=="GEMIAC"){
    if(verbose){
      print("GEMIAC bonus question selected")
    }
    column_types <-list(
    StartDate = col_datetime(format="%Y-%m-%d %H:%M:%S"),
    EndDate = col_datetime(format="%Y-%m-%d %H:%M:%S"),
    Status = col_double(),
    Progress = col_double(),
    `Duration (in seconds)` = col_double(),
    Finished = col_double(),
    RecordedDate = col_datetime(format="%Y-%m-%d %H:%M:%S"),
    ResponseId = col_character(),
    DistributionChannel = col_character(),
    UserLanguage = col_character(),
    `Prolific ID` = col_character(),
    Consent = col_double(),
    Age = col_character(),
    Gender = col_double(),
    OMSI = col_double(),
    Gen_1 = col_double(),
    Gen_2 = col_double(),
    Gen_3 = col_double(),
    Gen_4 = col_double(),
    Gen_5 = col_double(),
    Gen_6 = col_double(),
    Gen_7 = col_double(),
    Gen_8 = col_double(),
    Gen_9 = col_double(),
    Gen_10 = col_double(),
    Gen_11 = col_double(),
    Gen_12 = col_double(),
    Gen_13 = col_double(),
    Gen_14 = col_double(),
    Gen_15 = col_double(),
    Gen_16 = col_double(),
    Gen_17 = col_double(),
    Gen_18 = col_double(),
    Gen_19 = col_double(),
    Gen_20 = col_double(),
    Gen_21 = col_double(),
    Gen_22 = col_double(),
    Gen_23 = col_double(),
    Gen_24 = col_double(),
    Gen_25 = col_double(),
    Gen_26 = col_double(),
    Gen_27 = col_double(),
    Gen_28 = col_double(),
    Gen_29 = col_double(),
    Gen_30 = col_double(),
    Gen_31 = col_double(),
    Gen_32 = col_double(),
    Gen_33 = col_double(),
    Gen_34 = col_double(),
    Gen_35 = col_double(),
    Gen_36 = col_double(),
    Emotion = col_character(),
    PROLIFIC_PID = col_character(),
    Vignette = col_character(),
    VigNro = col_double()
  )
}

  if(bonusquestion=="HAAS"){
    if(verbose){
      print("HAAS bonus question selected")
    }
    column_types <-list(
      StartDate = col_datetime(format="%Y-%m-%d %H:%M:%S"),
      EndDate = col_datetime(format="%Y-%m-%d %H:%M:%S"),
      Status = col_double(),
      Progress = col_double(),
      `Duration (in seconds)` = col_double(),
      Finished = col_double(),
      RecordedDate = col_datetime(format="%Y-%m-%d %H:%M:%S"),
      ResponseId = col_character(),
      DistributionChannel = col_character(),
      UserLanguage = col_character(),
      `Prolific ID` = col_character(),
      Consent = col_double(),
      Age = col_character(),
      Gender = col_double(),
      OMSI = col_double(),
      Gen_1 = col_double(),
      Gen_2 = col_double(),
      Gen_3 = col_double(),
      Gen_4 = col_double(),
      Gen_5 = col_double(),
      Gen_6 = col_double(),
      Gen_7 = col_double(),
      Gen_8 = col_double(),
      Gen_9 = col_double(),
      Gen_10 = col_double(),
      Gen_11 = col_double(),
      Gen_12 = col_double(),
      Gen_13 = col_double(),
      Gen_14 = col_double(),
      Gen_15 = col_double(),
      Gen_16 = col_double(),
      Gen_17 = col_double(),
      Gen_18 = col_double(),
      Gen_19 = col_double(),
      Gen_20 = col_double(),
      Gen_21 = col_double(),
      Gen_22 = col_double(),
      Gen_23 = col_double(),
      Gen_24 = col_double(),
      Gen_25 = col_double(),
      Gen_26 = col_double(),
      Gen_27 = col_double(),
      Gen_28 = col_double(),
      Gen_29 = col_double(),
      Gen_30 = col_double(),
      Gen_31 = col_double(),
      Gen_32 = col_double(),
      Gen_33 = col_double(),
      Gen_34 = col_double(),
      Gen_35 = col_double(),
      Gen_36 = col_double(),
      `HAAS _1` = col_double(),
      `HAAS _2` = col_double(),
      `HAAS _3` = col_double(),
      `HAAS _4` = col_double(),
      `HAAS _5` = col_double(),
      `HAAS _6` = col_double(),
      `HAAS _7` = col_double(),
      `HAAS _8` = col_double(),
      `HAAS _9` = col_double(),
      `HAAS _10` = col_double(),
      `HAAS _11` = col_double(),
      `HAAS _12` = col_double(),
      PROLIFIC_PID = col_character(),
      Vignette = col_character(),
      VigNro = col_double()
    )
  }



  tmp <- readr::read_csv(filename,
                         skip=3,
                         col_names = col_names,
                         col_types=column_types,
                         na = c("","NA"),
                         show_col_types=FALSE)

  # clean extra backtick from age
  tmp$Age<- gsub("`","",tmp$Age)
  tmp$Age<-as.numeric(tmp$Age)

  if(bonusquestion=="HAAS"){
    # Rename HAAS columns
    names(tmp)[names(tmp)=="HAAS _1"]<- "HAAS_1"
    names(tmp)[names(tmp)=="HAAS _2"]<- "HAAS_2"
    names(tmp)[names(tmp)=="HAAS _3"]<- "HAAS_3"
    names(tmp)[names(tmp)=="HAAS _4"]<- "HAAS_4"
    names(tmp)[names(tmp)=="HAAS _5"]<- "HAAS_5"
    names(tmp)[names(tmp)=="HAAS _6"]<- "HAAS_6"
    names(tmp)[names(tmp)=="HAAS _7"]<- "HAAS_7"
    names(tmp)[names(tmp)=="HAAS _8"]<- "HAAS_8"
    names(tmp)[names(tmp)=="HAAS _9"]<- "HAAS_9"
    names(tmp)[names(tmp)=="HAAS _10"]<- "HAAS_10"
    names(tmp)[names(tmp)=="HAAS _11"]<- "HAAS_11"
    names(tmp)[names(tmp)=="HAAS _12"]<- "HAAS_12"
  }
  #tmp <- read.csv(filename, header = TRUE, na.strings = "")
  #names(tmp) <- gsub("\\.", "", names(tmp)) # remove full stops from the column names
  #tmp <- tmp[3:nrow(tmp), ]     # delete two first rows (metadata)

#  tmp <- tmp %>% mutate(across(starts_with("Gen_"), as.numeric)) # items into numeric

  #Ensure only consenting participants to remove NA
  tmp <- filter(tmp,Consent==2)
  # convert into numeric
 # tmp <- tmp %>% mutate(across(starts_with("Prog"), as.numeric))
#  tmp <- tmp %>% mutate(across(starts_with("Dur"), as.numeric))
#  tmp <- tmp %>% mutate(across(starts_with("Age"), ~ as.numeric(as.character(.))))
#  tmp <- tmp %>% mutate(across(matches("^(Gender|OMSI|Music.)"), as.numeric)) # items into numeric
  # rename some columns
  names(tmp)[names(tmp)=="Prolific ID"]<- "ProlificID"
  names(tmp)[names(tmp)=="Duration (in seconds)"]<- "Duration"

  if(verbose){
    print(paste("Number of participants in the data:", nrow(tmp)))
  }
#  print(paste("Number of unique IDs:", length(unique(tmp$Prolific_ID))))
  ## Filtering
  tmp2 <- filter(tmp,Progress==100)
  tmp2 <- filter(tmp2,DistributionChannel!="preview")
  tmp2 <- filter(tmp2,Age!=99)
  if(verbose){
    print(paste("Number of participants after filter incomplete/preview/99 age:", nrow(tmp2)))
  }
  # Eliminate unnecessary columns
  d <- dplyr::select(tmp2, -any_of(c('StartDate', 'EndDate', 'Status', 'Progress', 'Duration', 'Finished', 'RecordedDate', 'DistributionChannel', 'UserLanguage', 'ResponseId','Consent','PROLIFIC_PID','Vignette')))
#  d <- dplyr::select(d, -any_of(c("IPAddress", "RecipientFirstName","RecipientLastName","RecipientEmail","LocationLatitude","LocationLongitude","ExternalReference","ID")))


  d$Gender <- case_when(d$Gender == 1 ~ "Male", d$Gender == 2 ~ "Female", d$Gender == 3 ~ "Non-binary", d$Gender == 4 ~ "Other", d$Gender == 5 ~ "Prefer not to say")
  d$OMSI <- case_when(d$OMSI == 1 ~ "Nonmusician", d$OMSI == 2 ~ "Music-loving Nonmusician", d$OMSI == 3 ~ "Amateur musician", d$OMSI == 4 ~ "Serious amateur musician", d$OMSI == 5 ~ "Semiprofessional musician", d$OMSI == 6 ~ "Professional musician")
  d$Gender<-factor(d$Gender)
  d$OMSI<-factor(d$OMSI, levels=c("Nonmusician","Music-loving Nonmusician","Amateur musician","Serious amateur musician","Semiprofessional musician","Professional musician"), ordered=TRUE)
    # Add check whether c_id exists ----
    # Except for EDR, this wasn't set, so set it
    inattentive <- as.numeric(d$Gen_36)!=1
    if(verbose){
      print(paste("Inattentive in total:",sum(inattentive)))
      print("The IDs of the inattentive participants are:")
      print(d$ProlificID[inattentive])
    }
    d <- d[!inattentive,] # remove inattentive participants
    d<- dplyr::select(d, -Gen_36) # remove control question

    #### Remove last 3 participants from Vignette 2:
    candidates <- d$ProlificID[d$VigNro==2]
    d <- dplyr::filter(d, !(ProlificID %in% tail(candidates,3)))

  # Display at least the diagnostics, N
    if(verbose){
      print(paste("Number of accepted participants:", nrow(d)))
    }
  return(d)

}
