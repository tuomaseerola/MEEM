convert_items_long <- function(d = NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  verbose <- TRUE

  item_ID <- str_detect(names(d), "[EDRCSMFUPIXBLNOATZGHJVWQ]_") # find the item names
  df <- pivot_longer(d, cols = names(d)[item_ID],
                     names_to = "item", values_to = "value")
  # clean up variable names
  # Remove the 3 representativeness questions from the items
  df <- filter(df, str_detect(item, '._Rep|._Rel') == FALSE)
  df$vignette <- str_extract(df$item, '^[EDRCSMFUPIXBLNOATZGHJVWQ]') # extract the vignette name
  df$itemnro <- as.numeric(str_remove(df$item, '^[EDRCSMFUPIXBLNOATZGHJVWQ]_')) # extract item nro
  df$itemID <- paste0("I",str_remove(df$item, '^[EDRCSMFUPIXBLNOATZGHJVWQ]_')) # Impose item Id

  if (names(d)[item_ID][1]=="D_1"){
    questions <- read.csv('data/EDR_items.csv', header = TRUE, na.strings = "")
    df$item_label <- questions$ID[df$itemnro]
    df$item_text <- questions$text[df$itemnro]
    if(verbose==TRUE){print("EDR items loaded")}
  }
  if (names(d)[item_ID][1]=="M_1"){
    questions <- read.csv('data/FM_items.csv', header = TRUE, na.strings = "")
    df$item_label <- questions$ID[df$itemnro]
    df$item_text <- questions$text[df$itemnro]
    if(verbose==TRUE){print("FM items loaded")}
  }
  if (names(d)[item_ID][1]=="C_1"){
    questions <- read.csv('data/CB_items.csv', header = TRUE, na.strings = "")
    df$item_label <- questions$ID[df$itemnro]
    df$item_text <- questions$text[df$itemnro]
    if(verbose==TRUE){print("CB items loaded")}
  }
  if (names(d)[item_ID][1]=="U_1"){
    questions <- read.csv('data/PEP_items.csv', header = TRUE, na.strings = "")
    df$item_label <- questions$ID[df$itemnro]
    df$item_text <- questions$text[df$itemnro]
    if(verbose==TRUE){print("PEP items loaded")}
  }

  if (names(d)[item_ID][1]=="I_1"){
    questions <- read.csv('data/AIA_items.csv', header = TRUE, na.strings = "")
    df$item_label <- questions$ID[df$itemnro]
    df$item_text <- questions$text[df$itemnro]
    if(verbose==TRUE){print("AIA items loaded")}
  }

  return(df)

}

