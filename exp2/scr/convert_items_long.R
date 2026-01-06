convert_items_long <- function(d = NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  verbose <- TRUE

  item_ID <- str_detect(names(d), "Gen_") # find the item names
  df <- pivot_longer(d, cols = names(d)[item_ID],
                     names_to = "item", values_to = "value")
  # clean up variable names
  # Remove the 3 representativeness questions from the items
  df$itemnro <- as.numeric(str_remove(df$item, '^Gen_')) # extract item nro

  items <- read.csv('data/items35.csv', header = TRUE, na.strings = "")
  df$item_label <- items$ID[df$itemnro]



#  print(knitr::kable(dplyr::summarise(group_by(df,VigNro),n=n()/35)))

  if(verbose) {
    print(paste("Number of rows in the long data:", nrow(df)))
    print(paste("Number of unique participants in the long data:", length(unique(df$ProlificID))))
    print(paste("Number of unique items in the long data:", length(unique(df$item))))
  }

  return(df)

}

