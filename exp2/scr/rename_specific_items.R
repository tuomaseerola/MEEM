rename_specific_items <- function(data = NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }
  # print("Exp. 2 renaming items")
  # Rename  sub-constructs:

  # Aesthetic-Interest-Awe
  # B "Being moved/spirituality" (was I)
  # I "Curiosity/Interest" (was X)
  # A "Aesthetics" (was B)
#  data$item_label <- str_replace_all(data$item_label,
#                                     c("B" = "A",
#                                       "I" = "B",
#                                       "X" = "I"
#                                       ))


  #   Connection-Belonging
  # NEW								OLD
  # G "Group Cohesion/socialisation" (was C)
  # L "(Reducing) loneliness" (was S)
  data$item_label <- str_replace_all(data$item_label,
                                    c("C" = "G",
                                      "S" = "L"))
  
  # Aesthetic-Interest-Awe
  # S "Being moved/spirituality" (was I)
  # I "Curiosity/Interest" (was X)
  # B "Aesthetics/Beauty" (was B)


  data$item_label <- str_replace_all(data$item_label,
                                     c("B" = "B",
                                       "I" = "S",
                                       "X" = "I"
                                       ))

  # Personal-Emotional-Processing
  #
  # C "Reflection/Coping" (was U)
  # X "Expressing feelings" (was P)
  data$item_label <- str_replace_all(data$item_label,
                                     c("U" = "C",
                                       "P" = "X"))

  return(data)

}
