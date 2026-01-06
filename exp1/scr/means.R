means <- function(data = NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

data %>%
dplyr::group_by(item_label) %>%
dplyr::summarise(n=n(),m=mean(value,na.rm = TRUE),sd=sd(value,na.rm = TRUE),.groups = 'drop') %>%
dplyr::mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) %>%
print ()

}