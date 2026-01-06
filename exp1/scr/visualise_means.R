visualise_means <- function(data = NULL){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  data$item_category <- factor(stringr::str_extract(data$item_label,"[A-Z]+"))
  data$vignette <- factor(data$vignette)

#  S <- summarise(group_by(data,vignette,item_label,item_category),M=mean(value),SD=sd(value))

  S <- data %>%
    dplyr::group_by(vignette,item_label,item_category) %>%
    dplyr::summarise(n=n(),m=mean(value,na.rm = TRUE),sd=sd(value,na.rm = TRUE),.groups = 'drop') %>%
    dplyr::mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se)


  figure <- ggplot(S,aes(x=item_label,y=m,color=item_category,group=item_category)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=LCI,ymax=UCI,width=0.2)) +
    facet_wrap(forcats::fct_reorder(vignette,-m)~forcats::fct_reorder(item_category,-m),nrow=3,scales="free_x")+
    labs(x="Item Id",y="Mean rating (95%CI)") +
    scale_color_brewer(palette = "Set1", name = "Item Category") +
#    scale_color_brewer(palette = "Set1", name = "Item Category") +
    #  scale_x_continuous(breaks=seq(1,22,1)) +
    #scale_y_continuous(limits=c(1,5))+
    theme_classic() +
    theme(legend.position = "bottom")
  print(figure)


  return(figure)

}
