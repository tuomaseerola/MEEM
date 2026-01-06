visualise_means <- function(data = NULL,Construct="EDR"){
  if(is.null(data)){
    stop("Data is NULL. Please provide a data frame.")
  }

  if(Construct=="EDR"){
    data$item_label <- factor(data$item_label,levels = c("E1", "E6", "E7", "R1", "R3", "R7", "D1", "D6", "D7"))
    construct <- 1:3
  }

  if(Construct=="FM"){
    data$item_label <- factor(data$item_label,levels = c("F3", "F6", "F10", "M_F1","M1","M3"))
    construct <- 4:5
  }

  if(Construct=="CB"){
    data$item_label <- factor(data$item_label,levels = c("G2", "G5", "G9","L5", "L6", "L7"))
    construct <- 6:7
  }

  if(Construct=="PEP"){
    data$item_label <- factor(data$item_label,levels = c("C2", "C3", "C6","X4", "X8", "X9"))
    construct <- 8:9
  }

  if(Construct=="AIA"){
    data$item_label <- factor(data$item_label,levels = c("B3", "B4", "B5",
                                                         "I1", "I2",
                                                         "A1", "A3", "A5"
                                                          ))
    construct <- 10:12
  }


  data$item_category <- factor(stringr::str_extract(data$item_label,"[A-Z]+"))
  #data$VigNro <- factor(data$VigNro)

#  S <- summarise(group_by(data,vignette,item_label,item_category),M=mean(value),SD=sd(value))

  S <- data %>%
    dplyr::group_by(VigNro,item_label,item_category) %>%
    dplyr::summarise(n=n(),m=mean(value,na.rm = TRUE),sd=sd(value,na.rm = TRUE),.groups = 'drop') %>%
    dplyr::mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se)


  # Same colours as in gemiac plot later on
  library(RColorBrewer)
  colors_12 <- c(
    brewer.pal(6, "Set2")[1:6],
    brewer.pal(6, "Set1")
  )


  figure <- ggplot(S,aes(x=item_label,y=m,color=item_category,group=item_category)) +
    geom_line(show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    geom_errorbar(aes(ymin=LCI,ymax=UCI,width=0.2),show.legend = FALSE) +
#    facet_wrap(forcats::fct_reorder(VigNro,-m)~forcats::fct_reorder(item_category,-m),nrow=3,scales="free_x")+
#_    facet_wrap(.~VigNro,nrow=6,scales="free_x")+
    facet_wrap(.~VigNro,ncol = 3)+
    labs(x="Item Id",y="Mean rating (95%CI)") +
#    scale_color_brewer(palette = "Set1", name = "Item Category") +
    scale_color_manual(name="Item Category",values = colors_12[construct]) +
#    scale_color_brewer(palette = "Set1", name = "Item Category") +
    #  scale_x_continuous(breaks=seq(1,22,1)) +
#    scale_y_continuous(limits=c(2,5))+
    theme_classic(base_size = 14) +
    theme(legend.position = "bottom")
  #print(figure)


  return(figure)

}
