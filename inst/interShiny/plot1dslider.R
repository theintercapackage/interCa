plot1dslider <- function(df,avg) {
  e1 <- as.vector(df)
  n <- length(e1)
  avge <- avg
  ye1 <- rep(0,length(e1))
  lbl<-names(df)
  auto_mca_tablee1 <- data.frame(cbind(e1,ye1,lbl))
  auto_mca_tablee1$e1 <- as.numeric(auto_mca_tablee1$e1)
  auto_mca_tablee1$ye1 <- as.numeric(auto_mca_tablee1$ye1)
  auto_mca_tablee1$lbl <- as.factor(auto_mca_tablee1$lbl)
  interpretive1 <- auto_mca_tablee1%>%
    ggplot() +
    aes(x = e1, y = ye1) +
    geom_point(shape = "circle", size = 1, colour = "#B22222") +
    labs(x = paste("Interpretive axis"), y="")+theme(axis.text.y=element_blank(),
                                                         axis.ticks.y=element_blank())+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    geom_label_repel(
      aes(label = lbl),
      size = 6.5,max.overlaps = Inf,
      point.padding = 0.9,
      min.segment.length = 0.6,
      box.padding = 0.1
    )

  interpretive1<- interpretive1+geom_vline(color="red",xintercept = avge)+
    geom_vline(xintercept = -avge,color='red')

  interpretive1
}




