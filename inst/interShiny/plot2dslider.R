plot2dslider <- function(first,second,ave) {
  e1 <- first
  e2 <- second
  avge1 <- mean(abs(e1))
  avge2 <- mean(abs(e2))
  avepl1 <- ave
  lbl <- names(first)
  c=abs(e1)+abs(e2)
  auto_mca_table <- data.frame(cbind(e1,e2,lbl,c))
  auto_mca_table$e1 <- as.numeric(auto_mca_table$e1)
  auto_mca_table$e2 <- as.numeric(auto_mca_table$e2)
  auto_mca_table$lbl <- as.factor(auto_mca_table$lbl)
  auto_mca_table$c <- as.numeric(auto_mca_table$c)
  averageccc = avepl1
  avexx=c(averageccc,0,-averageccc,0,averageccc)
  aveyy=c(0,-averageccc,0,averageccc,0)
  averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)

  ggplot()->interpretive_plane1
  interpretive_plane1+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1

  for (i in 1:nrow(auto_mca_table)){

    thexx=c(auto_mca_table$c[i],0,-auto_mca_table$c[i],0,auto_mca_table$c[i])
    theyy=c(0,-auto_mca_table$c[i],0,auto_mca_table$c[i],0)
    ccc=cbind(thexx,theyy)
    ccc=as.data.frame(ccc)

    interpretive_plane1 +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = T,size=0.1)->interpretive_plane1

  }
  interpretive_plane1+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
    geom_point(data=auto_mca_table,aes(x = e1, y = e2),shape = "circle", size = 1.5, colour = "black")->interpretive_plane1

  interpretive_plane1+geom_label_repel(data=auto_mca_table,
                                       mapping=aes(x=e1,y=e2,label = paste(lbl)), # data point size
                                       size = 3.5,
                                       max.overlaps = Inf,
                                       point.padding = 0.7,
                                       min.segment.length = 0.9,
                                       box.padding = 1.2

  )+
    labs(x = paste("Interpretive x-axis"),y=paste("Interpretive y-axis"))->interpretive_plane1

  interpretive_plane1
}



