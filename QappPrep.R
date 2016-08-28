library(ggplot2)
library(grid)
source("multiplot.R")
# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
p1 <- ggplot(filteredData) +
  geom_line(aes(x=EFRMonth, y=YTDEFR,colour="orange")) +
  geom_line(aes(x=EFRMonth, y=EFRTarget2016,colour="green")) +
  geom_line(aes(x=EFRMonth, y=EFRResult2015,colour="blue")) +
  geom_point(aes(x=EFRMonth, y=YTDEFR,colour="pink"))+
  geom_smooth(alpha=0.3,size=2)+
  ylim(0,5)+
  #scale_x_continuous(breaks=seq(1, 12, 1))+
  scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("EFR2 (Rolling 12 months; Aggregated view 2016)")



# Second plot
p2 <- ggplot(filteredData) +
  geom_line(aes(x=EFRMonth, y=COR90D,colour="orange")) +
  geom_line(aes(x=EFRMonth, y=COR90D_YTD2016,colour="green")) +
  geom_line(aes(x=EFRMonth, y=COR90D_YTD2015,colour="blue")) +
  geom_point(aes(x=EFRMonth, y=COR90D,colour="red"))+
  geom_smooth(alpha=0.3,size=2)+
  ylim(0,16)+
  #scale_x_continuous(breaks=seq(1, 12, 1))+
  scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("90 Days COR")


  # Third plot
p3 <- ggplot(filteredData) +
  geom_line(aes(x=EFRMonth, y=FYCOR,colour="orange")) +
  geom_line(aes(x=EFRMonth, y=FYCOR_YTD2016,colour="green")) +
  geom_line(aes(x=EFRMonth, y=FYCOR_YTD2015,colour="blue")) +
  geom_point(aes(x=EFRMonth, y=FYCOR,colour="red"))+
  geom_smooth(alpha=0.3,size=2)+
  ylim(0,16)+
  #scale_x_continuous(breaks=seq(1, 12, 1))+
  scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggtitle("90 Days COR")

# Fourth plot
#p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
 # geom_histogram(colour="black", binwidth=50) +
  #facet_grid(Diet ~ .) +
  #ggtitle("Final weight, by diet") +
  #theme(legend.position="none")        # No legend (redundant in this graph)  

multiplot(p1, p2, p3, cols=2)
