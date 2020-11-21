library(ggplot2)
library(reshape)
x <-c("2013","2014", "2015", "2016", "2017", "2018", "2019")
Num_Customer_first_trx <- c(1007,4954,11235,17656,31828,30327,2993)
data <- data.frame(x,Num_Customer_by_year)
data
#Year_Last_Transaction
Num_Customer_last_trx= c(557,2091,5331,11833,26616,41898,11674)
datamu = data.frame(x,x,Num_Customer_first_trx,Num_Customer_last_trx)
#Year_First_Transaction
i <- c("2013","2014", "2015", "2016", "2017", "2018", "2019")
Num_Transaction_first_trx <- c(23154,165494,297445,278707,299199,99989,5862)
data2 <- data.frame(i,Num_Transaction_Customer_by_year)
#Year_Last_Transaction
Num_Transaction_last_trx=c(784,3628,13399,39805,119906,499105,493223)
dataku = data.frame(i,i,Num_Transaction_first_trx,Num_Transaction_last_trx)
datamu$x = as.numeric(as.character(datamu$x))
datamu = melt(datamu)
str(dataku) 
datamu
options(scipen = 10000)
p=ggplot(datamu, aes(x = x, y = value)) + 
  geom_line(aes(size = variable,color = variable, linetype = variable)) + 
  scale_size_manual(values = c(3,1) ) +
  scale_color_manual(values = c("steelblue4", "steelblue1")) +
  scale_x_continuous(breaks=seq(2013, 2019))+
  #scale_y_continuous()+
  labs(x = "", y = "", 
     title = "Num of Customer by year") +
  theme(axis.text = element_text(size = 10, colour = "black"),
        #axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 16), 
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        #panel.background = element_rect(fill = "white"),
        legend.position = "bottom")
ggsave(p, filename = "test2.png",  bg = "transparent")
summary(dataku)
library(reshape)
dataset3 <- data.frame(data,data2)
dataset3
dt <- melt(dataset3)
dt
position = position_dodge(width = .75)
width = .65
ggplot(dt, aes(x=x, y=value, fill=variable)) +
  #geom_bar(stat='identity', position='dodge') +
  geom_line()+
  #coord_flip() +
  #geom_text(aes(y = value, label = value, group =variable), color = "black", position = position) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.5, size=3)+
  labs(colour = "", x = "", y = "", 
       title = "Num of Customer VS Num of Transaction by year") + 
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom")+
  scale_fill_manual(values = c( "snow2","royalblue1"))

ggplot(data = data, aes(x=x, y=y)) +
  geom_bar(stat = "identity", width = 0.5, fill="seagreen3", aes(y = 	y)) + 
  geom_text(aes(label=y), position=position_dodge(width=0.9), vjust=-0.25) +
  #coord_flip() +
  scale_y_continuous(labels = scales::comma)+ scale_fill_manual(values =c("#638c80","#638c80","#638c80","#638c80","#638c80")) +
  labs(colour = "", x = "", y = "", 
       title = "Graph of Customer Acquisition", subtitle = "Num of Customer yearly.") + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom") 
options(repr.plt.width = 10, repr.plt.height = 2)

library(ggplot2)
a= c('1. 1','1. 1','2. 2-3','2. 2-3','3. 4-6','3. 4-6','4. 7-10','4. 7-10','5. >10','5. >10')
b = c(49255,14272,12126,2890,21457)
#Count_Transaction_Group  is_churn
is_churn = c(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE)         
count_trx = c(10309,38946,3254,11018,3245,8881,905,1985,15718,5739)

dataframe = data.frame(a,is_churn,count_trx)

library(dplyr)
df2 <- dataframe %>%
  group_by(a) %>%
  arrange(a, desc(is_churn)) %>%
  mutate(lab_ypos = cumsum(count_trx) - 0.5 * count_trx) 
df2

plot=ggplot(data = df2, aes(x = a, y = count_trx)) +
  geom_col(aes(fill = is_churn), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = count_trx, group =is_churn), color = "white")+
  labs(colour = "", y = "", x = "Count_Transaction_Group", 
       title = "Customer Distribution by Transaction Group") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"), 
        legend.position = "bottom")
ggsave(plot,filename = "plot.png",bg="transparent")
ggplot(data = dataframe, aes(x=a, y=b)) +
  geom_bar(stat = "identity", width = 0.5, fill=c("steelblue4","steelblue3","steelblue2","steelblue1","steelblue"), aes(y = 	b)) + 
  #geom_text(aes(label=b), position=position_dodge(width=0), vjust=-0.25) +
  geom_text(aes(x = a, y = b*0.75, label = b), colour = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+ scale_fill_manual(values =c("#638c80","#638c80","#638c80","#638c80","#638c80")) +
  labs(colour = "", y = "", x = "Count_Transaction_Group", 
       title = "Customer Distribution by Transaction Group") + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom")
options(repr.plt.width = 10, repr.plt.height = 2)


Average_Transaction_Amount_Group
c = c("100.000 - 250.000",
      "100.000 - 250.000",
      ">250.000 - 500.000",
      ">250.000 - 500.000",
      ">500.000 - 750.000",
      ">500.000 - 750.000",
      ">750.000 - 1.000.000",
      ">750.000 - 1.000.000",
      ">1.000.000 - 2.500.000",
      ">1.000.000 - 2.500.000",
      ">2.500.000 - 5.000.000" ,
      ">2.500.000 - 5.000.000" ,
      ">5.000.000 - 10.000.000",
      ">5.000.000 - 10.000.000",
      ">10.000.000",
      ">10.000.000")

ha = factor(c,levels=c("100.000 - 250.000",
           ">250.000 - 500.000",
           ">500.000 - 750.000",
           ">750.000 - 1.000.000",
           ">1.000.000 - 2.500.000",
           ">2.500.000 - 5.000.000" ,
           ">5.000.000 - 10.000.000",
           ">10.000.000"))
z = c(4912,18857,15171,12298,32819,9027,3689,3227)
#Average_Transaction_Amount_Group  
is_churn2 = c(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,
              TRUE,FALSE,TRUE,FALSE,TRUE)
avg_trx = c(1385,3527,5327,13530,4528,10643,4260,8038,13001,19818,2984,
            6043,1009,2680,937,2290)

dataa = data.frame(ha,is_churn2,avg_trx)
dataa
df <- dataa %>%
  group_by(ha) %>%
  arrange(ha, desc(is_churn2)) %>%
  mutate(lab_ypos2 = cumsum(avg_trx) - 0.5 * avg_trx) 

ju=ggplot(data = df, aes(x = ha, y = avg_trx)) +
  geom_col(aes(fill = is_churn2), width = 0.7)+
  geom_text(aes(y = lab_ypos2, label = avg_trx), size=3, color = "black")+
  coord_flip() +
  #scale_fill_manual("Is churn:")+
  #scale_y_continuous(labels = scales::comma)+ scale_fill_manual(values =c("#638c80","#638c80","#638c80","#638c80","#638c80")) +
  labs(colour = "", y = "", x = "Average_Transaction_Amount_Group", 
       title = "Customer Distribution by Avarage Transaction Amount Group") + 
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"), 
        legend.position = "bottom") +guides(fill=guide_legend("Is Churn:\n"))
ggsave(ju,filename = "plot2.png",bg="transparent")
ggplot(data = dataa, aes(x=as.factor(ha), y=z)) +
  geom_bar(stat = "identity", width = 0.5, aes(y = 	z)) + 
  #geom_text(aes(label=b), position=position_dodge(width=0), vjust=-0.25) +
  #geom_text(aes(x = ha, y = z*0.9, label = z), colour = "steelblue") +
  geom_text(aes(label = z, hjust = -0.2)) +
  #ylim(NA, 100)+
  coord_flip() +
  #scale_y_continuous(labels = scales::comma)+ scale_fill_manual(values =c("#638c80","#638c80","#638c80","#638c80","#638c80")) +
  labs(colour = "", y = "", x = "Average_Transaction_Amount_Group", 
       title = "Customer Distribution by Avarage Transaction Amount Group") + 
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom")
options(repr.plt.width = 10, repr.plt.height = 2)
