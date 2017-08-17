library(plyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(gplots)
abia=read.csv('ABIA.csv')
abia_dep=abia[abia$Origin=='AUS',]
abia_dep_df=tbl_df(abia_dep)

abia_arv=abia[abia$Origin!='AUS',]
abia_arv_df=tbl_df(abia_arv)

abia_df=tbl_df(abia)
##########
# DEP
##########
abia_dep_1=subset(abia_dep_df,Month==1)
abia_dep_2=subset(abia_dep_df,Month==2)
abia_dep_3=subset(abia_dep_df,Month==3)
abia_dep_4=subset(abia_dep_df,Month==4)
abia_dep_5=subset(abia_dep_df,Month==5)
abia_dep_6=subset(abia_dep_df,Month==6)

carrier_1=group_by(abia_dep_1,UniqueCarrier, DayOfWeek)
carrier_2=group_by(abia_dep_2,UniqueCarrier, DayOfWeek)
carrier_3=group_by(abia_dep_3,UniqueCarrier, DayOfWeek)
carrier_4=group_by(abia_dep_4,UniqueCarrier, DayOfWeek)
carrier_5=group_by(abia_dep_5,UniqueCarrier, DayOfWeek)
carrier_6=group_by(abia_dep_6,UniqueCarrier, DayOfWeek)


carrier_delay1 = summarize(carrier_1, delay = mean(ArrDelay, na.rm = T))
carrier_delay2 = summarize(carrier_2, delay = mean(ArrDelay, na.rm = T))
carrier_delay3 = summarize(carrier_3, delay = mean(ArrDelay, na.rm = T))
carrier_delay4 = summarize(carrier_4, delay = mean(ArrDelay, na.rm = T))
carrier_delay5 = summarize(carrier_5, delay = mean(ArrDelay, na.rm = T))
carrier_delay6 = summarize(carrier_6, delay = mean(ArrDelay, na.rm = T))

carrier_de1=cast(carrier_delay1, UniqueCarrier~DayOfWeek)
carrier_de2=cast(carrier_delay2, UniqueCarrier~DayOfWeek)
carrier_de3=cast(carrier_delay3, UniqueCarrier~DayOfWeek)
carrier_de4=cast(carrier_delay4, UniqueCarrier~DayOfWeek)
carrier_de5=cast(carrier_delay5, UniqueCarrier~DayOfWeek)
carrier_de6=cast(carrier_delay6, UniqueCarrier~DayOfWeek)

heatmap(as.matrix(carrier_de1),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de2),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de3),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de4),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de5),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de6),Rowv=NA, Colv=NA)

#############
#END
#############

#############
# ARIVAL
#############

abia_dep_1=subset(abia_arv_df,Month==1)
abia_dep_2=subset(abia_arv_df,Month==2)
abia_dep_3=subset(abia_arv_df,Month==3)
abia_dep_4=subset(abia_arv_df,Month==4)
abia_dep_5=subset(abia_arv_df,Month==5)
abia_dep_6=subset(abia_arv_df,Month==6)

carrier_1=group_by(abia_dep_1,UniqueCarrier, DayOfWeek)
carrier_2=group_by(abia_dep_2,UniqueCarrier, DayOfWeek)
carrier_3=group_by(abia_dep_3,UniqueCarrier, DayOfWeek)
carrier_4=group_by(abia_dep_4,UniqueCarrier, DayOfWeek)
carrier_5=group_by(abia_dep_5,UniqueCarrier, DayOfWeek)
carrier_6=group_by(abia_dep_6,UniqueCarrier, DayOfWeek)


carrier_delay1 = summarize(carrier_1, delay = mean(ArrDelay, na.rm = TRUE))
carrier_delay2 = summarize(carrier_2, delay = mean(ArrDelay, na.rm = TRUE))
carrier_delay3 = summarize(carrier_3, delay = mean(ArrDelay, na.rm = TRUE))
carrier_delay4 = summarize(carrier_4, delay = mean(ArrDelay, na.rm = TRUE))
carrier_delay5 = summarize(carrier_5, delay = mean(ArrDelay, na.rm = TRUE))
carrier_delay6 = summarize(carrier_6, delay = mean(ArrDelay, na.rm = TRUE))

carrier_de1=cast(carrier_delay1, UniqueCarrier~DayOfWeek)
carrier_de2=cast(carrier_delay2, UniqueCarrier~DayOfWeek)
carrier_de3=cast(carrier_delay3, UniqueCarrier~DayOfWeek)
carrier_de4=cast(carrier_delay4, UniqueCarrier~DayOfWeek)
carrier_de5=cast(carrier_delay5, UniqueCarrier~DayOfWeek)
carrier_de6=cast(carrier_delay6, UniqueCarrier~DayOfWeek)

heatmap(as.matrix(carrier_de1),Rowv=NA, Colv=NA,col=terrain.colors(1000))
heatmap(as.matrix(carrier_de2),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de3),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de4),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de5),Rowv=NA, Colv=NA)
heatmap(as.matrix(carrier_de6),Rowv=NA, Colv=NA)
#############
# END
############
library(gplots)

color1=colorRampPalette(c('white','firebrick1'))
heatmap.2(as.matrix(carrier_de1),Rowv=NA,Colv=NA,col=color1(100),main='title') 

############
# END
############

############
# Beautiful HEATMAPS FOR MARKDOWN
############
abia_dep_1=subset(abia_dep_df,Month==1)
abia_dep_2=subset(abia_dep_df,Month==2)
abia_dep_3=subset(abia_dep_df,Month==3)
abia_dep_4=subset(abia_dep_df,Month==4)
abia_dep_5=subset(abia_dep_df,Month==5)
abia_dep_6=subset(abia_dep_df,Month==6)

carrier_1=group_by(abia_dep_1,UniqueCarrier, DayOfWeek)
carrier_2=group_by(abia_dep_2,UniqueCarrier, DayOfWeek)
carrier_3=group_by(abia_dep_3,UniqueCarrier, DayOfWeek)
carrier_4=group_by(abia_dep_4,UniqueCarrier, DayOfWeek)
carrier_5=group_by(abia_dep_5,UniqueCarrier, DayOfWeek)
carrier_6=group_by(abia_dep_6,UniqueCarrier, DayOfWeek)


carrier_delay1 = summarize(carrier_1, delay = mean(ArrDelay, na.rm = T))
carrier_delay2 = summarize(carrier_2, delay = mean(ArrDelay, na.rm = T))
carrier_delay3 = summarize(carrier_3, delay = mean(ArrDelay, na.rm = T))
carrier_delay4 = summarize(carrier_4, delay = mean(ArrDelay, na.rm = T))
carrier_delay5 = summarize(carrier_5, delay = mean(ArrDelay, na.rm = T))
carrier_delay6 = summarize(carrier_6, delay = mean(ArrDelay, na.rm = T))

carrier_de1=cast(carrier_delay1, UniqueCarrier~DayOfWeek)
carrier_de2=cast(carrier_delay2, UniqueCarrier~DayOfWeek)
carrier_de3=cast(carrier_delay3, UniqueCarrier~DayOfWeek)
carrier_de4=cast(carrier_delay4, UniqueCarrier~DayOfWeek)
carrier_de5=cast(carrier_delay5, UniqueCarrier~DayOfWeek)
carrier_de6=cast(carrier_delay6, UniqueCarrier~DayOfWeek)

color1=colorRampPalette(c('white','red'))
h1=heatmap.2(as.matrix(carrier_de1),Rowv=NA,Colv=NA,col=color1(100),main='January',trace='none') 
h2=heatmap.2(as.matrix(carrier_de2),Rowv=NA,Colv=NA,col=color1(100),main='February',trace='none') 
h3=heatmap.2(as.matrix(carrier_de3),Rowv=NA,Colv=NA,col=color1(100),main='March',trace='none') 
h4=heatmap.2(as.matrix(carrier_de4),Rowv=NA,Colv=NA,col=color1(100),main='April',trace='none') 
h5=heatmap.2(as.matrix(carrier_de5),Rowv=NA,Colv=NA,col=color1(100),main='May',trace='none') 
h6=heatmap.2(as.matrix(carrier_de6),Rowv=NA,Colv=NA,col=color1(100),main='June',trace='none') 

##########
# END 
##########





library(reshape)
carrier_de=cast(carrier_delay, UniqueCarrier~DayOfWeek)

heatmap(as.matrix(carrier_de),Rowv=NA, Colv=NA)



