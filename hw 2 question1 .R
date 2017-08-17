library(plyr)
library(dplyr)
library(ggplot2)

abia=read.csv('ABIA.csv')
abia_df=tbl_df(abia)

carrier=group_by(abia_df,UniqueCarrier, DayOfWeek)
carrier

carrier_delay = summarize(carrier, delay = mean(ArrDelay, na.rm = T))
carrier_delay

library(reshape)
carrier_de=cast(carrier_delay, UniqueCarrier~DayOfWeek)




heatmap(as.matrix(carrier_de),Rowv=NA, Colv=NA)



