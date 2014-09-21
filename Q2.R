# require(datasets)
# require(gdata)
require(ggplot2)
# require(lattice)
require(knitr)
require(plyr)
# require(reshape2)
require(xtable)
# require(markdown)

opts_chunk$set(echo=TRUE, message=FALSE, cache=FALSE)

data(ToothGrowth)

ToothGrowth$dose = factor(ToothGrowth$dose, levels=c(0.5,1.0,2.0), labels=c("low","med","high"))
print(ToothGrowth[ToothGrowth$dose=="low" | ToothGrowth$dose=="med",])


# ci <- rbind(        
#   c('Supplement',t.test(len ~ supp, data=ToothGrowth[,])$conf),
#   c('Dose 0.5mg',t.test(len ~ supp, data=ToothGrowth[ToothGrowth$dose==0.5,])$conf),
#   c('Dose 1.0mg',t.test(len ~ supp, data=ToothGrowth[ToothGrowth$dose==1.0,])$conf),
#   c('Dose 2.0mg',t.test(len ~ supp, data=ToothGrowth[ToothGrowth$dose==2.0,])$conf)
# )

tTest <- function(compare, data, type) {
  r <- t.test(compare, data=data)
  c(type, r$conf, r$statistic, r$p.value)
}

tt_supp <- tTest(len ~ supp, data=ToothGrowth, 'Length ~ Supplement [OJ, VC]')
tt_dose_l_m <- tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="low" | ToothGrowth$dose=="med",], 'Length ~ Dose [0.5mg, 1.0mg]')
tt_dose_m_h <- tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="med" | ToothGrowth$dose=="high",], 'Length ~ Dose [1.0mg, 2.0mg]')
# tt_supp[1]
# round(as.numeric(tt_supp[2]), 3)
# round(as.numeric(tt_supp[3]), 3)
# 
# print(tTest(len ~ supp, data=ToothGrowth))
# print(tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="low" | ToothGrowth$dose=="med",]))
# print(tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="med" | ToothGrowth$dose=="high",]))
# print(tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="med" | ToothGrowth$dose=="high",])[1])
# print(tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="med" | ToothGrowth$dose=="high",])[2])
# print(tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="med" | ToothGrowth$dose=="high",])[3])
# print(tTest(len ~ dose, data=ToothGrowth[ToothGrowth$dose=="med" | ToothGrowth$dose=="high",])[4])

#print(tTest(ToothGrowth[ToothGrowth$dose==0.5,]))

# 
# # ci[[3]]<-as.numeric(ci[[3]])
# 
# 
# # col(ci)[2] < as.numeric(ci[2,])
# 
# # print( round(as.numeric(ci[1,2]),3) )