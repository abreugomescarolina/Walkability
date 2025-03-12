##script Diogo

spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
Dateb<-spss2date(NDVI$BEBENasc)
Date1<-spss2date(NDVI$DataAvaliaçao_48m)
Date2<-spss2date(NDVI$DataAvaliaçao_84M)
Date3<-spss2date(NDVI$DataAvaliaçao_120M)

BEBENasc<-as.Date(Dateb , format="%Y-%m-%d")###Datas: nascimento, 1, 2 e 3º FU
Data48m<-as.Date(Date1 , format="%Y-%m-%d")
Data84m<-as.Date(Date2 , format="%Y-%m-%d")
Data120M<-as.Date(Date3 , format="%Y-%m-%d")

NDVI$Dif0<-round(difftime(BEBENasc,BEBENasc,units="days")/30.43,1)
NDVI$Dif1<-round(difftime(Data48m,BEBENasc,units="days")/30.43,1)
NDVI$Dif2<-round(difftime(Data84m,BEBENasc,units="days")/30.43,1)
NDVI$Dif3<-round(difftime(Data120M,BEBENasc,units="days")/30.43,1)

library(lcmm)

MIXT1<-hlme(fixed= ndviy500 ~  DifTime , random=~DifTime,
            subject="IDA",   ng=1,
            data=BASE)


MIXT2<-hlme(fixed= ndviy500 ~  DifTime , random=~DifTime,
            subject="IDA", mixture=~ DifTime,  ng=2,
            data=BASE)

MIXT3<-hlme(fixed= ndviy500 ~  DifTime , random=~DifTime,
            subject="IDA", mixture=~ DifTime,  ng=3,
            data=BASE)

MIXT4<-hlme(fixed= ndviy500 ~  DifTime , random=~DifTime,
            subject="IDA", mixture=~ DifTime,  ng=4,
            data=BASE)


summarytable(MIXT1, MIXT2, MIXT3,MIXT4, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))

# MIXT4 the best model

postprob(MIXT4)

BASE$IDA <- as.numeric(BASE$IDA)
people4 <- as.data.frame(MIXT4$pprob[,1:2])
BASE$group4 <- factor(people4$class[sapply(BASE$IDA, function(x) which(people4$IDA==x))])
p01 <- ggplot(BASE, aes(DifTime, ndviy500 , group=IDA, colour=group4)) + geom_line() + geom_smooth(aes(group=group4), method="loess", size=2, se=F)+ scale_y_continuous(limits = c(0,0.6))  + labs(x="x",y="y",colour="Latent Class") 
p02 <- ggplot(BASE, aes(DifTime,ndviy500 , group=IDA, colour=group4)) + geom_smooth(aes(group=IDA, colour=group4),size=0.5, se=F) + geom_smooth(aes(group=group4), method="loess", size=2, se=T)+ scale_y_continuous(limits = c(0,0.6))
grid.arrange(p01,p02, ncol=2)


nrow(people4)
class(people4$class)
NDVIorder$NDVILatentClasse<-as.factor(people4$class)

##Class 1- Descending NDVI
##Class 2 - Stable low NDVI
##Class 3 - Ascending NDVI
##Class 4 - Stable high NDVI
