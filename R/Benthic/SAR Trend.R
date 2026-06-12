#Estyimate AIS coverage
library(readxl)

# first get the effort by year
Power=read_excel("data/effort data.xlsx",sheet="data")
#fit a third order polynomial to y as a function of year
res=lm(Power$y~Power$year+I(Power$year^2)+I(Power$year^3))
Years=data.frame(Year=seq(1950,2020))
#predict the values of y for each year
Years$Pre=Years$Year*res$coefficients[2]+Years$Year^2*res$coefficients[3]+Years$Year^3*res$coefficients[4]+res$coefficients[1]
Years$Scaled=Years$Pre/Years$Pre[which(Years$Year==2020)]
plot(Years$Year,Years$Scaled,type="l",xlim=c(1950,2020),ylim=c(0,1),xlab="Year",ylab="Relative effort")


GFW <- read_excel("data/GFW trawl data.xlsx",sheet="ForR")
names(GFW)[1] <- "Country"

SAR=array(dim=c(nrow(GFW),length(Years$Year)))
rownames(SAR)=GFW$Country; colnames(SAR)=Years$Year
#for each year set SAR to be the SAR for that year multiplied by the relative effort for that year
for(i in 1:length(Years$Year)){
  SAR[,i]=GFW$SAR1*Years$Scaled[i]
}
RBS=exp(-.123*SAR)
#Total RBS for each year is the RBS by country weighted by GFW$shelf
Wmean=apply(RBS*GFW$Shelf,2,sum,na.rm=T)/sum(GFW$Shelf,na.rm=T)
plot(Years$Year,Wmean,type="l",xlim=c(1950,2020),ylim=c(0.8,1),xlab="Year",ylab="Relative biomass")
SARdouble=SAR*2
RBSdouble=exp(-.123*SARdouble)

WmeanDouble=apply(RBSdouble*GFW$Shelf,2,sum,na.rm=T)/sum(GFW$Shelf,na.rm=T)
plot(Years$Year,WmeanDouble,type="l",xlim=c(1950,2020),ylim=c(0.8,1),
     xlab="Year",ylab="Relative biomass",main="Assuming double the SAR")
SAR10=SARdouble #for countries with poor AIS coverage, assume 10 times the SAR
i=which(GFW$AIS==0)
   SAR10[i,]=SARdouble[i,]*5

RBS10=exp(-.123*SAR10)
Wmean10=apply(RBS10*GFW$Shelf,2,sum,na.rm
=T)/sum(GFW$Shelf,na.rm=T)
plot(Years$Year,Wmean10,type="l",xlim=c(1950,2020),ylim=c(0.8,1),
     xlab="Year",ylab="Relative biomass",main="Assuming 10 times the SAR for countries with poor AIS coverage")
  
#NOW ALL RESULTWS
plot(Years$Year,Wmean,type="l",xlim=c(1950,2020),ylim=c(0.8,1),
     xlab="Year",ylab="Relative benthic biomass",lwd=2)
lines(Years$Year,WmeanDouble,col="red",lwd=2)
lines(Years$Year,Wmean10,col="blue",lwd=2)
legend("bottomleft",cex=.7,
legend=c("SAR","Double SAR","10x SAR for poor AIS"),
col=c("black","red","blue"),lwd=2)

# write.csv(cbind(Years$Year,Wmean,WmeanDouble,Wmean10),"data/benthic_infauna_output.csv")
