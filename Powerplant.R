###section3: project with power plant data set 
###section3: project with power plant data set
###question1: the number of power plant in each country, which country has most power plants and the rate of fuel used for that power plant. 
gppd <- read.csv("~/Documents/dataincubator/globalpowerplantdatabasev110/global_power_plant_database.csv", na.strings = c("",".","NA"))
gppd.p.f <- gppd[,c(1,8)]
fre <- data.frame(table(gppd.p.f))
fre.nomis <- subset(fre, fre$Freq >= 170)
library(ggplot2)
ggplot(fre.nomis,aes(x=as.factor(fre.nomis$country),y=fre.nomis$Freq/2460,fill=factor(fre.nomis$fuel1)))+  
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="fuel1",
                      labels=c("Biomass","Coal","Cogeneration","Gas","Geothermal",
                               "Hydro","Nuclear","Oil","Other","Petcoke","Solar","Stroage","Waste","Wave and tidal","Wind"))+
  xlab("country")+ylab("Percentage")+ 
  ggtitle("Rate of fuel in 164 countries")+theme(plot.title = element_text(size = 20, face = "bold"))+
  geom_text(aes(label=paste0(round((fre.nomis$Freq/2460)*100,1),"%"),
                y=(fre.nomis$Freq/2460)+0.002), vjust=-0.25, position=position_dodge(width=0.9), size=4)

###question2: using plots to show the plant that has highest generation_gwh and show which type of fuels and also show their located country
gppd.c <- gppd[,c(4,1,5,8,12)]
t <- subset(gppd.c, gppd.c$capacity_mw >= 7000)
with(gppd.c, plot(gppd.c$country, gppd.c$capacity_mw, xlab="Country", ylab="Capacity_mw" ,main= "Capacity VS countries",data=gppd.c,col="blue") )
abline(v=1:164, col="gray", lty=3)
text(t[, 2:3],labels =t[,1],pos = 1,col="red")
text(t[, 2:3],labels =t[,2],pos = 2,col="blue")
text(t[, 2:3],labels =t[,4],pos = 4, col="green")
text(t[, 2:3],labels =t[,5],pos = 3, col="green")

##question 3: the treand of generation_mw with year
a=18
b=21
outcome.2 <- matrix(NA, nrow = 4,
                ncol =1, dimnames = list(colnames(gppd[a:b]), "generation"))
number=1
for (i in a:b){
  gppd.generation <- sum(gppd[i],na.rm=T)
  ##get total number of accident
  outcome.2[number] <-gppd.generation
  number=number+1
}
gen.year<- data.frame(outcome.2)
gen.year$year <- c("2013","2014","2015","2016")
gen.year$year <- as.Date(gen.year$year, format = "%Y")
plot(gen.year$generation ~ gen.year$year, gen.yearxlab="Year", ylab="Total Generation_mw ", main="Total generation VS year")
### there is a increassing generation_gwh with year. 













