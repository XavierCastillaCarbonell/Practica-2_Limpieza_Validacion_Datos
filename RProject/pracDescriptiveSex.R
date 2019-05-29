

# Suicidios Mundial por sexo y por rango de años de 5 en 5



dfSuicidios <- read.csv("/Users/anablanesmartinez/MasterCienciaDatos/TipologiaCicloVidaDatos/PRAC2/suicide.csv", encoding = "UTF-8", header = TRUE);

#install.packages("countrycode")
#library(countrycode)

dfSuicidios$continent <- countrycode(sourcevar = dfSuicidios[, "country"], origin = "country.name", destination = "continent")
colnames(dfSuicidios)[colnames(dfSuicidios)=="gdp_per_capita...."] <- "gdp_per_capita_USDollar"
colnames(dfSuicidios)[colnames(dfSuicidios)=="gdp_for_year...."] <- "gdp_for_year_USDollar"
colnames(dfSuicidios)[colnames(dfSuicidios)=="HDI.for.year"] <- "HDI_for_year"
colnames(dfSuicidios)[colnames(dfSuicidios)=="country.year"] <- "country_year"
colnames(dfSuicidios)[colnames(dfSuicidios)=="suicides.100k.pop"] <- "suicides_100k_pop"

keeps <- c("country", "year","sex","suicides_no")
dfSuicidiosFiltered <- dfSuicidios[keeps]


dfSuicidiosFiltered$yearRange5Years<-cut(dfSuicidiosFiltered$year, c(1980,1985,1990,1995,2000,2005,2010,2015,2020))

library(dplyr)
library(plyr)


dfSuicidiosFilteredSum <- ddply(dfSuicidiosFiltered, .(sex,yearRange5Years), summarize,  sum_suicides_no=sum(suicides_no))

dfSuicidiosFilteredYears <- dfSuicidiosFilteredSum[dfSuicidiosFilteredSum[,"yearRange5Years"]!="(2015,2020]",]

summary(dfSuicidiosFilteredYears[dfSuicidiosFilteredYears[,"sex"]=="female",])
summary(dfSuicidiosFilteredYears[dfSuicidiosFilteredYears[,"sex"]=="male",])

boxplot(sum_suicides_no~sex,data=dfSuicidiosFilteredYears, ylim=c(30000, 1200000), main="Suicidios por sexo durante 40 años a nivel mundial",xlab="Sexo", ylab="Suicidios")

ggplot(dfSuicidiosFilteredYears, aes(yearRange5Years, sum_suicides_no, fill = sex)) + geom_bar(stat = "identity", width = 0.2, position = "dodge") + labs(list(x = "x", y = "count",fill = "group"))




