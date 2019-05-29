

# Predicción del HDI en Europa en los próximos años


dfHDI <- read.csv("/Users/anablanesmartinez/MasterCienciaDatos/TipologiaCicloVidaDatos/PRAC2/suicide.csv", encoding = "UTF-8", header = TRUE);

#install.packages("countrycode")
#library(countrycode)

dfHDI$continent <- countrycode(sourcevar = dfHDI[, "country"], origin = "country.name", destination = "continent")
colnames(dfHDI)[colnames(dfHDI)=="gdp_per_capita...."] <- "gdp_per_capita_USDollar"
colnames(dfHDI)[colnames(dfHDI)=="gdp_for_year...."] <- "gdp_for_year_USDollar"
colnames(dfHDI)[colnames(dfHDI)=="HDI.for.year"] <- "HDI_for_year"
colnames(dfHDI)[colnames(dfHDI)=="country.year"] <- "country_year"
colnames(dfHDI)[colnames(dfHDI)=="suicides.100k.pop"] <- "suicides_100k_pop"

keeps <- c("country", "continent","year","HDI_for_year")
dfHDI <- dfHDI[keeps]

dfHDIFiltered <- dfHDI[dfHDI[,"continent"]=="Europe",]

#library(dplyr)
#library(plyr)


completeHDI_For_YearDatoPosteriorMedia <- function(df){
		j <- 0;
		country <- "";
		vectorNA = c();
		lastValue <- NA
			
		for(i in 1:nrow(df)) {
			
			if(i> 1 && (df[i-1,]$country !=df[i,]$country)) {
				j <- 0;
				vectorNA = c();
				lastValue <- NA
			}

			
			if(is.na(df[i,]$HDI_for_year)) {
				vectorNA <- c(vectorNA,i);
				j <- j+1;
			} else {

				if(is.na(lastValue)) {
					lastValue <-df[i,]$HDI_for_year
				}
				
				if(length(vectorNA)>0) {
					for(value in vectorNA) {
						hdi <- df[i,]$HDI_for_year
						if(!is.na(lastValue)) {
							hdi = (lastValue + df[i,]$HDI_for_year)/2
							
						}

						df[value,]$HDI_for_year <- hdi;
						vectorNA = c();
						
						j<-0;
					}
					lastValue <- df[i,]$HDI_for_year
				}
			}
		}
		return (df);
		
	}


dfHDIFilteredComplete <- completeHDI_For_YearDatoPosteriorMedia(dfHDIFiltered)

dfHDIFilteredCompleteWithoutNA <- dfHDIFilteredComplete[complete.cases(dfHDIFilteredComplete), ]

dfHDIFilteredCompleteWithoutNAByYear <- ddply(dfHDIFilteredCompleteWithoutNA, .(year), summarize,  HDI_for_year=mean(HDI_for_year, na.rm = TRUE))

model <- lm(HDI_for_year ~ year, data=dfHDIFilteredCompleteWithoutNAByYear)
new.df <- data.frame(year=c(2015,2016,2018,2019,2020))
predict(model, new.df)



