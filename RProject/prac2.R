dfSuicidios <- read.csv("/Users/anablanesmartinez/MasterCienciaDatos/TipologiaCicloVidaDatos/PRAC2/suicide.csv", encoding = "UTF-8", header = TRUE);
#install.packages("countrycode")
#library(countrycode)
dfSuicidios$continent <- countrycode(sourcevar = dfSuicidios[, "country"], origin = "country.name", destination = "continent")
colnames(dfSuicidios)[colnames(dfSuicidios)=="gdp_per_capita...."] <- "gdp_per_capita_USDollar"
colnames(dfSuicidios)[colnames(dfSuicidios)=="gdp_for_year...."] <- "gdp_for_year_USDollar"
colnames(dfSuicidios)[colnames(dfSuicidios)=="HDI.for.year"] <- "HDI_for_year"
colnames(dfSuicidios)[colnames(dfSuicidios)=="country.year"] <- "country_year"
colnames(dfSuicidios)[colnames(dfSuicidios)=="suicides.100k.pop"] <- "suicides_100k_pop"

keeps <- c("country","continent", "year","sex","age","suicides_100k_pop","HDI_for_year","gdp_per_capita_USDollar","generation")
dfSuicidiosFiltered <- dfSuicidios[keeps]

library(dplyr)





# Completa HDIs según el dato anterior
completeHDI_For_YearDatoAnterior <- function(df){
	
	value <- NA;
		
	for(i in 1:nrow(df)) {
		if(i>1) {
			if(df[i-1,]$country !=df[i,]$country) {
				cambioPais <- TRUE;
				value <- NA;
			} else {
				cambioPais <- FALSE;
			}
		
			if(!cambioPais) {
				if(is.na(df[i,]$HDI_for_year)) {
					df[i,]$HDI_for_year<- value;
				} else {
					value <- df[i,]$HDI_for_year; 
				}
			}
		}

		
	}
	return (df);
	
}


dfSuicidiosFiltered2 <- completeHDI_For_YearDatoAnterior(dfSuicidiosFiltered)

dfSuicidiosFiltered3 <- filter(dfSuicidiosFiltered2, age == "5-14 years")
dfSuicidiosFiltered4 <- filter(dfSuicidiosFiltered3, sex == "male")




# Obtiene los datos para cada 5 años -> Problema, hay países que pasan del 86 al 93 y se pierden 7 años
getYearEveryYear <- function(df) {
	years <- c(1985,1990,1995,2000,2005,2010,2015,2020)
	j <-0
	rows <- c()
	
	for(i in 1:nrow(df)) {
		if(is.element(df[i,]$year,years)) {
			rows[j] <- i;
			j <- j+1
		}
	}
	return (df[rows,]);
}

dfSuicidiosFiltered5 <- getYearEveryYear(dfSuicidiosFiltered4)




# Obtiene para cada país, los años disponibles
getYearsForCountry <- function(df) {
	results <- c()
	j=0
	k=0
	country <- df[1,]$country
	years <- c()
	for(i in 1:nrow(df)) {
		if (df[i,]$country != country) {
			yearsString = ""
			for(year in unique(years)) {
				yearsString <- paste(yearsString,",",year);
			}
			results[k] <- paste(country,",",yearsString,"\n")
			k <- k +1
			years <- c()
			j=0
		}
		years[j] <- df[i,]$year
		j <- j + 1
		country <- df[i,]$country
	}
	return(results)
}


write.table(getYearsForCountry(dfSuicidiosFiltered4), quote = FALSE, row.names=FALSE, file = "/Users/anablanesmartinez/MasterCienciaDatos/TipologiaCicloVidaDatos/PRAC2/results.txt")







# Completa con los valores medios del valor anterior y próximo
# Los últimos valores si son NA se quedan NA

completeHDI_For_YearDatoPosterior <- function(df){
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


dfSuicidiosFiltered7 <- 	completeHDI_For_YearDatoPosterior(dfSuicidiosFiltered)











		







