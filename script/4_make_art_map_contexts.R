######################################################################################
#This makes a map for the contexts covid-19 paper based on the wide pubmed.articles2.Rda 
#file from (step 1) and the long disambiguated file pubmed_authors2 (step 2).
#Due to differences in the way country names are written by authors we have
#to engage some cleaning of the data "by hand."
######################################################################################


library(dplyr)
library(tidyr)
library(reshape2)
library(tmap)
library(tm)

load("data/pubmed_articles2.Rda")
load("data/pubmed_authors2.Rda")

# First, let's strip out everything that can't plausibly be about this particular outbreak
pma <- article_df
npap <- length(unique(pma$pmid))
nauth <- length(unique(pma$fullname))


#the map situates each paper by the first author. we select the first author here. 

auth1 <- pma %>% filter(whichauth=="au1")

#matching words in address with country names
library(maps)
data("world.cities")
auth1$address.update <- gsub("[[:punct:]\n]","",auth1$address)
auth1$address.update <- strsplit(auth1$address.update, " ")
auth1$country <- (lapply(auth1$address.update, function(x)x[which(toupper(x) %in% toupper(world.cities$country.etc))]))

#finding country as last word after comma in the address
auth1$lwrd <- sub('.*\\,', '', auth1$address)

#comparing results
pm_country <- auth1 %>% select(pmid, address, lwrd)

pm_country$lwrd <- trimws(pm_country$lwrd, which = "both")


#exporting pm_country to clean it by hand
#write.csv(pm_country, "data/country_uncleaned.csv", row.names = FALSE)


#reading in the hand cleaned file of countries and the tail of each address 
country_cleaned <- read.csv("data/country_cleaned.csv", stringsAsFactors = F)

country_cleaned <- country_cleaned %>% select(lwrd, country)

mcountry.df <- left_join(pm_country, country_cleaned, by="lwrd")

mdup <- mcountry.df[!duplicated(mcountry.df[c(1,4)]),]

mdup <- mdup %>% filter(country!="character(0)")


#this cleans out some type-os in the country variable for matching with the map package
mdup$country[mdup$country == "Uk"] <- "United Kingdom"
mdup$country[mdup$country == "UK"] <- "United Kingdom"

mdup$country[mdup$country == "USA"] <- "United States"
mdup$country[mdup$country == "Jersey"] <- "United States"
mdup$country[mdup$country == "Georgia"] <- "United States"

mdup$country[mdup$country == "Slovak Republic"] <- "Slovakia"

mdup$country[mdup$country == "South Korea"] <- "Korea"
mdup$country[mdup$country == "South"] <- "Korea" #excel didnt autofill 3 times and it was korea for each
mdup$country[mdup$country == "South Koea"] <- "Korea" 

mdup$country[mdup$country == "South AFrica"] <- "South Africa"

mdup$country[mdup$country == "Czech Republic"] <- "Czech Rep."

mdup$country[mdup$country == "Dominican Republic"] <- "Dominican Rep."

mdup$country[mdup$country == "Cina"] <- "China"
mdup$country[mdup$country == "china"] <- "China"
mdup$country[mdup$country == "Hong Kong"] <- "China"

mdup$country[mdup$country == "Bosnia and Herzegovina"] <- "Bosnia and Herz."

mdup$country[mdup$country == "Brunei Darussalam"] <- "Brunei"

mdup$country[mdup$country == "india"] <- "India"

mdup$country[mdup$country == "Taiwan"] <- "Tawain"

#let's save this for other things to merge using pmid

pma_country <- mdup

save(pma_country, file="contexts_2/data_contexts2/pmid_country.Rda")

mdup_freq <- as.data.frame(table(mdup$country))


sum(mdup_freq$Freq)

clean_country_freq <-rename(mdup_freq, name=Var1, artnum=Freq)




# make map

data("World")


#merge countries from the data with map dataframe
World <- left_join(World, clean_country_freq)

art_map <- tm_shape(World) +
  tm_polygons("artnum", title = "Number of Articles", 
              breaks=c(1, 10, 50, 100, 200, 1000, 2500, 5000),
              palette = "Blues",
              textNA = "0 articles",
              colorNA = "white")

# save

tmap_save(art_map, "figs_contexts/artmap_fin.jpg")
