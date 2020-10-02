######################################################################################
#This provides a quick means for looking at how the co-authorship network communities 
#are related to geographic location using the author_long2.Rda file from the 
#disambiguation routine (step 2), pubmed_articles2.Rda (step1) and the commemb.RDA file 
#from step 3
######################################################################################


library(tidyr)
library(dplyr)

load("data/commemb.Rda")
load("data/author_long2.Rda")
load("data/pubmed_articles2.Rda")

#first make quick dataframe capturing location - more involved version in 5_make_art_map_contexts

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


amerge <- amerge %>% drop_na(bcommunity)

bmer <- left_join(amerge, pma2, by=c("name"="nid"))

bmer$pmid <- as.numeric(bmer$pmid)

cmer <- left_join(bmer, pm_country)

cmer <- cmer %>% drop_na(country)

countrybycolor <- as.data.frame(table(cmer$country, cmer$bcolor))

check <- as.data.frame(table(pm_country$country))