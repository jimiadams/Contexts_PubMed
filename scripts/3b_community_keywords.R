######################################################################################
#This provides a quick means for looking at how the co-authorship network communities 
#are related to keywords using the author_long2.Rda file from the 
#disambiguation routine (step 2), pubmed_articles2.Rda (step1) and the commemb.RDA file 
#from step 3
#####################################################################################

library(dplyr)
library(tidyr)
library(reshape2)

load("contexts_2/data_contexts2/pubmed_articles2.Rda")
load("contexts_2/data_contexts2/author_long2.Rda")
load("contexts_2/data_contexts2/commemb.Rda")


npap <- length(unique(pma2$pmid))
nauth <- length(unique(pma2$nid))

pma3 <- left_join(pma2, article_df)

pma_kw <- pma3 %>% drop_na(keywords)

pma_kw <- pma_kw %>% group_by(nid) %>% mutate(nkw=seq_along(nid))

pma_kw$whichkw <-  sub("^", "kw", pma_kw$nkw)

kw_df <- dcast(pma_kw, nid ~ whichkw, value.var="keywords")

kw_df <- kw_df %>% unite(keyfull, matches("^kw"), sep=";", na.rm=TRUE, remove=FALSE)

#let's get rid of very common words that we used to download

mesh_list <- c("SARS-COV-2", "covid-19" , "COVID-19" , "novel corona virus", 
"novel coronavirus" , "severe acute respiratory syndrome coronavirus 2" , "2019-nCoV",
"COVID19 virus" , "Wuhan coronavirus", "coronavirus disease 2019 virus",
"Wuhan seafood market pneumonia virus" , "2019 novel coronavirus", "SARS2")

mesh_list <- tolower(mesh_list)

library("tm")

kw_df$keyfull <- tolower(kw_df$keyfull)

kw_df$keyfull <- removeWords(kw_df$keyfull, mesh_list)

library(splitstackshape)

au_kw <- cSplit(kw_df, "keyfull", ";")

#merge with author by groups file

amerge <- left_join(amerge, au_kw, by=c("name"="nid"))

amerge <- amerge %>% select(bcommunity, starts_with("keyfull"))

along <- amerge %>% gather(keyword, gwrds, `keyfull_001`:`keyfull_036`)

along <- along %>% drop_na(gwrds)

along <- along %>% select(bcommunity, gwrds)

library("stringr")

along$cv <- str_count(along$gwrds, "coronavirus")

along <- along %>% filter(cv==0)

along <- along %>% na_if("") %>% drop_na(gwrds)

kw_count <- along %>% group_by(bcommunity) %>% count(gwrds)

kw_count <- arrange(kw_count, bcommunity, desc(n)) 

kw_top<- kw_count %>% filter(n>5)

kw_top <- kw_top  %>% drop_na(bcommunity)

kw_top <- kw_top[order(bcommunity, -n),]

#For reference
#colrs <- c("cadetblue2" , "darkseagreen", "goldenrod1", "tomato2", "darkolivegreen1", "darkorchid3", "blue", "pink3")


#save the top keywords file
#save(kw_top, file="data/top_keywords.Rda")


#####BELOW NOT USED################################

#generate list of authors on multiple papers


au.freq <- as.data.frame(table(pma$fullname))
au.freq <- mutate(au.freq, auid = rownames(au.freq))
au.freq <- rename(au.freq, fullname = Var1)



pma <- left_join(pma, au.freq)

multi <- pma %>% filter(Freq>1)

write.csv(multi, file="multi_auth.csv")