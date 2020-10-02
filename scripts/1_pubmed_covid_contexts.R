#First we use the easyPubMed package (https://cran.r-project.org/web/packages/easyPubMed/index.html)
#to download relevant articles. 


library(easyPubMed)
my.query <- "\"SARS-COV-2\" OR \"covid-19\" OR \"COVID-19\" OR \"novel corona virus\" OR \"novel coronavirus\" OR \"severe acute respiratory syndrome coronavirus 2\" OR \"2019-nCoV\" OR \"COVID19 virus\" OR \"Wuhan coronavirus\" OR \"coronavirus disease 2019 virus\" OR \"Wuhan seafood market pneumonia virus\" OR \"2019 novel coronavirus\" OR \"SARS2"
my.idlist <- get_pubmed_ids(my.query)

my.idlist$Count

#Note: 28,909 as of 7/3/2020

# This grabs pubmed as text files
pm_text <- batch_pubmed_download(pubmed_query_string = my.query,
                               dest_file_prefix = "COVID_PM_")

# Then we can read them by author

library(data.table)
library(readr)

list.records <- list.files(pattern = "*.txt")

pm_list <- lapply(as.list(list.records), read_file)

new_PM_df <- list()
new_PM_file <- list()

for(i in 1:length(pm_list)) {
  new_PM_file[[i]] <- pm_list[[i]]
  new_PM_df[[i]] <- table_articles_byAuth(pubmed_data = new_PM_file[[i]], 
                        included_authors = "all", 
                        max_chars = 10000,
                        autofill=TRUE,
                        getKeywords=TRUE,
                        encoding = "UTF-8")
}


#Next we reshape to make a long author file

library(dplyr)
library(reshape2)
library(tidyr)

#author_pm_long <- bind_rows(new_PM_df, new_PM_df2, new_PM_df3, new_PM_df4)

author_pm_long <- rbindlist(new_PM_df)

author_pm_long <- author_pm_long %>% unite(fullname, lastname:firstname, sep=".", remove=FALSE)

author_pm_long <- author_pm_long %>% group_by(pmid) %>% mutate(nauth=seq_along(pmid))

author_pm_long$whichauth <-  sub("^", "au", author_pm_long$nauth )

author_pm_long$whichauth <- as.factor(author_pm_long$whichauth ) 

article_df <- dcast(author_pm_long, pmid ~ whichauth, value.var="fullname")

library(gtools)

article_df <- article_df[mixedorder(colnames(article_df))]

art_dat <- author_pm_long[match(unique(author_pm_long$pmid), author_pm_long$pmid),]

article_df <- left_join(article_df, art_dat)

article_df <- as.data.frame(1:3)

#save the article file
save(article_df, file="data/pubmed_articles.Rda")

#save the long file
save(author_pm_long, file="data/pubmed_authors.Rda")


#gathering 1000 top stemmed words from titles, abstracts, keywords
library(dplyr)
library(tidytext)
library(janeaustenr)
library(SnowballC)

article_txt <- article_df %>% unite(abstr.title.key, title, abstract, keywords, sep=", ", remove=FALSE)

abstr.title.key <- article_txt %>% select(abstr.title.key)

stemmed.list <- abstr.title.key %>%
  unnest_tokens(word, abstr.title.key) %>%
  mutate(word = wordStem(word))

stemmed.list <- stemmed.list %>%
  count(word, sort = T)

#this ensures the correct dates for the wide article file

article_df <- article_df %>% filter(month!="07", year>2019)

save(article_df, file="contexts_2/data_contexts2/pubmed_articles2.Rda")


