######################################################################################
#This makes structural topic models for the contexts article on COVID-19 from
#the wide articles file (step 1) and also uses the county file pmid_country.Rda from
#step 4
######################################################################################


library(dplyr)
library(tidyr)
library(stm)
library(igraph)


load("data/pubmed_articles2.Rda")

#Gather the text files

article_txt <- article_df %>% unite(abstr.title.key, title, abstract, keywords, sep=", ", remove=FALSE)

jcount <- as.data.frame(table(article_txt$journal))

abstr.title.key <- article_txt %>% select(pmid, title, abstr.title.key, month)


#This is the covariate for "time" 

abstr.title.key$time <- abstr.title.key$month

abstr.title.key$time <- recode(abstr.title.key$time, '12'='00')

abstr.title.key$time <- as.numeric(abstr.title.key$time)

table(abstr.title.key$time)


#This is the first step of the stm
processed <- textProcessor(abstr.title.key$abstr.title.key, metadata = abstr.title.key)

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 50)

age_topics <- searchK(out$documents, out$vocab, K = seq(10, 20, 2),
                      prevalence =~ s(time), proportion=.2, heldout.seed=02138, data = out$meta)

plot(age_topics)

plot(age_topics$results$semcoh, age_topics$results$exclus, )
text(age_topics$results$semcoh, age_topics$results$exclus, labels=age_topics$results$K,
     cex=0.7, pos=2)

lda_age <- stm(out$documents, out$vocab, K = 15,
               prevalence =~ s(time), data = out$meta, init.type = "Spectral")

#save especially because the stem can take hours to run
#save(lda_age, file="data/lda_age.rda")

#load the above

load("data/lda_age.rda")

labelTopics(lda_age, c(1:15), n=30, frexweight=.75)

sageLabels(lda_age, n=20)

topicQuality(lda_age, documents=out$documents)

plot(lda_age, type="summary")

mod.out.corr <- topicCorr(lda_age)

plot(mod.out.corr)

plot(mod.out.corr, cutoff=.0)

cormat <- mod.out.corr$cor

cornet <- graph_from_adjacency_matrix(cormat, mode=c("undirected"), weighted=TRUE, diag=FALSE)

plot(cornet)

cornet2 <- delete.edges(cornet, which(E(cornet)$weight < 0))

l = layout.fruchterman.reingold(cornet2)


V(cornet2)$color <- "gold"

topsize <- colSums(lda_age$theta)

V(cornet2)$size <- topsize/50


#lwrds <- c("1.Forestry & Ecosystems", "2.Water & Geology", "3.Ice & Glaciers", "4.Ecology and Agronomy", "5.Genetics",
#           "6.Meteorology", "7.Policy & Prediction", "8.Methods", "9.Oceans", "10.Atmosphere")


#Topic Correlation Figure

plot(cornet2, layout=l, , main = "Topic Correlation Network", vertex.label.color="black",
     vertex.label.family="Helvetica")

#find whether countries are related to topics

load("data/pmid_country.Rda")

theta <- as.data.frame(lda_age$theta)

theta_vars <- cbind(out$meta, theta)

theta_vars$pmid <- as.numeric(theta_vars$pmid)

pma_country$pmid <- as.numeric(pma_country$pmid)

theta_vars <- left_join(theta_vars, pma_country)

cntry <- as.data.frame(table(theta_vars$country))

theta_vars$us <- ifelse(theta_vars$country=="United States", 1, 0)
theta_vars$china <- ifelse(theta_vars$country=="China", 1, 0)
theta_vars$italy <- ifelse(theta_vars$country=="Italy", 1, 0)
theta_vars$uk <- ifelse(theta_vars$country=="United Kingdom", 1, 0)
theta_vars$france <- ifelse(theta_vars$country=="France", 1, 0)
theta_vars$india <- ifelse(theta_vars$country=="India", 1, 0)

#nwidth index - MAKE SURE TO ADJUST COLUMN IDS

theta_vars$nwidth <- 1 - apply(theta_vars[,6:20]^2, 1, sum, na.rm=T) 


#These OLS regressions give some additional insight into the communities to be used for evaluation 
#and visualization. They are substantively similar to logistic regressions. And is only needed
#because some pieces of the stm package are broken

reg1 <- lm(V1 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg1)

reg2 <- lm(V2 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg2)

reg3 <- lm(V3 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg3)

reg4 <- lm(V4 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg4)

reg5 <- lm(V5 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars )

summary(reg5)


reg6 <- lm(V6 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg6)


reg7 <- lm(V7 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg7)


reg8 <- lm(V8 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg8)

reg9 <- lm(V9 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg9)

reg10 <- lm(V10 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg10)

reg11 <- lm(V11 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg11)

reg12 <- lm(V12 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg12)


reg13 <- lm(V13 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg13)

reg14 <- lm(V14 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg14)

reg15 <- lm(V15 ~ us + china + italy + uk + france + india + nwidth + time, data=theta_vars)

summary(reg15)

save(theta_vars, file="data/topic_loadings.rda")

#This creates a mds plot of the topics

library(vegan)

topmat <- as.matrix(theta)

tmat <- t(topmat)

topic.mds <- metaMDS(comm = tmat, distance = "bray", trace = FALSE, autotransform = FALSE)

topic.mds$stress

MDS_xy <- data.frame(topic.mds$points)

topsize <- colSums(lda_age$theta)

library(ggplot2)

lab <- c("1. dermatolog", "2. retrospect" , "3. swab" , "4. survey" , "5. outpatient",
           "6. genetics" , "7. cytokin" , "8. surgery" , "9. mental" , "10. pneumonia" ,
           "11. respiratoriy" , "12. estimate" , "13. china" ,"14. trial" , "15. aerosol")

rate <- as.factor(c(1, 1, 1, 1, 1, 3, 1, 1, 2, 3, 2, 1, 3, 1, 1))

colr <- c("darkseagreen" , "darkseagreen" , "darkseagreen" , "darkseagreen" , "darkseagreen" , 
          "tomato1" , "darkseagreen" , "darkseagreen" , "goldenrod2" , "tomato1" , 
          "goldenrod2" , "darkseagreen" , "tomato1" , "darkseagreen" , "darkseagreen")

topic_mds <- ggplot(MDS_xy, aes(MDS1, MDS2, label=lab, color=rate)) +
ggtitle("COVID-19 Science Topics") +
geom_point(size=topsize/125) +
geom_text(hjust = 0, nudge_x = 0.05, color="black") +
theme_bw() +
theme(panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y = element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
      scale_color_manual("Topic Change" , breaks = c("1", "2", "3"),
                     values=c("darkseagreen", "goldenrod2", "tomato1"),
                     labels = c("Increasing", "No Change", "Decreasing"))
  

ggsave("figs_contexts/topic_mds.jpg", topic_mds)





