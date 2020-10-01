######################################################################################
#This makes the cumulative line graph (wihtout Loess) for Contexts manuscript from
#the wide file (step 1)
######################################################################################


library(ggplot2)
library(dplyr)

load("data/pubmed_articles2.Rda")

article_df$date <- as.Date(with(article_df, paste(year, month, day,sep="-")), "%Y-%m-%d")

article_df$date

count_day <- as.data.frame(table(article_df$date))

count_day$date <- as.Date(count_day$Var1)

count_day <- count_day %>% filter(date !=	"2020-06-12")

p <- ggplot(count_day, aes(x=date, y=Freq, group=1)) +
  geom_smooth(method = "loess", se=FALSE, color="Black")+
  labs(y= "# of Articles", x = "Month (2020)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(p, file="figs_contexts/line_growth.png")

#cumulative graph

c <- ggplot(count_day, aes(x=date, y=cumsum(Freq), group=1)) +
  geom_smooth(method = "loess", se=FALSE, color="tomato1" , size=2)+
  labs(y= "# of Articles", x = "Month (2020)", size=14)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=12))

ggsave(c, file="figs_contexts/line_cumulative.jpg")

nosmooth <- ggplot(count_day, aes(x=date, y=cumsum(Freq), group=1)) +
  geom_line(color="tomato1", size=2)+
  labs(y= "# of Articles", x = "Month (2020)", size=14)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=12))

ggsave(nosmooth, file="figs_contexts/line_cumulative_nosmooth.jpg")
