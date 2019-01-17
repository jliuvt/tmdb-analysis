library(plyr) #data manipulation
library(tidyverse)# data manipulation
library(formattable)# table
library(splitstackshape) # split columns
library(jsonlite) #JSON format 
library(wordcloud) #wordcloud
library(RColorBrewer) # Color Theme
library(ggthemes) #Themes for plot
library(tm) # Sentiment Analysis 
library(RSentiment) # Sentiment Analysis
library(zoo) # Time 
library(stringr) #String Manipulation
movie=read_csv("tmdb_5000_movies.csv",col_names=TRUE,na="NA")
credits=read_csv("tmdb_5000_credits.csv",col_names=TRUE,na="NA")


glimpse(movie)
summary(movie)

# assign each genre labels from the genres to 
genredf=movie %>% filter(nchar(genres)>2) %>% mutate(js=lapply(genres,fromJSON)) %>% unnest(js) %>% select(id,title,genre=name) #Convert JSON format into data frame
slice(genredf)


#genre=aggregate(genre ~.,data=genre,paste,collapse=",") # remove duplicates 
temp=genredf %>% group_by(genre) %>% summarise(count=length(genre)) %>% arrange(desc(count))#A look at the genre variety in our dataset
#wordcloud
wordcloud(words=temp$genre,freq=temp$count,min.freq=100,max.words = 20,random.order=FALSE,random.color=FALSE,rot.per=0,colors = brewer.pal(20,"Dark2"),scale=c(5,.2))




movie %>% select(original_title,budget) %>% drop_na(original_title)%>% arrange(desc(budget)) %>% head(10) %>%  ggplot(aes(reorder(original_title,budget),budget,fill=original_title))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Red",face="italic"),legend.position="none")+scale_y_continuous(labels=scales::comma)+labs(x="",y="Total Budget in $",title="Most Expensive Movies -Top 10")
movie %>% select(original_title, revenue) %>% drop_na(revenue) %>% arrange(desc(revenue)) %>% head(5) %>% ggplot(aes(reorder(original_title, revenue), revenue, fill=original_title)) +geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 90), plot.title = element_text(color = "Red", face = "italic"), legend.position = "none")+scale_y_continuous(labels = scales::comma)+labs(x="", y="Total Revenue in $", title = "Most Profitable Movie - Top10")


movie %>% select(original_title,revenue) %>% drop_na(original_title)%>% arrange(desc(revenue)) %>% head(10)  %>% ggplot(aes(reorder(original_title,revenue),revenue,fill=original_title))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Red",face="italic"),legend.position="none")+scale_y_continuous(limits=c(0,3000000000),breaks=seq(0,3000000000,500000000),labels=scales::comma)+labs(x="",y="Total Revenue in $",title="Highest Grossing Movies -Top 10")


movie %>% group_by(original_title) %>% arrange(desc(popularity))%>%head(10)%>%ggplot(aes(factor(original_title, levels=original_title), popularity, fill=original_title))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 15, color = "red"), legend.position="none")+labs(x="Title", y="Rating", title="popularity")+scale_x_discrete(labels=function(x)str_wrap(x, width=15))




production=movie %>% filter(nchar(production_companies)>2) %>% mutate(js=lapply(production_companies,fromJSON)) %>% unnest(js) %>% select(budget,revenue,company=name)
lapply(production,class)
temp=production %>% group_by(company) %>% summarise(count=n()) %>% arrange(desc(count)) 
wordcloud(words=temp$company,freq=temp$count,max.words = 25,color =rainbow(7),scale=c(2,0.2))


class(movie$release_date)
movie$Year=as.factor(format(movie$release_date,"%Y"))
movie$Date=as.factor(format(movie$release_date,"%d"))
movie$month=month.abb[(as.factor(format(movie$release_date,"%m")))]
movie %>% group_by(month) %>% drop_na(month) %>% summarise(count=n()) %>% arrange(desc(month)) %>% ggplot(aes(reorder(month,count),count,fill=month))+geom_bar(stat="identity")+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Total number of movies released",title="Number of Movies Releases per month")+coord_flip()+geom_label(aes(label=count))


movie %>% drop_na(month) %>% ggplot(aes(month,vote_average,fill=month))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()
