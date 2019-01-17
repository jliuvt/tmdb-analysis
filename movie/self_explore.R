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
library(plotly)
library(ggplot2)

movie=read_csv("tmdb_5000_movies.csv",col_names=TRUE,na="NA")
credits=read_csv("tmdb_5000_credits.csv",col_names=TRUE,na="NA")


new_data_movie = read_csv("./the-movies-dataset/movies_metadata.csv",col_names=TRUE,na="NA")
new_data_movie$Year=as.integer(format(new_data_movie$release_date,"%Y"))

glimpse(movie)
summary(movie)
Year=as.integer(format(movie$release_date,"%Y"))
movie$Year=as.factor(format(movie$release_date,"%Y"))
movie$decade = as.factor(Year-Year%%10)
movie$Date=as.factor(format(movie$release_date,"%d"))
movie$month=(as.factor(format(movie$release_date,"%m")))
# assign each genre labels from the genres to 
genredf=movie %>% filter(nchar(genres)>2) %>% mutate(js=lapply(genres,fromJSON)) %>% unnest(js) %>% select(id,title,decade, Year,month, popularity, budget, revenue, vote_average,vote_count,genre=name) #Convert JSON format into data frame
slice(genredf)



genredf=movie %>% filter(nchar(genres)>2) %>% mutate(js=lapply(genres,fromJSON)) %>% unnest(js) %>% select(id,title,decade, Year,month, popularity, vote_average,vote_count,genre=name) #Convert JSON format into data frame
slice(genredf)

uniq_genre = unique(genredf$genre)

tt= matrix(0,nrow=20, ncol=20)
colnames(tt) = uniq_genre
rownames(tt) = uniq_genre
uniq_id = unique(genredf$id)

movie_one_genre = data.frame()


for (idd in 1:length(uniq_id))
{
  movie_one_genre = rbind(movie_one_genre, genredf[genredf$id==uniq_id[idd],][1,])
}

for (idd in 1:length(uniq_id))
{
  temp_df = genredf[genredf$id==uniq_id[idd],]
  
  
  m_genre <- temp_df$genre
  m_len = length(m_genre)
  
  for (i in 1: m_len)
  {
    for (j in 1:m_len)
    {
      
      tt[m_genre[i],m_genre[j]]=1+tt[m_genre[i],m_genre[j]]
    }
    
  }

}


sim_mat=tt

diag(sim_mat)=0

aff_mat = sim_mat/colSums(sim_mat)




uniq_genre = unique(genredf$genre)



#genre=aggregate(genre ~.,data=genre,paste,collapse=",") # remove duplicates 
order_count=genredf %>% group_by(genre, decade) %>% summarise(count=length(genre))%>%filter(decade==2010)%>%arrange(desc(count)) %>% head(10) 
#wordcloud
order_popularity=genredf %>% group_by(genre, decade) %>% filter(popularity>0)%>% summarise(pop=mean(popularity))%>%filter(decade==2010)%>%arrange(desc(pop)) %>% head(10) 

order_vote=genredf %>% filter(vote_average>0 & vote_average<10) %>% group_by(genre, decade) %>% summarise(vote=mean(vote_average))%>%filter(decade==2010)%>%arrange(desc(vote)) %>% head(10) 
order_vote_count=genredf %>% filter(vote_count>50) %>% group_by(genre, decade) %>% summarise(vote_c=mean(vote_count))%>%filter(decade==2010)%>%arrange(desc(vote_c)) %>% head(10) 
p <- ggplot(temp, aes(decade,count,group=genre, colour = genre)) + geom_point()+geom_line()

gg <- ggplotly(p)

wordcloud(words=temp$genre,freq=temp$count,min.freq=100,max.words = 20,random.order=FALSE,random.color=FALSE,rot.per=0,colors = brewer.pal(20,"Dark2"),scale=c(5,.2))

temp_month=genredf %>% group_by(genre, month) %>% summarise(count=length(genre))
#wordcloud
p <- ggplot(temp_month, aes(month,count)) + geom_point()+geom_line()

ggplotly(p)


temp_year=movie_one_genre %>% group_by(genre,decade) %>% summarise(rev_mean=mean(revenue))

p <- ggplot(temp_year, aes(decade,rev_mean,group=genre, colour = genre)) + geom_point()+geom_line()

ggplotly(p)

#wordcloud
p <- ggplot(temp_month, aes(month,count,group=genre, colour = genre)) + geom_point()+geom_line()

ggplotly(p)

temp_f = genredf %>% group_by(month) %>% summarise(count=length(unique(id)))
p <- ggplot(temp_f, aes(month,count,group=1)) + geom_point()+geom_line()
ggplotly(p)


temp_f = genredf%>%filter(genre %in% order_count$genre) %>% group_by(month,genre) %>% summarise(count=length(genre))
p <- ggplot(temp_f, aes(month,count)) + geom_bar(stat = "identity",aes(fill=genre))
ggplotly(p)

temp_f1 = temp_f %>% group_by(month) %>% mutate(total_num=sum(count))%>% mutate(percet= count/total_num)
p <- ggplot(temp_f1, aes(month,percet)) + geom_bar(stat = "identity",aes(fill=genre))
ggplotly(p)

tt= matrix(nrow=10, ncol=12)
colnames(tt) = 1:12
rownames(tt) =1:10

for (i in 1:12)
{
  x <- temp_f %>% filter(as.numeric(month) == i)%>%arrange(desc(count))
  
  tt[,i]=x$genre
  
}



movie %>% select(original_title,budget) %>% drop_na(original_title)%>% arrange(desc(budget)) %>% head(10) %>%  ggplot(aes(reorder(original_title,budget),budget,fill=original_title))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Red",face="italic"),legend.position="none")+scale_y_continuous(labels=scales::comma)+labs(x="",y="Total Budget in $",title="Most Expensive Movies -Top 10")
movie %>% select(original_title, revenue) %>% drop_na(revenue) %>% arrange(desc(revenue)) %>% head(5) %>% ggplot(aes(reorder(original_title, revenue), revenue, fill=original_title)) +geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 90), plot.title = element_text(color = "Red", face = "italic"), legend.position = "none")+scale_y_continuous(labels = scales::comma)+labs(x="", y="Total Revenue in $", title = "Most Profitable Movie - Top10")


movie %>% select(original_title,revenue) %>% drop_na(original_title)%>% arrange(desc(revenue)) %>% head(10)  %>% ggplot(aes(reorder(original_title,revenue),revenue,fill=original_title))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Red",face="italic"),legend.position="none")+scale_y_continuous(limits=c(0,3000000000),breaks=seq(0,3000000000,500000000),labels=scales::comma)+labs(x="",y="Total Revenue in $",title="Highest Grossing Movies -Top 10")


movie %>% group_by(original_title) %>% arrange(desc(popularity))%>%head(10)%>%ggplot(aes(factor(original_title, levels=original_title), popularity, fill=original_title))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 15, color = "red"), legend.position="none")+labs(x="Title", y="Rating", title="popularity")+scale_x_discrete(labels=function(x)str_wrap(x, width=15))



profitable_genre <- movie %>% filter(nchar(genres)>2 & budget>0 & revenue >0) %>% mutate(js=lapply(genres,fromJSON)) %>% unnest(js) %>% select(budget,revenue,genre=name)%>%mutate(profitability= (revenue-budget)/budget )


profitable_genre%>% group_by(genre)%>%summarise(mean_profitability=max(profitability))%>%arrange(desc(mean_profitability))%>%head(10)

temp=genredf %>% filter(nchar(genre)>2 & budget>0 & revenue >0)%>%mutate(profitability= (revenue-budget)/budget )



genredf %>% filter(nchar(genre)>2 & budget>500 & revenue >50)%>%mutate(profitability= (revenue-budget)/budget )%>% filter(profitability<100) %>% ggplot(aes(genre,profitability,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()

movie_one_genre %>% filter(decade=="2010"|decade=="2000" & budget>500 & revenue >500)%>% ggplot(aes(genre,revenue,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()
movie_one_genre %>% filter(decade=="2010"|decade=="2000" & budget>500 & revenue >500)%>%mutate(profitability= (revenue-budget)/budget )%>% filter(profitability<100) %>% ggplot(aes(genre,profitability,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()

movie_one_genre %>% filter(decade=="2010"|decade=="2000" & budget>500 & revenue >500)%>% ggplot(aes(genre,budget,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()

movie_one_genre %>% filter((decade=="2010"|decade=="2000") & budget>500 & revenue >500 & popularity>0 & popularity<25)%>% ggplot(aes(genre,popularity,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()

temp=movie_one_genre %>% filter((decade=="2010"|decade=="2000") & budget>500 & revenue >500 & popularity>0 & popularity<25)


movie_new =movie_one_genre %>% filter(decade=="2010"|decade=="2000" & budget>500 & revenue >500)


movie_one_genre %>% filter(decade=="2010"|decade=="2000" & budget>500 & revenue >500)%>% ggplot(aes(genre,popularity,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()



genredf %>% filter(nchar(genre)>2 & budget>500 & revenue >50)%>%mutate(profitability= (revenue-budget)/budget )%>% filter(profitability>500) 

genredf %>% filter(nchar(genre)>2 & budget>0 & revenue >0)%>% ggplot(aes(genre,budget,fill=genre))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()


movie %>% drop_na(month) %>% ggplot(aes(month,vote_average,fill=month))+geom_boxplot(outlier.colour = "red",na.rm=TRUE)+theme(plot.title=element_text(size=14,face="italic",colour="red"),axis.text.x = element_text(angle=90),legend.position="none")+labs(x="",y="Average Vote",title="Boxplot of Average votes received by month")+coord_flip()



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





# Let us now move into credits dataset and explore.The columns are in json format and like we did for geners we use the *jsonlite* library to convert it into proper format.


credit=credits %>% filter(nchar(cast)>2) %>% mutate(js=lapply(cast,fromJSON)) %>% unnest(js)
cat("Before JS the columns are ",names(credit),sep='\n')
credit= credit%>% select(-c(crew,cast_id,credit_id,id))

cat("After removing id columns the names are",names(credit),sep='\n')


directors = credits %>% filter(nchar(crew)>2)%>% mutate(js=lapply(crew,fromJSON)) %>% unnest(js)
cat("Before JS the columns are ",names(directors),sep='\n')
directors = directors %>% select(-c(cast,crew,credit_id)) %>% subset(job=='Director')

directors %>% group_by(name) %>%tally() %>% arrange(desc(n))%>% head(10)%>% ggplot(aes(factor(name,levels=name),n,fill=name))+geom_bar(stat="identity")+labs(x="Director",y="Count",title="Top 10 directors with most movies")+theme_few()+theme(axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0.5,color="red"),legend.position="none")

#Artist with most movies:
#Let us see which artist has acted in more movies
credit %>% group_by(name) %>% tally() %>% arrange(desc(n)) %>% head(10) %>% ggplot(aes(factor(name,levels=name),n,fill=name))+geom_bar(stat="identity")+labs(x="Artist",y="Count",title="Top 10 artist with most movies")+theme_few()+theme(axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0.5,color="red"),legend.position="none")

# Popular movies of the artist with most movies:{.tabset .tabset-pill}
db=movie %>% left_join(credits,by=c("id"="movie_id"))
db_credit=db %>% filter(nchar(cast)>2) %>% mutate(js=lapply(cast,fromJSON)) %>% unnest(js)
revenue=function (df,col_name,x,y,title){
  temp_df=df %>% filter(name==col_name) %>% arrange(desc(revenue)) %>% head(10)
  df_plot= ggplot(temp_df,aes(reorder(original_title,revenue),revenue,fill=original_title))+geom_bar(stat="identity")+theme_few()+theme(axis.text.x = element_text(angle=90,vjust=0.5),plot.title=element_text(hjust=0.5,size=15),legend.position="none")+labs(x=x,y=y,title=title)+coord_flip()+scale_x_discrete(labels=function(x)str_wrap(x,width=15))+scale_y_continuous(labels=scales::comma)
  print(df_plot)
}



## Samuel L.Jackson:

revenue(db_credit,col_name="Samuel L. Jackson",x="Title",y="Revenue",title="Samuel L.Jackson-Top 10 by revenue" )



## Robert De Niro:

revenue(db_credit,col_name="Robert De Niro",x="Title",y="Revenue",title="Robert De Niro-Top 10 by revenue" )


## Bruce Willis:

revenue(db_credit,col_name="Bruce Willis",x="Title",y="Revenue",title="Bruce Willis-Top 10 by revenue" )



## Matt Damon:
revenue(db_credit,col_name="Matt Damon",x="Title",y="Revenue",title="Matt Damon-Top 10 by revenue" )



## Morgan Freeman:
revenue(db_credit,col_name="Morgan Freeman",x="Title",y="Revenue",title="Morgan Freeman-Top 10 by revenue")


# Artist with High Budget Movies :

# Let us see which artists movies have been made at high budgets.
db_credit %>%  filter(order==0) %>% group_by(name) %>% summarise(total=sum(budget)) %>% arrange(desc(total)) %>% head(10) %>% ggplot(aes(factor(name,levels=name),total,fill=name))+geom_bar(stat="identity")+theme(legend.position="none",plot.title=element_text(size=10,hjust=0.5),axis.text.x=element_text(angle=90))+labs(x="Actor Name",y="Sum of the budget",title="Actor with Highest Budgets till date..")+scale_y_continuous(labels=scales::comma)


#Tom Cruise movies have always been on a higher budget followed by Bruce Wills.Let us compare the popularity of the movies with respect to their top 10 high budget.

# Does high budget movie necessarily mean high popularity among viewers? {.tabset .tabset-pill}
# Let us correlate budget and popularity and see if there exist any correlation.

get_cor <- function(df){
  m <- cor(df$x,df$y, use="pairwise.complete.obs");
  eq <- substitute(expr=r==cor,env=list(cor=format(m, digits = 4)))
  return(as.character(as.expression(eq) ))                
}

temp=db_credit %>%  select(budget,popularity) %>% distinct() 
ggplot(temp,aes(budget,popularity))+stat_bin_hex(bins=15)+scale_fill_distiller(palette="Spectral")+stat_smooth(method="lm",color="orchid",size=2)+scale_x_continuous(labels=scales::comma)
get_cor(data.frame(x=temp$budget,y=temp$popularity))



## For Tom Cruise

temp=db_credit %>% filter(name=="Tom Cruise") %>% subset(!(duplicated(original_title))) %>% arrange(desc(budget)) %>% head(20)  

ggplot(temp,aes(budget,as.integer(popularity),col=factor(original_title),size=popularity))+geom_point()+stat_smooth(method="lm",color="orchid",size=2)+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=10),axis.text.x = element_text(angle = 90,hjust=0.5))+labs(x="Revenue",y="Popularity",title="Tom Cruise-Correlation between popularity and budget",subtitle="Filter by Top 20 Higest budget movies")+scale_x_continuous(labels=scales::comma)

# It seems that there exists a linear line between revenue and popularity for tom cruise movies as shown in the plot.

## For Bruce Wills:
temp=db_credit %>% filter(name=="Bruce Willis") %>% subset(!(duplicated(original_title))) %>% arrange(desc(budget)) %>% head(20)  

ggplot(temp,aes(budget,as.integer(popularity),col=factor(original_title),size=popularity))+geom_point()+stat_smooth(method="lm",color="orchid",size=2)+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=10),axis.text.x = element_text(angle = 90,hjust=0.5))+labs(x="Revenue",y="Popularity",title="Bruce Wills-Correlation between popularity and budget",subtitle="Filter by Top 20 Higest budget movies")+scale_x_continuous(labels=scales::comma)



