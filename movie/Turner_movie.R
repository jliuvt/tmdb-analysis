library("RODBC")
library(data.table)

cn <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=INSIGHTDATA;database=Genesis;trusted_connection=yes;")

hquery <- ("select * from [TNT&TBSMoviesSince2008_Genres]")
df_genre <- sqlQuery(cn, hquery)


hquery <- ("select * from [TNT&TBSMoviesSince2008_Awards]")
df_awards <- sqlQuery(cn, hquery)



hquery <- ("select * from [TNT&TBSMoviesSince2008]")
df_bc_o <- sqlQuery(cn, hquery)

unique_names <- sort(unique(df_bc_o$Title_Name))






df_bc = unique(subset(df_bc_o,select = -AKA))
rm(df_bc_o)

example = df_bc[df_bc$Title_ID== 344218,]


df_bc_n = unique(subset(df_bc,select = -c(`Opening Gross`, `Opening Screens`,`Opening Rank`)))

df_bc_r = as.data.table(subset(df_bc,select = c(Title_ID, `Airing Datetime`,`Program ID`,`Opening Gross`, `Opening Screens`,`Opening Rank`)))

r_list = split(data.table(df_bc), by='Title_ID')


open_screens_num = numeric(length(r_list))

for (i in 1: length(r_list))
{

    if (length(unique(r_list[[i]]$`Opening Screens`))>1)
        { temp=r_list[[i]]
          temp = temp[`Opening Gross`== max(temp$`Opening Gross`),]
          r_list[[i]] =temp}
# The box office of Nightmare Before Christmas is off comparing the record on box office mojo 
  }


open_screens_num = numeric(length(r_list))

for (i in 1: length(r_list))
{
  
  open_screens_num[i] =length(unique(r_list[[i]]$`Opening Screens`))
}


rem = do.call("rbind", r_list)
sort(table(rem$Network_CD),decreasing = T)
length(unique(rem$Title_ID[rem$Network_CD=="AMC"]))

rem$New_Name = rem$Title_Name


movie_attributes = c(1:8, 32:38)
# Theatrical distributor, Opening Rank, and Opening Holidays have duplicated but not exactly same words record 

TV_attributes = c(1:5, 11:31)

movie_subset=unique(subset(rem,select=movie_attributes))

Standardize_Names <- function(x)
{
  len=nchar(as.character(x))
  if (len>6)
  {
    if (substring(x,len-4)==", The") {x=paste("The",substring(x,1,len-5))
    }else if (substring(x,len-3)==", An") {x=paste("An",substring(x,1,len-4))
    }else if (substring(x,len-2)==", A") {x=paste("A", substring(x,1,len-3))
    }else {x=x}
  }
  return(as.character(x))
}

# Standardize_Names <- function(x)
# {
#   len=nchar(as.character(x))
#   if (len>6)
#   {
#     if (substring(x,len-4)==", The") {x=substring(x,1,len-5)
#     }else if (substring(x,len-3)==", An") {x=substring(x,1,len-4)
#     }else if (substring(x,len-2)==", A") {x=substring(x,1,len-3)
#     }else {x=as.character(x)}
#   }
#   return(as.character(x))
# }


New_Names =mapply(Standardize_Names,movie_subset$Title_Name)

movie_subset$New_Name = New_Names

TV_attributes = c(1:5, 11:31)

TV_subset=subset(rem,select=TV_attributes)


TV_subset$Date = as.Date(TV_subset$`Airing Datetime`)
TV_subset$Time = format(TV_subset$`Airing Datetime`, "%H:%M:%S")

Unique_movie_BO = unique(subset(rem,select = c(`Title_ID`,`US Cumulative`,`Opening Gross`)))

day_part_names = c("Morning", "Daytime", "Early Fringe", "Prime", "Late Fringe","Late Fringe", "Overnight")
day_part_start=c("06:00:00", "09:00:00", "16:30:00", "19:00:00", "23:00:00","00:00:00", "01:00:00")
day_part_end=c("08:59:59", "16:29:59", "18:59:59", "22:59:59", "23:59:59", "00:59:59", "05:59:59")
day_part_table = data.frame(day_part_names, day_part_start,day_part_end)


Determine_Daypart <- function(x)
{
  bc_time = strptime(x,format="%H:%M:%S")
  
  start_time = strptime(day_part_table$day_part_start,format="%H:%M:%S")
  end_time = strptime(day_part_table$day_part_end,format="%H:%M:%S")
  num = which(as.numeric(difftime(bc_time,start_time, units="mins"))>=0 & as.numeric(difftime(bc_time,end_time, units="mins"))<0)
  
  return(num)
}

num = mapply(Determine_Daypart, TV_subset$Time)

TV_subset$Day_part = day_part_table$day_part_names[num]


##
movie_joint_new = merge(movie_subset, new_data_movie, by.x = c("New_Name", "Release Year"), by.y = c("title","Year"))
