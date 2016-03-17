library(nycflights13)
library(dplyr)
library(ggplot2)
flights_sqlite<-tbl(nycflights13_sqlite(),"flights")
flights_sqlite

df <- flights_sqlite %>% left_join(weather, by = "origin", copy = TRUE)
df <- as.data.frame(df)
p <- ggplot(df, aes(x= pressure,y=dep_delay)) + geom_point()
p
df <- na.omit(df)
p <- ggplot(df, aes(x= wind_speed,y=dep_delay)) + geom_point()
p

year_bar <- group_by(flights_sqlite, year) %>% summarise(count = mean(dep_delay))
df <- group_by(flights_sqlite, month) %>% summarise(count = mean(dep_delay))
df <- as.data.frame(df)
p <- ggplot(df, aes(x= factor(month),y=count)) + geom_bar(stat="identity", fill = "indianred")
p

df <- group_by(flights_sqlite, day) %>% summarise(count = mean(dep_delay))
df <- as.data.frame(df)
p <- ggplot(df, aes(x= day,y=count)) + geom_line()
p

df <- group_by(flights_sqlite, dest) %>% summarise(count = mean(dep_delay))
df <- as.data.frame(df)
p <- ggplot(df, aes(x= dest,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
p

df <- flights_sqlite %>% left_join(planes, by = "tailnum", copy = TRUE)

df2 <- group_by(df, manufacturer) %>% summarise(count = mean(dep_delay))
df2 <- as.data.frame(df2)
p <- ggplot(df2, aes(x= manufacturer,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
p

df2 <- group_by(df, engine) %>% summarise(count = mean(dep_delay))
df2 <- as.data.frame(df2)
p <- ggplot(df2, aes(x= engine,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
p

df2 <- group_by(df, engines) %>% summarise(count = mean(dep_delay))
df2 <- as.data.frame(df2)
p <- ggplot(df2, aes(x= engines,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
p
