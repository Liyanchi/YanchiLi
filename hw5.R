####HW5####
print("Yanchi Li")
print(1505113)
print("yli248@ucsc.edu")

####1####
#a
install.packages("ggplot2")
library(ggplot2)
data("diamonds")
d <- ggplot(diamonds,
            aes(x=x*y*z, y=price,
                color=clarity))
d+geom_point(aes(size=carat),alpha=0.3)+scale_x_log10()+scale_y_log10()

#b
b<- ggplot(diamonds, aes(carat,..density..))
b+geom_histogram(aes(fill=clarity),bins=25)+facet_grid(cut~.)

#c#
c<- ggplot(diamonds,aes(x=cut,price))
c+geom_jitter(alpha=0.1)+geom_violin()

####3####
#a
library(foreign)
require(dplyr)
org_example <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
a <- org_example %>%
  dplyr::group_by(year,month)%>%
  dplyr::summarise(
    decile1st = quantile(rw, .1, na.rm = T),
    decile9st = quantile(rw, .9, na.rm = T),
    quantile1 = quantile(rw, .25, na.rm = T),
    quantile3 = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())

a <- a %>% 
  mutate(date=paste(year,month,"01", sep="-"),
         date=as.Date(date,format="%Y-%m-%d"))

pa <- ggplot(a, aes(x=date, y=Median.RW))
pa + geom_ribbon(aes(ymin=quantile1, ymax=quantile3),alpha=0.5) + geom_ribbon(aes(ymin=decile1st, ymax=decile9st),alpha=0.2) + geom_line(aes(y=Median.RW))+lims(y=c(0,50))

#b
p_b <- org_example %>%
  dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
    Median.RW = median(rw, na.rm = T),
    count = n())

p_b <- p_b %>%
  mutate(date=paste(year,month,"01", sep="-"),
         date=as.Date(date,format="%Y-%m-%d"))

pb <- ggplot(p_b, aes(x=date, y=Median.RW,group=educ))
pb + geom_line(aes(color=educ))

