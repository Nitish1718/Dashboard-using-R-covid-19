install.packages('lubridate')
install.packages("tidyverse")
install.packages("plotly")
library(plotly)
library(tidyverse)
library('lubridate')
covid <- read.csv("C:\\Users\\rohit\\OneDrive\\Desktop\\Desktop\\R Studio\\Data\\covid project\\country_daywise.csv")
head(covid)
dim(covid)
covid$Date <- ymd(covid$Date)
covid <- arrange(covid,Date)
tail(covid)
daywise <- read.csv("C:\\Users\\rohit\\OneDrive\\Desktop\\Desktop\\R Studio\\Data\\covid project\\daywise.csv")
head(daywise)
daywise$Date <- ymd(daywise$Date)
daywise <- arrange(daywise , Date)
head(daywise)
plot_ly(daywise, x=~Date,y=~Confirmed,type = 'scatter',mode='line')
names(daywise)
fig <- plot_ly(daywise, x=~Date)
fig <- fig %>% add_trace(y=~Confirmed, name='Confirmed', mode='lines', type='scatter')
fig <- fig %>% add_trace(y=~Recovered, name ='Recovered', mode='lines',type='scatter')
fig <- fig %>% add_trace(y=~Deaths, name ='Deaths', mode='lines',type='scatter')
cnf <- "#893ea6"
rec<- "#ff2e63"
dth <- "#21bf73"
act <- "#fe9801"
fig <- plot_ly(daywise, x=~Date)  
cnf.line <- list(color = cnf , width = 4)
dth.line <- list(color = dth , width = 4)

rec.line <- list(color = rec ,width = 4)
fig <- fig %>% add_trace(y=~Confirmed, name='Confirmed', mode='lines', type='scatter',line=cnf.line)
fig <- fig %>% add_trace(y=~Recovered, name ='Recovered', mode='lines',type='scatter',line=rec.line)
fig <- fig %>% add_trace(y=~Deaths, name ='Deaths', mode='lines',type='scatter',line=dth.line)
fig <- fig %>% layout(title = 'Total Worldwide Corona Cases',
                      xaxis = list(title='Date'),
                                   yaxis = list(title='Teotal Cases'))



fig

#BAR CHART FOR TOP 10 WORST HIT COUNTRIES BY COVID_19 

head(covid)
tail(covid)
latest <- covid %>% filter(Date==max(Date)) %>% arrange(desc(Confirmed))
top10 <- latest %>% slice(1:10)
top10
summary(top10)
plot_ly(top10,x =~Country,y=~Confirmed, type = 'bar',name='Confirmed cases')
factor(top10$Country,levels = c(as.character(top10$Country)))
top10$Country <- factor(top10$Country,levels = c(as.character(top10$Country)))
plot_ly(top10,x=~Country,y=~Confirmed ,type = 'bar',names='Confirmed cases')
values <- as.character(top10$Confirmed)
plot_ly(top10, x=~Country, y =~Confirmed ,type ='bar',names='Confirmed cases',
        text=values,textposition='auto',
        marker= list(color=heat.colors(n=10),
                     line=list(color='magneta',width=0)))

#Complete Case Analysis for USA
US <- covid %>% filter(Country=='US') %>% arrange(Date)
head(US)
tail(US)

fig1 <- plot_ly(US,x=~Date,y=~Confirmed,type='scatter',mode='lines',name = 'Confirmed Cases')
fig2 <- plot_ly(US,x=~Date,y=~Recovered,type='scatter',mode='lines',name = 'Recovered Cases')
fig3 <- plot_ly(US,x=~Date,y=~Deaths,type='scatter',mode='lines',name = 'Deaths Cases')
subplot(fig1,fig2,fig3,nrows=2,shareX = F)

#Deaths VS Confirmed Cases (scatter Plot)
plot_ly(data = top10, x =~Confirmed,
        y=~Deaths,type='scatter',
        mode='markers',color =~Country,
        colors =terrain.colors(n=10) ,
        size =~Confirmed,
        marker=list(size=~1e-4*Deaths))
#Deaths TOP 10 COUNTRYPERCENTES 
fig<- plot_ly(top10,labels=~Country,values=~Deaths,type='pie',textinfo='label+percent')
fig

