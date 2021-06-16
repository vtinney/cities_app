library(shiny)
library(plotly)
library(dplyr)
library(highcharter)
library(tidyverse)


df <-read_excel("data/PM25_2000_2019_urban.xlsx")

df$"WHORegion" <- as.character(df$"WHORegion")
colnames(df)[6] <- 'PM2.5'
colnames(df)[15] <- 'Cases'
df$"WHORegion"[df$"WHORegion" == 'WPRO'] <- 'Western Pacific'
df$"WHORegion"[df$"WHORegion" == 'SEARO'] <- 'South-East Asia'
df$"WHORegion"[df$"WHORegion" == 'EURO'] <- 'Europe'
df$"WHORegion"[df$"WHORegion" == 'AMRO'] <- 'Americas'
df$"WHORegion"[df$"WHORegion" == 'AFRO'] <- 'Africa'
df$"WHORegion"[df$"WHORegion" == 'EMRO'] <- 'Eastern Mediterranean'
df$popxconc <- df$PM2.5*df$Population

df2 <- as.data.frame(df %>%
                       group_by(Year,WHORegion)%>%
                       summarize(Population = sum(Population,na.rm=T),
                                 popxconc=sum(popxconc,na.rm=T),
                                 cases=sum(Cases,na.rm=T)))

df2$avgpm <- df2$popxconc/df2$Population
df2$avgrate <- (df2$cases*100000)/df2$Population

df <- merge(df, df2, by=c('Year','WHORegion'))

colnames(df)[2] <- "Region" 

df <- df[,c(1,2,4,6,7,15,25,26)]
colnames(df)[4] <- 'Population'
df$rate100 <- (df$Cases*100000)/df$Population
df$PM2.5 <- round(df$PM2.5,2)

df$rate100 <- round(df$rate100,2)
df$avgpm <- round(df$avgpm,2)
df$avgrate <- round(df$avgrate,2)

df <- df[,c(1:5,7:9)]

df <- df %>% gather(Analysis, Value, "PM2.5":"rate100")

df <- subset(df, !City %in% 'UNNAMED')
df$Analysis[df$Analysis=='PM2.5'] <- 'PM<sub>2.5</sub>'
df$Analysis[df$Analysis=='avgpm'] <- 'Regional PM<sub>2.5</sub>'
df$Analysis[df$Analysis=='avgrate'] <- 'Rate per 100,000'
df$Analysis[df$Analysis=='rate100'] <- 'Regional rate per 100,000'



df$Analysis <- factor(df$Analysis, c("PM<sub>2.5</sub>", "Regional PM<sub>2.5</sub>", "Rate per 100,000", "Regional rate per 100,000")) 

citiesdf = data_frame(name = df$Analysis,
                      y = df$Value,
                      drilldown = tolower(paste(name,'City')))



server = function(input, output, session) {

  data2 <- reactive({
    df %>%
      filter(Region == input$Region) %>%
      filter(City == input$City) %>%
      filter(Year >= input$Year[1] & Year <= input$Year[2])
  })
  
  output$Cities<-renderHighchart({
    
    
    hc <- hchart(data2(), "spline",
                 hcaes(x = Year, y = Value, group=Analysis), color = c("#f9a242", "#fad7b1", "#403891","#a5bed6")) %>%
      hc_legend(useHTML = TRUE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,useHTML = TRUE, shared = TRUE)%>%
      hc_yAxis(title = list(text = "")) %>%
      hc_credits(enabled = T, text = "") %>% 
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list(
          list(
            id = "Cities",
            data = citiesdf,
            keys = list('name','y','drilldown')
          ) 
        ))
    
    hc
  })
}