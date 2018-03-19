library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(chorddiag)
library(readr)
library(zoo)
library(dplyr)
library(tidyr)
library(chorddiag)
library(htmlwidgets)

ui = dashboardPage(
  dashboardHeader(title = "WhatsApp Chat Analysis"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(dateInput('dateStart',
                    label = "Choose start Date",
                    value = '2017-04-11',
                    min = '2017-04-11',
                    max = '2018-03-14',
                    format='yyyy-mm-dd',
                    startview = "month")),
      box(dateInput('dateEnd',
                    label = "Choose end Date",
                    value = '2017-06-14',
                    min = '2017-04-11',
                    max = '2018-03-14',
                    format='yyyy-mm-dd',
                    startview = "month")),
      chorddiagOutput("flcdplt",height = 1000)
    ) #the default settings if nothinng is selected
    
  ))

server = function(input,output) {
  
  df = read.csv('./Data/chat_raw.csv',sep=',')
  df$date_time = paste(df$date,df$military_time)
  df$date_time <- strptime(df$date_time,format="%m-%d-%y %H:%M:%S")
  reply = diff(df$date_time,lag=1)
  reply[4416] = 0
  df = data.frame(cbind(df,reply))
  df$thread = ifelse(df$reply < 1800,1,0)
  df$tp = lag(df$thread)
  
  t = df$tp
  t[is.na(t)] <- 0
  counter = 0
  g=c()
  g[1] = 999
  
  for (i in 1:length(t)){
    temp = ifelse(t[i]==0, 1,0)
    counter = counter+temp
    g[i]=counter  
  }
  
  df = cbind(df,g)

  df$date_time = as.Date(df$date_time)
  
  output$flcdplt = renderChorddiag({
    
    df = df %>% filter(date_time >= input$dateStart,date_time <= input$dateEnd)
    
    chartData = df %>% 
      distinct(name,g) %>% 
      group_by(g) %>% 
      filter (n_distinct(name) > 1) %>% 
      filter(row_number()==1 | row_number()==2) %>% 
      select(g,name) %>% 
      group_by(g) %>% 
      mutate(initiate = row_number()) %>% 
      spread(initiate,name) %>% 
      select(thread = g, thread_start = `1`,first_respondent=`2`) %>% 
      group_by(thread_start,first_respondent) %>% 
      summarise(count=n()) %>% 
      spread(first_respondent,count,fill = 0)
    
    temp = chartData
    
    s = temp$thread_start
    e = colnames(temp)[2:ncol(temp)]
    srm = as.matrix(temp[, -1])
    dimnames(srm) = list(Thread_Start = as.vector(s),First_Respondent = as.vector(e))
    
    chorddiag(srm, showGroupnames = TRUE, type = "bipartite", showTicks = FALSE, 
                     groupnameFontsize = 14, categorynameFontsize = 20)
    
  })
}

shinyApp(server = server, ui=ui)

