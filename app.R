
library(data.table)
library(shiny)
library(shinyWidgets)
library(reshape2)
library(scales)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinyjs)
library(DT)
library(dplyr)
library(latticeExtra)
library(shinydashboard)
library(dtwclust) 
library(doParallel)
library(shinybusy)
library(showtext)
library(rsconnect)

# Sys.setlocale("LC_ALL", "cp949")


tst <- read.csv("uplus_month.csv", fileEncoding = "euc-kr", header=T)
tst<-tst[,-c(2,3)]
tst<-tst[,c(3,2,1)]
tst <- tst[!(tst$ratio == '0' ),]

normalized = function(x){
  m = mean(x)
  s = sd(x)
  n = (x-m)/s
  return(n)
}
#######################################################################################

merge_count <- tst %>%
  group_by(keywords) %>%
  summarise(count = n(),)

boxplot(merge_count$count)

merge_count$count <-as.integer(merge_count$count)

tst_v2 <-inner_join(tst, merge_count, by="keywords")

ui <- fluidPage(title = "Advertising Data",
                useShinyjs(),
                
                tabPanel(title = "Cluster",
                         div(style = 'margin-left:20px',
                             
                             fluidRow(
                               div(style = "display : inline-block;",
                                   numericInput("tst_v2_count", "tst_v2 count select", min = 1, step = 1, value = 12)),
                               div(style = "display : inline-block;" ,
                                   actionButton(
                                     # style = 'margin-left: 25px; width: 100px; ',
                                     inputId = "tst_v2_count_click",
                                     label = "OK"
                                   ))
                             ),
                             fluidRow(
                               fluidRow(
                                 div(style = "display : inline-block;" ,plotOutput(width = '400px', height = "300px", "sil_plot1")),
                                 div(style = "display : inline-block;"  ,plotOutput(width = '400px', height = "300px","sil_plot2")),
                                 div(style = "display : inline-block;" ,plotOutput(width = '400px', height = "300px","sil_plot3"))
                               ),
                               fluidRow(
                                 div(style = "display : inline-block;"  , plotOutput(width = '400px', height = "300px","sil_plot4")),
                                 div(style = "display : inline-block;" ,plotOutput(width = '400px', height = "300px","sil_plot5")),
                                 div(style = "display : inline-block;"  ,plotOutput(width = '400px', height = "300px","sil_plot6")),
                                 div(style = "display : inline-block;" ,plotOutput(width = '400px', height = "300px","sil_plot7"))
                               )
                             ),
                             fluidRow(
                               div(style = "display : inline-block;",
                                   numericInput("dtw_cluster_k", "dtw_cluster k select", min = 1, step = 1, value = 5)),
                               div(style = "display : inline-block;", 
                                   actionButton(
                                     # style = 'margin-left: 25px; width: 100px; ',
                                     inputId = "dtw_cluster_click",
                                     label = "OK"
                                   ))
                             ),
                             fluidRow(
                               plotlyOutput("plot2"),
                               plotlyOutput("plot3")
                               
                             ),
                             
                             fluidRow(
                               div(style = "display : inline-block;" ,
                                   numericInput("cluster_num", "select cluster's number", min = 1, step = 1, value = 1)),
                               div(style = "display : inline-block;"  ,
                                   actionButton(
                                     # style = 'margin-left: 25px; width: 100px; ',
                                     inputId = "cluster_num_click",
                                     label = "OK"
                                   ))),
                             fluidRow(
                               column(6,
                                      plotlyOutput("cluster_plot")),
                               column(6,
                                      plotlyOutput("cent_plot"))
                             ),
                             fluidRow(
                               DTOutput("resultdt")))
                )
) # body


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$tst_v2_count_click, {
    
    show_modal_spinner()
    
    tst_v3 <- tst_v2 |> filter(!(count < input$tst_v2_count))
    tst_v3 <<- tst_v3[,-c(4)]
    
    tst <<- tst_v3
    tst$keywords <- gsub("[[:punct:]]", ".", tst$keywords) 
    df_dcast_tst <- dcast(tst, period~keywords, value.var = "ratio", sum, fill=0)
    kk = df_dcast_tst[,-1] 
    data_list <- split(kk, seq(nrow(kk))) 
    
    normalized_search <<- lapply(kk, function(x) normalized(x))
    
    cl <- makeCluster(detectCores())
    
    invisible(clusterEvalQ(cl, library(dtwclust)))
    registerDoParallel(cl)
    
    pc.dtw <- tsclust(normalized_search,
                      type="partitional",
                      centroid = "pam",
                      k = 2L:15L,
                      distance = "dtw_basic",
                      seed=1000,
                      trace=T,
                      args = tsclust_args(dist = list(window.size = 60L))
                      
    )
    
    stopCluster(cl)
    registerDoSEQ()
    cl
    
    eval_clust <<-sapply(pc.dtw,cvi)
    
    remove_modal_spinner()
    
    
    output$sil_plot1 <- renderPlot({
      plot(eval_clust[1,],type="l", main="sil index", xlab="The number of clusters", ylab="To Be Maximum") 
      
    })
    
    output$sil_plot2 <- renderPlot({
      plot(eval_clust[2,],type="l", main="SF index", xlab="The number of clusters", ylab="To Be Maximum") 
      
    })
    
    output$sil_plot3 <- renderPlot({
      plot(eval_clust[3,],type="l", main="CH index", xlab="The number of clusters", ylab="To Be Maximum") 
      
    })
    
    output$sil_plot4 <- renderPlot({
      plot(eval_clust[4,],type="l", main="DB index", xlab="The number of clusters", ylab="To Be Minimum") 
      
    })
    
    output$sil_plot5 <- renderPlot({
      plot(eval_clust[5,],type="l", main="Modified DB index", xlab="The number of clusters", ylab="To Be Minimum") 
      
    })
    
    output$sil_plot6 <- renderPlot({
      plot(eval_clust[6,],type="l", main="Dunn index", xlab="The number of clusters", ylab="To Be Maximum") 
      
    })
    
    output$sil_plot7 <- renderPlot({
      plot(eval_clust[7,],type="l", main="COP index", xlab="The number of clusters", ylab="To Be Minimum") 
      
    })
    
  })
  
  
  observeEvent(input$dtw_cluster_click, {
    
    dtw_cluster <<- tsclust(normalized_search, type="partitional",k=input$dtw_cluster_k,
                            distance="dtw_basic",centroid = "pam",seed=1234,trace=T,
                            args = tsclust_args(cent = list(window.size = 60L)))
    
    output$plot2 <- renderPlotly({
      dtw_plot2 <<- plot(dtw_cluster)
      dtw_plot2 %>% ggplotly()
    })
    output$plot3 <- renderPlotly({
      dtw_plot3 <<- plot(dtw_cluster, type="centroids")
      dtw_plot3 %>% ggplotly()
    })
  })
  
  observeEvent(input$cluster_num_click,{
    
    cluster_num <<- input$cluster_num
    
    output$cluster_plot <- renderPlotly({
      
      plot(dtw_cluster, type="series", clus= cluster_num) %>% ggplotly()
      
    }) 
    
    output$cent_plot <- renderPlotly({
      
      plot(dtw_cluster, type="centroids", clus= cluster_num) %>% ggplotly()
      
    }) 
    options(scipen=500)
    # output$resultdt <- renderDT({
    #   
    #   cl <- cl_raw_df %>% group_by(keywords, cluster) %>% summarise(mean = mean(value, na.rm = T)) %>% filter(cluster == cluster_num) 
    #   cl[order(cl$mean),]
    # }) 
    
  })
  
  
  
  
}

# Run the application 
runApp(list(ui = ui, server = server))
