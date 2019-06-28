# setwd("C:/Users/SinghV54/Desktop/GPRS Research/Codementor/Self Learning/Premier League/Ali_Shinyapp/")
source("eplV2 - source.r")
library(shiny)
library(DT)
library(shinycssloaders)
library(curl)

# Define UI for application that draws a histogram
shinyApp(
  ui = shinyUI(fluidPage(
     titlePanel(
       tags$head(
         tags$style("label{font-family: COMIC SANS MS;}")
       ),
       title=div(img(src="picture.jpg", width=80, style="float:right;"),HTML("<B><center><font color= purple family=Radikal Bold> WELCOME TO PREMIER LEAGUE PREDICTIONS ! </center><B></font><font color=red>"))),
    sidebarLayout(
      sidebarPanel( 
        selectInput("home_team", 
                    label = "Home Team",
                    choices = list()
        ),
        selectInput("away_team", 
                    label = "Away Team",
                    choices = list()
        ),
        actionButton("btn", "Predict Winner !")
      ),
      mainPanel(
        withSpinner(dataTableOutput("summary_table"))
        )
    )
  )),
  server = shinyServer(function(input, output, session) {
#    df <- read.csv("d.csv")
    '%!in%' <- function(x,y)!('%in%'(x,y))
    
    # df <- df  %>% dplyr::filter(HomeTeam %!in% c("Reading","QPR","Wigan","Swansea","Sunderland","Norwich","Hull","Stoke","Middlesbrough","Blackburn","Bolton","Aston Villa","West Brom")) %>%
    #   dplyr::filter(AwayTeam %!in% c("Reading","QPR","Wigan","Swansea","Sunderland","Norwich","Hull","Stoke","Middlesbrough","Blackburn","Bolton","Aston Villa","West Brom"))
      
    updateSelectInput(session, "home_team", choices = df$HomeTeam)
    
    # updateSelectInput(session, "home_team", choices = df$HomeTeam[,-which(names(df) %in% c("Reading","QPR","Wigan","Swansea","Sunderland","Norwich","Hull","Stoke","Middlesbrough","Blackburn","Bolton","Aston Villa","West Brom"))])
      updateSelectInput(session, "away_team", choices = df$HomeTeam,selected=as.factor(levels(df$HomeTeam)[5]))
    #
     
     
     data <- eventReactive(input$btn, {validate(
       need(input$home_team != input$away_team, "Please select two different teams")
     )
     main(input$home_team, input$away_team)})
     
     output$summary_table <- renderDataTable({
       data()
       
     })
     
     
     
     }))
    
     
     # observeEvent(input$btn, 
     #      { myreturn <- reactiveValues()
     #        output$what <- main(input$home_team, input$away_team)
     # })
     # 
 

