# Load R packages
library(tidyverse)
library(shiny)
library(dplyr)
library(readr)
library(shinythemes)
library(tmap)
library(sf)
library(rgdal)
library(cartogram)
library(igraph)
library(ggraph)

# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Parental Income and Graduation Rate among Colleges and Universities in the US",
                  tabPanel("Scatterplot by State ",
                           titlePanel(""),
                           sidebarLayout(  
                             sidebarPanel(
                               checkboxGroupInput("NAME1", 
                                                  label="State",
                                                  choices = levels(as.factor(college$state)),
                                                  selected = levels(as.factor(college$state)))),
                             
                             mainPanel(plotOutput("distPlot")))), # Navbar 1, tabPanel
                  tabPanel("Facet by School Type",
                           titlePanel(""),
                           sidebarLayout(  
                             sidebarPanel(
                               checkboxGroupInput("fourcat", 
                                                  label="School Type",
                                                  choices = levels(as.factor(college$fourcat)),
                                                  selected = levels(as.factor(college$fourcat)))),
                             mainPanel(plotOutput("distPlot1")))),
                  tabPanel("Boxplot by State", 
                           titlePanel(""),
                           sidebarLayout(  
                             sidebarPanel(
                               selectInput("NAME2", 
                                           label="State",
                                           choices = levels(as.factor(college$state)))),
                             mainPanel(plotOutput("distPlot2")))
                           
                           
                           
                  ),
                  tabPanel("Parental Income Map",
                           titlePanel(" Parental Income Map"),
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput("NAME3", 
                                                  label="State",
                                                  choices = levels(as.factor(college_map_income$NAME)),
                                                  selected = levels(as.factor(college_map_income$NAME)))
                             ),mainPanel(plotOutput("distPlot3")))),
                  tabPanel(" Graduation Rate Map",
                           titlePanel(" Graduation Rate Map"),
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput("NAME4", 
                                                  label="State",
                                                  choices = levels(as.factor(college_map_grad$NAME)),
                                                  selected = levels(as.factor(college_map_grad$NAME)))
                             ),mainPanel(plotOutput("distPlot4"))))
                  
                  
                  
                  
                ) # navbarPage 
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  college <- readRDS("college.rds")
  
  output$distPlot <- renderPlot({
    college%>%
      filter(state %in% input$NAME1)%>% 
      ggplot(mapping = aes(x = grad_100_percentile , y = par_median, color = state )) + geom_point() +
      labs(x = "4-Year Graduation Rate", y = "Median Parental Income")
  })
  
  output$distPlot1 <- renderPlot({
    
    college%>%
      filter(fourcat %in% input$fourcat)%>% 
      ggplot(mapping = aes(x = grad_100_percentile , y = par_median)) + geom_point() +
      facet_wrap(~fourcat, nrow = 2) +
      labs(x = "4-Year Graduation Rate", y = "Median Parental Income")
  })
  
  output$distPlot2 <- renderPlot({
    college %>%
      filter(state %in% input$NAME2)%>%
      ggplot(mapping = aes(x = grad_100_percentile, y = par_median)) +
      geom_boxplot(mapping = aes(group = cut_width(grad_100_percentile, 25))) +
      labs(x = "4-Year Graduation Rate", y = "Median Parental Income")
  })
  output$distPlot3 <- renderPlot({
    
    
    college_map_income <- readRDS("map_income.rds")
    
    
    st_sf(college_map_income) %>%
      filter(NAME %in% input$NAME3)%>%
      tm_shape() +
      tm_polygons("Median Parental Income", palette = "Greens")
  })
  
  output$distPlot4 <- renderPlot({
    
    college_map_grad <- readRDS("map_grad.rds")
    
    st_sf(college_map_grad) %>%
      filter(NAME %in% input$NAME4)%>%
      tm_shape() +
      tm_polygons("4-Year Graduation Rate", palette = "Greens")
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)