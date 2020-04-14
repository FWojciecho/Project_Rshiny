library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(shinyWidgets)
library(data.table)
library(DT)
library(plotly)
library(Cairo)
source("data_processing.R")

ui <- navbarPage(
  title = "Medical data exploring app",
  theme = shinythemes::shinytheme("flatly"),
  # first tab with info about patients
  tabPanel(title = "Patients info", 
           sidebarLayout(
             sidebarPanel(
               
               # Age range selector
               sliderInput("slider_age", label = h3("Select age range"), min = min(unique_age), 
                           max = max(unique_age), value = c(min(unique_age), max(unique_age))),
               helpText(h3("Select characteristic you want to filter by")),
               
               # Filtring by sex
               prettySwitch(
                 inputId = "sex_switch", label = "Select sex", slim = TRUE, status = 'info'
               ),
               
               conditionalPanel(
                 condition = "input.sex_switch == 1",
                 selectInput(inputId = "sex_selector", 
                             label = "Display records when:",  
                             choices = c("Female",
                                         "Male"),
                             selected = "Female",
                             multiple = FALSE)
               ),
               
               # Filtring by race
               prettySwitch(
                 inputId = "race_switch", label = "Select race", slim = TRUE, status = 'info'
               ),
               
               conditionalPanel(
                 condition = "input.race_switch == 1",
                 selectInput(inputId = "race_selector", 
                             label = "Display records when:",  
                             choices = unique(pat_data$RACE),
                             selected = "B: Placebo",
                             multiple = T)
               ),
               
               # Filtring by treatment 
               prettySwitch(
                 inputId = "treatment_switch", label = "Select treatment", slim = TRUE, status = 'info'
               ),
               
               conditionalPanel(
                 condition = "input.treatment_switch == 1",
                 selectInput(inputId = "treatment_selector", 
                             label = "Display records when:",  
                             choices = unique(pat_data$ACTARM),
                             selected = "B: Placebo",
                             multiple = T)
               ), 
               prettyCheckbox(
                 inputId = "table_1", label = "Create table",
                 outline = FALSE, status = "info", icon = icon("table"), animation = "jelly"
               )
             ),
             #sidebar_panelend
             
             mainPanel(
               plotly::plotlyOutput("histogram_filtred"),
               
               conditionalPanel(
                 condition = "input.table_1 == 1 ",
                 DT::dataTableOutput('data_table_1')
               )
               #mainPanel end
             )
           )
  ),
  # patients info end
  
  # Next tabpanel with lab data
  tabPanel(title = "Clinical data",
           sidebarLayout(
          
             sidebarPanel(
               helpText(h4('Lab statistics for criteria selected in the "Patients info" tab')),
               
               switchInput(
                 inputId = "pat_info_switch",
                 value = FALSE,
                 onStatus = "info"
               )
             ), 
             #sidebarPanel the end 
            
            # main panel
             mainPanel(
               conditionalPanel(
                 condition = "input.pat_info_switch == 1",
                 helpText(h4('Growth/decline in % of laboratory tests after treatment for given patients group')),
                 plotly::plotlyOutput("boxplot")
               )
               
             ) 
            # mainPanel the end
           ) 
           # sidebarLayout the end
  ) 
  # tabPanel clinical data the end
)

server <- function(input, output) {
   
  # "Patients info" tab backend
  data_filtring <- eventReactive(
    eventExpr = c(input$slider_age,
                  input$sex_switch,
                  input$sex_selector,
                  input$race_switch,
                  input$race_selector,
                  input$treatment_switch,
                  input$treatment_selector),
    valueExpr = {
      # Age filtring
      data_filter <- filter(pat_data,
                           between(AGE, input$slider_age[1], input$slider_age[2]))
      
      
      # Sex filtring
      if(input$sex_switch == 1){
        data_filter <- filter(data_filter,
                             SEX == input$sex_selector)
      }
      
      if(input$race_switch == 1){
        data_filter <- filter(data_filter,
                             RACE %in% input$race_selector)
      }
      if(input$treatment_switch == 1){
        data_filter <- filter(data_filter,
                              ACTARM %in% input$treatment_selector)
      }
      
      
      # returing data
      return(data_filter)
    }  
  )
  
  output$histogram_filtred <- renderPlotly({
    histogram <- ggplot(data = data_filtring(),
                        aes(AGE)) +
      geom_histogram(aes(fill = ..count..),
                     bins = 75) +
      theme_minimal() +
      scale_fill_gradient("Count", low = '#B7DEE8', high = '#31869B')
    
    # making interactive
    plotly::ggplotly(histogram)
  })
  output$data_table_1 <- renderDataTable({data_filtring()})
  
  # Clinical data backend
  
  data_filtring_2 <- eventReactive(
    eventExpr = c(input$pat_info_switch), 
    valueExpr = {
      if(input$pat_info_switch== 1){
         vec_temp <- data_filtring() %>% select(USUBJID) %>% unique() %>% as.vector()
         vec_temp <- vec_temp$USUBJID
         dt_filtred_lab <- dt_temp %>% filter(pat_id %in% vec_temp)
    
      }
      
      # returning data
      return(dt_filtred_lab)
    }
  )
  
  
  output$boxplot <- renderPlotly({
    # creating boxplot
    boxplot <- data_filtring_2() %>% 
        
        # boxplot
        ggplot(aes(x = LBTEST,
                   y = diffrence)) +
        geom_boxplot(fill = "#31869B",
                     outlier.colour = "#9BBB59") +
        coord_flip() +
        xlab("Name of Laboratory test") + ylab("% growth of laboratory measures\n of population after treatment") +
        theme_minimal()

      
      # adding interactivity  
      ggplotly(boxplot)
      

  })
  
  


}

# Run the application 
shinyApp(ui = ui, server = server)

