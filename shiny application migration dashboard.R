library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis) # scale_fill
library(ggsci) # color palettes
# https://frontex.europa.eu/we-know/migratory-map/
emigration <- read_excel("C:/Users/Tanji/Downloads/Monthly_detections_of_IBC_2022_08_05.xlsx")
start_date <- as.Date("01-01-09", "%d-%m-%y") # define start date from the dataset

# Dashboard
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title="Migratory Map"),
  
  dashboardSidebar(
    sidebarMenu(selectInput("input1","Select Route",
                            choices = unique(emigration$Route)),
                selectInput("input2","Select Nationality",
                            choices = unique(emigration$Nationality)),
                sliderInput("background_intencity", "Select intencity of the background", 80,99, 90)
    )
  ),
  dashboardBody(
    fluidRow(
      plotOutput("plot_output"),
      #div("The data presented refer to detections of illegal border-crossing rather than the number of persons, as the same person may cross the external border several times. However, there is currently no EU system in place capable of tracing each person's movements following an illegal border-crossing. Therefore, it is not possible to establish the precise number of persons who have illegally crossed the external border"),
      box(plotOutput("plot_output_mo"), background = "navy", br(), height = 420),
      box(plotOutput("plot_output_ye"), background = "teal")
    )
  )
)

server <- function(input, output) {
  emigration_func <- reactive({
    emigration %>%
      filter(Nationality == input$input2 & Route == input$input1)
  })
  
  data <- reactive({
    data.frame(Number=c(as.matrix(emigration_func()[4:165])), dates=names(emigration_func()[4:165]),
               dates_numeric = as.Date(seq(start_date, by="month", length.out=162)),
               Month=factor(substr(names(emigration_func()[4:165]),1,3), labels = substr(names(emigration_func()[4:165]),1,3)[1:12]),
               Year=factor(substr(names(emigration_func()[4:165]),4,8), labels = unique(substr(names(emigration_func()[4:165]),4,8))))
  })
  
  output$plot_output <- renderPlot({
    data() %>%
      ggplot(aes(x=dates_numeric, y=Number))+
      labs(title = paste("Number of", input$input2, "immigrants"), subtitle = "Western Balkan Route",
           caption = "Source: FRAN and JORA data as of August 2022")+
      geom_point(size=.9)+
      scale_x_date(date_breaks = "6 month")+
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5, face= "italic"),
            panel.background = element_rect(fill = paste0("grey", input$background_intencity)))
  })
  
  output$plot_output_mo <- renderPlot({
    data() %>%
      ggplot(aes(x=Month, y=Number, fill=Month))+
      scale_fill_viridis(discrete = TRUE, option = "A")+
      geom_bar(stat="identity")+
      labs(title=paste("Number of", input$input2, "immigrants by Month"))+
      theme_dark()
  })
  
  output$plot_output_ye <- renderPlot({
    data() %>%
      ggplot(aes(x=Year, y=Number, fill=Year))+
      scale_fill_viridis(discrete = TRUE, option = "B")+
      geom_bar(stat="identity")+
      labs(title=paste("Number of", input$input2, "immigrants by Year"))+
      theme_dark()
  })
}

shinyApp(ui, server)