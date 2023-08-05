

library(shiny)
library(DT)
library(tidyverse)
library(magrittr)
library(readr)
library(plotly)

data_df <- read_rds("impute.rds")

data_df$variable_clean <- round(data_df$variable_clean)

districts <- unique(data_df$district)
facilites <- unique(data_df$facility)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      h1 {
        font-family: 'Poppins', sans-serif;
          font-size: 50px;
          font-weight: 400;
          color: #264653;

      }
      h3{
        font-family: 'Lato', sans-serif;
        color: #264653;
        
      .centreAlign{float:right;
      }
     "))
  ),
  
  tabPanel("Details"),
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("district","Select District:",districts),
      br(),
      uiOutput("facility_selector"),
      br(),
      helpText("All tables will be blank for details if no facility is selcted or if no outlier exists for that facility"),
      br(),
      actionButton("click", "Send Email!", class = "btn-danger"),
      br(),
      br(),
      flowLayout(radioButtons('format', 'Document format', c('HTML', 'PDF','Word'),
                              inline = TRUE),
                 downloadButton(outputId = "report", label = "Generate District Report:",class = "centreAlign"))
      #downloadButton(outputId = "report", label = "Generate District Report:")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h1("Data Quality - HMIS Tools"),
      
      tabsetPanel(
        tabPanel('District Overview',
                 h3("Summary of counts of possible Outliers per Facility"),
                 br(),
                 plotlyOutput("graph"),
                 dataTableOutput("district_summary"),
                 downloadButton("download_district", "Download District Data set",class = "centreAlign")
                 ),
        tabPanel("Facility Details",
                 h3( textOutput("text")),
                 dataTableOutput("summary"),
                 br(),
                 h3(textOutput("text_detail")),
                 dataTableOutput("mytable"),
                 downloadButton("download", "Download  Facility Data set",class = "centreAlign"),
          
        )
      )

    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  district_selected <- reactive(
    data_df %>%
      dplyr::filter(district == input$district)
  )
  
  
  output$district_summary <- renderDataTable(
      
  district_selected() %>%
      group_by(facility,flag)  %>%
      count() %>%
      pivot_wider(names_from = flag,values_from = n) %>%
      rowwise() %>%
      mutate(total = sum(None,Outlier),
             Percentage_outliers = round(Outlier/total*100))%>%
      select(-None) %>%
      filter(Outlier > 0) %>%
    dplyr::arrange(desc(Outlier)) %>%
      rename(
        `Possible Outliers` = Outlier,
        `Total Records` = total, 
        `% Outliers` = Percentage_outliers) 
      
  )
  
  
  # plotly

  output$graph <- renderPlotly({

    plot_data <- district_selected() %>%
      group_by(facility,flag)  %>%
      count() %>%
      pivot_wider(names_from = flag,values_from = n) %>%
      rowwise() %>%
      mutate(total = sum(None,Outlier),
             Percentage_outliers = round(Outlier/total*100))%>%
      select(-None) %>%
      filter(Outlier > 0) %>%
      dplyr::arrange(desc(Outlier))

    plot_ly(plot_data,x = ~facility,y = ~Outlier, type = 'bar') %>%
      layout(xaxis = list(categoryorder = "total descending"))



  })
  
  
  ## ------ Facility choices 
  
  filtered_facilities <- reactive({
    selected_district <- input$district
    data_df[data_df$district == selected_district, "facility"]
  })
  
  # Dynamically update the facility selector based on selected district
  output$facility_selector <- renderUI({
    selectInput(inputId = "facility",
                label = "Select Facility:",
                choices = filtered_facilities())
  })
  
  output$text <- renderText(
    paste0("Data Quality summary for",input$facility)
  ) 

  selected <- reactive(data_df %>%
                         dplyr::filter(facility == input$facility))
  
  
  output$summary <- renderDataTable(
    selected()  %>%
      group_by(indicator,flag)  %>%
      count() %>%
      pivot_wider(names_from = flag,values_from = n) %>%
      rowwise() %>%
      mutate(total = sum(None,Outlier),
             Percentage_outliers = round(Outlier/total*100))%>%
      select(-None) %>%
      filter(Outlier > 0) %>%
      dplyr::arrange(desc(Outlier)) %>%
      rename(
        `Possible Outliers` = Outlier,
        `Total Records` = total, 
        `% Outliers` = Percentage_outliers,
        `Data Element` = indicator
        
      )
  )
  
  output$text_detail <- renderText(
    paste0("Detailed Breakdown for ",input$facility)
  ) 
    
  output$mytable <- renderDataTable(
    selected() %>%
      filter(flag == "Outlier") %>%
      select(-c(facility,flag,region,district)) %>%
      relocate(indicator , .before = period) %>%
      dplyr::arrange(desc(period)) %>%
      rename(
        `Suggested Imputation` = variable_clean, 
        `Possible Outlier` = variable,
        `Data Element` = indicator,
        Month = period
      )
      
      
  )
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$facility,".csv")
    },
    content = function(file){
      write_csv(selected(),file)
    }
  )
  
  output$download_district <- downloadHandler(
    filename = function(){
      paste0(input$district,".csv")
    },
    content = function(file){
      write_csv(district_selected(),file)
    }
  )

  output$report <- downloadHandler(
    #filename = "report_quality.html",
    
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      
      src <- normalizePath("data_quality.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src,"data_quality.Rmd",overwrite = T)

      # tempReport <- file.path(tempdir(),("data_quality.Rmd"))
      # file.copy(here("data_quality.Rmd"), tempReport,overwrite = T)

      #params <- list(district = input$district)
      
      out <- rmarkdown::render("data_quality.Rmd", 
                    params = list(district = input$district),
                    switch(input$format,
                           PDF = pdf_document(), 
                           HTML = html_document(), 
                           Word = word_document()))
      file.rename(out, file)

      # rmarkdown::render("data_quality.Rmd",output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv()))


    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
