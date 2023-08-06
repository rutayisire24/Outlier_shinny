library(shiny)
library(DT)
library(tidyverse)
library(magrittr)
library(readr)
library(plotly)
library(rmarkdown)

data_df <- read_rds("impute.rds")
#write_csv(data_df,"impute.csv")

contacts <- readxl::read_xlsx("contacts.xlsx")

data_df$variable_clean <- round(data_df$variable_clean)

districts <- unique(data_df$district)
#facilites <- unique(data_df$facility)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(title = span(img(src = "Logo.png", height = 35), "HOT (Pro-Type)")),
  h3("HMIS Observation Toolkit"),
  
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
  
  theme = bslib::bs_theme(bootswatch = "journal"),
  

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("district","Select District:",districts,selected = sample(districts,1)),
      fluidRow(selectInput("facility_s","Select Facility", multiple = FALSE,choices = character(0))),
      #uiOutput("facility_selector"),
      flowLayout(radioButtons('format', 'Document format', c('HTML','PDF'),
                              inline = TRUE),
                 downloadButton(outputId = "report", label = "Generate District Report",class = "centreAlign")),
      br(),
      HTML("<p> Biostat Contact : </p>"),
      textOutput("email"),
      br(),
      HTML("<p> Send an  <a href='https://mail.google.com/mail/u/0/#inbox'> Email </a>!</p>"),
      br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('District Overview',
                 h3("Counts of Possible Outliers per Facility | 2020 - 2023"),
                 br(),
                 plotlyOutput("graph"),
                 br(),
                 h3("Counts of Possible Outliers per Facility | 2020 - 2023"),
                 dataTableOutput("district_summary"),
                 br(),
                 downloadButton("download_district", "Download District Data set",class = "centreAlign"),
                 br(),
                 br(),
                 HTML("<p> <b>About HOT:</b>  It automates detection of possible outliers and inconsitences in submitted data and renders district specific reports to 
           support verification and follow up. To see the documentation Click <a href='https://rmeddy.quarto.pub/documentationdata-quality-tool/'>HERE</a>!</p>"),
                 textInput(inputId = "feedback",label = "Feedback:",placeholder = "What is on your Mind ? ")
                 ),
        tabPanel("Facility Details",
                 h3( textOutput("text")),
                 dataTableOutput("summary"),
                 br(),
                 h3(textOutput("text_detail")),
                 dataTableOutput("mytable"),
                 br(),
                 downloadButton("download", "Download  Facility Data set",class = "centreAlign"))

        )
      )

    )
  )
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  district_selected <- reactive(
    data_df %>%
      dplyr::filter(district == input$district)
  )
  
  # filtered_facilities <- reactive({
  #   selected_district <- input$district
  #   data_df[data_df$district == selected_district, "facility"]
  # })
  
  observe({
    x <- input$district
    facilities <- data_df[data_df$district == x,"facility"]
    updateSelectizeInput(session, "facility_s",choices = facilities, selected = tail(facilities,1))
  })
  
  ## ------ Facility choices

  
  # # Dynamically update the facility selector based on selected district
  # output$facility_selector <- renderUI({
  #   selectInput(inputId = "facility",
  #               label = "Select Facility:",
  #               choices = filtered_facilities())
  # })
  
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
        `Possible Outliers(Counts)` = Outlier,
        `Total Records Reviewed` = total, 
        `% Outliers` = Percentage_outliers,
        #`Expected Value` = `Suggested Imputation`
        ) 
      
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
  
  

  
  output$text <- renderText(
    paste0("Data Quality summary for ",input$facility_s)
  ) 
  


  contact <- reactive(
    contacts %>%
      dplyr::filter(District == input$district)
    
  )
  
  output$email <- renderText(
    
    
    paste0("Email: "," ",contact()[,5],"  ", contact()[4])
     
  )

  selected <- reactive(data_df %>%
                         dplyr::filter(facility == input$facility_s))
  
  
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
        `Data Element` = indicator,
        #`Expected Value` = `Suggested Imputation`
        
      )
  )
  
  output$text_detail <- renderText(
    paste0("Detailed Breakdown for ",input$facility_s," In 2023")
  ) 
    
  output$mytable <- renderDataTable(
    selected() %>%
      filter(flag == "Outlier") %>%
      select(-c(flag,region,district)) %>%
      relocate(indicator , .before = period) %>%
      dplyr::arrange(desc(period)) %>%
      filter(period > "2023-01-01")%>%      rename(
        `Expected Value` = variable_clean, 
        `Possible Outlier` = variable,
        `Data Element` = indicator,
        Month = period,
        Facility = facility,
        #`Expected Value` = `Suggested Imputation`
      )
      
      
  )
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$facility_s,".csv")
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
        paste0(input$district,"_quality_report", sep = '.', switch(
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
        
        rm(params)
        #params <- list(district = input$district)
        
        out <- rmarkdown::render("data_quality.Rmd", 
                      params = list(district = input$district),
                      switch(input$format,
                             PDF = pdf_document(), 
                             HTML = html_document(), 
                             Word = word_document()),
                      envir = new.env())
        file.rename(out, file)
        
        # rmarkdown::render("data_quality.Rmd",output_file = file,
        #                   params = params,
        #                   envir = new.env(parent = globalenv()))
        
        
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
