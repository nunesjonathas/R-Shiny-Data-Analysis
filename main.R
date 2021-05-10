library(shiny)
library(data.table)
library(ggplot2)
library(ggsci)
library(plyr)
source("func.R")

not_sel = "Not Selected"
plot_choices <- character(0)
options(shiny.maxRequestSize=400*1024^2)



draw_plot_2 <- function(data_input, num_var_1 = NULL, num_var_2 = NULL, fact_var = NULL, plot_type){
  v_vars <- c(num_var_1, num_var_2, fact_var)
  len_vars <- length(which(not_sel == v_vars))
  print(len_vars)
  switch (as.character(len_vars),
    "0" = {
      print(v_vars)
      ggplot(data = data_input,
             aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
        draw_plot(plot_type)
    },
    "1" = {
      print(v_vars)
      v_vars <- v_vars[-which(not_sel == v_vars)]
      print(v_vars)
      ggplot(data = data_input,
             aes_string(x = v_vars[1], y = v_vars[2])) +
        draw_plot(plot_type)
    },
    "2" = {
      print(v_vars)
      v_vars <- v_vars[-which(not_sel == v_vars)]
      print(v_vars)
      ggplot(data = data_input,
             aes_string(x = v_vars)) +
        draw_plot(plot_type)
    }
  )
}

draw_plot_1 <- function(data_input, num_var_1 = NULL, num_var_2 = NULL, fact_var = NULL){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  #All selected
  if(num_var_1 != not_sel &
     num_var_2 != not_sel &
     fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  #factor not selected
  }else if(fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  #factor selected
  }else {
    if(num_var_1 == not_sel & num_var_2 == not_sel){
      ggplot(data = data_input,
             aes_string(x = fact_var, color = fact_var)) +
        geom_bar(aes(color = fact_var)) +
        scale_fill_jco() +
        scale_color_jco()
    }else{
      ggplot(data = data_input,
             aes_string(x = fact_var, y = ifelse(num_var_1!=not_sel,num_var_1, num_var_2))) +
        geom_point()
    }
  }
}


server <- function(input, output) {
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
    updateSelectInput(inputId = "plot_type", choices = plot_choices)
    
  })
  
  observe({
    x <- input$num_var_1
    y <- input$num_var_2
    z <- input$fact_var
    w <- validate_plot(data_input(), x,y,z)
    
    # Can use character(0) to remove all choices
    if (is.null(w))
      w <- "Not Selected"
    
    # Can also set the label and select items
    updateSelectInput(inputId = "plot_type",
                      label = paste("Available Charts: ", length(w)),
                      choices = w,
                      selected = head(w, 1)
    )
  })
  
  
  
  #Select the columns
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  plot_type <- eventReactive(input$run_button,input$plot_type)
  print(plot_type)
  
  
  
  #Plot
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_2(data_input = data_input(), num_var_1 = num_var_1(), num_var_2 = num_var_2(), fact_var = fact_var(), plot_type = plot_type())
  })
  
  
  
  
  output$plot_1 <- renderPlot(plot_1())
}



ui <- fluidPage()

main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input","Select CSV File to Import",accept=".csv"),
      selectInput("num_var_1","Numerical Variable 1",choices=c(not_sel)),
      selectInput("num_var_2","Numerical Variable 2",choices=c(not_sel)),
      selectInput("fact_var","Factor Variable",choices=c(not_sel)),
      selectInput("plot_type","Chart Type",choices=c(not_sel)),
      actionButton("run_button","Run Analysis",icon=icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("num_var_2_title"))),
            column(width = 4, strong(textOutput("fact_var_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("num_var_2_summary_table")),
            column(width = 4, tableOutput("fact_var_summary_table"))
          ),
          fluidRow(
            column(width = 4,strong("Combined Statistics"))
          ),
          fluidRow(
            column(width = 4,tableOutput("combined_summary_table"))
          )
        )
      )
    )
  )
)




about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Created with R Shiny",
  br(),
  "2021 April"
)


ui <- navbarPage(
  title = "Data Analyser", 
  main_page, 
  about_page
)




shinyApp(ui = ui, server = server)