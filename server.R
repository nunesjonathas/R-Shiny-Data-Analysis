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
