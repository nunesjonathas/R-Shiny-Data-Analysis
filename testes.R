library(shiny)
## Only run examples in interactive R sessions

plot_choices <- c("ggplot", "histogram")
#choices <- c("geom_area()","geom_density()","geom_dotplot()","geom_freqpoly()", "geom_histogram()", "geom_qq()")
not_sel = "Not Selected"
escolhas <- c("A", "B", "C", "D")

#1 continuous variable
plot_choices <- c("Histogram", "Density", "Area", "DotPlot", "FreqPoly", "QQ")
#1 discrete variable
plot_choices <- c("Bar")
#2 continuous variables
plot_choices <- c("Point", "Jitter", "Quantile", "Rug", "Smooth", "Text", "Label")
#2 Variables X-discrete y-continuous
plot_choices <- c("Violin", "BoxPlot", "DotPlot", "Col")
#2 Variables X-discrete y-discrete
plot_choices <- c("Count")


validate_plot <- function(x,y,z){
  if (x==not_sel & y==not_sel & z == not_sel) {
    plot_choices <- character(0)
    return(plot_choices)
  }else if(x=="A"){
    plot_choices <- c("ggplot", "histogram", "print")
    return(plot_choices)  
  }else {
    plot_choices <- c("ggplot", "histogram")
    return(plot_choices)  
  }
  
}

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(

    #Test Update Select
    selectInput("num_var_1","Numerical Variable 1",choices=c(not_sel,escolhas)),
    selectInput("num_var_2","Numerical Variable 2",choices=c(not_sel,escolhas)),
    selectInput("fact_var","Factor Variable",choices=c(not_sel,escolhas)),
    selectInput("plot_type","Chart Type",choices=not_sel),
    
  )
  
  server <- function(input, output, session) {
    observe({
      x <- input$num_var_1
      y <- input$num_var_2
      z <- input$fact_var
      w <- validate_plot(x,y,z)

      # Can use character(0) to remove all choices
      if (is.null(w))
         w <- "Not Selected"

      # Can also set the label and select items
      updateSelectInput(session, "plot_type",
                        label = paste("Select input label", length(w)),
                        choices = w,
                        selected = head(w, 1)
      )
    })
  }
  
  shinyApp(ui, server)
}