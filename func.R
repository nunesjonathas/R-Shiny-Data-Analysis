validate_plot <- function(data_input, x, y, z) {
  v_vars <- c(x, y, z)
  len_vars <- length(which(not_sel == v_vars))
  
  switch (as.character(len_vars),
          "3" = {
            plot_choices <- character(0)
            return(plot_choices)
          },
          "2" = {
            v_vars <- v_vars[-which(not_sel == v_vars)]
            if (is.discrete(data_input[, get(v_vars)])) {
              #1 discrete variable
              plot_choices <- c("Bar Plot")
              return(plot_choices)
            } else {
              #1 continuous variable
              plot_choices <-
                c(
                  "Histogram",
                  "Smoothed Density",
                  "Area Plot",
                  "Dot Plot",
                  "Frequency Polygons",
                  "Quantile-Quantile"
                )
              return(plot_choices)
            }
          },
          {
            if (len_vars != 0) {
              v_vars <- v_vars[-which(not_sel == v_vars)]
            }
            print(v_vars)
            if (is.discrete(data_input[, get(v_vars[1])]) ||
                is.discrete(data_input[, get(v_vars[2])])) {
              if (is.discrete(data_input[, get(v_vars[1])]) &
                  is.discrete(data_input[, get(v_vars[2])])) {
                #2 Variables X-discrete y-discrete
                plot_choices <- c("Count")
                return(plot_choices)
              } else{
                #2 Variables X-discrete y-continuous
                plot_choices <-
                  c("Violin", "BoxPlot", "Dot Plot", "Col Plot")
                return(plot_choices)
              }
              
            } else{
              #2 continuous variables
              plot_choices <-
                c(
                  "Points",
                  "Jittered Points",
                  "Quantile",
                  "Rug Plot",
                  "Smoothed Conditional Means",
                  "Text Plot",
                  "Label Plot"
                )
              return(plot_choices)
            }
          })
}


draw_plot <- function(plot_type) {
  print(plot_type)
  switch (
    plot_type,
    "Points" = geom_point(),
    "Jittered Points" = geom_jitter(),
    "Quantile" = geom_quantile(),
    "Rug Plot" = geom_rug(),
    "Smoothed Conditional Means" = geom_smooth(),
    "Text Plot" = geom_text(),
    "Label Plot" = geom_label(),
    "Violin" = geom_violin(),
    "BoxPlot" = geom_boxplot(),
    "DotPlot" = geom_dotplot(),
    "Col Plot" = geom_col(),
    "Histogram" = geom_histogram(),
    "Smoothed Density" = geom_density(),
    "Area Plot" = geom_area(),
    "Frequency Polygons" = geom_freqpoly(),
    "Quantile-Quantile" = geom_qq(),
    "Bar Plot" = geom_bar()
  )
}