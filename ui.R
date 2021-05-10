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