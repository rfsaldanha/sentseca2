# Packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(duckdb)

# Database connection
con <- DBI::dbConnect(
  duckdb::duckdb(), 
  dbdir = "../rclone-datasus/rclone_logs.duckdb", 
  read_only = TRUE
)

# Dabatase tables
logs_tb <- dplyr::tbl(con, "logs")

# Interface
ui <- page_sidebar(
  heigth = "100px",
  title = "Secas",
  sidebar = sidebar(
    
  ),
  layout_columns(
    col_widths = c(5,7),
    row_heights = c(2, 3),
    card(
      card_header("Summary"),
      card_body(
        
      )
    )
  )

)

# Server
server <- function(input, output) {

  react_sel <- reactive({
    
  })

  
}

shinyApp(ui, server)