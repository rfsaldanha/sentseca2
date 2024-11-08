# Packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(duckdb)
library(leaflet)

# Database connection
# con <- DBI::dbConnect(
#   duckdb::duckdb(), 
#   dbdir = "../rclone-datasus/rclone_logs.duckdb", 
#   read_only = TRUE
# )

# # Dabatase tables
# logs_tb <- dplyr::tbl(con, "logs")

# Interface
ui <- page_navbar(
  title = "Saúde no Semiárido", 
  
  # Sidebar
  sidebar = sidebar(
    textInput("teste1", label = "Teste 1")
  ),
  
  # Map page
  nav_panel(
    title = "Mapa",
    
  ),
  nav_panel(
    title = "Gráficos"
  )
)

# Server
server <- function(input, output) {
  
}

shinyApp(ui, server)