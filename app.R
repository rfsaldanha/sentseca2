# Packages
library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(stringr)
library(glue)
library(sf)
library(lubridate)
library(vchartr)
library(leaflet)

# Load base map geometry
geo <- readRDS("data/geo.rds")

# Load precipitation data
prec <- readRDS("data/prec.rds")

# Load NDVI data
ndvi <- readRDS("data/ndvi.rds")

# Load health data
health <- readRDS("data/health.rds")

# Precipication color ramp for map
prec_ramp <- colorRamp(c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"), interpolate="spline")

# Municipality list for selector
geo_codnames <- geo$cod_mun
names(geo_codnames) <- paste(geo$name_mun, "-", geo$name_uf)

# Interface
ui <- page_navbar(
  title = "Saúde no Semiárido", 
  
  # Sidebar
  sidebar = sidebar(
    # Select indicator
    selectizeInput(
      inputId = "indicator", 
      label = "Indicador de seca",
      choices = c("Precipitação", "NDVI")
    ),

    # Select year
    sliderInput(
      inputId = "year",
      min = 2001,
      max = 2022,
      step = 1, 
      value = 2022, 
      label = "Ano",
      sep = ""
    ),

    # Select month
    sliderInput(
      inputId = "month",
      min = 1,
      max = 12,
      step = 1, 
      value = 6, 
      label = "Mês",
      sep = "", animate = TRUE
    ),
    
    # Select municipality
    selectizeInput(
      inputId = "mun", 
      label = "Município", 
      choices = NULL
    )
  ),
  
  # Map page
  nav_panel(
    title = "Mapa",
    card(
      full_screen = TRUE,
      card_body(
        class = "p-0",
        leafletOutput("out_map")
      )
    )
    
  ),
  nav_panel(
    title = "Gráficos",
    card(
      card_header("Saúde"),
      card_body(
        selectInput(
          inputId = "health_indi",
          label = "Indicador",
          choices = c("Taxa de internação por asma", "Taxa de internação por dengue", "Taxa de internação por diarréia")
        ),
        selectInput(
          inputId = "age_group",
          label = "Faixa etária",
          choices = c("De 0 a 4 anos", "De 5 a 9 anos", "De 10 a 19 anos", "De 20 a 64 anos", "De 65 a 99 anos")
        ),
        vchartr::vchartOutput(outputId = "graph_health")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Fill municipality selector
  updateSelectizeInput(
    session = session, 
    inputId = "mun",
    server = FALSE,
    choices = geo_codnames
  )

  # Map data
  geo_data <- reactive({
    if(input$indicator == "Precipitação"){
      prec_subset <- prec |>
        filter(
          year == input$year,
          month == input$month
        )
      
      left_join(geo, prec_subset, by = "cod_mun")
    } else if (input$indicator == "NDVI"){
      ndvi_subset <- ndvi |>
        filter(
          year == input$year,
          month == input$month
        )
      
      left_join(geo, ndvi_subset, by = "cod_mun")
    }
  })

  # Map render
  output$out_map <- renderLeaflet(
    if(input$indicator == "Precipitação"){
      leaflet() |>
        addTiles() |>
        addPolygons(data = geo, fill = "", stroke = "") |>
          addLegend(
            layerId = "legend",
            position = 'topright',
            colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
            labels = c("muito úmido","úmido",
                       "seco","muito seco","extremamente seco"), opacity = 0.5,
            title = "Precipitação"
          )
    } else if(input$indicator == "NDVI"){
      leaflet() |>
        addTiles() |>
        addPolygons(data = geo, fill = "", stroke = "") |>
          addLegend(
            layerId = "legend",
            position = 'topright',
            colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
            labels = c("muito úmido","úmido",
                       "seco","muito seco","extremamente seco"), opacity = 0.5,
            title = "NDVI"
          )
    }
    
  )

  # Update map
  observeEvent(geo_data(), {
    if(input$indicator == "Precipitação"){
      prec_pal <- colorNumeric(
        palette = prec_ramp,
        domain = geo_data()$value, 
        n = 10
      )

      leafletProxy("out_map", session) |>
        leaflet::clearShapes() |>
        addPolygons(
          data = geo_data(),
          fillColor = ~prec_pal(value),
          weight = .5,
          color = "white",
          fillOpacity = .5, 
          label = ~glue("{name_mun} - {name_uf}: {round(value, 2)} mm"),
          layerId = ~cod_mun
        )
    } else if(input$indicator == "NDVI"){
      ndvi_pal <- colorNumeric(
        palette = prec_ramp,
        domain = geo_data()$value, 
        n = 10
      )

      leafletProxy("out_map", session) |>
        leaflet::clearShapes() |>
        addPolygons(
          data = geo_data(),
          fillColor = ~ndvi_pal(value),
          weight = .5,
          color = "white",
          fillOpacity = .5, 
          label = ~glue("{name_mun} - {name_uf}: {round(value, 2)}"),
          layerId = ~cod_mun
        )
    }
  })

  # Select municipality
  observeEvent(input$out_map_shape_click, {
    click <- input$out_map_shape_click
    updateSelectizeInput(
      session = session, 
      server = FALSE,
      inputId = "mun",
      selected = click$id
    )
  })

  # Render health graph
  output$graph_health <- renderVchart({
    health_subset <- health |>
      filter(
        cod_mun == input$mun,
        indi == input$health_indi,
        age_group == input$age_group
      ) |>
      mutate(
        yearmonth = paste0(year, str_pad(month, 2, pad = "0")),
        indi = input$health_indi,
      )

    message(nrow(health_subset))
    
    if(input$indicator == "Precipitação"){
      indi_subset <- prec |>
        filter(
          cod_mun == input$mun
        ) |>
          mutate(
            yearmonth = paste0(year, str_pad(month, 2, pad = "0")),
            indi = "Precipitação" 
          )
    } else if(input$indicator == "NDVI"){
      indi_subset <- ndvi |>
        filter(
          cod_mun == input$mun
        ) |>
          mutate(
            yearmonth = paste0(year, str_pad(month, 2, pad = "0")),
            indi = "NDVI" 
          )
    }

    bind_rows(health_subset, indi_subset) |>
      vchart() |>
      v_line(aes(x = yearmonth, y = value)) |>
      v_facet_wrap(vars(indi), ncol = 1, scales = "free_y")
    
  })

}

shinyApp(ui, server)