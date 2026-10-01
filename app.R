library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(sf)
library(DBI)
library(duckdb)
source("R/data_access.R", local = TRUE)
source("R/ai.R", local = TRUE)

dataset <- read_dashboard_data()
if (is.null(dataset)) {
  ui <- page_fluid(theme = bs_theme(bootswatch = "flatly"),
    h2("Saúde no Semiárido"), p("A atualização dos dados está em preparação. O painel estará disponível após a validação das séries."))
  server <- function(input, output, session) {}
} else {
  geo <- dataset$geo
  climate_names <- dataset$climate$variables
  climate_names <- climate_names[climate_names$name %in% unique(dataset$climate$coverage$name), ]
  indicators <- dataset$health_metadata$indicators
  indicators <- indicators[indicators$indicator_id %in% unique(dataset$availability$indicator_id), ]
  tc_con <- dataset$climate_con
  onStop(function() close_dashboard_data(dataset))
  default_climate <- if ("pdsi" %in% climate_names$name) "pdsi" else climate_names$name[1]
  default_indicator <- indicators$indicator_id[1]
  mun_choices <- setNames(geo$cod_mun, paste(geo$name_mun, "–", geo$name_uf))
  climate_choices <- setNames(climate_names$name, paste0(climate_names$label, " (", climate_names$unit, ")"))
  ai_available <- all(vapply(c("httr2", "promises", "typedjs"), requireNamespace, logical(1), quietly = TRUE)) &&
    utils::packageVersion("httr2") >= "1.3.0" && file.exists("pcdas_token.R")
  ui <- page_navbar(title = "Saúde no Semiárido", theme = bs_theme(bootswatch = "flatly"),
    nav_panel("Mapa", layout_sidebar(
      sidebar = sidebar(
        selectInput("indicator", "Indicador climático", climate_choices, default_climate),
        selectInput("year", "Ano", sort(unique(dataset$climate$coverage$year)), max(dataset$climate$coverage$year)),
        selectInput("month", "Mês", setNames(1:12, c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")), 12),
        textOutput("climate_info"),
        if (ai_available) input_task_button("ia_map", "IA PCDaS", icon = icon("wand-magic-sparkles"),
          label_busy = "Carregando IA PCDaS...", icon_busy = icon("spinner", class = "fa-spin"))
      ), card(full_screen = TRUE, leafletOutput("out_map", height = "75vh")))),
    nav_panel("Gráficos", layout_sidebar(
      sidebar = sidebar(
        selectizeInput("mun", "Município", choices = NULL),
        selectInput("health_indi", "Indicador de saúde", setNames(indicators$indicator_id, indicators$indi)),
        selectInput("measure", "Medida", c("Contagem" = "count", "Taxa por 100 mil habitantes" = "rate"), "rate"),
        selectInput("age_group", "Faixa etária", dataset$metadata$age_groups, "Total"),
        textOutput("health_info"),
        if (ai_available) input_task_button("ia_series", "IA PCDaS", icon = icon("wand-magic-sparkles"),
          label_busy = "Carregando IA PCDaS...", icon_busy = icon("spinner", class = "fa-spin"))
      ), card(full_screen = TRUE, plotlyOutput("graph_health", height = "70vh")))),
    nav_panel("Conceitos e fontes", card(
      h3("Clima e saúde no Semiárido"),
      p("Indicadores mensais por município de residência. A apresentação conjunta das séries descreve sua evolução e não estabelece uma relação causal."),
      p(sprintf("Território: %s municípios. O recorte territorial é fixo ao longo da série histórica.", nrow(geo))),
      p(sprintf("Delimitação: %s; lista municipal: %s; malha: %s.", dataset$metadata$territory$edition,
                dataset$metadata$territory$list_edition, dataset$metadata$territory$geometry_edition)),
      h4("TerraClimate"),
      p("Versão 1.1. Médias mensais municipais ponderadas pela área de interseção das células. As temperaturas são médias mensais das máximas e mínimas, não extremos observados no mês."),
      a("Fonte e metodologia TerraClimate", href = "https://www.climatologylab.org/terraclimate.html"),
      h4("Saúde"),
      p("SIH/SUS: AIHs normais financiadas pelo SUS, por diagnóstico principal e mês da internação, excluída longa permanência. SIM: óbitos por causa básica e mês do óbito."),
      p(paste0("SINAN: ", dataset$health_metadata$indicators$indi[dataset$health_metadata$indicators$indicator_id == "sinan_dengue"],
               ", por município de residência e mês de início dos sintomas.")),
      tableOutput("indicator_definitions"),
      p("As taxas mensais são calculadas por 100 mil residentes da mesma faixa etária e ano, sem anualização. Idade ignorada integra a contagem total, mas não tem taxa específica. Pessoas com 100 anos ou mais integram a faixa de 65 anos ou mais."),
      p("Lacunas indicam dados indisponíveis; zero representa ausência de registros nas publicações adquiridas com cobertura completa. Séries recentes podem ser revistas. Contagens com cobertura parcial são identificadas, e suas taxas não são calculadas."),
      a("População municipal por idade — Ministério da Saúde", href = "https://www.gov.br/saude/pt-br/composicao/seidigi/demas/dados-populacionais/dados-populacionais-1"),
      p(paste("Dados preparados em", dataset$metadata$created_at)),
      if (isTRUE(dataset$metadata$sample)) p("Amostra de validação: cobertura limitada dos dados."),
      p("Observatório de Clima e Saúde — ICICT/Fiocruz")
    ))
  )
  server <- function(input, output, session) {
    updateSelectizeInput(session, "mun", choices = mun_choices, selected = geo$cod_mun[1], server = TRUE)
    shown_ages <- reactiveVal(dataset$metadata$age_groups)
    shown_measures <- reactiveVal(c("count", "rate"))
    observeEvent(input$health_indi, {
      available <- dataset$availability
      ages <- dataset$metadata$age_groups[dataset$metadata$age_groups %in%
        available$age_group[available$indicator_id == input$health_indi]]
      req(length(ages))
      if (identical(ages, shown_ages())) return()
      shown_ages(ages)
      selected <- if (!is.null(input$age_group) && input$age_group %in% ages) input$age_group else ages[1]
      freezeReactiveValue(input, "age_group")
      updateSelectInput(session, "age_group", choices = ages, selected = selected)
    })
    observeEvent(list(input$health_indi, input$age_group), {
      available <- dataset$availability
      measures <- unique(available$measure[available$indicator_id == input$health_indi & available$age_group == input$age_group])
      req(length(measures))
      choices <- c("Contagem" = "count", "Taxa por 100 mil habitantes" = "rate")
      choices <- choices[choices %in% measures]
      if (identical(unname(choices), shown_measures())) return()
      shown_measures(unname(choices))
      selected <- if (!is.null(input$measure) && input$measure %in% measures) input$measure else choices[1]
      freezeReactiveValue(input, "measure")
      updateSelectInput(session, "measure", choices = choices, selected = selected)
    })
    observeEvent(input$indicator, {
      years <- sort(unique(dataset$climate$coverage$year[dataset$climate$coverage$name == input$indicator]))
      selected <- if (!is.null(input$year) && as.integer(input$year) %in% years) input$year else max(years)
      updateSelectInput(session, "year", choices = years, selected = selected)
    })
    observeEvent(list(input$indicator, input$year), {
      req(input$indicator, input$year)
      c <- dataset$climate$coverage
      months <- sort(unique(c$month[c$name == input$indicator & c$year == as.integer(input$year)]))
      req(length(months))
      selected <- if (!is.null(input$month) && as.integer(input$month) %in% months) input$month else max(months)
      updateSelectInput(session, "month", choices = setNames(months, c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")[months]), selected = selected)
    })
    map_data <- reactive({
      req(input$indicator, input$year, input$month)
      values <- dbGetQuery(tc_con, "SELECT cod_mun,value FROM terraclimate WHERE name=? AND year=? AND month=?",
                           params = list(input$indicator, as.integer(input$year), as.integer(input$month)))
      merge(geo, values, by = "cod_mun", all.x = TRUE, sort = FALSE)
    })
    output$out_map <- renderLeaflet({
      box <- st_bbox(geo)
      leaflet(geo) |> addTiles() |> fitBounds(unname(box["xmin"]), unname(box["ymin"]), unname(box["xmax"]), unname(box["ymax"]))
    })
    observe({
      data <- map_data(); unit <- climate_names$unit[match(input$indicator, climate_names$name)]
      ramp <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6")
      if (!input$indicator %in% c("pdsi", "ppt", "soil")) ramp <- rev(ramp)
      pal <- colorNumeric(ramp, domain = data$value, na.color = "#cccccc")
      text <- ifelse(is.na(data$value), "Sem dados", paste(round(data$value, 2), unit))
      leafletProxy("out_map", session) |> clearShapes() |> clearControls() |>
        addPolygons(data = data, fillColor = pal(data$value), weight = 0.4, color = "white", fillOpacity = 0.75,
                    label = paste(data$name_mun, data$name_uf, text, sep = " — "), layerId = data$cod_mun) |>
        addLegend(pal = pal, values = data$value, title = unit, na.label = "Sem dados")
    })
    observeEvent(input$out_map_shape_click, updateSelectizeInput(session, "mun", selected = input$out_map_shape_click$id))
    output$climate_info <- renderText({
      req(input$indicator); c <- dataset$climate$coverage
      c <- c[c$name == input$indicator, ]
      paste0("TerraClimate 1.1 · ", min(c$year), "–", max(c$year))
    })
    health_series <- reactive({
      req(input$mun, input$health_indi, input$age_group, input$measure)
      x <- query_health(dataset, input$health_indi, input$mun, input$age_group, input$measure)
      complete_months(x)
    })
    climate_series <- reactive({
      req(input$mun, input$indicator)
      x <- dbGetQuery(tc_con, "SELECT year,month,value FROM terraclimate WHERE cod_mun=? AND name=? ORDER BY year,month",
                      params = list(as.integer(input$mun), input$indicator))
      complete_months(x)
    })
    output$health_info <- renderText({
      x <- health_series()
      if (!nrow(x)) return("Sem dados para esta seleção.")
      good <- !is.na(x$value)
      if (input$measure == "rate" && input$age_group == "Idade ignorada")
        return("Taxa indisponível para idade ignorada. Selecione Contagem para consultar os eventos.")
      if (!any(good)) return(if (input$measure == "rate") "Taxa indisponível: falta população compatível ou cobertura completa dos dados." else "Sem dados para esta seleção.")
      note <- if (any(!x$complete, na.rm = TRUE)) " Cobertura parcial em alguns períodos." else ""
      prelim <- if (any(x$preliminary, na.rm = TRUE)) " Inclui dados preliminares ou sujeitos a revisão." else ""
      gap <- if (any(!good)) " Há períodos sem dados ou sem denominador compatível." else ""
      paste0(unique(na.omit(x$source))[1], " · ", format(min(x$date[good]), "%m/%Y"), "–",
             format(max(x$date[good]), "%m/%Y"), note, prelim, gap)
    })
    output$graph_health <- renderPlotly({
      h <- health_series(); c <- climate_series()
      validate(need(nrow(h) > 0, "Sem dados de saúde para esta seleção."))
      label <- indicators$indi[match(input$health_indi, indicators$indicator_id)]
      unit <- if (input$measure == "rate") "Por 100 mil habitantes" else "Número de eventos"
      h$status <- ifelse(is.na(h$complete), "Sem dados", ifelse(!h$complete, "Cobertura parcial",
        ifelse(h$preliminary, "Preliminar / sujeito a revisão", "Publicado")))
      h$status[is.na(h$value)] <- if (input$measure == "rate") "Taxa indisponível" else "Sem dados"
      h$tooltip <- paste(format(h$date, "%m/%Y"), label, h$status, sep = "<br>")
      a <- plot_ly(h, x = ~date, y = ~value, text = ~tooltip, hoverinfo = "text+y",
                   type = "scatter", mode = "lines+markers", marker = list(size = 3), name = label, connectgaps = FALSE) |>
        layout(yaxis = list(title = unit))
      b <- plot_ly(c, x = ~date, y = ~value, type = "scatter", mode = "lines",
                   name = climate_names$label[match(input$indicator, climate_names$name)], connectgaps = FALSE) |>
        layout(yaxis = list(title = climate_names$unit[match(input$indicator, climate_names$name)]))
      subplot(a, b, nrows = 2, shareX = TRUE, titleY = TRUE) |>
        layout(legend = list(orientation = "h"), xaxis = list(title = ""), xaxis2 = list(title = "Período"))
    })
    output$indicator_definitions <- renderTable({
      data.frame(Indicador = indicators$indi, Definição = indicators$definition, Fonte = indicators$source, check.names = FALSE)
    })
    if (ai_available) {
      register_ai_observer(input, output, session, map_data, climate_names)
      register_series_ai_observer(input, output, session, health_series, climate_series,
        geo, indicators, climate_names)
    }
  }
}
shinyApp(ui, server)
