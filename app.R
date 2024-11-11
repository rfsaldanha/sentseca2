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
# remotes::install_github("JohnCoene/typedjs")
library(typedjs)
library(rpcdas)
source("pcdas_token.R", local = TRUE)

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

# NDVI color ramp for map
ndvi_ramp <- colorRamp(c("#d83d40", "#f3fd61", "#35a328"), interpolate="spline")

# Municipality list for selector
geo_codnames <- geo$cod_mun
names(geo_codnames) <- paste(geo$name_mun, "-", geo$name_uf)

# Interface
ui <- page_navbar(
  title = "Saúde no Semiárido", 
  theme = bs_theme(bootswatch = "flatly"),

  # Logo
  tags$head(
    tags$script(
      HTML('$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });')),
    tags$style(
      HTML('@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}')
    )),

  # Translation
  tags$script(
    HTML("
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    ")
  ),

  # Map page
  nav_panel(
    title = "Mapa",

    # Sidebar
    layout_sidebar(
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
          sep = "", 
          animate = TRUE
        ),

        # IA button
        input_task_button(id = "ia_map", label = "", icon = fontawesome::fa_i("fas fa-wand-magic-sparkles"), label_busy = "Carregando IA PCDaS...")
      ),

      # Map card
      card(
        full_screen = TRUE,
        card_body(
          class = "p-0",
          leafletOutput("out_map")
        )
      )

    )
  ),

  # Graphs page
  nav_panel(
    title = "Gráficos",

    layout_sidebar(
      sidebar = sidebar(
        # Select municipality
        selectizeInput(
          inputId = "mun", 
          label = "Município", 
          choices = NULL
        ),

        # Select health indicator
        selectInput(
          inputId = "health_indi",
          label = "Indicador",
          choices = c("Taxa de internação por asma", "Taxa de internação por dengue", "Taxa de internação por diarréia")
        ),

        # Select age group
        selectInput(
          inputId = "age_group",
          label = "Faixa etária",
          choices = c("De 0 a 4 anos", "De 5 a 9 anos", "De 10 a 19 anos", "De 20 a 64 anos", "De 65 a 99 anos")
        )
      ),

      # Graphs card
      card(
        full_screen = TRUE,
        card_header("Saúde"),
        card_body(
          vchartr::vchartOutput(outputId = "graph_health")
        )
      )
    )
  ),

  # About page
  nav_panel(
    title = "Conceitos e definições",
    card(
      card_header("Seca e saúde"),
      p("A seca é um fenômeno climático que, além de afetar a produção agrícola e o abastecimento de água, representa uma grave ameaça à saúde humana. A escassez de água potável compromete o acesso a condições mínimas de higiene e saneamento, aumentando o risco de doenças infecciosas e parasitárias, como a diarreia. A redução de umidade no ar e a queima de vegetação durante períodos de seca contribuem para a poluição atmosférica, agravando problemas respiratórios, como asma. Além disso, o armazenamento de água em recipientes inapropriados, como galões e tonéis sem tampa, contribui para a proliferação de mosquitos que podem transmitir a dengue."),
      p("Este painel interativo de visualização tem por objetivo apresentar indicadores relacionados a seca e saúde na região do semiárido brasileiro."),
      p("O projeto é desenvolvido e mantido pela equipe do Observatório de Clima e Saúde, do Instituto de Comunicação e Informação em Saúde (ICICT) da Fundação Oswaldo Cruz (Fiocruz).")
    ),
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Precipitação",
        p("Volume médio mensal de chuva em mm nos municípios."),
        p("Fonte: ???")
      ),
      accordion_panel(
        "NDVI",
        p("Índice de estado da vegetação, que indica a produção primária (produção de clorofila) e umidade local por meio de um indicador numérico obtido por sensoriamento remoto."),
        p("Os valores de NDVI variam entre -1.0 e +1.0. Observa-se pela sua definição matemática que o NDVI de uma área contendo uma vegetação densa típica de florestas temperadas e tropicais tende a ter valores positivos altos entre 0.5 e 1.0. Grama e vegetação esparsa possuem valores positivos mais baixos, aproximadamente entre 0.2 e 0.5, enquanto solos expostos possuem valores ainda mais baixos entre 0.1 e 0.2, podendo alcançar valores negativos dependendo do tipo de solo. As nuvens apresentam valores próximos de zero. No caso de corpos de água, o NDVI apresenta valores negativos. A interpretação dos valores adquiridos atraves do NDVI pode auxiliar na identificação de áreas de seca e areas sujeitas a queimadas."),
        p("Fonte: ???")
      ),
      accordion_panel(
        "Taxa de internação por asma",
        p("Internações por asma na população residente em determinado espaço geográfico, no período considerado, por 100.000 habitantes."),
        p("Fonte: Sistema de Informações Hospitalares do SUS - SIH, DataSUS, Ministério da Saúde.")
      ),
      accordion_panel(
        "Taxa de internação por dengue",
        p("Casos de dengue na população residente em determinado espaço geográfico, no período considerado, por 100.000 habitantes."),
        p("Fonte: Sistema de Informações de Agravos de Notificação - SINAN, DataSUS, Ministério da Saúde.")
      ),
      accordion_panel(
        "Taxa de internação por diarréia",
        p("Internações por diarréia e gastroenterite com origem de infecção presumível em determinado espaço geográfico, no período considerado, por 100.000 habitantes."),
        p("Fonte: Sistema de Informações Hospitalares do SUS - SIH, DataSUS, Ministério da Saúde.")
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

  # Map render (initial, updated with leafletProxy)
  output$out_map <- renderLeaflet(
    if(input$indicator == "Precipitação"){
      leaflet() |>
        addTiles() |>
        setView(lng = -40.5, -10.4, zoom = 6) |>
        addLegend(
          layerId = "legend",
          position = 'topright',
          colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
          labels = c("úmido","","","","seco"), 
          opacity = 0.5,
          title = "Precipitação"
        )
    } else if(input$indicator == "NDVI"){
      leaflet() |>
        addTiles() |>
        setView(lng = -40.5, -10.4, zoom = 6) |>
        addLegend(
          layerId = "legend",
          position = 'topright',
          colors = c("#35a328", "#a5fd61", "#f3fd61", "#d8953d", "#d83d40"),
          labels = c("úmido","","","","seco"), 
          opacity = 0.5,
          title = "NDVI"
        )
    }
    
  )

  # Update map
  observeEvent(geo_data(), {

    if(input$indicator == "Precipitação"){
      prec_pal <- colorQuantile(
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
        palette = ndvi_ramp,
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

  # IA map
  observeEvent(input$ia_map, {

    tmp <- geo_data() |>
      select(name_mun, name_uf, value) |>
      st_drop_geometry() |>
      mutate(value = round(value, 2))

    tryCatch({
      res_ia <- get_text_description(
        df = tmp, 
        prompt = "Este arquivo json contêm dados de precipitação na região semiárida brasileiral. A variável `name_mun` contêm os nomes dos municípios. A variável `name_uf` contêm os nomes dos estados dos municípios. A variavel `value` apresent o dado de precipitação em milimetros. Escreva um parágrafo técnico em português sobre os dados, incluindo valores e coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante.",
        pcdas_token = pcdas_token
      )
    }, error = function(e){
      res_ia <- ""
    })
    
    showModal(modalDialog(
      title = tags$img(src = "image_IA_PCDaS.png", style = "width: 20%; padding: 0;"),
      tagList(
        tags$html(typedjs::typed(markdown(trimws(res_ia)), contentType = "html", showCursor = FALSE))
      ),
      size = "l",
      easyClose = TRUE,
      fade = TRUE,
      footer = modalButton("Fechar")
    ))
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
      rename(Valor = value) |>
      vchart() |>
      v_line(aes(x = yearmonth, y = Valor)) |>
      v_facet_wrap(vars(indi), ncol = 1, scales = "free_y") |>
        v_specs_crosshair(
          xField = list(
            visible = TRUE,
            line = list(type = "rect"), 
            defaultSelect = list(
              axisIndex = 0, 
              datum = "201201"
            ), 
            label = list(visible = FALSE)
          ), 
          yField = list(
            visible = TRUE, 
            defaultSelect = list(
              axisIndex = 1,
              datum = 100
            ), 
            line = list(
              style = list(
                lineWidth = 1, 
                opacity = 1, 
                stroke = "#000", 
                lineDash = c(2, 2)
              )
            ),
            label = list(visible = FALSE)
          )
        )
    
  })

}

shinyApp(ui, server)