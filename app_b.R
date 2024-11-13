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
library(DBI)
library(duckdb)
source("pcdas_token.R", local = TRUE)

# Load base map geometry
geo <- readRDS("data/geo.rds")

# Connect to terraclimate data
tc_con <- dbConnect(duckdb(), "data/terraclimate.duckdb", read_only = TRUE)
tc_tbl <- tbl(tc_con, "terraclimate") |>
  filter(cod_mun %in% geo$cod_mun)

# Climate indicators list for selector
clim_indi_names <- c("def","pet","ppt","soil","pdsi","vap","srad","vpd","aet","tmin","tmax","ws")
names(clim_indi_names) <- c(
  "Déficit climático de água",
  "Evapotranspiração potencial",
  "Precipitação",
  "Umidade do solo",
  "Índice de severidade de seca de Palmer",
  "Pressão de vapor",
  "Radiação de onda curta de superfície descendente",
  "Déficit de pressão de vapor",
  "Evapotranspiração real",
  "Temperatura mínima",
  "Temperatura máxima",
  "Velocidade do vento"
)

# Load health data
health <- readRDS("data/health.rds")

# Municipality list for selector
geo_codnames <- geo$cod_mun
names(geo_codnames) <- paste(geo$name_mun, "-", geo$name_uf)

# Color ramp function for map
color_ramp <- function(indi){
  colors_vct <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6")
  
  if(indi %in% c("ppt", "soil", "pdsi")){
    colorRamp(colors_vct, interpolate="spline")
  } else {
    colorRamp(rev(colors_vct), interpolate="spline")
  }
}

# "def","pet","ppt","soil","pdsi","vap","srad","vpd","aet","tmin","tmax","ws"

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
          label = "Indicador climático",
          choices = clim_indi_names, 
          selected = "pdsi"
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
        input_task_button(id = "ia_map", label = "IA PCDaS", icon = fontawesome::fa_i("fas fa-wand-magic-sparkles"), label_busy = "Consultando a IA PCDaS...")
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
          label = "Indicador de saúde",
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
    accordion(
      multiple = FALSE,      
      accordion_panel(
        "Seca e saúde",
        p("A seca é um fenômeno climático que, além de afetar a produção agrícola e o abastecimento de água, representa uma grave ameaça à saúde humana. A escassez de água potável compromete o acesso a condições mínimas de higiene e saneamento, aumentando o risco de doenças infecciosas e parasitárias, como a diarreia. A redução de umidade no ar e a queima de vegetação durante períodos de seca contribuem para a poluição atmosférica, agravando problemas respiratórios, como asma. Além disso, o armazenamento de água em recipientes inapropriados, como galões e tonéis sem tampa, contribui para a proliferação de mosquitos que podem transmitir a dengue."),
        p("Este painel interativo de visualização tem por objetivo apresentar indicadores relacionados a seca e saúde na região do semiárido brasileiro."),
        p("O projeto é desenvolvido e mantido pela equipe do Observatório de Clima e Saúde, do Instituto de Comunicação e Informação em Saúde (ICICT) da Fundação Oswaldo Cruz (Fiocruz).")
      ),
      accordion_panel(
        "Déficit climático de água",
        p("Mede a escassez de água disponível no solo. Considera a diferença entre a quantidade de água necessária para manter o solo úmido (com base na evapotranspiração) e a quantidade efetivamente disponível pela precipitação. Quando o clima é mais seco, com pouca chuva e alta evapotranspiração, o déficit hídrico aumenta, indicando maior estresse hídrico."),
        p("Unidade: mm"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Evapotranspiração potencial",
        p("Mede a quantidade máxima de água que poderia ser perdida para a atmosfera, se a água estivesse plenamente disponível no solo."),
        p("Unidade: mm"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Precipitação",
        p("Mede a quantidade de água que cai sobre uma determinada área, em forma de chuva, neve, granizo ou outros tipos de condensação atmosférica. Esse indicador é essencial para avaliar a disponibilidade hídrica de uma região, pois influencia diretamente a umidade do solo, o nível dos rios, reservatórios e aquíferos, e o desenvolvimento da vegetação."),
        p("Unidade: mm"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Umidade do solo",
        p("Mede a quantidade de água retida no solo. A umidade do solo depende de fatores como precipitação, temperatura, tipo de solo e cobertura vegetal."),
        p("Unidade: mm"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Índice de severidade de seca de Palmer - PDSI",
        p("Avalia a intensidade, duração e abrangência de secas em uma determinada região. O indicador é baseado em um balanço hídrico que considera a precipitação, a evapotranspiração, a capacidade de retenção de água do solo e as condições de umidade acumulada. O índice é expresso em uma escala que vai de valores negativos, indicando condições de seca, a positivos, sugerindo umidade acima do normal, enquanto o valor zero representa uma situação de equilíbrio hídrico."),
        p("Unidade: sem unidade"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Pressão de vapor",
        p("Mede a quantidade de vapor d'água presente no ar, ou seja, a pressão exercida pelo vapor de água na atmosfera. Esse indicador é essencial para avaliar a umidade relativa do ar e influencia diversos processos meteorológicos, como a formação de nuvens e a precipitação. A pressão de vapor depende da temperatura do ar: quanto mais quente o ar, maior sua capacidade de reter vapor d'água."),
        p("Unidade: kPa"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Radiação de onda curta de superfície descendente",
        p("Mede a quantidade de energia solar em forma de radiação de ondas curtas que atinge a superfície da Terra. Essa radiação inclui luz visível e parte da radiação ultravioleta e infravermelha, sendo fundamental para o aquecimento da superfície e a condução de processos como evaporação, fotossíntese e a regulação do clima. O valor desse indicador depende de fatores como a localização geográfica, a época do ano, a cobertura de nuvens e a poluição atmosférica, que podem bloquear ou dispersar a radiação solar."),
        p("Unidade: W/m2"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Déficit de pressão de vapor",
        p("Mede a diferença entre a quantidade de vapor d'água presente no ar e a quantidade máxima que o ar pode reter a uma determinada temperatura. Valores elevados indicam maior extresse hídrico."),
        p("Unidade: kPa"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Evapotranspiração real",
        p("Mede a quantidade real de água que é transferida da superfície terrestre para a atmosfera, por meio dos processos de evaporação."),
        p("Unidade: mm"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Temperatura mínima",
        p("Mede a temperatura mínima média mensal observada."),
        p("Unidade: °C"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Temperatura máxima",
        p("Mede a temperatura máxima média mensal observada."),
        p("Unidade: °C"),
        p("Fonte dos dados: Terraclimate")
      ),
      accordion_panel(
        "Velocidade do vento",
        p("Mede a velocidade média do vento."),
        p("Unidade: m/s"),
        p("Fonte dos dados: Terraclimate")
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
    res <- tc_tbl |>
      filter(
        name == input$indicator,
        year == input$year,
        month == input$month
      ) |>
      collect()

    left_join(geo, res, by = "cod_mun")
  })

  # Map render (initial, updated with leafletProxy)
  output$out_map <- renderLeaflet(
    leaflet() |>
        addTiles() |>
        setView(lng = -40.5, -10.4, zoom = 6)
  )

  # Update map
  observeEvent(geo_data(), {
    pal <- colorNumeric(
      palette = color_ramp(input$indicator),
      domain = geo_data()$value, 
      n = 10
    )

    leafletProxy("out_map", session) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        data = geo_data(),
        fillColor = ~pal(value),
        weight = .5,
        color = "white",
        fillOpacity = .5, 
        label = ~glue("{name_mun} - {name_uf}: {round(value, 2)} mm"),
        layerId = ~cod_mun
      ) |>
        addLegend(pal = pal, values = geo_data()$value, opacity = 0.7, title = NULL,
          position = "topright")
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
        prompt = glue("Este arquivo json contêm dados de {names(clim_indi_names[clim_indi_names==input$indicator])} na região semiárida brasileiral em {input$month} de {input$year}. A variável `name_mun` contêm os nomes dos municípios. A variável `name_uf` contêm os nomes dos estados dos municípios. A variavel `value` apresent o dado de {names(clim_indi_names[clim_indi_names==input$indicator])}. Escreva um parágrafo técnico em português sobre os dados, incluindo valores e coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante."),
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
        indi = input$health_indi
      )
    
    indi_subset <- tc_tbl |>
      filter(
        cod_mun == input$mun,
        name == input$indicator
      ) |>
      collect() |>
      mutate(
        yearmonth = paste0(year, str_pad(month, 2, pad = "0")),
        indi = names(clim_indi_names[clim_indi_names==input$indicator]) 
      )
    
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