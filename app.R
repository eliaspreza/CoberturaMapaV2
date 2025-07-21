

# =================================================================
# 1. INSTALAR Y CARGAR LIBRERÍAS
# =================================================================
# install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr", "readr", "DT", "plotly", "lubridate", "ggplot2"))

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readr)
library(DT)
library(plotly)
library(lubridate)
library(ggplot2)

# =================================================================
# 2. CARGA Y PROCESAMIENTO DE DATOS
# =================================================================

# --- DATOS PESTAÑA 1: SEGUIMIENTO TEACH ---
url_teach <- "https://kf.kobotoolbox.org/api/v2/assets/agTsgKzBXGTsemxJbwhWMp/export-settings/escKRR5okxeuM2pkuQqe58m/data.csv"
teach <- read_csv2(url_teach)

df_teach_full <- teach %>%
  dplyr::select(
    starttime, endtime, `Nombre y ID de Centro Educativo`, `Nombre del centro educativo`,
    Zona, Departamento, Distrito,`Seleccione el identificador del docente según el orden en el que lo ha abordado dentro del Centro Educativo`,
    `ID Observador`,
    `Cluster (oculto)`, `GrupoN (oculto)`, `GruposT (oculto)`,
    `_Coordenadas GPS del Centro Educativo_latitude`, `_Coordenadas GPS del Centro Educativo_longitude`
  ) %>%
  dplyr::rename(
    id_escuela = `Nombre y ID de Centro Educativo`,
    nombre_escuela = `Nombre del centro educativo`,
    Identificador_Docente= `Seleccione el identificador del docente según el orden en el que lo ha abordado dentro del Centro Educativo`,
    Observador_id=`ID Observador`,
    grupo_cluster = `Cluster (oculto)`,
    grupo_n = `GrupoN (oculto)`,
    grupo_t = `GruposT (oculto)`,
    latitud_raw = `_Coordenadas GPS del Centro Educativo_latitude`,
    longitud_raw = `_Coordenadas GPS del Centro Educativo_longitude`
  ) %>%
  mutate(
    latitud = as.numeric(latitud_raw),
    longitud = as.numeric(longitud_raw),
    Fecha = as.Date(endtime)
  ) %>%
  dplyr::filter(Fecha >= as.Date("2025-07-07"))


# --- DATOS PESTAÑA 2: GESTIÓN DIRECTIVA ---
url_directiva <- "https://kf.kobotoolbox.org/api/v2/assets/afCDdDvQa32m7JhvVY7Kyr/export-settings/esYtgbGnuJnLxWthRviHkzC/data.csv"
gdirectiva <- read_csv2(url_directiva)

df_gdirectiva_full <- gdirectiva %>%
  dplyr::select(
    start, end, `Código de CE`, `Nombre completo CE`, Zona, Departamento, Distrito,
    `D. Código del encuestador/a`,`Nombre completo director(a):`, `Director interino:`, `Sexo:`, `Edad:`,
    `G. Tome fotografía de la ficha llena con firma y sello del director del CE/ informante calificado_URL`,
    `_H. Punto GPS del CE_latitude`, `_H. Punto GPS del CE_longitude`
  ) %>%
  dplyr::rename(
    codigo_ce = `Código de CE`,
    nombre_ce = `Nombre completo CE`,
    codigo_encuestador=`D. Código del encuestador/a`,
    nombre_director = `Nombre completo director(a):`,
    es_interino = `Director interino:`,
    sexo_director = `Sexo:`,
    edad_director = `Edad:`,
    Foto_ficha=`G. Tome fotografía de la ficha llena con firma y sello del director del CE/ informante calificado_URL` ,
    latitud_raw = `_H. Punto GPS del CE_latitude`,
    longitud_raw = `_H. Punto GPS del CE_longitude`
  ) %>%
  mutate(
    latitud = as.numeric(latitud_raw) / 1000000,
    longitud = as.numeric(longitud_raw) / 1000000,
    Fecha = as.Date(end)
  ) %>%
  dplyr::filter(Fecha >= as.Date("2025-07-07"))


# =================================================================
# 3. INTERFAZ DE USUARIO (UI)
# =================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "|Dashboard::Avance|"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabBox(
      id = "tabset1",
      width = 12,
      tabPanel("Seguimiento TEACH",
               fluidRow(
                 valueBoxOutput("totalGrupos", width = 2),
                 valueBoxOutput("coberturaMuestra", width = 3),
                 valueBoxOutput("porcentajeControl", width = 2),
                 valueBoxOutput("porcentajeT1", width = 2),
                 valueBoxOutput("porcentajeT2", width = 3)
               ),
               fluidRow(
                 box(
                   title = "Cobertura de la Muestra TEACH", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   dateRangeInput(
                     "date_range_teach",
                     label = "Filtrar por fecha:",
                     start = min(df_teach_full$Fecha, na.rm = TRUE),
                     end = max(df_teach_full$Fecha, na.rm = TRUE),
                     min = min(df_teach_full$Fecha, na.rm = TRUE),
                     max = max(df_teach_full$Fecha, na.rm = TRUE),
                     format = "dd/mm/yyyy",
                     separator = " - ",
                     language = "es"
                   ),
                   leafletOutput("mapaCobertura", height = 500)
                 )
               ),
               fluidRow(
                 box(
                   title = "Conteo de Encuestas por Día", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   plotlyOutput("plot_teach")
                 )
               ),
               fluidRow(
                 box(
                   title = "Datos de las Escuelas (filtrados por fecha)", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 12, dataTableOutput("tablaDatos")
                 )
               )
      ),
      tabPanel("Gestión Directiva",
               fluidRow(
                 valueBoxOutput("avanceDirectiva", width=12)
               ),
               fluidRow(
                 box(
                   title = "Mapa de Cobertura de Gestión Directiva", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   dateRangeInput(
                     "date_range_directiva",
                     label = "Filtrar por fecha:",
                     start = min(df_gdirectiva_full$Fecha, na.rm = TRUE),
                     end = max(df_gdirectiva_full$Fecha, na.rm = TRUE),
                     min = min(df_gdirectiva_full$Fecha, na.rm = TRUE),
                     max = max(df_gdirectiva_full$Fecha, na.rm = TRUE),
                     format = "dd/mm/yyyy",
                     separator = " - ",
                     language = "es"
                   ),
                   leafletOutput("mapaDirectiva", height = 500)
                 )
               ),
               fluidRow(
                 box(
                   title = "Conteo de Encuestas por Día", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   plotlyOutput("plot_directiva")
                 )
               ),
               fluidRow(
                 box(
                   title = "Datos de Gestión Directiva (filtrados por fecha)", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = 12, dataTableOutput("tablaDirectiva")
                 )
               )
      )
    )
  )
)

# =================================================================
# 4. LÓGICA DEL SERVIDOR (SERVER)
# =================================================================
server <- function(input, output, session) {
  
  # --- LÓGICA REACTIVA PARA PESTAÑA 1: SEGUIMIENTO TEACH ---
  filtered_teach_data <- reactive({
    req(input$date_range_teach)
    df_teach_full %>%
      filter(Fecha >= input$date_range_teach[1] & Fecha <= input$date_range_teach[2])
  })
  
  output$totalGrupos <- renderValueBox({
    valueBox(sum(!is.na(filtered_teach_data()$grupo_t)), "Total Encuestas (en el rango de 2,376)", icon = icon("layer-group"), color = "blue")
  })
  output$coberturaMuestra <- renderValueBox({
    total_registros <- nrow(filtered_teach_data())
    porcentaje <- round((total_registros / 2376) * 100, 1)
    valueBox(paste0(porcentaje, "%"), "Cobertura de Muestra (en rango)", icon = icon("check-double"), color = "green")
  })
  output$porcentajeControl <- renderValueBox({
    conteo_control <- sum(filtered_teach_data()$grupo_t == "Control", na.rm = TRUE)
    porcentaje <- round((conteo_control / 880) * 100, 1)
    valueBox(paste0(conteo_control, " (", porcentaje, "%)"), "Grupo Control (de 880)", icon = icon("cogs"), color = "purple")
  })
  output$porcentajeT1 <- renderValueBox({
    conteo_t1 <- sum(filtered_teach_data()$grupo_t == "GrupoT1", na.rm = TRUE)
    porcentaje <- round((conteo_t1 / 748) * 100, 1)
    valueBox(paste0(conteo_t1, " (", porcentaje, "%)"), "Grupo T1 (de 748)", icon = icon("school"), color = "orange")
  })
  output$porcentajeT2 <- renderValueBox({
    conteo_t2 <- sum(filtered_teach_data()$grupo_t == "GrupoT2", na.rm = TRUE)
    porcentaje <- round((conteo_t2 / 748) * 100, 1)
    valueBox(paste0(conteo_t2, " (", porcentaje, "%)"), "Grupo T2 (de 748)", icon = icon("book-reader"), color = "teal")
  })
  
  output$mapaCobertura <- renderLeaflet({
    df_map <- filtered_teach_data() %>%
      filter(!is.na(latitud) & !is.na(longitud), latitud >= 13.0 & latitud <= 14.6, longitud >= -90.2 & longitud <= -87.6)
    leaflet(data = df_map) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Calles") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addMarkers(lng = ~longitud, lat = ~latitud,
                 popup = ~paste("<b>Escuela:</b>", nombre_escuela, "<br>", "<b>Fecha:</b>", endtime, "<br>", "<b>Observador id:</b>", Observador_id, "<br>", "<b>Departamento:</b>", Departamento, "<br>", "<b>Distrito:</b>", Distrito, "<br>", "<b>Docente:</b>", Identificador_Docente, "<br>", "<b>ID:</b>", id_escuela, "<br>",  "<b>Clúster:</b>", grupo_cluster ,"<b>Grupo:</b>", grupo_t)) %>%
      addLayersControl(baseGroups = c("Calles", "Satélite"), options = layersControlOptions(collapsed = FALSE))
  })
  
  output$plot_teach <- renderPlotly({
    plot_data <- filtered_teach_data() %>% count(Fecha) %>% rename(Conteo = n)
    if(nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = list(text = "No hay datos para el rango de fechas seleccionado.")))
    }
    
    plot_ly(
      data = plot_data,
      x = ~Fecha,
      y = ~Conteo,
      type = 'scatter',
      mode = 'lines+markers',
      stackgroup = 'one',
      # **NUEVOS COLORES: CELESTE SUAVE**
      fill = 'tozeroy',
      fillcolor = 'rgba(133, 193, 233, 0.7)', # Celeste suave con transparencia
      line = list(color = '#2471A3', dash = 'dash'), # Azul oscuro para línea
      marker = list(color = '#2471A3', size = 6), # Azul oscuro para marcadores
      hoverinfo = 'text',
      text = ~paste("Fecha:", format(Fecha, "%d/%m/%Y"), "<br>Encuestas:", Conteo)
    ) %>%
      layout(
        xaxis = list(title = "Fecha de Encuesta"),
        yaxis = list(title = "Número de Encuestas"),
        showlegend = FALSE
      )
  })
  
  output$tablaDatos <- renderDataTable({ DT::datatable(filtered_teach_data(), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE) })
  
  # --- LÓGICA REACTIVA PARA PESTAÑA 2: GESTIÓN DIRECTIVA ---
  filtered_directiva_data <- reactive({
    req(input$date_range_directiva)
    df_gdirectiva_full %>%
      filter(Fecha >= input$date_range_directiva[1] & Fecha <= input$date_range_directiva[2])
  })
  
  output$avanceDirectiva <- renderValueBox({
    total_registros <- nrow(filtered_directiva_data())
    porcentaje <- round((total_registros / 1820) * 100, 1)
    valor_mostrado <- paste0(total_registros, " (", porcentaje, "%)")
    valueBox(valor_mostrado, "Avance del Operativo (en rango de 1,820)", icon = icon("sitemap"), color = "aqua")
  })
  
  output$mapaDirectiva <- renderLeaflet({
    df_map <- filtered_directiva_data() %>%
      filter(!is.na(latitud) & !is.na(longitud), latitud >= 13.0 & latitud <= 14.6, longitud >= -90.2 & longitud <= -87.6)
    leaflet(data = df_map) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Calles") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addMarkers(lng = ~longitud, lat = ~latitud,
                 popup = ~paste("<b>CE:</b>", nombre_ce, "<br>", "<b>Código:</b>", codigo_ce, "<br>", "<b>Código_Encuestador:</b>",codigo_encuestador, "<br>", "<b>Departamento:</b>", Departamento, "<br>", "<b>Distrito:</b>",  Distrito, "<br>", "<b>Zona:</b>",  Zona, "<br>", "<b>Director:</b>", nombre_director, "<br><hr>", "<b>Ficha:</b><br>", "<a href='", Foto_ficha, "' target='_blank'>", "<img src='", Foto_ficha, "' width='200px'>", "</a>")) %>%
      addLayersControl(baseGroups = c("Calles", "Satélite"), options = layersControlOptions(collapsed = FALSE))
  })
  
  output$plot_directiva <- renderPlotly({
    plot_data <- filtered_directiva_data() %>% count(Fecha) %>% rename(Conteo = n)
    if(nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = list(text = "No hay datos para el rango de fechas seleccionado.")))
    }
    
    plot_ly(
      data = plot_data,
      x = ~Fecha,
      y = ~Conteo,
      type = 'scatter',
      mode = 'lines+markers',
      stackgroup = 'one',
      # **NUEVOS COLORES: VERDE MENTA SUAVE**
      fill = 'tozeroy',
      fillcolor = 'rgba(169, 223, 191, 0.7)', # Verde suave con transparencia
      line = list(color = '#1E8449', dash = 'dash'), # Verde oscuro para línea
      marker = list(color = '#1E8449', size = 6), # Verde oscuro para marcadores
      hoverinfo = 'text',
      text = ~paste("Fecha:", format(Fecha, "%d/%m/%Y"), "<br>Encuestas:", Conteo)
    ) %>%
      layout(
        xaxis = list(title = "Fecha de Encuesta"),
        yaxis = list(title = "Número de Encuestas"),
        showlegend = FALSE
      )
  })
  
  output$tablaDirectiva <- renderDataTable({ DT::datatable(filtered_directiva_data(), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE) })
}

# =================================================================
# 5. EJECUTAR LA APLICACIÓN
# =================================================================
shinyApp(ui, server)