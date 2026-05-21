install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "DT", "scales", "tidyr"))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)
library(tidyr)

# =========================================================
# DATOS
# =========================================================
datos_economia <- data.frame(
  anio = 2010:2025,
  pbi = c(226.1, 238.4, 249.7, 260.8, 272.6, 280.9, 291.5, 301.2,
          312.8, 318.7, 287.3, 303.9, 320.6, 334.8, 350.4, 365.7),
  empleo = c(15.2, 15.5, 15.8, 16.1, 16.4, 16.8, 17.1, 17.4,
             17.6, 17.9, 17.0, 17.3, 17.6, 17.9, 18.2, 18.5),
  desempleo = c(6.8, 6.6, 6.4, 6.1, 5.9, 5.8, 5.6, 5.5,
                5.4, 5.3, 8.5, 7.6, 6.8, 6.2, 5.8, 5.6),
  inflacion = c(2.1, 2.3, 2.5, 2.8, 3.1, 3.0, 2.7, 2.4,
                2.2, 2.0, 1.9, 4.0, 8.5, 6.3, 3.8, 2.4),
  inversion = c(21.5, 22.0, 22.4, 22.7, 23.1, 23.5, 23.8, 24.0,
                24.3, 24.6, 21.0, 22.1, 23.0, 23.8, 24.4, 25.1),
  consumo = c(65.0, 65.3, 65.7, 66.1, 66.4, 66.8, 67.1, 67.4,
              67.8, 68.1, 66.0, 66.7, 67.5, 68.0, 68.6, 69.2)
) %>%
  mutate(
    crecimiento_pbi = c(NA, diff(pbi) / pbi[-length(pbi)] * 100),
    brecha_empleo = c(NA, diff(empleo)),
    nivel_pbi = case_when(
      pbi < 280 ~ "Bajo",
      pbi < 320 ~ "Medio",
      TRUE ~ "Alto"
    ),
    nivel_pbi = factor(nivel_pbi, levels = c("Bajo", "Medio", "Alto"))
  )

datos_economia$crecimiento_pbi[1] <- 0
datos_economia$brecha_empleo[1] <- 0

# =========================================================
# INTERFAZ
# =========================================================
ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(
    title = "Dashboard Económico Perú"
  ),
  
  dashboardSidebar(
    width = 260,
    
    h4("Filtros"),
    
    sliderInput(
      "rango_anios",
      "Selecciona el rango de años:",
      min = min(datos_economia$anio),
      max = max(datos_economia$anio),
      value = c(2012, 2025),
      step = 1,
      sep = ""
    ),
    
    checkboxGroupInput(
      "variables",
      "Variables visibles:",
      choices = c("PBI", "Empleo", "Desempleo", "Inflación", "Inversión", "Consumo"),
      selected = c("PBI", "Empleo", "Desempleo", "Inflación")
    ),
    
    selectInput(
      "x_var",
      "Variable eje X:",
      choices = c(
        "Año" = "anio",
        "PBI" = "pbi",
        "Empleo" = "empleo",
        "Desempleo" = "desempleo",
        "Inflación" = "inflacion",
        "Inversión" = "inversion",
        "Consumo" = "consumo"
      ),
      selected = "anio"
    ),
    
    selectInput(
      "y_var",
      "Variable eje Y:",
      choices = c(
        "PBI" = "pbi",
        "Empleo" = "empleo",
        "Desempleo" = "desempleo",
        "Inflación" = "inflacion",
        "Inversión" = "inversion",
        "Consumo" = "consumo",
        "Crecimiento PBI" = "crecimiento_pbi"
      ),
      selected = "pbi"
    ),
    
    checkboxInput("tendencia", "Mostrar línea de tendencia", TRUE),
    checkboxInput("mostrar_etiquetas", "Mostrar etiquetas", FALSE),
    
    hr(),
    p("Dashboard elaborado en RStudio con Shiny.")
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("kpi_pbi", width = 3),
      valueBoxOutput("kpi_empleo", width = 3),
      valueBoxOutput("kpi_desempleo", width = 3),
      valueBoxOutput("kpi_inflacion", width = 3)
    ),
    
    fluidRow(
      box(
        title = "Evolución del PBI",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("graf_pbi", height = 300)
      ),
      box(
        title = "Evolución del empleo",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("graf_empleo", height = 300)
      )
    ),
    
    fluidRow(
      box(
        title = "Relación entre indicadores",
        width = 7,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("graf_dispersion", height = 340)
      ),
      box(
        title = "Inflación por año",
        width = 5,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("graf_inflacion", height = 340)
      )
    ),
    
    fluidRow(
      box(
        title = "Matriz de correlación",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("graf_correlacion", height = 340)
      ),
      box(
        title = "Ranking de años por PBI",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("graf_ranking", height = 340)
      )
    ),
    
    fluidRow(
      box(
        title = "Tabla de datos",
        width = 12,
        status = "danger",
        solidHeader = TRUE,
        DTOutput("tabla_datos")
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    datos_economia %>%
      filter(anio >= input$rango_anios[1],
             anio <= input$rango_anios[2])
  })
  
  output$kpi_pbi <- renderValueBox({
    valueBox(
      value = round(mean(datos_filtrados()$pbi), 1),
      subtitle = "PBI promedio",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  output$kpi_empleo <- renderValueBox({
    valueBox(
      value = round(mean(datos_filtrados()$empleo), 1),
      subtitle = "Empleo promedio",
      icon = icon("briefcase"),
      color = "yellow"
    )
  })
  
  output$kpi_desempleo <- renderValueBox({
    valueBox(
      value = round(mean(datos_filtrados()$desempleo), 1),
      subtitle = "Desempleo promedio",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$kpi_inflacion <- renderValueBox({
    valueBox(
      value = round(mean(datos_filtrados()$inflacion), 1),
      subtitle = "Inflación promedio",
      icon = icon("percent"),
      color = "green"
    )
  })
  
  output$graf_pbi <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes(
      x = anio,
      y = pbi,
      text = paste0(
        "Año: ", anio,
        "<br>PBI: ", round(pbi, 1),
        "<br>Empleo: ", round(empleo, 1),
        "<br>Desempleo: ", round(desempleo, 1)
      )
    )) +
      geom_line(color = "#A84A44", linewidth = 1) +
      geom_point(color = "#A84A44", size = 2.5) +
      theme_minimal() +
      labs(x = "Año", y = "PBI") +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$graf_empleo <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes(
      x = anio,
      y = empleo,
      text = paste0(
        "Año: ", anio,
        "<br>Empleo: ", round(empleo, 1),
        "<br>PBI: ", round(pbi, 1)
      )
    )) +
      geom_line(color = "#8BBCBD", linewidth = 1) +
      geom_point(color = "#8BBCBD", size = 2.5) +
      theme_minimal() +
      labs(x = "Año", y = "Empleo") +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$graf_dispersion <- renderPlotly({
    df <- datos_filtrados()
    
    p <- ggplot(df, aes_string(
      x = input$x_var,
      y = input$y_var,
      color = "nivel_pbi",
      text = "anio"
    )) +
      geom_point(size = 3, alpha = 0.85) +
      theme_minimal() +
      labs(x = input$x_var, y = input$y_var, color = "Nivel PBI")
    
    if (input$tendencia) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed")
    }
    
    if (input$mostrar_etiquetas) {
      p <- p + geom_text(aes(label = anio), vjust = -0.8, size = 3, show.legend = FALSE)
    }
    
    ggplotly(p)
  })
  
  output$graf_inflacion <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes(
      x = anio,
      y = inflacion,
      text = paste0("Año: ", anio, "<br>Inflación: ", round(inflacion, 1), "%")
    )) +
      geom_col(fill = "#D97D77", alpha = 0.9) +
      theme_minimal() +
      labs(x = "Año", y = "Inflación (%)") +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$graf_correlacion <- renderPlotly({
    df <- datos_filtrados() %>%
      select(pbi, empleo, desempleo, inflacion, inversion, consumo, crecimiento_pbi)
    
    matriz <- round(cor(df), 2)
    corr_df <- as.data.frame(as.table(matriz))
    names(corr_df) <- c("Var1", "Var2", "Correlacion")
    
    p <- ggplot(corr_df, aes(
      x = Var1,
      y = Var2,
      fill = Correlacion,
      text = paste0("Variables: ", Var1, " - ", Var2,
                    "<br>Correlación: ", Correlacion)
    )) +
      geom_tile(color = "white") +
      geom_text(aes(label = Correlacion), size = 4, fontface = "bold") +
      scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0) +
      theme_minimal() +
      labs(x = NULL, y = NULL)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$graf_ranking <- renderPlotly({
    df <- datos_filtrados() %>%
      arrange(desc(pbi)) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(
      x = reorder(as.factor(anio), pbi),
      y = pbi,
      fill = nivel_pbi,
      text = paste0("Año: ", anio, "<br>PBI: ", round(pbi, 1))
    )) +
      geom_col(alpha = 0.9) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Año", y = "PBI") +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tabla_datos <- renderDT({
    datatable(
      datos_filtrados() %>%
        mutate(
          pbi = round(pbi, 1),
          empleo = round(empleo, 1),
          desempleo = round(desempleo, 1),
          inflacion = round(inflacion, 1),
          inversion = round(inversion, 1),
          consumo = round(consumo, 1),
          crecimiento_pbi = round(crecimiento_pbi, 2),
          brecha_empleo = round(brecha_empleo, 2)
        ),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# =========================================================
# EJECUCIÓN
# =========================================================
shinyApp(ui, server)