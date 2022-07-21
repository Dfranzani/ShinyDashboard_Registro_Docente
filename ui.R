library(shiny) # paquete principal del panel de visualización
library(shinydashboard) # paquete que modifica la estructura de comandos shiny (algunos), pero da un mejor aspecto visual
library(fmsb) # gráfico de araña
library(openxlsx) # carga de archivos
library(ggplot2) # gráficos
library(gridExtra) # agrupar gráficos de ggplot
library(viridis) # paleta de colores
library(kableExtra) # renderizado de tablas
library(dplyr) # tubería
library(ggpubr) # agrupar gráficos de ggplot con una sola leyenda
# library(plotly) # gráficos interactivos (se puede combinar con ggplot fácilmente)
library(googlesheets4) # Conección a la cuenta de Google Drive
library(Cairo) # mejora resolución y renderizado de gráficos
library(shinyjs)
options(shiny.usecairo = T)

########################### Elementos generales del panel ###########################

# Header Panel
headerpanel = dashboardHeader(title = "U.F.M.E.",
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://www.linkedin.com/in/dfranzani/",
                                             icon("linkedin"), "", target = "_blank")),
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://github.com/Dfranzani",
                                             icon("github"), "", target = "_blank")),
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://dfranzani.netlify.app/",
                                             icon("blog"), "", target = "_blank")))

# Sidebar Menu Item pages
sidebar = dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(
    menuItem("Profesores", tabName = "registros", icon = icon("dashboard", lib = "glyphicon")),
    menuItem("Asignaturas", tabName = "asignaturas", icon = icon("dashboard", lib = "glyphicon"))
  )
)

# Incluir filtros en el sidebar menu

########################### Elementos de paneles: Hoja 1 y 2 ###########################

########################### Distribución de paneles: Hoja 1 y 2 ###########################

# Como la cantidad de profesor es baja podemos crear los paneles manualmente, pero lo ideal es automatizar
# las pestañas de con insertabpanel

all1 = fluidPage(
  fluidRow(
    box(id = "Profesor1", width = 12, solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,
        title = "Profesor 1",
        tabBox(id = "Resumen1", width = 12,
              tabPanel(id = "p1r1.1", title = "Resumen General"),
              tabPanel(id = "p1r1.2", title = "Curso 1"),
              tabPanel(id = "p1r1.3", title = "Curso 2")
              # Solo rellenar con tablas
              )
        ),
    box(id = "Profesor2", width = 12, solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,
        title = "Profesor 2",
        tabBox(id = "Resumen2", width = 12,
               tabPanel(id = "p2r2.1", title = "Resumen General"),
               tabPanel(id = "p2r2.2", title = "Curso 1"),
               tabPanel(id = "p2r2.3", title = "Curso 2")
               )
        )
  )
)
all2 = fluidPage(
  fluidRow(
    box(id = "Resumen_asigs", width = 12, solidHeader = TRUE, collapsed = FALSE, collapsible = TRUE,
        title = "Resumen de Asignaturas")
    # Añadir resumen de la signatura por profesor y uno general (todo en la misma tabla y añadir gráfico)
  )
)

########################### Despliegue ###########################

# Primera hoja
hoja1 = tabItem(tabName = "registros", all1)
# Segunda hoja
hoja2 = tabItem(tabName = "asignaturas", all2)

# Cuerpo de todas las hojas
bodypanel = dashboardBody(tabItems(hoja1, hoja2))

# Panel completo
ui = dashboardPage(header = headerpanel, sidebar = sidebar, body = bodypanel, skin = "black")