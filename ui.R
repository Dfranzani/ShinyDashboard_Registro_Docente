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
options(shiny.usecairo = T)


########################### Datos del panel ###########################

# Datos de los filtros (BBDD)

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
    menuItem("Registros", tabName = "registros", icon = icon("dashboard", lib = "glyphicon"))
  )
)

# Incluir filtros en el sidebar menu

########################### Elementos de paneles: Hoja 1 ###########################

########################### Distribución de paneles: Hoja 1 ###########################

all = fluidPage()

########################### Despliegue ###########################

# Primera hoja
hoja1 = tabItem(tabName = "registro", all)

# Cuerpo de todas las hojas
bodypanel = dashboardBody(tabItems(hoja1))

# Panel completo
ui = dashboardPage(header = headerpanel, sidebar = sidebar, body = bodypanel, skin = "black")