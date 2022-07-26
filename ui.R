library(shiny) # paquete principal del panel de visualización
library(shinydashboard) # paquete que modifica la estructura de comandos shiny (algunos), pero da un mejor aspecto visual
library(openxlsx) # carga de archivos
library(ggplot2) # gráficos
library(viridis) # paleta de colores
library(dplyr) # tubería
library(googlesheets4) # Conección a la cuenta de Google Drive
library(Cairo) # mejora resolución y renderizado de gráficos
library(DT) # paquete para renderizar DataTable
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

#### Profesor 1

promedios_profesor1 = uiOutput("valueboxes_p1")
data_p1 = downloadButton('downloadData_p1', 'Descargar resúmenes de notas')
profesor1_curso1_seccion1 =  DT::dataTableOutput("p1_c1_s1")
profesor1_curso1_seccion2 =  DT::dataTableOutput("p1_c1_s2")

#### Profesor 2

promedios_profesor2 = uiOutput("valueboxes_p2")
data_p2 = downloadButton('downloadData_p2', 'Descargar resúmenes de notas')
profesor2_curso1_seccion3 =  DT::dataTableOutput("p2_c1_s3")
profesor2_curso2_seccion1 =  DT::dataTableOutput("p2_c2_s1")

#### Asignaturas

promedios_cursos = plotOutput("resultados_cursos")

########################### Distribución de paneles: Hoja 1 y 2 ###########################

# Como la cantidad de profesor es baja podemos crear los paneles manualmente, pero lo ideal es automatizar
# las pestañas de con insertabpanel

all1 = fluidPage(
  fluidRow(
    box(id = "Profesor1-main", width = 12, solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,
        title = "Profesor 1",
        tabBox(id = "Profesor1", width = 12,
              tabPanel(id = "general1", title = "Resumen General",
                       fluidRow(
                         column(width = 12, data_p1),
                         column(width = 12, h3("Promedio general por curso")),
                         column(width = 12, promedios_profesor1)
                         )
                       ),
              tabPanel(id = "Curso1-seccion1-main", title = "Curso 1 - sección 1", 
                       fluidRow(
                         column(width = 12, profesor1_curso1_seccion1)
                         ),
                       hr(),
                       tabBox(id = "Curso1-seccion1", title = "Detalle de las evaluaciones", width = NULL
                              # Automatización de los tab panel
                              )
                       ),
              tabPanel(id = "Curso1-seccion2-main", title = "Curso 1 - sección 2",
                       fluidRow(
                         column(width = 12, profesor1_curso1_seccion2)
                       ),
                       hr(),
                       tabBox(id = "Curso1-seccion2", title = "Detalle de las evaluaciones", width = NULL
                              # Automatización de los tab panel
                              )
                       )
              )
        ),
    box(id = "Profesor2-main", width = 12, solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,
        title = "Profesor 2",
        tabBox(id = "Profesor2", width = 12,
               tabPanel(id = "general2", title = "Resumen General",
                        fluidRow(
                          column(width = 12, data_p2),
                          column(width = 12, h3("Promedio general por curso")),
                          column(width = 12, promedios_profesor2)
                          )
                        ),
               tabPanel(id = "Curso1-seccion3-main", title = "Curso 1 - sección 3",
                        fluidRow(
                          column(width = 12, profesor2_curso1_seccion3)
                        ),
                        hr(),
                        tabBox(id = "Curso1-seccion3", title = "Detalle de las evaluaciones", width = NULL
                               # Automatización de los tab panel
                               )
                        ),
               tabPanel(id = "Curso2-seccion1-main", title = "Curso 2 - sección 1",
                        fluidRow(
                          column(width = 12, profesor2_curso2_seccion1)
                        ),
                        hr(),
                        tabBox(id = "Curso2-seccion1", title = "Detalle de las evaluaciones", width = NULL
                               # Automatización de los tab panel
                               )
                        )
               )
        )
  )
)

all2 = fluidPage(
  fluidRow(
    box(id = "Resumen_asigs", width = 12, solidHeader = TRUE, collapsed = FALSE, collapsible = TRUE,
        title = "Resumen de Cursos",
        fluidRow(
          column(width = 6, promedios_cursos)
        ))
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