server = function(input, output, session){
  
  # Conexión a Google Drive y carga de bases de datos
  
  # Set authentication token to be stored in a folder called `.secrets`
  # options(gargle_oauth_cache = ".secrets")
  
  # Authenticate manually
  # gs4_auth()
  
  # If successful, the previous step stores a token file.
  # Check that a file has been created with:
  # list.files(".secrets/")
  
  # Check that the non-interactive authentication works by first deauthorizing:
  # gs4_deauth()
  
  # Authenticate using token. If no browser opens, the authentication works.
  gs4_auth(cache = ".secrets", email = "dfranzani@gmail.com")
  
  listado.cursos = as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/140kK9DxWKtgCtwSxtt3siqNtvTXBX5LlJgm8CB5UuwU/edit#gid=0",
                              sheet = "Listado"))
  
  # Contenido de todas las notas por profesor, en todos los aspectos
  
  urls = list("https://docs.google.com/spreadsheets/d/1OBmwY66WNfIv63GMsBF-gDf9xob0q1lXATQzZsQzwMw/edit#gid=1752874311",
              "https://docs.google.com/spreadsheets/d/1JLqReXrK7QtoamKSyWhfcSsUh4x7RdCXHm3kuVGJ2zM/edit#gid=1752874311",
              "https://docs.google.com/spreadsheets/d/1K8Z_802i5DrLc9CbgmWW6AdBjJTWUVnhhtbqU-V5upI/edit#gid=1752874311",
              "https://docs.google.com/spreadsheets/d/1cbQgCwhI0ZoFQ1gAQqzYhiLvHH53gu2bxgOWjbXx-P8/edit#gid=1752874311")

  docs = lapply(urls, function(url){
    doc = gs4_get(url)
    nombre = doc$name # nombre del archivo
    hojas = doc$sheets$name # nombre de las hojas
    doc = lapply(as.list(hojas), function(hoja){
      # Las ponderaciones, los controles y el examen siempre estarán en las primeras 3 columnas, el resto corresponde a pruebas
      return(as.data.frame(read_sheet(url, sheet = hoja)))
    })
    return(list("Nombre" = nombre, "Hoja" = hojas, "Documentos" = doc))
  })

  # Función para elaborar paneles de profesor
  # 1) Panel con métricas generales del profesor: Promedio general de cada curso (valuebox para cada promedio)
  # 2) Resultados por curso: Notas pruebas y controles, nota de presentación, nota del examen y nota final
  
  metricas_generales = function(profesor){
    aux = subset(listado.cursos, Profesor == profesor)
    aux.cursos_seccion = paste(aux$Curso, "- seccion", aux$Sección, sep = " ")
    aux.bbdd = lapply(docs, function(curso){ # listado de las métricas por profesor
      if (curso$Nombre %in% aux.cursos_seccion) { # Filtramos los cursos correspondientes al profesor
        # Calculamos inmediatamente los promedios por curso
        # Primero obtenemos las ponderaciones: primer están las pruebas, luego los controles y finalmente el examen
        aux.ponderacion = curso$Documentos[[1]]
        # Obtenemos los promedios de las pruebas, controles y exámenes, las pruebas empiezan siempre en la posición 4 de la lista
        aux.promedio.pruebas = lapply(curso$Documentos[4:length(curso$Documentos)], function(prueba){ # Información por prueba
          # print(prueba)
          ## Promedios prueba
          ideal = sum(as.numeric(unlist(prueba[2,-1]))) # Puntaje ideal de la prueba
          prueba = as.matrix(prueba[-c(1:2),]) # Quitamos la primera y segunda fila que contiene los resultados de aprendizaje y puntaje ideal
          cols = colnames(prueba) # Guardamos los nombre de columna, para ocuparlos como dimensión
          # Realizamos conversión a matriz porque es un dataframe de lista, ya que contiene distintos tipos de datos por columna
          prueba = matrix(as.character(unlist(prueba)), byrow = F, ncol = length(cols))[,-1] # Eliminamos lo nombres
          notas.por.prueba = apply(prueba, 1, function(puntos){ # Nota por estudiante
            puntos = as.numeric(puntos)
            puntos = sum(puntos)
            if (puntos <= ideal) {
              # (0,1), (60%, 4)
              corte = 0.6*ideal
              return(3/corte*puntos + 1)
            } else {
              # (60%, 4) (ideal, 7)
              return(3/(ideal - corte)*(puntos - ideal) + 7)
            }
          })
          promedio.todas.las.prueba = mean(notas.por.prueba) # Promedio de las notas obtenidas por los estudiantes
          return(promedio.todas.las.prueba)
          ## Promedio Controles y Exámenes
        })
        # ABORDAR EL HECHO DE TENER NOTAS INCOMPLEATAS, USAR na.omit AL MOMENTO DE SACAR PROMEDIOS EN TODO EL CÓDIGO
        return(mean(unlist(aux.promedio.pruebas)))
      }
    })
    aux.bbdd = aux.bbdd[!is.na(aux.bbdd)]
    return(aux.bbdd)
  }
  
 
}