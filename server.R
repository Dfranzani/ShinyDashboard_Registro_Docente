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
  
  notas_evaluacion = function(prueba){ # Calcula los promedios por estudiante en un prueba y examen
    prueba = as.data.frame(prueba)
    ideal = sum(as.numeric(unlist(prueba[2,-1]))) # Puntaje ideal de la prueba
    prueba = as.matrix(prueba[-c(1:2),]) # Quitamos la primera y segunda fila que contiene los resultados de aprendizaje y puntaje ideal
    cols = colnames(prueba) # Guardamos los nombre de columna para ocuparlos como dimensión
    # Realizamos conversión a matriz porque es un dataframe de lista, ya que contiene distintos tipos de datos por columna
    prueba = matrix(as.character(unlist(prueba)), byrow = F, ncol = length(cols))[,-1] # Eliminamos la columna de nombres
    notas.por.prueba = apply(prueba, 1, function(puntos){ # Nota por estudiante en la prueba
      puntos = as.numeric(puntos)
      puntos = sum(puntos)
      if (puntos <= ideal) { # Escala del 60%
        # (0,1), (60%, 4)
        corte = 0.6*ideal
        return(3/corte*puntos + 1)
      } else {
        # (60%, 4) (ideal, 7)
        return(3/(ideal - corte)*(puntos - ideal) + 7)
      }
    })
    return(notas.por.prueba)
  }
  
  notas_control = function(control){ # Calcula los promedios por estudiante en un control
    control = as.data.frame(control)
    control = control[,-1]
    notas.por.control = apply(control, 1, function(notas){ # Nota por estudiante en la prueba
      return(mean(as.numeric(notas))) # Revisar cuando haya notas faltantes
    })
  }
  
  metricas_generales = function(profesor){ # Función para determinar información en el primer panel
    aux = subset(listado.cursos, Profesor == profesor)
    aux.cursos_seccion = paste(aux$Curso, "- seccion", aux$Sección, sep = " ")
    aux.bbdd = lapply(docs, function(curso){ # listado de las métricas por profesor
      
      if (curso$Nombre %in% aux.cursos_seccion) { # Filtramos los cursos correspondientes al profesor
        
        # Primero obtenemos las ponderaciones
        aux.ponderacion = curso$Documentos[[1]]
        # print(aux.ponderacion)
        
        ### Obtenemos los promedios de las pruebas, controles y exámenes
        aux.promedios = lapply(as.list(curso$Hoja[-1]), function(evaluacion){ # Información por prueba
          # print(evaluacion)
          
          if(sum(grepl("Prueba", evaluacion), grepl("Examen", evaluacion)) == 1){ ## Notas de pruebas y examen
            return(notas_evaluacion(curso$Documentos[which(evaluacion == curso$Hoja)]))
          } else { ## Nota de controles
            # print(curso$Documentos[which(evaluacion == curso$Hoja)])
            return(notas_control(curso$Documentos[which(evaluacion == curso$Hoja)]))
          }
          
        })
        
        ### Obtenemos métricas generales por curso, considerando el orden en que son devueltos los datos (siempre el mismo para todos los profesores)
        ### Controles, Examen y el resto Pruebas en orden correlativo (Prueba 1, Prueba 2, ...)
        
        # ABORDAR EL HECHO DE TENER NOTAS INCOMPLETAS, USAR na.omit AL MOMENTO DE SACAR PROMEDIOS EN TODO EL CÓDIGO
        return(aux.promedios) # Promedio de de los promedios de las pruebas. También puede ser modificado por el promedio de general del
        # curso considerando examen y controles. La idea es sacar una métrica general por curso, de esta forma, si se desea ver el detalle, se consulta 
        # las pestañas por curso y sección.
      }
    })
    # aux.bbdd = aux.bbdd[!is.na(aux.bbdd)]
    return(aux.bbdd)
  }
  
 
}