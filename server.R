server = function(input, output, session){
  
  ######################### CONTENIDO #############################
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

  ######################## FUNCIONES ###########################
  
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
  
  tabla_de_notas_evaluacion = function(prueba, nombre){ # Diseña la tabla con los desglose de puntajes por pregunta en la prueba por cada estudiante
    estudiantes = prueba[[1]]$Estudiante[-c(1:2)] # Guardamos los nombres de los estudiantes
    prueba = as.data.frame(prueba)
    ideal = sum(as.numeric(unlist(prueba[2,-1]))) # Puntaje ideal de la prueba
    aux.ideal = as.numeric(unlist(prueba[2,-1])) # Puntos ideales por pregunta
    RE = as.matrix(prueba[1,-1]) # Guardamos los resultados de aprendizaje de la evaluación
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
        return(round(3/corte*puntos + 1, 1))
      } else {
        # (60%, 4) (ideal, 7)
        return(round(3/(ideal - corte)*(puntos - ideal) + 7, 1))
      }
    })
    
    logro.promedio = apply(prueba, 1, function(puntos){
      return(as.numeric(puntos)/aux.ideal)
    })
    
    logro.promedio = paste(format(round(rowSums(logro.promedio)/3*100,0), nsmall = 0), "%", sep = "")
    logro.promedio = c(logro.promedio, round(mean(notas.por.prueba), 1))
    
    prueba = as.data.frame(cbind(prueba, notas.por.prueba))
    prueba = as.data.frame(rbind(prueba, logro.promedio))
    row.names(prueba) = c(estudiantes, "Logro promedio")
    colnames(prueba) = c(paste0("P", 1:length(cols[-1])), "Nota")
    prueba = list(prueba, RE)
    names(prueba) = c(nombre, " RE")
    return(prueba)
  }
  
  tabla_de_notas_control = function(control, nombre){ # Diseña la tabla con los desglose de puntajes por pregunta en controles por cada estudiante
    estudiantes = control[[1]]$Estudiante # Guardamos los nombres de los estudiantes
    control = as.data.frame(control)
    control = control[,-1]
    notas.por.control = apply(control, 1, function(notas){ # Nota por estudiante en la prueba
      return(round(mean(as.numeric(notas)), 1)) # Revisar cuando haya notas faltantes
    })
    control = as.data.frame(cbind(control, notas.por.control))
    control = as.data.frame(rbind(control, round(colMeans(control), 1)))
    row.names(control) = c(estudiantes, "Promedio")
    colnames(control) = c(paste0("C", 1:(dim(control)[2]-1)), "NF")
    control = list(control)
    names(control) = nombre
    return(control)
  }
  
  ########################### MÉTRICAS ###################################
  
  metricas_generales = function(profesor){ # Función para determinar información en el primer panel
    aux = subset(listado.cursos, Profesor == profesor)
    aux.cursos_seccion = paste(aux$Curso, "- seccion", aux$Sección, sep = " ")
    aux.bbdd = lapply(docs, function(curso){ # listado de las métricas por profesor
      
      if (curso$Nombre %in% aux.cursos_seccion) { # Filtramos los cursos correspondientes al profesor
        
        aux.ponderacion = curso$Documentos[[1]] # Guardamos las ponderaciones
        aux.length = dim(curso$Documentos[[1]])[1]
        aux.ponderacion = aux.ponderacion[c((aux.length-1):aux.length, 1:(aux.length-2)),] # Control, Examen, Pruebas
        # print(aux.ponderacion)
        
        ### Obtenemos los promedios de las pruebas, controles y exámenes
        aux.promedios = lapply(as.list(curso$Hoja[-1]), function(evaluacion){ # Información por prueba
          ### Obtenemos métricas generales por curso, considerando el orden en que son devueltos los datos (siempre el mismo para todos los profesores)
          ### Controles, Examen y el resto Pruebas en orden correlativo (Prueba 1, Prueba 2, ...)
          if(sum(grepl("Prueba", evaluacion), grepl("Examen", evaluacion)) == 1){ ## Notas de pruebas y examen
            return(notas_evaluacion(curso$Documentos[which(evaluacion == curso$Hoja)]))
          } else { ## Nota de controles
            # print(curso$Documentos[which(evaluacion == curso$Hoja)])
            return(notas_control(curso$Documentos[which(evaluacion == curso$Hoja)]))
          }  
        })
        
        # Transformamos todas las notas en nota final en el curso
        aux.promedios = matrix(unlist(aux.promedios), byrow = T, nrow = length(aux.promedios[[1]])) # Pasamos de lista a matriz (cada estudiante una columna)
        # Esto nos deja con cada alumno en una fila, ahora multiplicamos por los ponderadores y obtenemos la nota final de cada uno
        aux.promedios = apply(aux.promedios, 1, function(notas.alumno){
          # print(notas.alumno)
          NP = sum(notas.alumno[-2]*aux.ponderacion[-2,2]) # Nota de presentación
          NF = notas.alumno[2]*aux.ponderacion[2,2] + NP*(1-aux.ponderacion[2,2])
          return(NF)
        })
        
        # Finalmente obtenemos la media general del curso
        aux.promedios = mean(aux.promedios)
        
        # ABORDAR EL HECHO DE TENER NOTAS INCOMPLETAS, USAR na.omit AL MOMENTO DE SACAR PROMEDIOS EN TODO EL CÓDIGO
        return(list("Curso" = curso$Nombre, "Pomedio" = aux.promedios)) # Promedio de de los promedios de las pruebas.
        # También puede ser modificado por el promedio de general del
        # curso considerando examen y controles. La idea es sacar una métrica general por curso, de esta forma, si se desea ver el detalle, se consulta 
        # las pestañas por curso y sección.
      }
    })
    
    aux.bbdd = aux.bbdd[!sapply(aux.bbdd, is.null)]
    aux.bbdd = as.data.frame(matrix(unlist(aux.bbdd), byrow = T, ncol = 2))
    colnames(aux.bbdd) = c("Curso", "Promedio")
    aux.bbdd$Promedio = round(as.numeric(aux.bbdd$Promedio),1)
    return(aux.bbdd)
  }
  
  informacion_cursos = function(profesor){ # Función para determinar información en el primer panel
    aux.nombres_cursos = c() # Lo guardamos para darle nombre a la lista final
    aux = subset(listado.cursos, Profesor == profesor)
    aux.cursos_seccion = paste(aux$Curso, "- seccion", aux$Sección, sep = " ")
    aux.bbdd = lapply(docs, function(curso){ # listado de las métricas por profesor
      
      if (curso$Nombre %in% aux.cursos_seccion) { # Filtramos los cursos correspondientes al profesor
        aux.nombres_cursos <<- c(aux.nombres_cursos, curso$Nombre)
        aux.nombre_alumnos = curso$Documentos[[3]]$Estudiante[-c(1:2)]
        aux.ponderacion = curso$Documentos[[1]] # Guardamos las ponderaciones
        aux.length = dim(curso$Documentos[[1]])[1]
        aux.ponderacion = aux.ponderacion[c((aux.length-1):aux.length, 1:(aux.length-2)),] # Control, Examen, Pruebas
        
        ### Obtenemos los promedios de las pruebas, controles y exámenes
        aux.promedios = lapply(as.list(curso$Hoja[-1]), function(evaluacion){ # Información por prueba
          ### Obtenemos métricas generales por curso, considerando el orden en que son devueltos los datos (siempre el mismo para todos los profesores)
          ### Controles, Examen y el resto Pruebas en orden correlativo (Prueba 1, Prueba 2, ...)
          if(sum(grepl("Prueba", evaluacion), grepl("Examen", evaluacion)) == 1){ ## Notas de pruebas y examen
            return(notas_evaluacion(curso$Documentos[which(evaluacion == curso$Hoja)]))
          } else { ## Nota de controles
            return(notas_control(curso$Documentos[which(evaluacion == curso$Hoja)]))
          }
        })
        
        # Construcción de la tabla
        aux.promedios = matrix(unlist(aux.promedios), byrow = F, ncol = length(aux.promedios))
        aux.promedios = as.data.frame(aux.promedios)
        colnames(aux.promedios) = curso$Hoja[-1]
        
        # Agregamos nota de presentación y final
        aux.promedios$NP = apply(aux.promedios, 1, function(notas.alumno){
          NP = sum(notas.alumno[-2]*aux.ponderacion[-2,2]) # Nota de presentación
          return(NP)
        })
        
        aux.promedios$NF = apply(aux.promedios[-c(dim(aux.promedios)[2])], 1, function(notas.alumno){
          NP = sum(notas.alumno[-2]*aux.ponderacion[-2,2]) # Nota de presentación
          NF = notas.alumno[2]*aux.ponderacion[2,2] + NP*(1-aux.ponderacion[2,2]) # Nota final
          return(NF)
        })
        
        aux.promedios= aux.promedios[c(which(grepl("Prueba", colnames(aux.promedios))),
                                       which(colnames(aux.promedios) == "Controles"),
                                       which(colnames(aux.promedios) == "NP"),
                                       which(colnames(aux.promedios) == "Examen"),
                                       which(colnames(aux.promedios) == "NF"))]
        aux.promedios = round(aux.promedios, 1)
        aux.promedios = rbind(aux.promedios, round(colMeans(aux.promedios), 1))
        rownames(aux.promedios) = c(aux.nombre_alumnos, "Promedio")
        return(aux.promedios)
      }
    })
    
    aux.bbdd = aux.bbdd[!sapply(aux.bbdd, is.null)]
    names(aux.bbdd) = aux.nombres_cursos
    return(aux.bbdd)
  }
  
  detalles_evaluaciones = function(profesor){ # Determina el detalles de los estudiantes en las evaluaciones
    aux.nombres_cursos = c() # Lo guardamos para darle nombre a la lista final
    aux = subset(listado.cursos, Profesor == profesor)
    aux.cursos_seccion = paste(aux$Curso, "- seccion", aux$Sección, sep = " ")
    aux.bbdd = lapply(docs, function(curso){ # listado de las métricas por profesor
      
      if (curso$Nombre %in% aux.cursos_seccion) { # Filtramos los cursos correspondientes al profesor
        aux.nombres_cursos <<- c(aux.nombres_cursos, curso$Nombre)
        aux.nombre_alumnos = curso$Documentos[[3]]$Estudiante[-c(1:2)]
        aux.ponderacion = curso$Documentos[[1]] # Guardamos las ponderaciones
        aux.length = dim(curso$Documentos[[1]])[1]
        aux.ponderacion = aux.ponderacion[c((aux.length-1):aux.length, 1:(aux.length-2)),] # Control, Examen, Pruebas
        
        ### Obtenemos los promedios de las pruebas, controles y exámenes
        aux.promedios = lapply(as.list(curso$Hoja[-1]), function(evaluacion){ # Información por prueba
          ### Reportamos toda la información de la prueba
          ### Notas y resultados de aprendizaje por pregunta
          if(sum(grepl("Prueba", evaluacion), grepl("Examen", evaluacion)) == 1){ ## Notas de pruebas y examen
            tabla = tabla_de_notas_evaluacion(curso$Documentos[which(evaluacion == curso$Hoja)],
                                              evaluacion) # Le damos el nombre para poder identificarlo mejor
            names(tabla)[1] = evaluacion
            return(tabla)
          } else { ## Nota de controles
            tabla = tabla_de_notas_control(curso$Documentos[which(evaluacion == curso$Hoja)],
                                           evaluacion) # Le damos el nombre para poder indentificarlo mejor
            names(tabla)[1] = evaluacion
            return(tabla)
          }
        })
        
        return(aux.promedios)
      }
    })

    aux.bbdd = aux.bbdd[!sapply(aux.bbdd, is.null)]
    names(aux.bbdd) = aux.nombres_cursos
    return(aux.bbdd)
  }
  
  metricas_generales.p1 = metricas_generales("Profesor 1")
  metricas_generales.p2 = metricas_generales("Profesor 2")
  
  informacion_cursos.p1 = informacion_cursos("Profesor 1")
  informacion_cursos.p2 = informacion_cursos("Profesor 2")
  
  ###################### ELEMENTOS DE LOS PANELES ############################
  
  # Métricas por profesor: resumen general
  
  output$valueboxes_p1 = renderUI({
    lapply(1:dim(metricas_generales.p1)[1], function(i){
      valueBox(metricas_generales.p1[i,2],
               format(metricas_generales.p1[i,1], nsmall = 1),
               width = 2, color = "blue")
    })
  })
  
  output$valueboxes_p2 = renderUI({
    lapply(1:dim(metricas_generales.p2)[1], function(i){
      valueBox(metricas_generales.p2[i,2],
               format(metricas_generales.p1[i,1], nsmall = 1),
               width = 2, color = "blue")
    })
  })
  
  # Métricas por curso
  
  output$resultados_cursos = renderPlot({
    aux = rbind(metricas_generales.p1, metricas_generales.p2)
    aux$Curso = matrix(unlist(strsplit(aux$Curso, " - ")), byrow = T, ncol = 2)[,1]
    aux = apply(as.matrix(unique(aux$Curso)), 1, function(solo_curso){
      solo_curso = c(solo_curso, as.character(mean(aux$Promedio[aux$Curso == solo_curso])))
      return(solo_curso)
    })
    aux = as.data.frame(t(aux))
    colnames(aux) = c("Curso", "Promedio")
    
    g = ggplot(data = aux, aes(x = Curso, y = Promedio)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Promedio por curso") +
      geom_text(mapping = aes(label = Promedio), vjust = -0.4, position = "stack")
    g    
      
    # Añadir tasa de aprobación media por curso o tasa de aprobación general por curso
  })
  
  output$p1_c1_s1 = DT::renderDataTable({
    DT::datatable(informacion_cursos.p1[[1]], filter = "top")
  })
  
  output$p1_c1_s2 = DT::renderDataTable({
    DT::datatable(informacion_cursos.p1[[2]], filter = "top")
  })
  
  output$p2_c1_s3 = DT::renderDataTable({
    DT::datatable(informacion_cursos.p2[[1]], filter = "top")
  })
  
  output$p2_c2_s1 = DT::renderDataTable({
    DT::datatable(informacion_cursos.p2[[2]], filter = "top")
  })
  
  # Automatizamos las pestañas de los detalles de curso, sin necesidad de un renderUI
  # Creamos un proceso que inserte los paneles según el profesor y el curso
  
  lapply(as.list(unique(listado.cursos$Profesor)), function(nombre.profesor){
    # nombre.profesor = "Profesor 1"
    aux = detalles_evaluaciones(nombre.profesor)
    cursos = names(aux)
    lapply(as.list(cursos), function(curso){
      # curso = cursos[1]
      lapply(as.list(1:length(aux[[curso]])), function(i){
        # i = 1
        aux2 = aux[[curso]]
        aux.pestana = gsub(" ", "", curso)
        nombre = ifelse(i == 1, names(aux2[[i]]), names(aux2[[i]])[1])
        tasa.aprobacion = unlist(aux2[[i]][[1]][dim(aux2[[i]][[1]])[2]])
        tasa.aprobacion = tasa.aprobacion[-length(tasa.aprobacion)]
        tasa.aprobacion = paste0(format(round(sum(tasa.aprobacion>=4)/length(tasa.aprobacion)*100,1), nsmall = 1), "%")
        insertTab(inputId = aux.pestana,
                  # El primer elemento siempre son los controles
                  tabPanel(title = nombre,
                           fluidRow(column(width = 6, DT::renderDataTable(aux2[[i]][[1]])),
                                    column(width = 6,
                                           valueBox(tasa.aprobacion,
                                                    "Tasa de aprobación",
                                                    width = 4, color = "blue"))
                                    ))
        )
      })
    })
  })
  
  

}