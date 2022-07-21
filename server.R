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
  
  # urls = list("https://docs.google.com/spreadsheets/d/1OBmwY66WNfIv63GMsBF-gDf9xob0q1lXATQzZsQzwMw/edit#gid=1752874311",
  #             "https://docs.google.com/spreadsheets/d/1JLqReXrK7QtoamKSyWhfcSsUh4x7RdCXHm3kuVGJ2zM/edit#gid=1752874311",
  #             "https://docs.google.com/spreadsheets/d/1K8Z_802i5DrLc9CbgmWW6AdBjJTWUVnhhtbqU-V5upI/edit#gid=1752874311",
  #             "https://docs.google.com/spreadsheets/d/1cbQgCwhI0ZoFQ1gAQqzYhiLvHH53gu2bxgOWjbXx-P8/edit#gid=1752874311")
  # 
  # docs = lapply(urls, function(url){
  #   doc = gs4_get(url)
  #   nombre = doc$name # nombre del archivo
  #   hojas = doc$sheets$name # nombre de las hojas
  #   doc = lapply(as.list(hojas), function(hoja){
  #     # Las ponderaciones y controles siempre estarán en las primeras 2 columnas, el resto corresponde a pruebas
  #     return(as.data.frame(read_sheet(url, sheet = hoja)))
  #   })
  #   return(list("Nombre" = nombre, "Hoja" = hojas, "Documentos" = doc))
  # }) 


 
}