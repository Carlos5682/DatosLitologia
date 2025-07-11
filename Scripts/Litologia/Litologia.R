rm(list=ls())
install.packages("ggrepel")
install.packages("ggspatial")
install.packages("mapSpain")
install.packages("sf")
install.packages("dplyr")
install.packages("ggspatial")
install.packages("ggplot2")
install.packages("future.apply")
library(ggrepel)
library(ggspatial)
library(mapSpain)
library(sf)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(future.apply)
setwd("C://Users//samue//Desktop//ProyectoTFG/DatosLitologia/")


#---------------------------------------------1 Recortar Comunidades autonomas--------------------------------------------

##---------------------------------------1.1 Procesar capas-----------------------------------

#El zip descargado de: https://datos.gob.es/es/catalogo/ea0010987-mapa-de-litologias-de-espana-1-1-000-0002 contiene varios shape file, 
#Canarias y Peninsula tienen diferentes capas, hay que hacer todo el proceso 2 veces 
#Comenzamos con la primera capa para Peninsula y Baleares

###----------------------------1.1.1 Peninsula y Baleares------------------------------------------

####---------------------1.1.1.1 Leer capa--------------------------

litpenybal <- st_read("Datos/Datosdescargados/geologico_1000_shapes/geopb_1000.shp") 

####--------------------1.1.1.2 Corregir geometrias----------------
litpenybal <- st_cast(litpenybal, "MULTIPOLYGON")
litpenybal <- st_make_valid(litpenybal)

litpenybal$LITOLOGIA[is.na(litpenybal$LITOLOGIA)] <- "Sin datos"
####----------------1.1.1.3 Disolver geometrias----------------

litpybdisueltos <- litpenybal %>%
  group_by(LITOLOGIA) %>%
  summarise(geom = st_union(geometry), .groups = "drop") 

####---------------1.1.1.4 Reproyectar geometrías--------------

litpenreproy <-  st_transform(litpybdisueltos, 3035)

####------------------1.1.1.5 Personalizar leyenda------------------
leyenda_litologia <- data.frame(
  litologia = c(
    "Otros granitoides",
    "Granitoides de dos micas",
    "Serpentinitas y peridotitas. Rocas básicas y ultrabásicas",
    "Micaesquistos, filitas, areniscas, mármoles, calizas, dolomías y margas",
    "Cuarcitas, pizarras, areniscas y calizas",
    "Areniscas, pizarras y calizas",
    "Gneisses",
    "Pizarras, grauwackas, cuarcitas y conglomerados",
    "Conglomerados, areniscas, arcillas y calizas. Evaporitas",
    "Dolomías, calizas y margas. Areniscas",
    "Gravas, conglomerados, arenas y limos",
    "Areniscas, conglomerados, arcillas; calizas y evaporitas",
    "Conglomerados, areniscas, calizas, yesos y arcillas versicolores",
    "Calizas, dolomías y margas. Areniscas y conglomerados",
    "Conglomerados, areniscas, pizarras y calizas. Carbón",
    "Calizas detríticas, calcarenitas, margas, arcillas y calizas",
    "Migmatitas, mármoles y granitoides indiferenciados",
    "Conglomerados, areniscas y lutitas",
    "Sin datos",
    "Vulcanitas y rocas volcanoclásticas"
  ),
  color = c(
    "#c0504d",  # rojo granítico
    "#e26b0a",  # naranja rojizo (dos micas)
    "#6e6e6e",  # gris oscuro (ultrabásicas)
    "#9b59b6",  # lila (metamórficas)
    "#c9b037",  # dorado (sedimentarias duras)
    "#d4a017",  # ocre
    "#999999",  # gris medio (gneises)
    "#a18f60",  # marrón oliváceo
    "#f4cccc",  # rosa claro (evaporitas)
    "#f9cb9c",  # beige (carbonatadas)
    "#deb887",  # marrón claro (sedimentos sueltos)
    "#f6b26b",  # naranja claro
    "#ffe599",  # amarillo pálido (evaporitas/yesos)
    "#f2f2f2",  # blanco grisáceo (carbonatos)
    "#000000",  # negro (carbón)
    "#b6d7a8",  # verde claro (detríticas)
    "#8e7cc3",  # violeta (migmatitas)
    "#c27ba0",  # rosa oscuro (lutitas)
    "white",    
    "#9900cc"   # púrpura (volcánicas)
  ),
  stringsAsFactors = FALSE
)


litologiafinal <- left_join(litpenreproy, leyenda_litologia, by = c("LITOLOGIA" = "litologia"))

####------------------1.1.1.6 Guardar capa final-------------------

st_write(litologiafinal, 
         dsn = "Datos/Datoscorregidos/Litologiacorregido/litologiacorregido.gpkg", 
         driver = "GPKG", 
         delete_layer = TRUE)
###----------------------------1.1.2 Canarias------------------------------------------

####---------------------1.1.2.1 Leer capa--------------------------

litcan <- st_read("Datos/Datosdescargados/geologico_1000_shapes/geocan_1000.shp") 

####--------------------1.1.2.2 Corregir geometrias----------------

litcan <- st_cast(litcan, "MULTIPOLYGON")
litcan <- st_make_valid(litcan)
litcan$LITOLOGIA[is.na(litcan$LITOLOGIA)] <- "Sin datos"

####----------------1.1.2.3 Disolver geometrias----------------

litcandisuelto <- litcan %>%
  group_by(LITOLOGIA) %>%
  summarise(geom = st_union(geometry), .groups = "drop") 

####---------------1.1.2.4 Reproyectar geometrías--------------

litcanreproy <-  st_transform(litcandisuelto, 3035)

####------------------1.1.2.5 Personalizar leyenda------------------
leyenda_litologia_can <- data.frame(
  litologia = c(
    "Rocas volcánicas máficas",
    "Rocas volcánicas félsicas",
    "Rocas volcánicas máficas y félsicas",
    "Rocas plutónicas máficas y félsicas",
    "Rocas plutónicas félsicas",
    "Gravas, conglomerados, arenas y limos",
    "Sin datos"
  ),
  color = c(
    "#4D4D4D",  # gris oscuro
    "#FFD1DC",  # rosa claro
    "#A67C52",  # marrón medio
    "#9966CC",  # púrpura suave
    "#FFB6C1",  # rosa pastel
    "#D2B48C",  # marrón claro (arena)
    "white"   # gris claro para 'Sin datos'
  ),
  stringsAsFactors = FALSE
)
litcanfinal <- left_join(litcanreproy, leyenda_litologia_can, by = c("LITOLOGIA" = "litologia"))

####------------------1.1.2.6 Guardar capa final-------------------

st_write(litcanfinal, 
         dsn = "Datos/Datoscorregidos/Litologiacorregido/litologiacancorregido.gpkg", 
         driver = "GPKG", 
         delete_layer = TRUE)


##---------------------------------------1.2 Bucle para las comunidades autonomas------------------------------------
###-------------------------1.2.1 Peninsula y Baleares---------------------------------
litologiafinal <- st_read("Datos/Datoscorregidos/Litologiacorregido/litologiacorregido.gpkg")

####-------------1.2.1.1 Obtener comunidades autónomas---------
ccaa_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)
ccaa_sf <- st_make_valid(ccaa_sf)

for (i in 1:nrow(ccaa_sf)) {
  nombre_ccaa <- ccaa_sf$ccaa.shortname.es[i]
  codigo_ccaa <- ccaa_sf$codauto[i]
  
  cat("Procesando:", nombre_ccaa, "\n")
  
  ccaa_geom <- ccaa_sf[i, ]
  
  ####----------1.2.1.2 Calcular y expandir bbox---------------
  bbox <- st_bbox(ccaa_geom)
  
  x_diff <- bbox$xmax - bbox$xmin
  y_diff <- bbox$ymax - bbox$ymin
  max_diff <- max(x_diff, y_diff)
  
  x_buffer <- max_diff * 0.15
  y_buffer <- max_diff * 0.15
  
  bbox_expandida <- structure(
    c(
      xmin = bbox$xmin - x_buffer,
      ymin = bbox$ymin - y_buffer,
      xmax = bbox$xmax + x_buffer,
      ymax = bbox$ymax + y_buffer
    ),
    class = "bbox",
    crs = st_crs(ccaa_geom)
  )
  
  ####----------1.2.1.3 Convertir bbox a polígono-------------
  bbox_poly <- st_as_sfc(st_bbox(bbox_expandida, crs = st_crs(ccaa_geom)))
  
  ####----------1.2.1.4 Intersección con polígono--------------
  lit_crop <- st_intersection(litologiafinal, bbox_poly)
  
  ####------------1.2.1.5 Guardar como GeoPackage---------------
  st_write(lit_crop, paste0("Datos/DatosporComunidad/Litologia/", nombre_ccaa, ".gpkg"), delete_dsn = TRUE)
}

###-----------------------------1.2.2 Canarias---------------------------------

litcanfinal <- st_read("Datos/Datoscorregidos/Litologiacorregido/litologiacancorregido.gpkg")

####-------------1.2.2.1 Obtener comunidades autónomas---------

ccaa_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)
ccaa_sf <- st_make_valid(ccaa_sf)

####---------------1.2.2.2 Filtrar Canarias------------------------

canarias_geom <- ccaa_sf[ccaa_sf$ccaa.shortname.es == "Canarias", ]

####----------1.2.2.3 Calcular y expandir bbox---------------

bbox <- st_bbox(canarias_geom)

x_diff <- bbox$xmax - bbox$xmin
y_diff <- bbox$ymax - bbox$ymin
max_diff <- max(x_diff, y_diff)

x_buffer <- max_diff * 0.15
y_buffer <- max_diff * 0.15

bbox_expandida <- structure(
  c(
    xmin = bbox$xmin - x_buffer,
    ymin = bbox$ymin - y_buffer,
    xmax = bbox$xmax + x_buffer,
    ymax = bbox$ymax + y_buffer
  ),
  class = "bbox",
  crs = st_crs(canarias_geom)
)

####----------1.2.2.4 Convertir bbox a polígono-------------
bbox_poly <- st_as_sfc(st_bbox(bbox_expandida, crs = st_crs(canarias_geom)))

####----------1.2.2.5 Intersección con polígono--------------
litcan_crop <- st_intersection(litcanfinal, bbox_poly)

####------------1.2.2.6 Guardar como GeoPackage---------------
st_write(litcan_crop, "Datos/DatosporComunidad/Litologia/Canarias.gpkg", delete_dsn = TRUE)

#---------------------------------------2 Recortar municipios-----------------------------------
##-----------------------------2.1 Configurar paralelización--------------------
###-------------------2.1.1 Maximo de memoria----------------
options(future.globals.maxSize = 2 * 1024^3)  

###-------------------2.1.2 Numero de nucleos-------------------
parallel::detectCores()
future::availableCores()

plan(multisession, workers = 12)

##-----------------------------2.2 Obtener municipios-----------------------
municipios <- esp_get_munic(moveCAN = FALSE, epsg = 3035)
CCAA_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)

##----------------------------2.3 Comunidades a paralelizar------------------
comunidades <- unique(CCAA_sf$ccaa.shortname.es)

##-----------------------------2.4 Función a paralelizar--------------------------

procesar_comunidad <- function(comunidad_objetivo) {
  cat("▶ Procesando comunidad:", comunidad_objetivo, "\n")
  
  ###------------------------2.4.1 Codigo de comunidad------------------------
  cod_comunidad <- CCAA_sf %>%
    filter(ccaa.shortname.es == comunidad_objetivo) %>%
    pull(codauto)
  
  ###------------------------2.4.2 Muncipios de comunidad---------------------
  municipios_comunidad <- municipios %>%
    filter(codauto == cod_comunidad) %>%
    st_make_valid()
  
  ###-----------------------2.4.3 Ruta de litologia por comunidad---------------
  
  lit_path <- paste0("Datos/DatosporComunidad/Litologia/", comunidad_objetivo, ".gpkg")
  dir_out <- paste0("Capasfinales/Litologia/", comunidad_objetivo)
  
  if (!file.exists(lit_path)) {
    return(paste0("⚠ Archivo litologia no encontrado para ", comunidad_objetivo))
  }
  
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  
  ###-----------------------2.4.4 Leer litologias---------------------------------
  lit_ccaa <- st_read(lit_path, quiet = TRUE)
  
  resultados <- vector("list", nrow(municipios_comunidad))
  ###-------------------------2.4.5 Bucle--------------------------------------
  for (i in seq_len(nrow(municipios_comunidad))) {
    muni <- municipios_comunidad[i, ]
    nombre_muni <- gsub(" ", "_", gsub("/", "o", muni$name))
    output_path <- file.path(dir_out, paste0(nombre_muni, ".geojson"))
    
    ####--------------------2.4.5.1 Mantener municipios ya obtenidos------------    
    if (file.exists(output_path)) {
      resultados[[i]] <- paste0("✔ Ya existe: ", nombre_muni)
      next
    }
    
    tryCatch({
      
      ###------------------2.4.5.2 Calcular y expandir bbox-----------------
      bbox <- st_bbox(muni)
      x_diff <- bbox$xmax - bbox$xmin
      y_diff <- bbox$ymax - bbox$ymin
      max_diff <- max(x_diff, y_diff)
      
      x_buffer <- max_diff * 0.15
      y_buffer <- max_diff * 0.15
      
      bbox_expandido <- structure(
        c(
          xmin = bbox$xmin - x_buffer,
          ymin = bbox$ymin - y_buffer,
          xmax = bbox$xmax + x_buffer,
          ymax = bbox$ymax + y_buffer
        ),
        class = "bbox",
        crs = st_crs(muni)
      )
      
      ###------------------2.4.5.3 Convertir bbox a polígono--------------
      bbox_poly <- st_as_sfc(st_bbox(bbox_expandido, crs = st_crs(muni)))
      
      ###------------------2.4.5.4 Intersección con poligono--------------
      recorte <- st_intersection(lit_ccaa, bbox_poly)
      
      ###------------------2.4.5.5 Detector de error---------------------
      
      if (nrow(recorte) > 0) {
        st_write(recorte, output_path, delete_layer = TRUE, quiet = TRUE)
        resultados[[i]] <- paste0("💾 Guardado: ", nombre_muni)
      } else {
        st_write(muni[0, ], output_path, delete_layer = TRUE, quiet = TRUE)  # Archivo vacío
        resultados[[i]] <- paste0("⚠ Vacío (guardado): ", nombre_muni)
      }
    }, error = function(e) {
      resultados[[i]] <- paste0("❌ Error en ", nombre_muni, ": ", e$message)
    })
  }
  
  return(resultados)
}

##----------------------------2.5 Ejecutar en paralelo-------------------
resultados_totales <- future_lapply(comunidades, procesar_comunidad)
##----------------------------2.6 Resumen final--------------------------
cat("\n✅ Procesamiento completado.\n")
