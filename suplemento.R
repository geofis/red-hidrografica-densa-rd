## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE, 
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  out.width = '100%',
  # res = 300,
  dpi = 300
  # fig.pos = "H", out.extra = "" #Figuras en el lugar insertadas
  )
# options(digits = 3)


## ----suphidropaquetes---------------------------------------------------------
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("distance", "raster")
conflicted::conflict_prefer("alpha", "ggplot2")
conflicted::conflict_prefer("rescale", "scales")
library(psych)
library(raster)
library(sf)
library(kableExtra)
library(tidyverse)
library(gdalUtilities)
library(e1071)
library(scales)
library(tmap)
source('R/funciones.R')
dem_proc_dir <- 'estadisticos'
figuras <- 'figuras'
umbral_espurias <- 4000 #Umbral por debajo del cual se considera una cuenca como espuria


## ----cargarfuentesotrormd, echo=F, include=F----------------------------------
res_h3 <- 7 #Escribir un valor entre 4 y 7, ambos extremos inclusive
ruta_ez_gh <- 'https://raw.githubusercontent.com/geofis/zonal-statistics/'
ez_ver <- 'd7f79365168e688f0d78f521e53fbf2da19244ef/'
if(!any(grepl('^pais_url$', ls()))){
  pais_url <- paste0(ruta_ez_gh, ez_ver, 'inst/extdata/dr.gpkg')
  pais <- invisible(st_read(pais_url, optional = T, layer = 'pais', quiet = T))
  st_geometry(pais) <- "geometry"
  pais <- st_transform(pais, 32619)
}


## python download-all-2023-04-20_00-30-00.py


## ----crearindice--------------------------------------------------------------
ind_orig <- invisible(
  st_read('alos-palsar-dem-rd/asf-datapool-results-2023-04-19_08-31-26.geojson',
          quiet = T)) %>% 
   rownames_to_column('fila') %>% mutate(fila = as.integer(fila))
distancias <- ind_orig %>% st_centroid() %>% st_distance() %>% units::drop_units()
distancias[upper.tri(distancias, diag = T)] <- NA
indices <- which(distancias < 1000, arr.ind = TRUE)
duplicados <- as.data.frame(indices) %>% 
  mutate(dup_id = 1:nrow(indices)) %>% 
  pivot_longer(-dup_id, names_to = 'tipo', values_to = 'fila') %>% 
  select(-tipo)
seleccionados <- duplicados %>%
  inner_join(ind_orig %>% select(fila, startTime) %>% st_drop_geometry) %>% 
  group_by(dup_id) %>% filter(startTime == max(startTime)) %>% pull(fila)
ind_orig_sel <- ind_orig %>%
  filter(!fila %in% duplicados$fila | fila %in% seleccionados) %>% 
  filter(centerLon < -72.1821)


## ----tablaindice--------------------------------------------------------------
ind_orig_sel %>% select(sceneName, startTime) %>% st_drop_geometry() %>%
  estilo_kable(titulo = paste('Escenas ALOS-PALSAR usadas para generar un DEM de 12.5 m de
                        resolución espacial de República Dominicana'))


## -----------------------------------------------------------------------------
ind_orig_sel_m <- ind_orig_sel %>%
  ggplot +
  geom_sf(alpha = 0.6, fill = 'grey90', color = 'grey20', size = 0.5) +
  geom_sf(data = pais, fill = 'transparent', color = 'black') +
  geom_sf_label(aes(label = sceneName), color = 'red', size = 1.5,
                label.padding = unit(0.1, "lines"), alpha = 0.9) +
  theme_bw() + 
  theme(plot.title = element_text(size = 11)) +
  ggspatial::annotation_scale(style = 'ticks')


## ---- eval=F------------------------------------------------------------------
## zip_path <- 'alos-palsar-dem-rd/'
## sapply(ind_orig_sel$fileName,
##        function(x)
##          unzip(
##            zipfile = paste0(zip_path, x),
##            exdir = paste0(zip_path, 'dem'), junkpaths = T,
##            files = paste0(gsub('.zip', '', x), '/', gsub('zip', 'dem.tif', x)))
##        )


## ---- eval=F------------------------------------------------------------------
## dems_orig_path <- list.files(path = 'alos-palsar-dem-rd/dem',
##                              pattern = '*dem.tif', full.names = T)
## crs_18n <- names(which(sapply(dems_orig_path, function(x){
##   crs_x <- gdal_crs(x)
##   is_z18 <- grepl('zone 18N', crs_x[['wkt']])
## })))
## sapply(crs_18n, function(x) file.rename(from = x, to = gsub('.tif', '_z18n.tif', x)))
## crs_18n_ren <- list.files(path = 'alos-palsar-dem-rd/dem',
##                           pattern = 'z18n.tif', full.names = T)
## sapply(crs_18n_ren, function(x){
##   gdalwarp(
##   srcfile = x,
##   dstfile = gsub('_z18n.tif', '.tif', x),
##   t_srs = 'EPSG:32619', overwrite = T)})


## ---- eval=F------------------------------------------------------------------
## gdalbuildvrt(gdalfile = dems_orig_path,
##              output.vrt = paste0(paste0(zip_path, 'dem'), '/dem_seamless.vrt'),
##              resolution = 'highest', r = 'average')


## # Usando Bash, desde la ruta ./alos-palsar-dem-rd/dem

## grass --text -c dem_seamless.vrt ./grassdata

## # Para abrir luego de cerrada: grass grassdata/PERMANENT/


## # Importar máscara

## v.import input=mascara-1km.gpkg output=mascara_1km

## 

## # Fijar máscara

## r.mask -r

## r.mask vector=mascara_1km

## 

## # Ver ambiente

## g.gisenv

## ## GISDBASE=/media/jose/datos/alos-palsar-dem-rd/dem

## ## LOCATION_NAME=grassdata

## ## MAPSET=PERMANENT

## ## GUI=text

## ## PID=1632142


## # Importar DEM a región de GRASS

## time r.import --overwrite input=dem_seamless.vrt output=dem

## ## real

## 

## # Ver en lista (q para salir)

## g.list type=raster


## ----demsinprocesar, echo=F, fig.cap='DEM sin procesar, representado como relieve sombreado. Nótesense los píxeles sin datos, destacados en color rojo (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics(paste(figuras, "dem-sin-procesar.jpg", sep = '/'))


## # Rellenar vacíos

## time r.fillnulls --overwrite --verbose \

##   input=dem method="bilinear" \

##   tension=40 smooth=0.1 edge=3 npmin=600 segmax=300 lambda=0.01 \

##   output=dem_relleno

## # Enviar mensaje al finalizar (ejecutar conjuntamente con anterior)

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real 10m11.925s


## ----demrelleno, echo=F, fig.cap='DEM sin procesar, representado como relieve sombreado. Los píxeles sin datos fueron eliminados (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics(paste(figuras, "dem-relleno.jpg", sep = '/'))


## # Exportar a GTiff con compresión LZW

## time r.out.gdal --overwrite --verbose createopt="COMPRESS=LZW,BIGTIFF=YES" \

##   input=dem_relleno \

##   format=GTiff type=Float64 output=dem_relleno.tif

## # Enviar mensaje al finalizar (ejecutar conjuntamente con anterior)

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	0m58.924s

## 

## # Comenzó a 23.20 de 22 de abril

## time ~/WhiteboxTools_linux_amd64/WBT/whitebox_tools \

##   --wd='/media/jose/datos/alos-palsar-dem-rd/dem/' \

##   --filter=25 --norm_diff=45 --num_iter=5 \

##   --run=FeaturePreservingSmoothing --input='dem_relleno.tif' \

##   --output='dem_relleno_suavizado.tif' -v

## # Enviar mensaje al finalizar (ejecutar conjuntamente con anterior)

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	9min46.103s


## ----demsuavizado, echo=F, fig.cap='DEM suavizado, representado como relieve sombreado. Nótese la conservación de las morfologías principales y la eliminación del ruido sobre éstas (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics(paste(figuras, "dem-suavizado.jpg", sep = '/'))


## time r.import input=dem_relleno_suavizado.tif output=dem_suavizado

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	0m21.593s


## # Importar DEM a región de GRASS

## r.import --overwrite input=egm2008-1_espanola.tif output=egm2008_1min

## 

## # Ver en lista (q para salir)

## g.list type=raster

## 

## # Ver atributos de la región

## g.region -p

## 

## # Alternativa 1. Usando r.resamp.rst. Más eficiente y precisa

## # Fijar la región al geoide importado

## g.region raster=egm2008_1min -ap

## # Realizar la interpolación

## r.resamp.rst --overwrite input=egm2008_1min ew_res=50 ns_res=50 elevation=egm2008_hires

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	

## # Fijar región a nuevo geoide

## g.region raster=egm2008_hires -ap

## 

## # Alternativa 2. Usando r.resamp.interp. También eficiente, pero eliminar áreas de borde

## # g.region res=50 -ap

## # r.resamp.interp --overwrite input=egm2008_1min \

## #  output=egm2008_hires method=bilinear

## 

## # Exportar para explorar visualmente

## # r.out.gdal --overwrite --verbose createopt="COMPRESS=LZW" \

## #  input=egm2008_hires \

## #  format=GTiff type=Float64 output=egm2008_hires.tif

## 

## # Volver a resolución de DEM rellenado y suavizado

## g.region raster=dem_suavizado -ap

## 

## # Aplicar álgebra de mapas

## r.mapcalc --overwrite "dem_pseudo_ortometrico = dem_suavizado - egm2008_hires"

## 

## #Estadísticos univariados

## r.univar dem_pseudo_ortometrico

## # n: 306462417

## # minimum: -51.4456

## # maximum: 3102.34

## # range: 3153.79

## # mean: 403.703

## # mean of absolute values: 403.858

## # standard deviation: 487.27

## # variance: 237432

## # variation coefficient: 120.7 %

## # sum: 123719658638.311


## ----alturasgeoideelipsoide, echo=F, fig.cap='Alturas respecto de geoide EGM08 ($\\sim$ortométrica) y sobre elipsoide WGS84, de un transecto descendente desde Bahoruco Oriental al Mar Caribe (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics(paste(figuras, "perfiles-dem/los-patos.png", sep = '/'))


## ----redcursoslargos, echo=F, fig.cap='Mapa de la red de cursos largos creada para el estudio a partir de varias fuentes (más detalles, en el texto).'----
knitr::include_graphics(paste(figuras, "red-cursos-largos.jpg", sep = '/'))


## # Importar red a GRASS

## v.import --overwrite input=red_mtn50k_cleaned_largos.gpkg \

##   output=red_mtn50k_cleaned_largos

## # Ver mapa importado en lista (q para salir)

## g.list type=vector

## # Calcular y pasar a archivo, la longitud de cursos

## # y número de segmentos (ejecutar en casos de actualización)

## v.to.db -p option=length map=red_mtn50k_cleaned_largos > \

##   stats_length_red_mtn50k_cleaned_largos.txt


## ---- message=F, warning=F----------------------------------------------------
stats_red_mtn50k_largos <- read_delim(
  paste0(dem_proc_dir, '/',
         'stats_length_red_mtn50k_cleaned_largos.txt'),
  progress = F, show_col_types = F)
n_seg_red_mtn50k_largos <- stats_red_mtn50k_largos %>%
  filter(!cat==-1) %>% nrow
length_mtn50k_largos <- stats_red_mtn50k_largos %>%
  filter(!cat==-1) %>% pull(length) %>% sum/1000


## # Limpiar red manualmente en QGIS

## # Para mejorar la topología, se puede aplicar v.clean directamente en QGIS

## 

## # Tallar red de cursos largos

## time r.carve --overwrite --verbose raster=dem_pseudo_ortometrico \

##   vector=red_mtn50k_cleaned_largos output=dem_tallado depth=100

## echo "r.carve finalizado" | mail -s "r.carve finalizado" USUARIO@MAIL

## ## real	97m3.970s


## ---- eval=F------------------------------------------------------------------
## # Limpiar red manualmente en QGIS
## # Para mejorar la topología, se puede aplicar v.clean directamente en QGIS
## 
## # Tallar
## # Rasterizar red (los píxeles de la red valdrán 1, el resto, nulo)
## v.to.rast --overwrite input=red_mtn50k_cleaned_largos type=line use=val \
##   output=red_mtn50k_cleaned_largos
## # Convertir nulos a cero
## r.null map=red_mtn50k_cleaned_largos null=0
## # Determinar estadísticas univariantes del DEM
## r.univar map=dem_pseudo_ortometrico
## # minimum: -51.4456
## # maximum: 3102.34
## 
## # Aplicar normalización y resta
## r.mapcalc --overwrite << EOF
## eval(stddem = (dem_pseudo_ortometrico - -51.4456) / (3102.34 - -51.4456), \
##      stddemburn = stddem - red_mtn50k_cleaned_largos)
## dem_tallado = (stddemburn * (3102.34 - -51.4456)) - 51.4456
## # dem_tallado = stddemburn * dem_pseudo_ortometrico # Alternativa
## EOF
## echo "Tallado finalizado" | mail -s "Mensaje sobre tallado" USUARIO@MAIL


## ----demtallado, echo=F, fig.cap='DEM sin aplicación de hidrografía (A), y con aplicación de hidrografía seleccionada (B). El DEM se representa como relieve sombreado y la aplicación se denota como un grabado oscurecido (cañón del río Payabo, Los Haitises, y río Yuna (proximidades de Arenoso, nordeste de República Dominicana)'----
knitr::include_graphics(paste(figuras, "dem-sin-tallar-tallado.png", sep = '/'))


## # Exportar dem_pseudo_ortometrico a GTiff con compresión LZW

## time r.out.gdal --overwrite --verbose createopt="COMPRESS=LZW,BIGTIFF=YES" \

##  input=dem_pseudo_ortometrico \

##  format=GTiff type=Float64 output=dem_pseudo_ortometrico.tif

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real 1m0.248s


## ---- eval=F------------------------------------------------------------------
## # Exportar red_mtn50k_cleaned_largos.gpkg a shapefile
## ogr2ogr(
##   src_datasource_name = paste0('/media/jose/datos/alos-palsar-dem-rd/',
##                                'dem/red_mtn50k_cleaned_largos.gpkg'),
##   dst_datasource_name = paste0('/media/jose/datos/alos-palsar-dem-rd/',
##                                'dem/red_mtn50k_cleaned_largos.shp'),
##   verbose=TRUE)


## # Tallar con WBT

## time ~/WhiteboxTools_linux_amd64/WBT/whitebox_tools \

##   --wd='/media/jose/datos/alos-palsar-dem-rd/dem/' \

##   --run=FillBurn --dem='dem_pseudo_ortometrico.tif' \

##   --streams=red_mtn50k_cleaned.shp --output='dem_tallado.tif' -v

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	9m21.980s

## # Importar a GRASS GIS

## time r.import --overwrite input=dem_tallado.tif output=dem_tallado

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	0m38.519s


## ----geomorfonosrd, echo=F, fig.cap='"Geomórfonos" de República Dominicana generados a partir de DEM ALOS PALSAR. En cartela, detalle del cañón del río Payabo'----
knitr::include_graphics(paste(figuras, "geomorfonos-de-rd.jpg", sep = '/'))


## # Crear geomórfonos

## # WBT

## # time ~/WhiteboxTools_linux_amd64/WBT/whitebox_tools \

## #   -r=Geomorphons -v --wd='/media/jose/datos/alos-palsar-dem-rd/dem/' \

## #   --dem=dem_tallado.tif -o=geomorfonos.tif --search=25 \

## #   --threshold=0 --tdist=0.0 --forms

## # echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real 6m52.298s #MUY EFICIENTE. Se prefirió la versión de GRASS

## ## para garantizar flujo de trabajo dentro de la base de datos.

## # GRASS GIS

## time r.geomorphon \

##   --overwrite --verbose \

##   elevation=dem_pseudo_ortometrico forms=geomorfonos search=25

## echo "r.geomorphon finalizado" | mail -s "Mensaje sobre r.geomorphon" USUARIO@MAIL

## ## real	33m16.508s #MUY LENTO

## 

## # Extraer depresiones desde geomorfonos

## r.mapcalc --overwrite \

##   expression="'depresiones_geomorfonos' = if(geomorfonos == 10, 1, null())"

## 

## # Importar depresiones manualmente digitalizadas a base de datos de GRASS GIS

## v.import --overwrite input=depresiones_digitalizadas.gpkg \

##   output=depresiones_digitalizadas

## 

## # Convertir depresiones digitalizadas manualmente a ráster

## v.to.rast --overwrite input=depresiones_digitalizadas \

##   type=area use=val output=depresiones_digitalizadas

## 

## # Importa la capa de calizas con depresiones en RD (de Mapa Geológico 250K)

## v.import --overwrite input=calizas_con_depresiones.gpkg output=calizas_con_depresiones

## 

## # Convertir la capa de calizas con depresiones a ráster

## v.to.rast --overwrite input=calizas_con_depresiones type=area \

##   use=val output=calizas_con_depresiones

## 

## # Adjuntar depresiones digitalizadas manualmente con calizas

## r.mapcalc --overwrite \

##   expression="'depresiones_geomorfonos_calizas' = \

##               'depresiones_geomorfonos' * 'calizas_con_depresiones'"

## 

## # Unir todas las depresiones en un único mapa

## r.patch --overwrite input=depresiones_geomorfonos_calizas,depresiones_digitalizadas \

##   output=depresiones_todas


## ----depresiones, echo=F, fig.cap='DEM ALOS PALSAR representado como mapa hipsómétrico (rojo y marrón representan terreno elevado, verde y azul claro terreno bajo) sobre relieve sombreado, mostrando el área de Guaraguao, Los Haitises, al sur del río Yuna (nordeste de República Dominicana). (A) sin mostrar depresiones, (B) mostrando depresiones en tonalidad azul oscuro'----
knitr::include_graphics(paste(figuras, "depresiones.png", sep = '/'))


## # Importar máscara

## v.import --overwrite input=mascara-1km-solo-en-frontera.gpkg \

##   output=mascara_1km_solo_en_frontera

## # Fijar máscara (EJECUTAR SÓLO SI ES ESTRICTAMENTE NECESARIO, PUES TARDA MUCHO)

## r.mask -r

## r.mask vector=mascara_1km_solo_en_frontera

## 

## # Acumulación de flujo

## time r.watershed --overwrite --verbose elevation=dem_tallado \

##  depression=depresiones_todas accumulation=rwshed_acum \

##  threshold=180 stream=rwshed_talwegs \

##  # El umbral 180 se usó en la extracción de una red de muestra, como forma de

##  # previsualizar una hidrografía inicial y no como red definitiva.

##  # Otras opciones posibles del addon son las siguientes (útiles en otras aplicaciones):

##  # drainage=rwshed_direccion_drenaje basin=rwshed_cuencas half_basin=rwshed_hemicuenca \

##  # tci=rwshed_tci spi=rwshed_spi \

##  # length_slope=rwshed_longitud_vertiente slope_steepness=rwshed_empinamiento \

##  # retention=rwshed_retencion_flujo max_slope_length=rwshed_max_longitud_vertiente \

##  memory=32000

## echo "r.watershed finalizado" | mail -s "Mensaje sobre r.watershed" USUARIO@MAIL

## ## real 9m47.041s


## ----acumyredrwshed, echo=F, fig.cap='Mapa de acumulación de flujo generado con `r.watershed`. En cartela, detalle del mapa en la cuenca del río Yaque del Sur.'----
knitr::include_graphics(paste(figuras, "acumulacion-flujo-y-red-rwshed.png", sep = '/'))


## # Extraer redes de drenaje para tres umbrales de acumulación distintos

## # En bucle

## for i in `echo {180..900..360}`; \

##   do echo -e "\nTRABAJANDO EL UMBRAL DE ACUMULACIÓN $i ...\n"; \

##   time r.stream.extract --overwrite elevation=dem_tallado accumulation=rwshed_acum \

##     depression=depresiones_todas threshold=$i \

##     stream_vector=rstream_talwegs_umbral_$i stream_raster=rstream_talwegs_umbral_$i \

##     direction=rstream_direccion_umbral_$i memory=32000; \

##   echo -e "r.stream.extract umbral $i finalizado" |\

##     mail -s "Mensaje sobre r.stream.extract" USUARIO@MAIL; \

## done

## ## real 11m28.455s

## ## real 11m26.908s

## ## real 11m30.074s

## # Único umbral, para testing

## # time r.stream.extract --overwrite elevation=dem_tallado accumulation=rwshed_acum \

## #   depression=depresiones_todas threshold=64 \

## #   stream_vector=rstream_talwegs_umbral_64 stream_raster=rstream_talwegs_umbral_64 \

## #     direction=rstream_direccion_umbral_64 memory=32000

## # echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	11m46.930s

## 

## # Calcular estadisticos, y pasar a archivo

## for i in `echo {180..900..360}`; \

##   do v.to.db -p option=length map=rstream_talwegs_umbral_$i >\

##     stats_length_rstream_talwegs_umbral_$i.txt;

## done


## ---- message=F, warning=F----------------------------------------------------
stats_rstream_talwegs <- sapply(as.character(c(180, 540, 900)), function(x) 
  read_delim(paste0(dem_proc_dir, '/', 'stats_length_rstream_talwegs_umbral_', x, '.txt'),
             progress = F, show_col_types = F), simplify = F)
n_rstream_talwegs <- stats_rstream_talwegs %>% 
  map(~ .x %>% filter(!cat==-1) %>% nrow) %>% unlist
length_rstream_talwegs <- stats_rstream_talwegs %>%
  map(~ .x %>% filter(!cat==-1) %>% pull(length) %>% sum/1000) %>% unlist


## ----redindiferenciada, echo=F, fig.cap='Red de drenaje extraída para tres umbrales de acumulación: (A) 180 celdas, equivalente a ~3 ha; (B) 540 celdas, equivalente a ~8 ha; (C) 900 celdas, equivalente a ~14 ha. La imagen de fondo es un relieve sombreado a partir de DEM ALOS PALSAR, mostrando el área de El Arroyazo en la reserva científica Ébano Verde (provincia La Vega, cordillera Central de República Dominicana)'----
knitr::include_graphics(paste(figuras, "red-indiferenciada.png", sep = '/'))


## # Extraer orden de red

## # En bucle

## for i in `echo {180..900..360}`; \

##   do echo -e "\nTRABAJANDO EL UMBRAL DE ACUMULACIÓN $i ...\n"; \

##   time r.stream.order --overwrite stream_rast=rstream_talwegs_umbral_$i \

##     direction=rstream_direccion_umbral_$i \

##     elevation=dem_tallado accumulation=rwshed_acum \

##     stream_vect=rstream_orden_de_red_umbral_$i \

##     strahler=rstream_orden_strahler_de_red_umbral_$i \

##     horton=rstream_orden_horton_de_red_umbral_$i \

##     topo=topologia_orden_umbral_$i memory=32000; \

##   echo -e "r.stream.order umbral de acumulación $i finalizado" |\

##     mail -s "Mensaje sobre r.stream.order" USUARIO@MAIL; \

## done

## ## real 1m34.983s

## ## real 1m18.662s

## ## real 1m14.986s

## # Aplicación de algoritmo con un único umbral, sólo para pruebas

## # time r.stream.order --overwrite \

## #     stream_rast=rstream_talwegs direction=rstream_direccion \

## #     elevation=dem_tallado accumulation=rwshed_acum stream_vect=order_todos \

## #     topo=topologia_orden memory=32000

## # echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real	


## ----redorden3umbrales, echo=F, fig.cap='Orden de red de Strahler para redes de drenaje generadas a partir de tres umbrales de acumulación: (A) 180 celdas, equivalente a ~3 ha; (B) 540 celdas, equivalente a ~8 ha; (C) 900 celdas, equivalente a ~14 ha. El área mostrada corresponde al río San Juan, afluente del río Yaque del Sur (vertiente sur de la cordillera Central de República Dominicana)'----
knitr::include_graphics(paste(figuras, "red-orden.png", sep = '/'))


## ----redordenumbral180, echo=F, fig.cap='Orden de red de Strahler en el área del pico de la Viuda y Sabana Vieja, provincia San Juan (vertiente sur de la cordillera Central de República Dominicana). Esta red fue generada usando un umbral de acumulación de 180 celdas, equivalente a ~3 ha. De fondo, mapa topográfico nacional escala 1:50,000 y relieve sombreado'----
knitr::include_graphics(paste(figuras, "red-orden-detalle-mtn.jpg", sep = '/'))


## # Delimitar cuencas según jerarquía

## # En bucle

## for i in `echo {180..900..360}`; \

##   do for j in `echo {1..8..1}`; \

##     do echo -e "\nTRABAJANDO EL UMBRAL DE ACUMULACIÓN $i, orden $j...\n"; \

##     time r.stream.basins -c --overwrite direction=rstream_direccion_umbral_$i \

##       stream_rast=rstream_orden_strahler_de_red_umbral_$i cats=$j \

##       basins=rstream_cuencas_strahler_umbral_${i}_orden_$j memory=32000; \

##   done; \

##   echo -e "r.stream.basins umbral de acumulación $i finalizado" |\

##     mail -s "Mensaje sobre r.stream.basins" USUARIO@MAIL; \

## done

## ## real ~ 0m40s repetido tantas veces como órdenes para cada umbral de acumulación


## # Delimitar cuencas terminales

## # En bucle

## for i in `echo {180..900..360}`; \

##   do for j in `echo {1..8..1}`; \

##     do echo -e "\nTRABAJANDO EL UMBRAL DE ACUMULACIÓN $i, orden $j...\n"; \

##     time r.stream.basins -lc --overwrite direction=rstream_direccion_umbral_$i \

##       stream_rast=rstream_orden_strahler_de_red_umbral_$i cats=$j \

##       basins=rstream_cuencas_strahler_terminal_umbral_${i}_orden_$j memory=32000; \

##   done; \

##   echo -e "r.stream.basins umbral de acumulación $i finalizado" |\

##     mail -s "Mensaje sobre r.stream.basins" USUARIO@MAIL; \

## done

## ## real ~ 0m40s repetido tantas veces como órdenes para cada umbral de acumulación


## # Cuencas y subcuencas según orden

## for i in `echo {1..8..1}`; \

##   do echo -e "\nTRABAJANDO EL ORDEN $i...\n"; \

##     time r.to.vect --overwrite input=rstream_cuencas_strahler_umbral_540_orden_$i \

##       output=rstream_cuencas_strahler_umbral_540_orden_$i type=area; \

##     v.db.addcolumn rstream_cuencas_strahler_umbral_540_orden_$i \

##       columns="strahler int"; \

##     v.db.update rstream_cuencas_strahler_umbral_540_orden_$i \

##       col=strahler value=$i where="strahler IS NULL"; \

##     # Calcular estadisticos, y pasar a archivo

##     ## Preparación de fuentes (corrección de topología >

##     ##                         actualización de área >

##     ##                         eliminar registros)

##     v.clean --overwrite layer=1 \

##       input=rstream_cuencas_strahler_umbral_540_orden_$i \

##       output=foo \

##       tool=rmarea threshold=4000

##     v.to.db --overwrite option=area type=centroid columns=area \

##       map=foo

##     v.db.droprow --overwrite \

##       input=foo \

##       output=rstream_cuencas_strahler_umbral_540_orden_$i where="area IS NULL"

##     g.remove -f type=vector \

##     name=foo

## done

## # Tiempos uso de CPU, órdenes 1 a 8

## # real	2m0.180s

## # real	0m49.778s

## # real	0m32.943s

## # real	0m28.137s

## # real	0m28.078s

## # real	0m26.906s

## # real	0m25.886s

## # real	0m25.624s

## # Limpiando las cuencas de órdenes 2 y 3 de menos de 60,000 m2

## for i in `echo {2..3..1}`; \

##   do v.db.droprow --overwrite \

##     input=rstream_cuencas_strahler_umbral_540_orden_$i \

##     output=foo \

##     where="area <= 6e4";

##     g.rename --overwrite \

##       vector=foo,rstream_cuencas_strahler_umbral_540_orden_$i; \

##     g.remove -f type=vector name=foo; \

## done

## 

## 

## # Cuencas terminales

## for i in `echo {1..8..1}`; \

##   do echo -e "\nTRABAJANDO EL ORDEN $i...\n"; \

##     r.to.vect --overwrite input=rstream_cuencas_strahler_terminal_umbral_540_orden_$i \

##       output=rstream_cuencas_strahler_terminal_umbral_540_orden_$i type=area; \

##     v.db.addcolumn rstream_cuencas_strahler_terminal_umbral_540_orden_$i \

##       columns="strahler int"; \

##     v.db.update rstream_cuencas_strahler_terminal_umbral_540_orden_$i \

##       col=strahler value=$i where="strahler IS NULL"; \

## done

## v.patch -e --overwrite \

##   input=`g.list type=v pattern='rstream_cuencas_strahler_terminal_umbral_540_orden_*' \

##     separator=comma` \

##   output=rstream_cuencas_strahler_terminal_umbral_540_todos

## # Calcular estadisticos, y pasar a archivo

## ## Preparación de fuentes (corrección de topología >

## ##                         actualización de área >

## ##                         eliminar registros)

## v.clean --overwrite layer=1 input=rstream_cuencas_strahler_terminal_umbral_540_todos \

##   output=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned \

##   tool=rmarea threshold=4000

## v.to.db --overwrite option=area type=centroid columns=area \

##   map=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned

## v.db.droprow --overwrite rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned \

##   output=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned_2 where="area IS NULL"

## g.rename --overwrite \

##   vector=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned_2,\

##   rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned

## # Excluir cuencas strahler>=4 y area<=1e6

## v.db.droprow --overwrite rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned \

##   output=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned_2 \

##   where="strahler >= 4 and area <= 1e6"

## g.rename --overwrite \

##   vector=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned_2,\

##   rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned

## # Generar tabla

## v.db.select --overwrite rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned \

##   where='cat!=-1' > stats_area_rstream_cuencas_strahler_terminal_umbral_540_todos.txt

## 

## 

## # Generar salidas GPKG y SHP para cuencas terminales

## ## Exportar el mapa 'rstream_orden_de_red_umbral_540' a GeoPackage

## v.out.ogr --overwrite \

##   input=rstream_orden_de_red_umbral_540 \

##   output=gpkg-shp/rstream_orden_de_red_umbral_540.gpkg \

##   type=line \

##   format=GPKG

## ## Exportar el mapa 'rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned'

## ## a GeoPackage

## v.out.ogr --overwrite \

##   input=rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned \

##   output=gpkg-shp/rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned.gpkg \

##   type=area \

##   format=GPKG

## ## Exportar el mapa 'rstream_orden_de_red_umbral_540' a Shapefile

## ogr2ogr -f "ESRI Shapefile" \

##   gpkg-shp/rstream_orden_de_red_umbral_540.shp \

##   gpkg-shp/rstream_orden_de_red_umbral_540.gpkg

## ## Exportar el mapa 'rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned'

## ## a Shapefile

## ogr2ogr -f "ESRI Shapefile" \

##   gpkg-shp/rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned.shp \

##   gpkg-shp/rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned.gpkg

## 

## 

## # Generar salidas GPKG y SHP para cuencas y subcuencas

## ## Exportar mapas 'rstream_cuencas_strahler_umbral_540_orden_$i' a GeoPackage

## for i in `echo {1..8..1}`; \

##   do echo -e "\nTRABAJANDO EL ORDEN $i...\n"; \

##     v.out.ogr --overwrite \

##      input=rstream_cuencas_strahler_umbral_540_orden_$i \

##      output=gpkg-shp/rstream_cuencas_strahler_umbral_540_orden_$i.gpkg \

##      type=area \

##      format=GPKG

## done


## ---- message=F, warning=F----------------------------------------------------
stats_rstream_cuencas_540 <- read_delim(
  paste0(dem_proc_dir, '/',
         'stats_area_rstream_cuencas_strahler_terminal_umbral_540_todos.txt'),
  progress = F, show_col_types = F)
rstream_cuencas_540_por_orden <- stats_rstream_cuencas_540 %>% 
  rename(`Orden de red (Strahler)` = strahler) %>% 
  group_by(`Orden de red (Strahler)`)  %>%
  summarise(`Número de cuencas` = n(),
            `Área promedio` = mean(area),
            `Área total` = sum(area))


## -----------------------------------------------------------------------------
# Cuencas terminales
cuencas <- st_read(
  dsn = 'gpkg-shp/rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned.gpkg')
cuencas4mas <- cuencas[cuencas$strahler >= 4, ]
# Cuencas y subcuencas
system.time(
  cuencas_subcuencas <- sapply(as.character(1:8), function(x) {
    st_read(paste0('gpkg-shp/rstream_cuencas_strahler_umbral_540_orden_', x, '.gpkg'))
    }, USE.NAMES = T, simplify = F)
)
cuencas_sub_areas_ordenes <- map(cuencas_subcuencas,
                         ~.['area'] %>% st_drop_geometry %>%
                           pull(area) %>% as_tibble %>%
                           mutate(`Área (kilómetros cuadrados)` = value/1e6,
                                  `Área (hectáreas)` = value/1e4) %>% 
                           rename(`Área (metros cuadrados)` = value)) %>% 
  bind_rows(.id = 'Orden de red')
cuencas_sub_areas_ordenes_r <- cuencas_sub_areas_ordenes %>%
  group_by(`Orden de red`) %>% 
  summarise(describe(`Área (kilómetros cuadrados)`, type = 2)) %>% 
  select(-`vars`, -trimmed, -mad, -se) %>%
  select(`Orden de red`, `Número` = n, `Media (km${^2}$)` = mean,
         `Mediana (km${^2}$)` = median, `Desv. estándar (km${^2}$)` = sd,
         `Mínimo (km${^2}$)` = min, `Máximo (km${^2}$)` = max,
         `Rango (km${^2}$)` = range, Sesgo = skew,
         Curtosis = kurtosis)
cuencas_sub_areas_ordenes_p <- cuencas_sub_areas_ordenes %>% ggplot +
  aes(x = `Orden de red`, y = `Área (kilómetros cuadrados)`) +
  geom_jitter(alpha = 0.2, height = 0, width = 0.05
              , aes(color = `Orden de red`, fill = `Orden de red`)
              ) +
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray"
              , aes(color = `Orden de red`)
              ) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_y_continuous(trans = 'log2', labels = decimales_y_enteros) +
  theme_bw() +
  theme(legend.position = 'none', text = element_text(size = 18))
png('figuras/cuencas-subcuencas-areas-ordenes-boxplot.png',
    width = 3500, height = 2400, res = 450)
cuencas_sub_areas_ordenes_p
dev.off()


## # Generar GPKG de país

## v.out.ogr --overwrite \

##   input=mascara \

##   output=gpkg-shp/mascara.gpkg \

##   type=area \

##   format=GPKG


## ---- eval=F------------------------------------------------------------------
## # Máscara
## mascara <- st_read('gpkg-shp/mascara.gpkg')
## # Objeto sf de las cuencas de todos los órdenes
## cuencas_sub_areas_ordenes_sf <- map(cuencas_subcuencas, ~.['area'] %>%
##                            mutate(`Área (kilómetros cuadrados)` = area/1e6,
##                                   `Área (hectáreas)` = area/1e4) %>%
##                            rename(`Área (metros cuadrados)` = area)) %>%
##   bind_rows(.id = 'Orden de red')
## # Objeto sf de los linderos de las cuencas, en objeto de tipo MULTILINESTRING
## cuencas_sub_areas_ordenes_lines_sf <- cuencas_sub_areas_ordenes_sf %>%
##   select(orden = `Orden de red`) %>%
##   mutate(grosor = ifelse(orden %in% 1:3, 0, 0.1)) %>%
##   mutate(orden = paste('Orden', orden)) %>%
##   st_cast('MULTILINESTRING')
## # Mapa en tmap
## cuencas_sub_areas_ordenes_tm <- cuencas_sub_areas_ordenes_sf %>%
##   select(orden=`Orden de red`, `km cuad.` = `Área (kilómetros cuadrados)`) %>%
##   mutate(grosor = ifelse(orden %in% 1:2, 0.0001, 0.1)) %>%
##   mutate(orden = paste('Orden', orden)) %>%
##   tm_shape() +
##   tm_fill(col='km cuad.', palette = "YlOrBr", style = 'quantile') +
##   tm_facets(by = "orden", ncol = 2, nrow = 4, free.coords = FALSE, free.scales = TRUE) +
##   tm_shape(cuencas_sub_areas_ordenes_lines_sf) +
##   tm_lines(lwd = 'grosor', col = 'grey80', legend.lwd.show = F) +
##   tm_facets(by = "orden", ncol = 2, nrow = 4, free.coords = FALSE, free.scales = TRUE) +
##   tm_layout(panel.label.size = 2.5, legend.stack = "horizontal",
##             legend.title.size = 2, legend.text.size = 1.5) +
##   tm_shape(shp = mascara) +
##   tm_borders(col = 'black', lwd = 0.8)
## # Mapa en PNG
## tmap_save(
##   tm = cuencas_sub_areas_ordenes_tm,
##   filename = "figuras/cuencas-subcuencas-areas-ordenes.png",
##   width = 3000, height = 4200, dpi = 200)


## -----------------------------------------------------------------------------
sessionInfo()

