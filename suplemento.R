## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE, 
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  out.width = '100%',
  # res = 300,
  dpi = 300)
# options(digits = 3)


## ----suphidropaquetes---------------------------------------------------------
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
library(raster)
library(sf)
library(kableExtra)
library(tidyverse)
library(gdalUtilities)
source('R/funciones.R')
dem_proc_dir <- 'alos-palsar-dem-rd/dem/'


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
  # kable(format = 'latex', escape = F, booktabs = T,
  #       caption = paste('Escenas ALOS-PALSAR usadas para generar un DEM de 12.5 m de
  #                       resolución espacial de República Dominicana')) %>%
  # kable_styling(bootstrap_options = c("hover", "condensed"), full_width = T)


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


## ----demsinprocesar, echo=F, fig.cap='DEM ALOS PALSAR sin procesar, representado como relieve sombreado. Nótesense los píxeles sin datos, destacados en color rojo (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics("out/dem-sin-procesar.jpg")


## # Rellenar vacíos

## time r.fillnulls --overwrite --verbose \

##   input=dem method="bilinear" \

##   tension=40 smooth=0.1 edge=3 npmin=600 segmax=300 lambda=0.01 \

##   output=dem_relleno

## # Enviar mensaje al finalizar (ejecutar conjuntamente con anterior)

## echo "Job finished" | mail -s "Job finished" USUARIO@MAIL

## ## real 10m11.925s


## ----demrelleno, echo=F, fig.cap='DEM ALOS PALSAR sin procesar, representado como relieve sombreado. Los píxeles sin datos fueron eliminados (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics("out/dem-relleno.jpg")


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


## ----demsuavizado, echo=F, fig.cap='DEM ALOS PALSAR suavizado, representado como relieve sombreado. Nótese la conservación de las morfologías principales y la eliminación del ruido sobre éstas (Los Patos-Ojeda-Paraíso, provincia Barahona, sudoeste de República Dominicana)'----
knitr::include_graphics("out/dem-suavizado.jpg")


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
knitr::include_graphics("out/perfiles-dem/los-patos.png")


## -----------------------------------------------------------------------------
sessionInfo()

