# Datos para el manuscrito "Generación de red hidrográfica densa de República Dominicana a partir de modelo digital de elevaciones de resolución media"

José Ramón Martínez Batlle, Universidad Autónoma de Santo Domingo (UASD) jmartinez19\@uasd.edu.do

Michela Izzo Gioiosa, Guakia Ambiente, michela.izzo\@guakiambiente.org

Este repositorio contiene datos geoespaciales resultantes de ejecutar el código reproducible de R y GRASS GIS del [repositorio de GitHub asociado](https://github.com/geofis/red-hidrografica-densa-rd), como apoyo al manuscrito contenido en dicho repo.

Los datos se distribuyen en dos directorios:

1. Directorio `gpkg-shp`, que constituye una alternativa rápida para disponer de los datos hidrográficos sin necesidad de abrir la base de datos de GRASS GIS. Este directorio que contiene versiones en formato GeoPackage y ESRI-Shapefile de los vectoriales siguientes:
  - Red de drenaje de República Dominicana, generada para un umbral de acumulación de 540 celdas (aproximadamente 8 hectáreas), incluyendo los siguientes campos (archivos `rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned`):
      - `cat` entero: categoría;
      - `stream` entero: número de corriente, usualmente igual a cat;
      - `next_stream` entero: corriente a la que contribuye la corriente actual (aguas abajo);
      - `prev_streams`; dos o más corrientes contribuyentes (aguas arriba);
      - `strahler` entero: Orden de corriente de Strahler;
      - `horton` entero: Orden de corriente de Horton;
      - `shreve` entero: Magnitud de corriente de Shreve;
      - `hack` entero: Corrientes principales de Hack o orden de Gravelius;
      - `topo_dim` entero: Orden topológico de corrientes;
      - `scheidegger` entero: Enteros Asociados Consistentes de Scheidegger;
      - `drwal` entero: Jerarquía de corrientes de Drwal;
      - `length` precisión doble: longitud de la corriente;
      - `stright` precisión doble: longitud de la corriente como línea recta;
      - `sinusoid` precisión doble: dimensión fractal: longitud de la corriente/longitud de la corriente recta;
      - `cum_length` precisión doble: longitud de la corriente desde la fuente;
      - `flow_accum` precisión doble:
      - `out_dist` precisión doble: distancia desde el inicio de la corriente actual hasta la salida;
      - `source_elev` precisión doble: elevación del inicio de la corriente;
      - `outlet_elev` precisión doble: elevación de la salida de la corriente;
      - `elev_drop` precisión doble: diferencia entre source_elev y outlet_elev + caída de la salida;
      - `out_drop` precisión doble: caída en la salida de la corriente;
      - `gradient` precisión doble: caída/longitud;
  - Cuencas hidrográficas de República Dominicana delimitadas en forma de polígonos, generadas para un umbral de acumulación de 540 celdas (aproximadamente 8 hectáreas), con indicación del orden de red de Strahler máximo de la cuenca y el área en metros cuadrados (archivos `rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned`).

2. Directorio `grassdata`, que contiene la base de datos de GRASS GIS. Esta incluye todos los mapas del flujo de trabajo íntegro, que incluye aquellos correspondientes al preprocesamiento del DEM, hasta los mapas correspondientes al procesamiento de hidrología computacional. Las fuentes ráster tienen resolución máxima de 12.5 metros, y la huellas espacial de la base de datos tiene los siguientes límites en el sistema WGS 84 / UTM zone 19N: norte 2214300, sur 1919000, oeste 180300, este 580700. Se recomienda abrir esta base de datos con GRASS GIS directamente o, alternativamente, a través de QGIS. Los mapas más relevantes son los siguientes:

  - `areas_prospeccion_###_km2` vectoriales: un área encerrada por un círculo imaginario de superficie `###` kilómetros cuadrados, que rodea a una estación meteoclimática existente o propuesta, creado con el objetivo de evaluar, de manera flexible, la idoneidad de instalar una estación hidrométrica. El mapa `areas_prospeccion_todas` las reúne a todas con independencia de su tamaño superficial.
  - `buffers` vectorial: áreas de influencia en torno a tramos de río idóneos para monitoreo por estación hidrométrica.
  - `calizas_con_depresiones` vectorial y ráster: áreas de calizas con depresiones delimitadas a partir del Mapa Geológico de República Dominicana escala 1:250,000.
  - `dem*` rásters: el modelo digital de elevaciones en todas sus versiones a lo largo del preprocesamiento, desde el original (`dem`), hasta el tallado (`dem_tallado`).
  - `depresiones*` vectorial y rásters: delimitación manual y asistida---con el complemento `r.geomorphon` de GRASS GIS---de las depresiones detectadas en áreas con calizas.
  - `egm2008_*` rásters: altura de geoide de La Española a 1 arco-minuto de resolución espacial (original EGM2008), e interpolado a 50 m.
  - `escenario_###_km2` vectoriales: distribución de puntos de estaciones meteoclimáticas propuestas en el estudio complementario titulado "Selección de sitios para el establecimiento de una red de estaciones meteoclimáticas en República Dominicana usando decisión multicriterio y análisis de vecindad", donde `###` son las densidades de 100, 150 y 250 kilómetros cuadrados por estación.
  - `estaciones_*` vectoriales: estaciones meteoclimáticas, en estado "bueno", de INDRHI y ONAMET.
  - `geomorfonos` ráster: mapa de geomórfonos de República Dominicana, derivado del DEM pseudo-ortométrico usando el complemento `r.geomorphon` de GRASS GIS.
  - `litologia_recodificado` ráster y vectorial: polígonos de las litologías extraídos del Mapa Geológico de República Dominicana, escala 1:250,000.
  - `mascara*` vectoriales: máscaras de país, que incluye la máscara por defecto, con área *buffer* de 1 km, y con área *buffer* de 1 km sólo en la frontera.
  - `prioridad*` vectoriales y rasters: prioridad, original y reescalada, a nivel global y para cada escenario de densidad, de instalacion de estaciones hidrometricas.
  - `puntos_para_areas_prospeccion_###_km2` vectoriales: Mapas de puntos conteniendo los centroides de las áreas de prospección para el establecimiento de estaciones hidrométricas.
  - `red_mtn50k_cleaned` vectorial: red de drenaje, aparentemente extraída desde el mapa topográfico nacioanal escala 1:50,000. No usada en el estudio, almacenada para fines de consultas posteriores. El sufijo "cleaned" hace referencia a que se aplicó una corrección de topología mediante la herramienta `v.clean` de GRASS GIS.
  - `red_mtn50k_cleaned_largos` vectorial: red de drenaje, aparentemente extraída desde el mapa topográfico nacioanal escala 1:50,000. No usada en el estudio, almacenada para fines de consultas posteriores.
  
