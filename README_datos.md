# Datos para el manuscrito "Generación de red hidrográfica densa de República Dominicana a partir de modelo digital de elevaciones de resolución media"

José Ramón Martínez Batlle, Universidad Autónoma de Santo Domingo (UASD) jmartinez19\@uasd.edu.do

Michela Izzo Gioiosa, Guakia Ambiente, michela.izzo\@guakiambiente.org

Este repositorio contiene datos geoespaciales resultantes de ejecutar el código reproducible de R y GRASS GIS del [repositorio de GitHub asociado](https://github.com/geofis/red-hidrografica-densa-rd), como apoyo al manuscrito contenido en dicho repo.

Los datos se distribuyen en dos archivos ZIP comprimidos, los cuales pueden descargarse de forma separada desde Zenodo.

## Archivo `gpkg-shp.zip`.

Este comprimido contiene versiones en formato GeoPackage y ESRI-Shapefile (vectoriales) de los principales mapas generados en GRASS GIS. Constituye una alternativa rápida para disponer de los datos hidrográficos sin necesidad de abrir la base de datos de GRASS GIS, muy recomendado si sólo se necesitan los productos hidrográficos más importantes. Los archivos incluidos son los siguientes:

- `cursos_mas_largos*`: cursos más largos de 35 ríos seleccionados de la República Dominicana basados en orden de red y representatividad territorial. Se priorizaron ríos de orden seis o superior. En áreas del sur y sudoeste, se incluyeron ríos de orden cinco de notable longitud.
- `desembocaduras_rios_grandes*`: puntos de desembocaduras (_outlet_) de ríos grandes, seleccionados para extraerles sus cursos más largos.
- `mascara.gpkg`: máscara de visualización, la cual encierra el territorio dominicano limitado por sus costas y la frontera domínico-haitiana. Esta máscara sólo tiene fines de visualización, pues los análisis de hecho se realizaron empleando una máscara ligeramente más grande, expandida aproximadamente 1 km hacia el oeste en la frontera.
- `rstream_cuencas_strahler_terminal_umbral_540_todos_cleaned`: cuencas hidrográficas exorreicas (cuya red desemboca en en el mar) de República Dominicana delimitadas en forma de polígonos, generadas para un umbral de acumulación de 540 celdas (aproximadamente 8 hectáreas), con indicación del orden de red de Strahler máximo de la cuenca y el área en metros cuadrados.
- `rstream_cuencas_strahler_umbral_540_orden_*`: representaciones de las cuencas según órdenes de red de Strahler para el umbral de acumulación de 540 celdas (aproximadamente 8 hectáreas), tanto las cuencas terminales (exorreicas) como las que cuya red desemboca en lagos interiores o en otros ríos, extraídas con el complemento `r.stream.basins`.

- Archivos `rstream_orden_de_red_umbral_540_cleaned*`: red de drenaje de República Dominicana, generada para un umbral de acumulación de 540 celdas (aproximadamente 8 hectáreas), incluyendo los siguientes campos:
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

## Archivos `grassdata##`.

Estos archivos son las 17 partes de un archivo ZIP (`##` son números del 00 al 16), las cuales, al concatenarse, producirán un único ZIP conteniendo la base de datos exhaustiva de GRASS GIS (ver instrucciones de concatenación en el siguiente apartado). Esta base de datos incluye todos los mapas del flujo de trabajo íntegro, que incluye los subproductos generados desde el preprocesamiento del DEM, hasta las salidas obtenidas por hidrología computacional. Las fuentes ráster tienen resolución máxima de 12.5 metros, y la extensión máxima de la base de datos tiene los siguientes límites en el sistema WGS 84 / UTM zone 19N: norte 2214300, sur 1919000, oeste 180300, este 580700. Por su gran tamaño, se recomienda usar esta base de datos sólo para fines de reproducibilidad. Se recomienda abrir esta base de datos con GRASS GIS directamente o, alternativamente, a través de QGIS. Los mapas más relevantes son los siguientes:

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
- `mascara*` vectoriales: máscaras de país, que incluye la máscara por defecto, con área _buffer_ de 1 km, y con área _buffer_ de 1 km sólo en la frontera.
- `prioridad*` vectoriales y rasters: prioridad, original y reescalada, a nivel global y para cada escenario de densidad, de instalacion de estaciones hidrometricas.
- `puntos_para_areas_prospeccion_###_km2` vectoriales: Mapas de puntos conteniendo los centroides de las áreas de prospección para el establecimiento de estaciones hidrométricas.
- `red_mtn50k_cleaned` vectorial: red de drenaje, aparentemente extraída desde el mapa topográfico nacioanal escala 1:50,000. No usada en el estudio, almacenada para fines de consultas posteriores. El sufijo "cleaned" hace referencia a que se aplicó una corrección de topología mediante la herramienta `v.clean` de GRASS GIS.
- `red_mtn50k_cleaned_largos` vectorial y ráster: red de cursos largos usada para el tallado del DEM. Esta red se compone de una selección de ríos largos y permanentes, extraídos desde imágenes satelitales (Google; Airbus, CNES; Airbus, Landsat; Copernicus; Maxar Technologies; U.S. Geological Survey, 2023), el mapa topográfico nacional escala 1:50,000 (Instituto Cartográfico Militar (ICM), 1989) y OpenStreetMap contributors (2017). Los ríos que llenan embalses, se representan mediante trazados históricos para así mantener la continuidad hidrológica.
- `rstream_cuencas_strahler_*` vectoriales y rásters: representaciones de las cuencas según órdenes de red de Strahler y umbrales de acumulación (180, 540 y 900 celdas), tanto las cuencas terminales (exorreicas) como las que cuya red desemboca en lagos interiores o en otros ríos, extraídas con el complemento `r.stream.basins`. Los archivos que no incluyen indicación de `orden`, y en su lugar aparece la palabra `todos`, contienen una relación exhausitva de todas las cuencas para el umbral en cuestión.
- `rstream_direccion_umbral_*` rásters: dirección de flujo de cada umbral de acumulación (180, 540 y 900 celdas).
- `rstream_orden_de_red_umbral_*` y `rstream_orden_strahler_*` rásters y vectoriales: red de _talwegs_ segregadas en función del orden de red de Strahler y del umbral de acumulación. Cuando no se especifica `orden` (e.g. rstream*orden_de_red_umbral*\*`), se trata de un vectorial que incluye todos los órdenes de forma comprehensiva.
- `rstream_orden_horton_*` rásters: red de _talwegs_ según orden de jerarquía de Horton y umbrales de acumulación de 180, 540 y 900 celdas.
- `rstream_talwegs_umbral_*` rasters y vectoriales: red _talwegs_ según umbrales de acumulación de 180, 540 y 900 celdas, obtenidos mediante el complemento `r.stream.extract`.
- `rvb_fondo_de_valle_pais` ráster: "índice múltiresolución de planitud de fondos de valle" MrVBF, obtenido mediante el complemento `r.valley.bottom`.
- `rwshed_acum` ráster: mapa de acumulación de flujo obtenido a través del complemento `r.watershed`.
- `topologia_orden*` rásters: dimensión topológica de los cursos obtenida por medio del complemento `r.stream.order`.

## Instrucciones para concatenar las partes ZIP de la base de datos de GRASS GIS

La base de datos de GRASS GIS está comprimida un archivo ZIP que hemos tenido que segmentar en pequeñas partes para facilitar la subida a Zenodo. Los archivos `grassdata##`, donde `##` corresponden a los números del 00 al 16, son las 17 partes de un archivo ZIP. Si concatenas las partes obtendrás un ZIP único que podrás descomprimir para acceder a la base de datos exhaustiva de GRASS GIS. A continuación te explicamos cómo hacer la concatenación, tanto en Linux como en Windows.

### Concatenar en Linux

Coloca todas las partes `grassdata##` en un directorio vacío. Desde la terminal, en dicho direcotiro, usa el siguiente comando:

```bash
cat grassdata* > grassdata.zip
```

### Concatenar en Windows

- En el símbolo de sistema (*command prompt*), con el comando `copy`:

```bash
copy /b grassdata00+grassdata01+...+grassdata16 grassdata.zip
```

Reemplaza `...` con el resto de tus archivos en orden. Por ejemplo, `grassdata00+grassdata01+grassdata02+grassdata03+grassdata04+grassdata05+grassdata06+`
`grassdata07+grassdata08+grassdata09+grassdata10+grassdata11+grassdata12+grassdata13+`
`grassdata14+grassdata15+grassdata16`

- En la interfaz gráfica (GUI), usando 7-Zip

  - Si aún no tienes 7-Zip instalado, puedes descargarlo desde [su sitio web oficial](https://www.7-zip.org/).
  - Instala el programa como cualquier otro software en Windows.
  - Ubica los archivos divididos en una carpeta.
  - Haz clic derecho en el primer archivo de la serie (`grassdata00`).
  - En el menú contextual, pasa el cursor sobre "7-Zip" y selecciona "Extraer aquí" o "Extraer en grassdata.zip".

Si los archivos están correctamente secuenciados, 7-Zip los unirá automáticamente y extraerá el archivo original.

- Otras herramientas de GUI:

  - **HJSplit**: Es una aplicación gratuita que permite dividir y unir archivos. Tiene versiones tanto para Windows como para otros sistemas operativos.
  - **File Splitter and Joiner**: Es otra aplicación gratuita para Windows que permite dividir archivos grandes en pequeñas porciones y luego unirlas.
