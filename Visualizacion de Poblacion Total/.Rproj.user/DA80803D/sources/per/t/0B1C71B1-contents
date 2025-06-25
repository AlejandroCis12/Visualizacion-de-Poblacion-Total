install.packages("readxl")
install.packages("dplyr")
datos=readxl::read_excel(path = "Datos/Originales/Poblacion Total.xlsx",sheet = 3)
names(x=datos)
names(datos)[1:4]
columna5=datos[5,]
columna5[1:4]

names(datos)[1:4]=columna5[1:4]
names(datos)

columna6=datos[6,]
names(datos)[5:ncol(datos)]=columna6[5:ncol(datos)]
names(datos)

datos = datos |>
  dplyr::filter(Sexo == "Total",`Grupos quinquenales de edad` == "Total")

datos = datos |>
  dplyr::select(`Entidad federativa`,Sexo,`Grupos quinquenales de edad`,`Población total1`)

datos_cortados = datos |>
  dplyr::select(`Entidad federativa`,`Población total1`)

datos_cortados = datos_cortados[-1,]

datos_cortados = datos_cortados |>
  dplyr::mutate(`Entidad federativa` = sub(x = `Entidad federativa`,pattern = "^.*? ",replacement =" "))

install.packages("sf")

geometria = sf::read_sf("Datos/Geometria del pais/00ent.shp")
plot(geometria$geometry)

datos_unir= datos[-1,]
datos_unir = datos_unir |>
  dplyr::mutate(clave=sub(x = `Entidad federativa`,pattern = " .*$",replacement = ""),
                `Entidad federativa` = sub(x = `Entidad federativa`,pattern = "^.*? ",replacement =" "))

datos_unir = datos_unir |>
  dplyr::select(clave,`Entidad federativa`,`Población total1`)

geometria = geometria |> 
  dplyr::select(-CVEGEO)

unir = merge(x = datos_unir,y = geometria, by.x = "clave", by.y = "CVE_ENT",all.x = T)

unir = unir |>
  dplyr::select(-NOMGEO)

unir = sf::st_as_sf(x = unir, crs = sf::st_crs(geometria)) # hacemsos el data frame para que se pueda graficar


unir = sf::st_transform(x = unir,crs = 4326) #Es una transformacion lineal

unir$`Población total1` = as.numeric(unir$`Población total1`)

clainstall.packages("leaflet")
install.packages("leaflet.extras")
install.packages("RColorBrewer")
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)

paletas = colorNumeric(
  palette = "YlGnBu",
  domain = unir$`Población total1`,
  reverse = T
)

mapa_web = leaflet() |>
  addTiles() |>
  addPolygons(data = unir,label = unir$`Entidad federativa`, 
              popup = paste("Clave Municipal:","<b>",unir$clave,"</b>", "<br>",
                            "Nombre del Municipio: ","<b>",unir$`Entidad federativa`,"</b>","<br>",
                            "Poblacion total : ", "<b>",unir$`Población total1`,"</b>"),
              color = paletas(x=unir$`Población total1`),
              group = "nacional") |>
  addLegend(position = "bottomright",pal = paletas, title ="Simbologia Tilines",values = unir$`Población total1`)|>
  addSearchFeatures(targetGroups = "nacional",
                    options = searchFeaturesOptions(
                      zoom = 12,
                      position="topleft"
                    ))
                      
mapa_web

install.packages("htmlwidgets")

library(htmlwidgets)
htmlwidgets::saveWidget(mapa_web,"Datos/primer_mapa.html")
