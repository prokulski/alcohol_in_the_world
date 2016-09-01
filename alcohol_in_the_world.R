library(reshape2)
library(dplyr)
library(ggplot2)
library(maptools)


# download file with data:
# http://apps.who.int/gho/athena/data/data-verbose.csv?target=GHO/SA_0000001398&profile=verbose&filter=COUNTRY:*;ALCOHOLTYPE:*
alco <- read.csv("data-verbose.csv")
alco <- select(alco, region_code=REGION..CODE., region_name=REGION..DISPLAY., country_code=COUNTRY..CODE., country_name=COUNTRY..DISPLAY., alco_type=ALCOHOLTYPE..CODE., alco_name=ALCOHOLTYPE..DISPLAY., per_cent=Numeric)

# download and unzip into "maps" folder archive
# http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip
shape <- readShapePoly("maps/ne_110m_admin_0_countries.shp")
projection <- CRS("+init=epsg:4269")
proj4string(shape) <- CRS("+proj=longlat +datum=WGS84")
map_prof <- spTransform(shape, projection)
map <- fortify(map_prof)
map <- merge(map, map_prof, by.x = 'id', by.y = "row.names")
map <- select(map, id, long, lat, order, group, country_code=iso_a3, country_name=name, region=continent, subregion=subregion)

rhg_cols <- rev(c("#771C19","#AA3929","#E25033","#F27314","#F8A31B","#E2C59F","#B6C5CC","#8E9CA3","#556670","#000000"))


alco2 <- alco[,c(3,5,7)]
datamap <- left_join(map, alco2, by="country_code")

datamap %>% filter(!is.na(alco_type)) %>%
	ggplot() +
	geom_polygon(aes(x = long, y = lat, group = group, fill=per_cent), color="grey") +
	scale_fill_gradientn(colours=rhg_cols) +
	theme_minimal() +
	facet_wrap( ~ alco_type, ncol=2)
