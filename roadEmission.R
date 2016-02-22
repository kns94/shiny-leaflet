library('geosphere')
library('RgoogleMaps')
library('RColorBrewer')

emission <- readRDS('emission.rds')

for(i in 1:nrow(emission)){
  mp <- midPoint(c(emission[i, 'lat1'], emission[i, 'lon1']), c(emission[i, 'lat2'], emission[i, 'lon2']))
  mp <- as.vector(mp)
  emission[i, 'latitude'] <- mp[1]
  emission[i, 'longitude'] <- mp[2]
}

emission <- subset(emission, select = -c(lat1, lon1, lat2, lon2))

#m = leaflet(emission) %>% addTiles()
#m %>% addCircleMarkers(radius = ~e_co, fill = FALSE)