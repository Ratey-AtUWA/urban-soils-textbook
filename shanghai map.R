require(OpenStreetMap)
require(prettymapr)
# require(astro)
# outdoors-v11 satellite-v9 streets-v11, satellite-streets-v11, light-v10, dark-v10,
# navigation-preview-day-v4, navigation-preview-night-v4, navigation-guidance-day-v4,
# navigation-guidance-night-v4
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/navigation-guidance-day-v4/tiles/256/{z}/{x}/{y}"
#
shanghai.osm <- openmap(c(31.3657,121.3058),
                          c(31.0756,121.6409),
                         type=paste0(baseUrl,apiKey), zoom=12.)
plot(shanghai.osm, removeMargin = F)
shanghai2.utm <- openproj(shanghai.osm, projection="+proj=utm +zone=60 +south") #
rm(shanghai.osm)