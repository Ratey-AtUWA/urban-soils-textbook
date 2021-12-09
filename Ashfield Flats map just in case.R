require(OpenStreetMap)
require(proj4)
require(beepr)
# The following Mapbox styles are available to all accounts using a valid access token:
#   mapbox://styles/mapbox/streets-v11
# mapbox://styles/mapbox/outdoors-v11
# mapbox://styles/mapbox/light-v10
# mapbox://styles/mapbox/dark-v10
# mapbox://styles/mapbox/satellite-v9
# mapbox://styles/mapbox/satellite-streets-v11
# mapbox://styles/mapbox/navigation-preview-day-v4
# mapbox://styles/mapbox/navigation-preview-night-v4
# mapbox://styles/mapbox/navigation-guidance-day-v4
# mapbox://styles/mapbox/navigation-guidance-night-v4
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/light-v10/tiles/256/{z}/{x}/{y}"
test.osm <- openmap(c(-31.914, 115.94), c(-31.922, 115.952),
                     type=paste0(baseUrl,apiKey), zoom = 17)
beep()
test.LL <- openproj(test.osm)
rm(test.osm)
beep(sound = 2)
par(mar = c(4,4,1,1), mgp = c(1.6, 0.3, 0), tcl = 0.3, lwd = 1)
plot(test.LL, removeMargin = FALSE)
box()
axis(1, at = seq(115.94, 115.95, 0.002),
     labels = seq(115.94, 115.95, 0.002))
mtext("Longitude (\u00B0E)", 1, 2, cex = 1.2, font = 2)
axis(2, at = seq(-31.92, -31.916, 0.002),
     labels = seq(-31.92, -31.916, 0.002))
mtext("Latitude (\u00B0S)", 2, 2, cex = 1.2, font = 2)
