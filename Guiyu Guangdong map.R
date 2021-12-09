require(OpenStreetMap)
require(proj4)
require(beepr)
# cities <- read.csv(file="cities.csv")
# styles are: mapbox://styles/mapbox/streets-v11
# mapbox://styles/mapbox/outdoors-v11
# mapbox://styles/mapbox/light-v10
# mapbox://styles/mapbox/dark-v10
# mapbox://styles/mapbox/satellite-v9
# mapbox://styles/mapbox/satellite-streets-v11

# make map ####
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
Guiyu.osm <- openmap(c(23.4, 116.31), c(23.3, 116.39),
                     type=paste0(baseUrl,apiKey), zoom=13)
beep()
Guiyu.LL <- openproj(Guiyu.osm)
# Guiyu.rob <- openproj(Guiyu.osm, projection=
#                            "+proj=robin +lon_0=155e +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") # , projection="+proj=utm +zone=50 +south"
beep(sound = 2)
par(mar = c(4,4,1,1), mgp = c(1.6, 0.3, 0), tcl = 0.3, lwd = 1)
plot(Guiyu.LL, removeMargin = FALSE)
box()
axis(1)
axis(2)
#
#
# plot the map ####
png(file = "Guiyu.png", height = 2000, width = 2000)
par(mar = c(10,10,4,4), mgp = c(7, 2, 0), tcl = -1.5, lend="square", 
    font.lab = 2, lwd = 2)
plot(Guiyu.LL, removeMargin = FALSE)
axis(1, cex.axis = 3)
axis(2, cex.axis = 3)
points(cities$lat ~ cities$long, col = "#00000080", pch=19, cex=1.5, 
       subset = cities$population>1e6)
# symbols(cities$long, cities$lat, 
#         circles = 2e-5 * sqrt(cities$population), add=TRUE, 
#         inches = FALSE, fg = "red3", bg = "#C0000040", lwd = 3)
dev.off()
# should default back to RStudio graphics device but optionally set it manually
options(device = "RStudioGD")
getOption('device')
