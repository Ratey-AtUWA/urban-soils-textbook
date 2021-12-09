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
PRDelta.osm <- openmap(c(23.9, 112.2), c(21.5, 115.3),
                     type=paste0(baseUrl,apiKey), zoom=10)
beep()
plot(PRDelta.osm)
PRdelta.LL <- openproj(PRDelta.osm)
beep()
# PRDelta.rob <- openproj(PRDelta.osm, projection=
#                            "+proj=robin +lon_0=155e +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") # , projection="+proj=utm +zone=50 +south"
beep(sound = 9)
par(mar = c(4,4,1,1), tcl = -1, lwd = 1)
plot(PRdelta.LL, removeMargin = FALSE)
box()
axis(1)
axis(2)
#
#
# plot the map ####
png(file = "PRdelta.png", height = 2000, width = 2000)
par(mar = c(10,10,4,4), mgp = c(7, 2, 0), tcl = -1.5, lend="square", 
    font.lab = 2, lwd = 2)
plot(PRdelta.LL, removeMargin = FALSE)
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
