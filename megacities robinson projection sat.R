require(OpenStreetMap)
require(proj4)
cities <- read.csv(file="cities.csv")
# make map ####
# apiKey <- paste0("?access_token=",
#                  "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
# baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
# world.osm <- openmap(c(80, -179.999), c(-80, 179.999),
#                      type=paste0(baseUrl,apiKey), zoom=3)
# beep(sound = 3)
# world_sat_rob <- openproj(world.osm, projection=
#                            "+proj=robin +lon_0=155e +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") # , projection="+proj=utm +zone=50 +south"
# beep(sound = 5)
# covert coordinates to new projection using proj4::project() ####
coords_ll <- cbind(cities$long, cities$lat)
coords_rob <- project(coords_ll, 
  "+proj=robin +lon_0=155e +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
cities$x_robinson <- coords_rob[,1]
cities$y_robinson <- coords_rob[,2]
rm(list = c("world.osm","coords_ll","coords_rob")) # remove temp objects

# plot the map ####
png(file = "world cities gt_1e6 sat_robin.png", height = 1826, width = 3840)
par(mar = c(1,1,1,1), mgp = c(2, 0.3, 0), tcl = 1, lend="square", 
    font.lab = 2)
plot(world_sat_rob)
points(cities$x_robinson, cities$y_robinson, col = "white", pch=19, cex=0.5)
symbols(cities$x_robinson, cities$y_robinson, 
        circles = 100 * sqrt(cities$population), add=TRUE, 
        inches = FALSE, fg = "white", bg = "#FFFFFF60", lwd = 3)
text(4e6, -1e6, labels = c("Population"), pos = 4, col = "white", 
     font = 4, cex = 4)
symbols(c(4e6, 4e6), c(-2e6, -3e6), circles = 100 * sqrt(c(1e7, 1e6)), 
        add=TRUE, inches = FALSE, fg = "white", bg = "#FFFFFF60", lwd = 3)
text(c(4e6, 4e6), c(-2e6, -3e6), 
     labels = c("10,000,000", "1,000,000"),
     col = "white", pos = 4, offset = 3.6, cex = 4)
dev.off()
# should default back to RStudio graphics device but optionally set it manually
options(device = "RStudioGD")
getOption('device')
