require(OpenStreetMap)
cities <- read.csv(file="cities.csv")
# terrain map ####
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/outdoors-v11/tiles/256/{z}/{x}/{y}"
world.osm <- openmap(c(80, -179.999), c(-80, 179.999),
                  type=paste0(baseUrl,apiKey), zoom=4)
world.ll <- openproj(world.osm) # , projection="+proj=utm +zone=50 +south"
rm(world.osm)
# 
png(file = "world cities over 1000000.png", height = 1708, width = 3840)
par(mar = c(3,3,1,1), mgp = c(2, 0.3, 0), tcl = 1, lend="square", 
    font.lab = 2)
plot(world.ll, removeMargin=TRUE)
points(cities$lat ~ cities$long, col = "darkred", pch=19, cex=0.5)
symbols(cities$long, cities$lat, circles = 0.001 * sqrt(cities$population), add=TRUE, 
        inches = FALSE, fg = "red3", bg = "transparent", lwd = 3)
text(-140, -15, labels = c("Population"), pos = 4, col = "black", font = 4, cex = 4)
symbols(c(-140, -140), c(-25, -35), circles = 0.001 * sqrt(c(1e7, 1e6)), add=TRUE, 
        inches = FALSE, fg = "red3", bg = "transparent", lwd = 3)
text(c(-140, -140), c(-25, -35), labels = c("10,000,000", "1,000,000"),
        col = "black", pos = 4, offset = 3.6, cex = 4)
dev.off()

# satellite map ####
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
world.osm <- openmap(c(80, -179.99), c(-80, 179.99),
                   type=paste0(baseUrl,apiKey), zoom=2)
world_sat.ll <- openproj(world.osm) # , projection="+proj=utm +zone=50 +south"
rm(world.osm)
#
png(file = "world cities over 1 million.png", height = 854, width = 1920)
par(mar = c(3,3,1,1), mgp = c(2, 0.3, 0), tcl = 1, lend="square", 
     font.lab = 2)
plot(world_sat.ll, removeMargin=TRUE)
points(cities$lat ~ cities$long, col = "white", pch=19, cex=0.5)
symbols(cities$long, cities$lat, circles = 0.001 * sqrt(cities$population), add=TRUE, 
         inches = FALSE, fg = "cyan", bg = "transparent", lwd = 2)
text(-140, -15, labels = c("Population"), pos = 4, col = "white", font = 4, cex = 2)
symbols(c(-140, -140), c(-25, -35), circles = 0.001 * sqrt(c(1e7, 1e6)), add=TRUE, 
        inches = FALSE, fg = "cyan", bg = "transparent", lwd = 2)
text(c(-140, -140), c(-25, -35), labels = c("10,000,000", "1,000,000"),
     col = "white", pos = 4, offset = c(2, 0.7), cex = 2)
dev.off()
