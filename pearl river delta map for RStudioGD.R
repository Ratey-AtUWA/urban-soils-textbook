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
par(new = F, fig = c(0, 1, 0, 1), 
    mar = c(4,4,1,1), mgp = c(1.7, 0.3, 0), tcl = -1.5, 
    lend="square", ljoin = "mitre",
    font.lab = 2, lwd = 1, tcl = 0.3)
plot(PRdelta.LL, removeMargin = FALSE)
axis(1, cex.axis = 1)
axis(2, cex.axis = 1)
points(cities$lat ~ cities$long, col = "#00000080", 
       pch=19, cex=1, 
       subset = cities$population>1e6)
# symbols(cities$long, cities$lat, 
#         circles = 2e-5 * sqrt(cities$population), add=TRUE, 
#         inches = FALSE, fg = "red3", bg = "#C0000040", lwd = 3)
# # should default back to RStudio graphics device but optionally set it manually
par(new = T, fig = c(0.7, 0.99, 0.75, 0.99), 
    mar = c(1,1,1,1), lend = "round", ljoin = "round")
plot(china, type = "l", asp =1.2, lwd=1, bty = "n",
     xaxt="n", yaxt = "n", xlab = "", ylab=NA)
polygon(china$Long, china$Lat, border = 1, col = "pink", lwd = 2)
rect(112.186, 21.29,115.313, 23.58, border = "red3", lwd = 2, 
     lend = "square", ljoin = "mitre")
text(101,28.5,labels = "Pearl R.\nDelta \u2193", 
     col = "red3", cex = 0.8, pos = 4, offset=0)
text(99,38,labels="P. R. China", font = 3)
