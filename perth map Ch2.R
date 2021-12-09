library(OpenStreetMap)
library(prettymapr)
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
perth.osm <- openmap(upperLeft = c(-31.94,115.84),
        lowerRight = c(-31.966,115.885),
        type = paste0(baseUrl,apiKey),
        zoom=15)
perth.ll <- openproj(perth.osm)
par(mgp=c(1.7,0.3,0), font.lab=2, tcl=0.3)
plot(perth.ll, removeMargin = F);axis(1);axis(2);box()
addnortharrow(border = "white",
              text.col = "white")
addscalebar(label.col = "white",
            linecol = "white",
            plotepsg = 4326)

AFR_wide.osm <- openmap(upperLeft = c(-31.903,115.924),
                        lowerRight = c(-31.9239, 115.9654),
                        type=paste0(baseUrl,apiKey),
                        zoom=15)
AFR_wide.utm <- openproj(AFR_wide.osm,
                         projection = "+proj=utm +zone=50 +south")
rm(AFR_wide.osm)
# palette(c("black", "mediumpurple1", "dodgerblue", "darkseagreen2", "#C9E39E",
#           "khaki", "darkorange", "tomato", "red2", "white",
#           "transparent", "gray", "yellow","#00000080"))

