remed <- read.table("clipboard",
                    sep = "\t",
                    header = T,
                    stringsAsFactors = F)
require(wordcloud2)
wordcloud2(remed, 
           size = 0.4,
           color=rep(rev(rainbow(21, start=0.6, end=0.9, v=0.5)),2),
           fontFamily = 'Ubuntu Condensed',
           minRotation = -pi/4, maxRotation = pi/4,
           gridSize = 15)
