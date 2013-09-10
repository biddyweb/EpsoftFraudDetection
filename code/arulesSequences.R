library(arulesSequences)
x <- read_baskets(con  = system.file("misc", "zaki.txt", package = 
                                         "arulesSequences"),
                  info = c("sequenceID","eventID","SIZE"))
tmp <- as(x, "data.frame")
