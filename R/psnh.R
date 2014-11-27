library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(rvest)
library(stringi)
library(scales)

# for theme_map
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")

# NH towns shapefile
nh <- readOGR("data/nhtowns/NHTOWNS_POLY.shp", "NHTOWNS_POLY")

# PSNH outage info
outage <- html("http://www.psnh.com/outagelist/") %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]]

# which we need to tweak a bit and add breaks for coloring
outage <- outage[complete.cases(outage),]
colnames(outage) <- c("id", "total_customers", "without_power", "percentage_out")
outage$id <- stri_trans_totitle(outage$id)
outage$out <- cut(outage$without_power,
    breaks=c(0, 25, 100, 500, 1000, 5000, 10000, 20000, 40000),
    labels=c("1 - 25", "26 - 100", "101 - 500", "501 - 1,000",
             "1,001 - 5,000", "5,001- 10,000", "10,001 - 20,000",
             "20,001 - 40,000"))

# for ggplot2 use
nh_map <- fortify(nh, region="NAME")

# so we can get the data we need into the map
nh_map <- merge(nh_map, outage, by="id", all.x=TRUE)

gg <- ggplot(data=nh_map, aes(map_id=id))
gg <- gg + geom_map(map=nh_map, aes(x=long, y=lat),
                    color="black", fill="white", size=0.2)
gg <- gg + geom_map(data=outage, map=nh_map, aes(fill=out),
                    color="black", size=0.2)
gg <- gg + scale_fill_brewer(type="seq", palette="RdPu",
                             name="Number of\ncustomer outages\nin each town")
gg <- gg + coord_equal()
gg <- gg + labs(title=sprintf("%s Total PSNH Customers Without Power",
                              comma(sum(outage$without_power))))
gg <- gg + theme_map() + theme(legend.position="right")
gg





