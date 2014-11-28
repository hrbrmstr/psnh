library(sp)
library(rgdal)
library(dplyr)
library(rvest)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(pbapply)

counties <- html("http://www3.cmpco.com/OutageReports/CMP.html") %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]]

counties <- counties[4:(nrow(counties)-1),]
colnames(counties) <- c("name", "total_customers", "without_power")
counties$total_customers <- as.numeric(gsub(",", "", counties$total_customers))
counties$without_power <- as.numeric(gsub(",", "", counties$without_power))

towns <- rbindlist(pblapply(counties$name, function(x) {

  county <- html(sprintf("http://www3.cmpco.com/OutageReports/CMP%s.html", x)) %>%
    html_nodes("table") %>%
    html_table() %>%
    .[[1]]

  county <- county[4:(nrow(county)-1),]
  colnames(county) <- c("id", "total_customers", "without_power")
  county$total_customers <- as.numeric(gsub(",", "", county$total_customers))
  county$without_power <- as.numeric(gsub(",", "", county$without_power))

  county

}))

me <- readOGR("data/metowns/METOWNS_POLY.shp", "METOWNS_POLY")

towns$out <- cut(towns$without_power,
    breaks=c(0, 25, 100, 500, 1000, 5000, 10000, 20000, 40000, 100000),
    labels=c("1 - 25", "26 - 100", "101 - 500", "501 - 1,000",
             "1,001 - 5,000", "5,001- 10,000", "10,001 - 20,000",
             "20,001 - 40,000", "41,000 - 100,000"))

me_map <- fortify(me, region="TOWN")
me_map$id <- toupper(me_map$id)

gg <- ggplot(data=me_map, aes(map_id=id))
gg <- gg + geom_map(map=me_map, aes(x=long, y=lat),
                    color="#0e0e0e", fill="white", size=0.2)
gg <- gg + geom_map(data=towns, map=me_map, aes(fill=out),
                    color="#0e0e0e", size=0.2)
gg <- gg + scale_fill_brewer(type="seq", palette="RdPu",
                             name="Number of\ncustomer outages\nin each town")
gg <- gg + coord_equal()
gg <- gg + labs(title=sprintf("%s Total CMP Customers Without Power",
                              comma(sum(towns$without_power))))
gg <- gg + theme_map() + theme(legend.position="right")
gg


