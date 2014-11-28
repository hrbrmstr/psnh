library(sp)
library(rgdal)
library(dplyr)
library(rvest)
library(httr)
library(stringi)
library(scales)
library(RColorBrewer)
library(ggplot2)

# for theme_map
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")

# NH towns shapefile
# via http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/adjacent-states-town-boundaries.html
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


unitil_pg <- GET("http://unitil.maps.sienatech.com/v1/data/unitil_outages.xml.table.js")
unitil_html <- content(unitil_pg, as="text") %>%
  gsub("document.write\\('[[:space:]]*|'\\);", "", .)
unitil <- html(unitil_html)
outage_tables <- unitil %>% html_nodes("table")

# This doesn't check whether "Table #4" is really NH
# TODO: Ensure it's NH

unitil_nh <- html_table(outage_tables[[4]])
unitil_nh <- unitil_nh[1:(nrow(unitil_nh)-1),1:4]
colnames(unitil_nh) <- c("id", "total_customers", "without_power", "percentage_out")
unitil_nh$id <- stri_trans_totitle(unitil_nh$id)
unitil_nh$total_customers <- as.numeric(gsub(",", "", unitil_nh$total_customers))
unitil_nh$without_power <- as.numeric(gsub(",", "", unitil_nh$without_power))

outage <- tbl_df(rbind(outage, unitil_nh))%>%
  group_by(id) %>%
  tally(wt=without_power) %>%
  select(id, without_power=n)

outage$out <- cut(outage$without_power,
    breaks=c(0, 25, 100, 500, 1000, 5000, 10000, 20000, 40000),
    labels=c("1 - 25", "26 - 100", "101 - 500", "501 - 1,000",
             "1,001 - 5,000", "5,001- 10,000", "10,001 - 20,000",
             "20,001 - 40,000"))

# for ggplot2 use
nh_map <- fortify(nh, region="NAME")

gg <- ggplot()
gg <- gg + geom_map(data=nh_map, map=nh_map, aes(x=long, y=lat, map_id=id),
                    color="#0e0e0e", fill="white", size=0.2)
gg <- gg + geom_map(data=outage, map=nh_map, aes(fill=out, map_id=id),
                    color="#0e0e0e", size=0.2)
gg <- gg + scale_fill_brewer(type="seq", palette="RdPu", drop=FALSE,
                             name="Number of\ncustomer outages\nin each town")
gg <- gg + coord_equal()
gg <- gg + labs(title=sprintf("%s Total PSNH Customers Without Power",
                              comma(sum(outage$without_power))))
gg <- gg + theme_map() + theme(legend.position="right")
gg
