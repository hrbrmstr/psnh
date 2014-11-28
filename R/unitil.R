library(sp)
library(rgdal)
library(dplyr)
library(rvest)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(pbapply)
library(httr)

unitil_pg <- GET("http://unitil.maps.sienatech.com/v1/data/unitil_outages.xml.table.js")
unitil_html <- content(unitil_pg, as="text") %>%
  gsub("document.write\\('[[:space:]]*|'\\);", "", .)
unitil <- html(unitil_html)
outage_tables <- unitil %>% html_nodes("table")
unitil_nh <- html_table(outage_tables[[4]])
unitil_nh

# which we need to tweak a bit and add breaks for coloring
unitil_nh <- unitil_nh[,1:4]
colnames(unitil_nh) <- c("id", "total_customers", "without_power", "percentage_out")
unitil_nh$id <- stri_trans_totitle(unitil_nh$id)
unitil_nh$total_customers <- as.numeric(gsub(",", "", unitil_nh$total_customers))
unitil_nh$without_power <- as.numeric(gsub(",", "", unitil_nh$without_power))
unitil_nh$out <- cut(unitil_nh$without_power,
    breaks=c(0, 25, 100, 500, 1000, 5000, 10000, 20000, 40000),
    labels=c("1 - 25", "26 - 100", "101 - 500", "501 - 1,000",
             "1,001 - 5,000", "5,001- 10,000", "10,001 - 20,000",
             "20,001 - 40,000"))
