outages <- tbl_df(rbind(outage, unitil_nh)) %>%
  group_by(id) %>%
  mutate(tc=sum(total_customers), wp=sum(without_power), pct=round((wp/tc)*100)) %>%
  select(id, without_power=pct)

outages

towns <- towns %>%
  group_by(id) %>%
  mutate(tc=sum(total_customers), wp=sum(without_power), pct=round((wp/tc)*100)) %>%
  select(id, without_power=pct)

towns

towns$out <- cut(towns$without_power,
    breaks=c(0, 15, 30, 50, 60, 75, 100, 1000),
    include.lowest=TRUE,
    labels=c("15%", "30%", "50%", "60%", "75%", "100%", ">100%"))

outages$out <- cut(outages$without_power,
    breaks=c(0, 15, 30, 50, 60, 75, 100, 1000),
    include.lowest=TRUE,
    labels=c("15%", "30%", "50%", "60%", "75%", "100%", ">100%"))

gg <- ggplot()
gg <- gg + geom_map(data=me_map, map=me_map, aes(x=long, y=lat, map_id=id),
                    color="#0f0f0f70", fill="white", size=0.2)
gg <- gg + geom_map(data=nh_map, map=nh_map, aes(x=long, y=lat, map_id=id),
                    color="#0e0e0e70", fill="white", size=0.2)
gg <- gg + geom_map(data=towns, map=me_map, aes(fill=out, map_id=id),
                    color="#0e0e0e", size=0.2, drop=FALSE)
gg <- gg + geom_map(data=outages, map=nh_map, aes(fill=out, map_id=id),
                    color="#0e0e0e", size=0.2, drop=FALSE)
gg <- gg + scale_fill_brewer(type="seq", palette="RdPu", drop=FALSE,
                             name="% of ncustomer\noutages in each town")
gg <- gg + labs(title="CMP + PSNH + Unitil Customers Without Power (% by Town)")
gg <- gg + coord_equal()
gg <- gg + theme_map() + theme(legend.position="right")
gg

ggsave("output/current-percent.svg", plot=gg, width=7.1, height=7, units="in")
