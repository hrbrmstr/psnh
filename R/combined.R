
## NOTE - This NEEDS the psnh & cmpco code to be run first
## I'll eventually combine them all into one

gg <- ggplot()
gg <- gg + geom_map(data=me_map, map=me_map, aes(x=long, y=lat, map_id=id),
                    color="#0f0f0f70", fill="white", size=0.2)
gg <- gg + geom_map(data=nh_map, map=nh_map, aes(x=long, y=lat, map_id=id),
                    color="#0e0e0e70", fill="white", size=0.2)
gg <- gg + geom_map(data=towns, map=me_map, aes(fill=out, map_id=id),
                    color="#0e0e0e", size=0.2, drop=FALSE)
gg <- gg + geom_map(data=outage, map=nh_map, aes(fill=out, map_id=id),
                    color="#0e0e0e", size=0.2, drop=FALSE)
gg <- gg + scale_fill_brewer(type="seq", palette="RdPu", drop=FALSE,
                             name="Number of\ncustomer outages\nin each town")
gg <- gg + labs(title=sprintf("%s Total CMP + PSNH Customers Without Power",
                              comma(sum(outage$without_power) + sum(towns$without_power))))
gg <- gg + coord_equal()
gg <- gg + theme_map() + theme(legend.position="right")
gg






