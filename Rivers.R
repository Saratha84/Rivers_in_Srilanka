libs<-c(
  "tidyverse","sf",
  "giscoR"
)

installed_libraries<-libs%in% rownames(
  installed.packages()
)

if(any(installed_libraries==F)){
  install.packages(
    libs[!installed_libraries]
  )
}

invisible(
  lapply(
    libs, library,
    character.only=T
  )
)
get_country_borders<-function(){
  country_borders<-giscoR::gisco_get_countries(
    resolution = "3",
    country = "LK"
  )
  return(country_borders)
}

country_borders<-get_country_borders()

#https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_as_lev03_v1c.zip
#https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip

get_basins<-function(){
  url<-"https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_as_lev03_v1c.zip"
  file_name<-"hybas_as_lev03_v1c.zip"
  download.file(
    url = url,
    destfile = file_name,
    mode="wb"
  )
  unzip(file_name)
}
get_basins()
list.files()

load_basins<-function(){
  filenames<-list.files(
    pattern = ".shp$",
    full.names=TRUE
  )
  south_asia_basin<-sf::st_read(
    filenames
  )
  return(south_asia_basin)
}
south_asia_basin<-load_basins()



sf::sf_use_s2(F)

sl_basin<-south_asia_basin|>
  sf::st_intersection(
    country_borders
  )|>
  dplyr::select(
    HYBAS_ID
  )

get_rivers<-function(){
  url<-"https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip"
  file_name<-"asia_rivers.zip"
  
  download.file(
    url=url,
    destfile = file_name,
    mode="wb"
  )
  unzip(file_name)
}

get_rivers()

list.files()

load_rivers<-function(){
  filenames<-list.files(
    path="HydroRIVERS_v10_as_shp",
    pattern = ".shp$",
    full.names = T
  )
  asia_rivers<-sf::st_read(
    filenames
  )
  return(asia_rivers)
}

asia_rivers<-load_rivers()

sl_rivers<-asia_rivers|>
  dplyr::select(
    ORD_FLOW
  )|>
  sf::st_intersection(
    country_borders
  )


sl_river_basin<-sf::st_intersection(
  sl_rivers,
  sl_basin
)

unique(sl_river_basin$ORD_FLOW)

sl_river_basin_width<-sl_river_basin|>
  dplyr::mutate(
    width=as.numeric(
      ORD_FLOW
    ),
    width=dplyr::case_when(
      width==4~.8,
      width==5~.7,
      width==6~.5,
      width==7~.3,
      TRUE~0
    )
  )|>
  sf::st_as_sf()


unique(
  sl_river_basin_width$HYBAS_ID
)
  
p<-ggplot()+
  geom_sf(
    data = sl_river_basin_width,
    aes(
      color=factor(
        HYBAS_ID
      ),
      size=width,
      alpha=width
    )
  )+
  geom_sf(
    data = country_borders,  # Adding country boundary outline
    fill = NA,               # Removing fill color
    color = "white",         # Setting outline color
    size = 0.1,
    alpha=0.9
  ) +
  scale_color_manual(
    name="",
    values = hcl.colors(
      1,"Dark 3",
      alpha=1
    )
  )+
  scale_size(
    range=c(.1,7)
  )+
  scale_alpha(
    range=c(.01,.8)
  )+
  theme_void()+
  theme(
    legend.position = "none",
    plot.caption = element_text(
      size=9,color="grey60",
      hjust=.1,vjust=10
    ),
    plot.margin=unit(
      c(t=0,r=0,b=0,l=0),
      "lines"
    ),
    plot.background =element_rect(
      fill = "black",
      color=NA
    ),
    panel.background =element_rect(
      fill = "black",
      color=NA
    )
  )+
  labs(
    title = "",
    x="",
    y="",
    caption = "Source: ©World Wildlife Fund, Inc. (2006-2013) \nHydroSHEDS database http://www.hydrosheds.org"
  )
  
  
ggsave(
  filename = "Sl_rivers.png",
  width = 7, height = 7.75, dpi = 600,
  bg = "white", device = "png", p
)

  
  
  
  
  








