### script download occurrences - spocc ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com 25/07/2017
# Updated by Renata Muylaert in 24/03/2020 (R 3.4.4)

###---------------------------------------------------------------------------###

## memory and set up

rm(list = ls())
gc()
memory.limit(size = 1.75e13) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## packages

install.packages(c("stringr","spocc", "data.table", "dplyr", "ggmap", "ggplot2", "ggsn", "maps", "mapdata", "ggrepel"))

P <- c("stringr","spocc", "data.table", "dplyr", "ggmap", "ggplot2", "ggsn", "maps", "mapdata", "ggrepel" )

lapply(P, library, character.only = TRUE)

# verify packages

search()

###---------------------------------------------------------------------------###

# Set region for your maps

area <- ggplot2::map_data("world", zoom=5) 

# For example, if you want a zoom in Brazil:
#area <-ggplot2::map_data("world", region="Brazil", zoom=5) 

###---------------------------------------------------------------------------###

## import list of species

sp<- c("Pygoderma bilabiatum", "Phyllostomus hastatus", "Sturnira tildae")
###---------------------------------------------------------------------------###

## bases
ba <- c("gbif", "ebird", "ecoengine", "bison", "vertnet", 
        "idigbio", "inat", "obis", "ala")

###---------------------------------------------------------------------------###

## download data

# for
for(i in sp){
  
  print(paste0("Species --", i, "--"))
  re <- occ(query = i, from = ba, has_coords = T, limit = 10000)
  
  if(length(occ2df(re)) == 0){
    print(paste0("Sorry, no data for synonymies --", j, "--"))
    
  } else{
    da <- data.table(occ2df(re)[1], 
                     sp_enm = str_to_lower(sub(" ", "_", i)), 
                     occ2df(re)[-1])
    
    colnames(da) <- c("sp", "sp_enm", "long", "lat", "base", "date", "key")
    
    da.d <- distinct(da, long, lat, .keep_all = T)
    
    da.d$long <- as.numeric(da.d$long)
    da.d$lat <- as.numeric(da.d$lat)
    
    fwrite(da.d, paste0("occurrences_spocc_", str_to_lower(sub(" ", "_", i)), ".csv"))
    
    # map
    g <- ggplot() + geom_polygon(data = area,
                                 aes(x=long, y = lat, group = group),
                                 fill = "grey", color = "lightgrey", size=0.04) + #Note que voce pode mudar as cores do fundo e da borda
      coord_fixed(1.1) + #Use isto para o mapa ficar proporcional
      geom_point(data = da.d, aes(x = long, y = lat, fill= base), 
                 shape = 21, 
                 size = 3, #Tamanho dos pontos
                 alpha = 0.6) + #Transparencia: quanto mais proximo de 1, menos transparente
      theme_bw() +
      ggtitle(i) + #De nome ao plot, caso seja necessario
      labs(x="Longitude", y = "Latitude") + #De nome aos eixos
      theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
            plot.title = element_text(size=20, hjust=0.5),
            axis.text.x = element_text(size = 10, angle=0, hjust=1),
            axis.text.y = element_text(size = 10, angle=0, vjust=1),
            axis.title.x = element_text(size = 12, angle=0),
            axis.title.y = element_text(size = 12, angle=90)) +
      #ggsn::scalebar(area, dist = 1000, location = "bottomright", transform = TRUE, #Adicione uma barra de escala
      #               dist_unit = "km", st.dist = 0.03, st.size = 2, model = 'WGS84') +
      ggsn::north(area, scale = .1) #Adicione uma seta com o norte
      # Export map
      ggsave(paste0("occurrences_map_", str_to_lower(sub(" ", "_", i)), ".tiff"), dpi = 300)
  }
}

###---------------------------------------------------------------------------###

