if(!require('pacman')) install.packages('pacman')
p_load(geojsonio, tidyverse,broom)

#from https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2/


url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"
spdf <- geojson_read(url, ,  what = "sp")

#subset the data to include only a few...things

spdf <- spdf[substr(spdf@data$code, 1, 2) %in%
                     c('06','83','13','30','34','11','66'),]

spdf_tidy <- tidy(spdf, region = 'code')

ggplot() + 
        geom_polygon(data = spdf_tidy, 
                     aes(x = long, y = lat, group = group)) + 
        theme_void() + 
        coord_map()

# reading in data of restaurants per region 
data <- read.table("https://www.r-graph-gallery.com/wp-content/uploads/2017/12/data_on_french_states.csv", header=T, sep=";")
head(data)

# what is the distribution of our data?

data %>% ggplot(aes(x = nb_equip)) + 
        geom_histogram(bins = 20, fill = 'skyblue', color = 'white') + 
        scale_x_log10() + 
        theme_void()

# merging our data together so that these new attributes are incorporated into
# the shapefile

spdf_tidy <- spdf_tidy %>%  left_join(., data, by = c("id" = "depcom"))

spdf_tidy$nb_equip[is.na(spdf_tidy$nb_equip)] = 0.001

# making your basic chloropleth
ggplot() + 
        geom_polygon(data = spdf_tidy, 
                     aes( fill = nb_equip, x = long, y = lat, group = group)) + 
        theme_void() + 
        coord_map()

#making an improvement
p_load(viridis)

p <- ggplot() + 
        geom_polygon(data = spdf_tidy, aes(fill = nb_equip, x = long, y = lat, group = group),
                     alpha = 0.9, size = 0) + 
        theme_void() + 
        scale_fill_viridis(trans = "log", breaks = c("1,5,10,20,50,100"), name = "Number of Restaurants",
                           guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                                 keywidth = unit(2, units = "mm"),
                                                 label.position = "bottom",
                                                 title.position = "top",
                                                 nrow = 1)) + 
        labs(title = "South of France Restaurant Concentration", 
             subtitle = "Number of restaurants per city district", 
             caption = "Data: INSEE | Creation: Yan Holitz | r-graph-gallery.com") + 
        theme(
                text = element_text(color = "#22211d"),
                plot.background = element_rect( fill = "#f5f5f2", color = NA),
                panel.background = element_rect( fill = "#f5f5f2", color = NA),
                legend.background = element_rect( fill = "f5f5f2" , color = NA),
                
                plot.title = element_text(size = 22, hjust = 0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                plot.subtitle = element_text(size = 17, hjust = 0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, t = 2, unit = "cm")),
                plot.captaion = element_text( size = 12, color = "#4e4d47", margin = margin(b = 0.3, r = -99, unit = "cm")), 
                legend.position = c(0.7, 0.09)) + 
        coord_map()
        
R
library(viridis)
p <- ggplot() +
        geom_polygon(data = spdf_fortified, aes(fill = nb_equip, x = long, y = lat, group = group) , size=0, alpha=0.9) +
        theme_void() +
        scale_fill_viridis(trans = "log", breaks=c(1,5,10,20,50,100), name="Number of restaurant", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
        labs(
                title = "South of France Restaurant concentration",
                subtitle = "Number of restaurant per city district", 
                caption = "Data: INSEE | Creation: Yan Holtz | r-graph-gallery.com"
        ) +
        theme(
                text = element_text(color = "#22211d"), 
                plot.background = element_rect(fill = "#f5f5f2", color = NA), 
                panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                
                plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
                plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
                
                legend.position = c(0.7, 0.09)
        ) +
        coord_map()

p <- ggplot() +
        geom_polygon(data = spdf_tidy, aes(fill = nb_equip, x = long, y = lat, group = group) , size=0, alpha=0.9) +
        theme_void() +
        scale_fill_viridis(trans = "log", breaks=c(1,5,10,20,50,100), name="Number of restaurant", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
        labs(
                title = "South of France Restaurant concentration",
                subtitle = "Number of restaurant per city district", 
                caption = "Data: INSEE | Creation: Yan Holtz | r-graph-gallery.com"
        ) +
        theme(
                text = element_text(color = "#22211d"), 
                plot.background = element_rect(fill = "#f5f5f2", color = NA), 
                panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                
                plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
                plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
                
                legend.position = c(0.7, 0.09)
        ) +
        coord_map()
p

