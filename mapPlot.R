library(data.table)
setwd("/home/bernhard/Dropbox/University of Goettingen/Semester III/Seminar Latin America/Plots")

data = read.csv("recipientCountriesDisaggregated.csv", stringsAsFactors = F)
data = as.data.table(data)

data[, (2:(ncol(data))) := lapply(.SD, as.numeric), 
           .SDcols = 2:(ncol(data))]


library(RColorBrewer)
library(maptools)
library(ggplot2)

data(wrld_simpl)

ddf = read.table(text="
                 country value
                 'United States' 10
                 'United Kingdom' 30
                 'Sweden' 50
                 'Japan' 70
                 'China' 90
                 'Germany' 100
                 'France' 80
                 'Italy' 60
                 'Nepal' 40
                 'Nigeria' 20", header=TRUE)

# Pascal had a #spiffy solution that is generally faster

plotPascal <- function() {
  
  pal <- colorRampPalette(brewer.pal(9, 'Reds'))(length(ddf$value))
  pal <- pal[with(ddf, findInterval(value, sort(unique(value))))]
  
  col <- rep(grey(0.8), length(wrld_simpl@data$NAME))
  col[match(ddf$country, wrld_simpl@data$NAME)] <- pal
  
  plot(wrld_simpl, col = col)
  
}

plotme <- function() {
  
  # align colors to countries
  
  ddf$brk <- cut(ddf$value, 
                 breaks=c(0, sort(ddf$value)), 
                 labels=as.character(ddf[order(ddf$value),]$country),
                 include.lowest=TRUE)
  
  # this lets us use the contry name vs 3-letter ISO
  wrld_simpl@data$id <- wrld_simpl@data$NAME
  
  wrld <- fortify(wrld_simpl, region="id")
  wrld <- subset(wrld, id != "Antarctica") # we don't rly need Antarctica
  
  gg <- ggplot()
  
  # setup base map
  gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
  
  # add our colored regions
  gg <- gg + geom_map(data=ddf, map=wrld, aes(map_id=country, fill=brk),  color="white", size=0.25)
  
  # this sets the scale and, hence, the legend
  gg <- gg + scale_fill_manual(values=colorRampPalette(brewer.pal(9, 'Reds'))(length(ddf$value)), 
                               name="Country")
  
  # this gives us proper coords. mercator proj is default
  gg <- gg + coord_map()
  gg <- gg + labs(x="", y="")
  gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                   panel.border = element_blank(),
                   panel.background = element_rect(fill = "transparent", colour = NA),
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   legend.position = "right")
  gg
  
}
