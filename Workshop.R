rm(list = ls())
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "raster", "sp")
lapply(x, install.packages)
lapply(x, library, character.only = TRUE) # load the required packages


lnd <- readOGR(dsn = "data", layer = "ne_10m_admin_0_countries")

head(lnd@data, n=2) #lets lok at the non-geographic attribute data
sapply(lnd@data, class)
attributes(lnd@data)
summary(lnd)

nrow(lnd) #get number of rows
ncol(lnd)

plot(lnd)

cpi_data <- read.csv("data/CPIdata.csv", stringsAsFactors = FALSE)
head(cpi_data$CPI2015)
sapply(cpi_data, class)


lnd$WB_A3 %in% cpi_data$wbcode

head(left_join(lnd@data, cpi_data)) # test it works
#get an error which is why you need to specify names
lnd@data <- left_join(lnd@data, cpi_data, by = c('WB_A3' = 'wbcode'))
head(lnd@data)

qtm(lnd, fill="CPI2015", fill.palette= "Blues" ) # plot the basic map


#gini data

gini_data <- read.csv("data/worldbankdata.csv", stringsAsFactors = FALSE)
head(gini_data, 3)
sapply(gini_data, class)

lnd@data <- left_join(lnd@data, gini_data, by = c('WB_A3' = 'CT_CD'))
head(lnd@data)

qtm(lnd, fill="GINI_2015", fill.palette= "Blues" ) # plot the basic map


#BOTH
qtm(shp = lnd, fill = c("CPI2015", "GINI_2015"), fill.palette = "Blues", ncol = 2)

#Both
qtm(shp = lnd, fill = c("CPI2015", "GINI_2015"), fill.palette = "Blues", ncol = 1)

#Both overlayed
qtm(shp = lnd, fill = "CPI2015", fill.palette = "Blues", title = "World CPI + GINI Data") +
  qtm(shp=lnd, fill= NULL, symbols.size="GINI_2015", symbols.col="purple")

#Both in tmap
tm_shape(lnd) + tm_fill("CPI2015", palette="Blues")+tm_bubbles("GINI_2015", "purple") +tm_layout(title="TEST TITLE")+tm_compass(north=0)+tm_scale_bar()

library(ggplot2)
#visualizing scatter plot
p <- ggplot(lnd@data, aes(CPI2015, GINI_2015))
p + geom_point(aes(colour = CPI2015, size = GINI_2015)) +
  geom_text(size = 2, aes(label = WB_A3))


lnddata <- lnd@data
lndnew <- fortify(lnd)

newlnd <- lnd[!(is.na(lnd$GINI_2015)),]
tm_shape(newlnd) +
  tm_fill("GINI_2015", thres.poly = 0)+
  tm_facets("Country", free.coords = TRUE, drop.units = TRUE)
