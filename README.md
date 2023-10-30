# uncontacted
Growth of isolated indigenous metapopulations

## Paper title
Scripts are for the manuscript titled 'Remote sensing evidence for population growth of isolated indigenous societies in Amazonia' by Walker RS, MV Flinn, SP Prall, MJ Hamilton. 

## Data
The exact locations of uncontacted societies is extremely sensitive. For security reasons these are not made publicly available

## Example code to arrange deforestation data and plot
```splus
library(terra)
r <- terra::rast("clippedhansenacre.tif")
y <- terra::patches(r)
rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
s <- ifel(rz < .1, NA, r)
plot(c(r,s))

library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "small", returnclass = "sf")

test_df <- terra::as.data.frame(s, xy=T)
names(test_df) <- c('x', 'y', 'year')
test_df$year[test_df$y < -9.6 & test_df$year < 16 ] <- NA
test_df$year[test_df$y < -9.6 & test_df$y > -9.7 ] <- NA
test_df$year <- test_df$year + 2000 

colSums(is.na(test_df))

test_df <- na.omit(test_df)
library(raster)
s <- rasterFromXYZ(test_df)

df <- test_df %>% 
  group_by(year) %>% 
  summarise(sumha = 0.07589 * n())
sum(df$sumha)
df

df <- df %>%
  arrange(year) %>%
  mutate(roll_sum = RcppRoll::roll_sum(sumha, 3, align = "right", na.rm = T, fill = NA)) 

summary(lm(log(roll_sum) ~ year, df[df$year >= 2015,]))

(b <- ggplot() + 
  geom_col(data=df, aes(x=year, y=sumha), color = 'red') +
  geom_point(data=df, aes(x=year, y=roll_sum), size = 2, color = 'blue') +
  geom_smooth(data=df, aes(x=year, y=roll_sum), size = 1, color = 'blue') +
  scale_x_continuous(breaks = c(2005,2010,2015,2020), limits = c(2000,2023), expand = c(0, 0))  +
  scale_y_continuous(expand = c(0, 5))  +
    ylab("Cleared area (ha)") + xlab("Year") +
  theme_cowplot() +
  geom_vline(xintercept = 2014.5, linetype=3) +
  annotate('text', x=2022, y=5, label= "NA", color = 'black', size = 4) +
  annotate('text', x=2017.5, y=115, label= "3 year sum", color = 'blue', size = 4) +
  annotate('text', x=2016, y=145, label= "Enhanced\ndetection", color = 'black', size = 4) +
  annotate('text', x=2013, y=145, label= "Version 1.0", color = 'black', size = 4) +
  annotate("text", x=2017.5, y=45, label= "Single year", color = 'red', size = 4)  )


```
