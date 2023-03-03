setwd("C:/Users/walkerro/Desktop/R scripts/uncont")
fires <- read.csv('firesclean.csv')
fires$yr <- as.numeric(gsub(".*/","",fires$acq_date))
fires$yr[fires$latitude < -9.6 & fires$yr < 2016 ] <- NA
fires$yr[fires$latitude < -9.57 & fires$latitude > -9.7 ] <- NA
colSums(is.na(fires))
fires <- na.omit(fires)

library(dplyr)
afires <- fires[fires$site == 'acre',] %>% 
  group_by(yr) %>% 
  summarise(sumfires = n())
afires <- rbind(afires,c(2005, 1))
afires <- rbind(afires,c(2015, 1))

library(RcppRoll)
afires <- afires %>%
  arrange(yr) %>%
  #group_by(yr) %>%
  mutate(roll_sum = RcppRoll::roll_sum(sumfires, 3, align = "right", na.rm = T, fill = NA)) 
sum(afires$sumfires)
afires

library(ggplot2);library(cowplot)
#afires$yr <- afires$yr - 2000
#add_nls <- nls(sumfires ~ a*exp(r*yr), data=afires, start = list(a = 0.5, r = 0.2))
#coef(add_nls)
#plot(afires$yr, resid(add_nls))
#abline(h = 0, lty = 2)

#m1 acre fire model 1 yr
library(brms);library(rstan);options(mc.cores = parallel::detectCores());rstan_options(auto_write = TRUE)
m1 <- brm(data = afires, family = gaussian(link = "identity"),
         log(sumfires) ~ 1  + yr ,# + (1|phyla),
         prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
         iter =  2e3, chains = 4, cores = 4, #save_all_pars = TRUE,
         control = list(adapt_delta = .999, max_treedepth = 20),
         seed = 14, backend = "cmdstanr")
prior_summary(m1)
m1 #.11
pl <- plot(m1, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m1)
bayes_R2(m1) #.44
conditional_effects(m1, points=T)
#saveRDS(m1,"m1.Rds")
m1 <- readRDS("m1.Rds") #ranef(m1)

mult_lm <- lm(log(sumfires) ~ yr, data=afires)
coef(mult_lm) #exp(mult_lm$coefficients[1])
plot(afires$yr, resid(mult_lm))
abline(h = 0, lty = 2)
#.12
log(2)/(coef(mult_lm)[2])

#m2 acre fire model 3 yr
m2 <- brm(data = afires, family = gaussian(link = "identity"),
          log(roll_sum) ~ 1  + yr ,# + (1|phyla),
          prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
          iter =  2e3, chains = 4, cores = 4, #save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, backend = "cmdstanr")
prior_summary(m2)
m2 #.13
pl <- plot(m2, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m2)
bayes_R2(m2) #.82
conditional_effects(m2, points=T)
#saveRDS(m2,"m2.Rds")
m2 <- readRDS("m2.Rds") #ranef(m1)

mult_lm2 <- lm(log(roll_sum) ~ yr, data=afires)
coef(mult_lm2) #exp(mult_lm$coefficients[1])
#.123
log(2)/(coef(mult_lm2)[2])

log.model <-lm(log(sumfires) ~ yr, afires)
#exp.model <-lm(sumfires ~ exp(yr), afires[afires$sumfires > 0,])
summary(log.model2 <-lm(log(roll_sum) ~ yr, afires))
log.model.df <- data.frame(x = afires$yr, y = exp(fitted(log.model)))
log.model.df2 <- data.frame(x = afires$yr, y = c(NA, NA, exp(fitted(log.model2))))

(a <- ggplot() + 
  geom_col(data=afires, aes(x=yr, y=sumfires), color = 'red') +
  geom_point(data=afires, aes(x=yr, y=roll_sum), size = 3, color = 'blue') +
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, color = 'red') +
  geom_line(data = log.model.df2, aes(x, y, color = "Log Model2"), size = 1, linetype = 2, color = 'blue') +
    scale_x_continuous(breaks = c(2005,2010,2015,2020), limits = c(2000,2023), expand = c(0, 0))  +
    #guides(fill = 'none')  +
  ylab("Fires") + xlab("Year") +
  theme_cowplot() +
  #theme(legend.position="none") 
  annotate('text', x=2017.5, y=34, label= "3 year sum", color = 'blue', size = 4) +
  annotate("text", x=2017.5, y=14.5, label= "Single year", color = 'red', size = 4) 
  )
  #guides(color = guide_legend(""))

#acre deforest
r <- terra::rast("clippedhansenacre.tif")
r

library(terra) # cellSize(r, unit="ha") = 0.07589 average
y <- terra::patches(r)
rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
s <- ifel(rz < .1, NA, r)
plot(c(r,s))
#n <- setValues(s, test_df$year)

library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "small", returnclass = "sf")

#library(maps)
#world <- map('world', interior = F, add = T)
#world <- map_data("world")

#test_spdf <- as(s, "SpatialPixelsDataFrame")
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
  #group_by(yr) %>%
  mutate(roll_sum = RcppRoll::roll_sum(sumha, 3, align = "right", na.rm = T, fill = NA)) 

#fires + deforest correlations
merge <- merge(afires, df, by.x = "yr", by.y = "year")
cor(merge$roll_sum.y, merge$roll_sum.x, use = "pairwise.complete.obs")
cor(merge$sumfires, merge$sumha, use = "pairwise.complete.obs")


#m3 acre deforest model 1 yr
m3 <- brm(data = df, family = gaussian(link = "identity"),
          log(sumha) ~ 1  + year ,# + (1|phyla),
          prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
          iter =  2e3, chains = 4, cores = 4, #save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, backend = "cmdstanr")
prior_summary(m3)
m3 #.12
pl <- plot(m3, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m3)
bayes_R2(m3) #.74
conditional_effects(m3, points=T)
#saveRDS(m3,"m3.Rds")
m3 <- readRDS("m3.Rds") #ranef(m1)

summary(mult_lm <- lm(log(sumha) ~ year, data=df))
coef(mult_lm) #exp(mult_lm$coefficients[1])
plot(df$year, resid(mult_lm))
abline(h = 0, lty = 2)
#.12
log(2)/(coef(mult_lm)[2])

#m4 acre deforest model 3 yr
m4 <- brm(data = df, family = gaussian(link = "identity"),
          log(roll_sum) ~ 1  + year ,# + (1|phyla),
          prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
          iter =  2e3, chains = 4, cores = 4, #save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, backend = "cmdstanr")
prior_summary(m4)
m4 #.12
pl <- plot(m4, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m4)
bayes_R2(m4) #.89
conditional_effects(m4, points=T)
#saveRDS(m4,"m4.Rds")
m4 <- readRDS("m4.Rds") #ranef(m1)

summary(mult_lm2 <- lm(log(roll_sum) ~ year, data=df))
coef(mult_lm2) #exp(mult_lm$coefficients[1])
#.12
#log(2)/log((1+coef(mult_lm2)[2]))
#1/log2(1+coef(mult_lm2)[2])

summary(log.model <-lm(log(sumha) ~ year, df))
#exp.model <-lm(sumfires ~ exp(yr), afires[afires$sumfires > 0,])
log.model2 <-lm(log(roll_sum) ~ year, df)
log.model.df <- data.frame(x = df$year, y = exp(fitted(log.model)))
log.model.df2 <- data.frame(x = df$year, y = c(NA, NA, exp(fitted(log.model2))))

(b <- ggplot() + 
  geom_col(data=df, aes(x=year, y=sumha), color = 'red') +
  geom_point(data=df, aes(x=year, y=roll_sum), size = 3, color = 'blue') +
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, color = 'red') +
  geom_line(data = log.model.df2, aes(x, y, color = "Log Model2"), size = 1, linetype = 2, color = 'blue') +
  #guides(fill = 'none')  +
  scale_x_continuous(breaks = c(2005,2010,2015,2020), limits = c(2000,2023), expand = c(0, 0))  +
  ylab("Cleared area (ha)") + xlab("Year") +
  theme_cowplot() +
  #theme(legend.position="none") 
  annotate('text', x=2017.5, y=115, label= "3 year sum", color = 'blue', size = 4) +
  annotate("text", x=2017.5, y=45, label= "Single year", color = 'red', size = 4)  )
#guides(color = guide_legend(""))

s <- aggregate(s, fact=10, fun="modal")
test_df <- as.data.frame(s, xy=T)
names(test_df) <- c('x', 'y', 'year')

library(tidyterra)
(c <- ggplot(world) +
  #geom_map(
  #  data = world, map = world,
  #  aes(long, lat, map_id = region),
  #      color = "lightgray", fill = "lightgray", size = 0.1)+
  geom_sf(fill="gray95") + 
  annotate('text', x=-71.44, y=-9.3, label= "1", size = 4) +
  annotate('text', x=-71.48, y=-9.35, label= "2", size = 4) +
  annotate('text', x=-71.51, y=-9.4, label= "3", size = 4) +
  annotate('text', x=-71.56, y=-9.45, label= "4", size = 4) +
  annotate('text', x=-71.63, y=-9.5, label= "5", size = 4) +
  annotate('text', x=-71.58, y=-9.85, label= "6", size = 4) +
  #geom_tile(data = test_df2 , mapping = aes(x = x, y = y, fill=forest, color=forest), alpha = .7, size = 1) +
  #scale_color_manual(values=c("red","green" ), name="") + 
  #geom_tile(data = test_df, mapping = aes(x = x, y = y, color=year, fill=year), alpha = 1, size =1) +
  #geom_spatraster(data = s, mapping = aes(fill=clippedhansenacre)) +
  geom_raster(data = test_df, mapping = aes(x=x, y=y, color=year, fill=year)) +
  #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
  #scale_fill_viridis_c(option="C", name = "") + #A - E...
  #scale_color_manual(values=c("red","green" ), name="Manioc") + 
  scale_fill_viridis_b(option="C", name = "Year", direction =-1, na.value = NA) + #A - E...
  #geom_point(data = fires, aes(x = longitude, y = latitude), alpha = 1, size = 1, color='red') +
  coord_sf(xlim = c(-71.8, -71.3), ylim = c(-9.9, -9.25), expand = FALSE) +
  guides(fill = 'none')  +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("") + ylab("") +
  theme_void() +
  ggtitle("Cleared area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
 )

(d <- ggplot(world) +
  #geom_map(
  #  data = world, map = world,
  #  aes(long, lat, map_id = region),
  #      color = "lightgray", fill = "lightgray", size = 0.1)+
  geom_sf(fill="gray95") + 
  #geom_tile(data = test_df2 , mapping = aes(x = x, y = y, fill=forest, color=forest), alpha = .7, size = 1) +
  scale_color_manual(values=c("red","green" ), name="") + 
  #geom_tile(data = test_df, 
  #          mapping = aes(x = x, y = y, 
  #                        color=year, 
  #                        fill=year), alpha = .6, size =1) +
  #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
  #scale_fill_viridis_c(option="C", name = "") + #A - E...
  #scale_color_manual(values=c("red","green" ), name="Manioc") + 
  geom_point(data = fires, aes(x = longitude, y = latitude, color=yr, fill=yr), alpha = 1, size = 1) +
  scale_color_viridis_b(option="C", name = "Year", direction =-1) + #A - E...
  coord_sf(xlim = c(-71.8, -71.3), ylim = c(-9.9, -9.25), expand = FALSE) +
  guides(fill = 'none')  +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("") + ylab("") +
  theme_void() +
  ggtitle("Fires") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm")) +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) )

library(ggpubr)
figure <- ggarrange(c, d,
                    #label.x = c(.1, .1, .1, .1, .1, .1), 
                    #  hjust=c(-.3, -.3), vjust=0,
                    #  label.y = .9, label.x = .3,
                    # labels = c("Stone", "Steel", "Stone", "Steel"),
                    ncol = 2, nrow = 1, align="hv")
figure
ggsave("fig2 acre.pdf", dpi=1000) #, units = "in", height = 6, width=8)

figure <- ggarrange(a, b, #c, d,
                    #label.x = c(.1, .1, .1, .1, .1, .1), 
                  #  hjust=c(-.3, -.3), vjust=0,
                  #  label.y = .9, label.x = .3,
                   # labels = c("Stone", "Steel", "Stone", "Steel"),
                    ncol = 1, nrow = 2, align="hv")
figure
ggsave("fig3 acre time.pdf", dpi=300, units = "in", height = 8, width=8)




#yanomami
r <- terra::rast("clippendhansenyan.tif")
r
plot(r)

library(terra) # cellSize(r, unit="ha") = 0.07686 average
y <- terra::patches(r)
rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
s <- ifel(rz < .1, NA, r)
plot(c(r,s))

library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "small", returnclass = "sf")

#library(maps)
#world <- map('world', interior = F, add = T)
#world <- map_data("world")

#test_spdf <- as(s, "SpatialPixelsDataFrame")
test_df <- terra::as.data.frame(s, xy=T)
names(test_df) <- c('x', 'y', 'year')
test_df$year <- test_df$year + 2000 
colSums(is.na(test_df))

test_df <- na.omit(test_df)
library(raster)
s <- rasterFromXYZ(test_df)

df <- test_df %>% 
  group_by(year) %>% 
  summarise(sumha = 0.07686 * n())
sum(df$sumha)
df <- rbind(df, c(2008,1))

df <- df %>%
  arrange(year) %>%
  #group_by(yr) %>%
  mutate(roll_sum = RcppRoll::roll_sum(sumha, 6, align = "right", na.rm = T, fill = NA)) 
df
#m5 yan deforest model 1 yr
m5 <- brm(data = df, family = gaussian(link = "identity"),
          log(sumha) ~ 1  + year ,# + (1|phyla),
          prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
          iter =  2e3, chains = 4, cores = 4, #save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, backend = "cmdstanr")
prior_summary(m5)
m5 #.09
pl <- plot(m5, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m5)
bayes_R2(m5) #.16
conditional_effects(m5, points=T)
#saveRDS(m5,"m5.Rds")
m5 <- readRDS("m5.Rds") #ranef(m1)

summary(mult_lm <- lm(log(sumha) ~ year, data=df))
coef(mult_lm) #exp(mult_lm$coefficients[1])
plot(df$year, resid(mult_lm))
abline(h = 0, lty = 2)
#.11
log(2)/(coef(mult_lm)[2])

#m6 yan deforest model 1 yr
m6 <- brm(data = df, family = gaussian(link = "identity"),
          log(roll_sum) ~ 1  + year ,# + (1|phyla),
          prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
          iter =  2e3, chains = 4, cores = 4, #save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, backend = "cmdstanr")
prior_summary(m6)
m6
pl <- plot(m6, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m6)
bayes_R2(m6) #.88
conditional_effects(m6, points=T)
#saveRDS(m6,"m6.Rds")
m6 <- readRDS("m6.Rds") #ranef(m1)

mult_lm2 <- lm(log(roll_sum) ~ year, data=df)
coef(mult_lm2) #exp(mult_lm$coefficients[1])
#.13
log(2)/log((1+coef(mult_lm2)[2]))
1/log2(1+coef(mult_lm2)[2])

log.model <-lm(log(sumha) ~ year, df)
#exp.model <-lm(sumfires ~ exp(yr), afires[afires$sumfires > 0,])
log.model2 <-lm(log(roll_sum) ~ year, df)
log.model.df <- data.frame(x = df$year, y = exp(fitted(log.model)))
log.model.df2 <- data.frame(x = df$year, y = c(rep(NA,5), exp(fitted(log.model2))))

(e <- ggplot() + 
  geom_col(data=df, aes(x=year, y=sumha), color = 'red') +
  geom_point(data=df, aes(x=year, y=roll_sum), size = 3, color = 'blue') +
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, color = 'red') +
  geom_line(data = log.model.df2, aes(x, y, color = "Log Model2"), size = 1, linetype = 2, color = 'blue') +
  #guides(fill = 'none')  +
  ylab("Cleared area (ha)") + xlab("Year") +
  theme_cowplot() +
  #theme(legend.position="none") 
  annotate('text', x=2017.5, y=32, label= "6 year sum", color = 'blue', size = 4) +
  annotate("text", x=2017.5, y=8, label= "Single\nyear", color = 'red', size = 4) 
)
#guides(color = guide_legend(""))

s <- aggregate(s, fact=4, fun="modal")
test_df <- as.data.frame(s, xy=T)
names(test_df) <- c('x', 'y', 'year')

(f <- ggplot(world) +
  geom_sf(fill="gray95") + 
  geom_raster(data = test_df, mapping = aes(x=x, y=y, color=year, fill=year)) +
  scale_fill_viridis_c(option="C", name = "", na.value = NA, direction =-1) + #A - E...
  coord_sf(xlim = c(-62.8, -62.64), ylim = c(2.4, 2.55), expand = FALSE) +
 # guides(fill = 'none')  +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("") + ylab("") +
  theme_void() +
  #ggtitle("Deforestation") +
  annotate('text', x=-62.7, y=2.451, label= "3", size = 4) +
  annotate('text', x=-62.77, y=2.54, label= "2", size = 4) +
  annotate('text', x=-62.68, y=2.47, label= "1", size = 4) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm")) +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 )

figure <- ggarrange(f, e, #c, d,
                    #label.x = c(.1, .1, .1, .1, .1, .1), 
                    #  hjust=c(-.3, -.3), vjust=0,
                    #  label.y = .9, label.x = .3,
                    # labels = c("Stone", "Steel", "Stone", "Steel"),
                    ncol = 1, nrow = 2)
figure
ggsave("fig4 yan.pdf", dpi=300, units = "in", height = 8, width=8)

#posterior plots
library(bayesplot)
color_scheme_set("red")
posterior <- as.array(m1)
plt1 <- mcmc_areas(
  posterior, 
  pars = c("b_yr"),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"#  border_size = .1
)
plt1
plot1 <- plt1 + 
  ylab("") + #xlab("Exponential growth parameter") +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=2) +
  #ylim(0,1)+
  theme_cowplot() +
  annotate('text', x=-.05, y=1.5, label= "Fires\n(Acre)", size = 4) +
  scale_y_discrete(labels = c(''), expand = c(0.0, 0.)) +
  scale_x_continuous(limits = c(-.09,.25), breaks = round(seq(0, .2, by = .1),2), expand = c(0, 0)) + 
  theme(#plot.margin = unit(c(0.,0.,0,0), "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank()) #axis.ticks.x=element_blank())
plot1

posterior <- as.array(m2)
plt2 <- mcmc_areas(
  posterior, 
  pars = c("b_yr"),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"#  border_size = .1
)
plt2
plot2 <- plt2 + 
  ylab("") + xlab("Intrinsic rate of increase") +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=2) +
  #ylim(0,1)+
  theme_cowplot() +
  annotate('text', x=-.05, y=1.5, label= "Fires 3yr\n(Acre)", size = 4) +
  scale_y_discrete(labels = c(''), expand = c(0.0, 0.)) +
  scale_x_continuous(limits = c(-.09,.25), breaks = round(seq(0, .2, by = .1),2), expand = c(0, 0)) + 
  theme(#plot.margin = unit(c(0.,0.,0,0), "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank())#  axis.ticks.x=element_blank())
plot2

color_scheme_set("blue")
posterior <- as.array(m3)
plt3 <- mcmc_areas(
  posterior, 
  pars = c("b_year"),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"#  border_size = .1
)
plt3
plot3 <- plt3 + 
  ylab("Density") + #xlab("Exponential growth parameter") +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=2) +
  #ylim(0,1)+
  theme_cowplot() +
  annotate('text', x=-.05, y=1.5, label= "Cleared area\n(Acre)", size = 4) +
  scale_y_discrete(labels = c(''), expand = c(0.0, 0.)) +
  scale_x_continuous(limits = c(-.09,.25), breaks = round(seq(0, .2, by = .1),2), expand = c(0, 0)) + 
  theme(#plot.margin = unit(c(0.,0.,0,0), "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank()) #axis.ticks.x=element_blank())
plot3

posterior <- as.array(m4)
plt4 <- mcmc_areas(
  posterior, 
  pars = c("b_year"),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"#  border_size = .1
)
plt4
plot4 <- plt4 + 
  ylab("") + #xlab("Exponential growth parameter") +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=2) +
  #ylim(0,1)+
  theme_cowplot() +
  annotate('text', x=-.045, y=1.5, label= "Cleared area 3yr\n(Acre)", size = 4) +
  scale_y_discrete(labels = c(''), expand = c(0.0, 0.)) +
  scale_x_continuous(limits = c(-.09,.25), breaks = round(seq(0, .2, by = .1),2), expand = c(0, 0)) + 
  theme(#plot.margin = unit(c(0.,0.,0,0), "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank()) #axis.ticks.x=element_blank())
plot4

posterior <- as.array(m5)
plt5 <- mcmc_areas(
  posterior, 
  pars = c("b_year"),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_size = 1,
  point_est = "mean"#  border_size = .1
)
plt5
plot5 <- plt5 + 
  ylab("") + xlab("Intrinsic rate of increase") +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=2) +
  #ylim(0,1)+
  theme_cowplot() +
  annotate('text', x=-.05, y=1.5, label= "Cleared area\n(Yanomami)", size = 4) +
  scale_y_discrete(labels = c(''), expand = c(0.0, 0.)) +
  scale_x_continuous(limits = c(-.09,.25), breaks = round(seq(0, .2, by = .1),2), expand = c(0, 0)) + 
  theme(#plot.margin = unit(c(0.,0.,0,0), "lines"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank())
plot5

posterior <- as.array(m6)
plt6 <- mcmc_areas(
  posterior, 
  pars = c("b_year"),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"#  border_size = .1
)
plt6
plot6 <- plt6 + 
  ylab("") + xlab("Intrinsic rate of increase") +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=2) +
  #ylim(0,1)+
  theme_cowplot() +
  annotate('text', x=-.045, y=1.5, label= "Cleared area 6yr\n(Yanomami)", size = 4) +
  scale_y_discrete(labels = c(''), expand = c(0.0, 0.)) +
  scale_x_continuous(limits = c(-.09,.25), breaks = round(seq(0, .2, by = .1),2), expand = c(0, 0)) + 
  theme(#plot.margin = unit(c(0.,0.,0,0), "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())
plot6

figure <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6,
                    #label.x = c(.1, .1, .1, .1, .1, .1), 
                    # hjust=c(-.3, -.3), vjust=0,
                    #label.y = 1, label.x = 2,
                    #labels = c("Fires\nAcre", "Fires 3yr", 'Deforestation', 'Deforestation 3yr',
                    #           'Deforestation\nYanomami', 'Deforestation 6yr\nYanomami'),
                    ncol = 2, nrow = 3, align="hv")
figure
ggsave("fig5 posteriors.pdf") #, units = "in", height = 6, width=8)

library(magick)
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(world) +
  #geom_map(
  #  data = world, map = world,
  #  aes(long, lat, map_id = region),
  #      color = "lightgray", fill = "lightgray", size = 0.1)+
  geom_sf(fill="gray95") + 
  #geom_tile(data = test_df2 , mapping = aes(x = x, y = y, fill=forest, color=forest), alpha = .7, size = 1) +
  #scale_color_manual(values=c("red","green" ), name="") + 
  #geom_tile(data = test_df, 
  #          mapping = aes(x = x, y = y, 
  #                        color=year, 
  #                        fill=year), alpha = .6, size =1) +
  #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
  #scale_fill_viridis_c(option="C", name = "") + #A - E...
  #scale_color_manual(values=c("red","green" ), name="Manioc") + 
  scale_color_viridis_c(option="C", name = "Year", direction =-1) + #A - E...
  #geom_point(data = test_df2, aes(x = x, y = y), alpha = 1, size = 1, color='gray') +
  
  coord_sf(xlim = c(-82, -34), ylim = c(-24, 13), expand = FALSE) +
  guides(fill = 'none')  +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("") + ylab("") +
  theme_void() +
  geom_segment(aes(x = -50, y = 4, xend = -62.72, yend = 2.455), color = "black", size = .5, arrow = arrow(length = unit(0.5,"cm"))) +
  draw_image( "yan.jpg", x = -58, y = -4.8, width = 807/30, height = 569/30) +
  geom_segment(aes(x = -50, y = -15, xend = -71.652, yend = -9.525889), color = "black", size = 0.5, arrow = arrow(length = unit(0.5,"cm"))) +
  draw_image( "picture2.png", x = -67, y = -24, width = 2989/120, height = 2189/120) +
  annotate('text', x=-61.7, y=1.3, label= "Yanomami", size = 4) +
  annotate('text', x=-71.652, y=-8.5, label= "Acre", size = 4) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm")) +
  theme(panel.background = element_rect(fill = 'lightblue', colour = 'blue'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("fig1 map.pdf", dpi=500, units = "in", height = 8, width=8)
              
              