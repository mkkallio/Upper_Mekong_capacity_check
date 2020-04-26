# need hydrostreamer 0.5 currently only in dev branch of the github repo
# remotes::install_github("mkkallio/hydrostreamer", ref = "dev")

library(sf)
library(raster)
library(dplyr)
library(hydrostreamer) 
library(lubridate)
library(units)
library(ggplot2)
library(gghighlight)
library(patchwork)
library(boot)

# define helper function to get mean and bootstrapped 95% conf intervals
meanci <- function(x) {
    if(inherits(x, "units")) {
        unit <- deparse_unit(x)
        boots <- boot(drop_units(x), 
                      function(x, index) mean(x[index]), R = 999) 
    } else {
        boots <- boot(x, function(x, index) mean(x[index]), R = 999) 
    }
    ci <- boot.ci(boots)
    
    out <- c(mean = boots$t0, low = ci$basic[4], high = ci$basic[5])
    if(inherits(x, "units")) out <- as_units(out, unit)
    
    return(out)
}


# ------------------------------------------------------------------------------
# Lancang dams
dams <- readxl::read_xlsx("data/lancang_dams_storage.xlsx")

dams %>% 
    group_by(STATUS) %>%
    summarise(storage = sum(STORAGE, na.rm=TRUE),
              n = n(),
              nas = sum(is.na(STORAGE)))

dams %>% 
    filter(STATUS == "COMM",
           !is.na(STORAGE)) %>%
    arrange(-STORAGE) %>%
    mutate(STORAGE = STORAGE/1000) %>%
    ggplot() +
    geom_col(aes(reorder(NAME, -STORAGE), STORAGE)) +
    labs(x = "", y = "Total storage, cubic km") + 
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks = element_blank())

ggsave(filename = "Dam_capacity.pdf")


# From Räsänen et al, 2017, https://doi.org/10.1016/j.jhydrol.2016.12.023

active_storage <- 0.12 + 9.9 + 0.26 + 0.37 + 12.3 + 0.25
total_storage_low <- 0.316 + 14.56 + 0.92 + 0.89 + 21.7 + 1.14
total_storage_high <- 0.51 + 15 + 0.92 + 0.94 + 23.7 + 1.23

estimated_share <- c(active_storage / total_storage_low,
                     active_storage / total_storage_high)

active_storage <- 46.4 * estimated_share



# ------------------------------------------------------------------------------
# ESTIMATE RUNOFF GRUN


# load data
lcg <- read_sf("data/lancang_above_jinghong.gpkg") %>%
    st_union() %>%
    st_sf() %>%
    mutate(riverID = 1,
           basin = "Lancang")
csn <- read_sf("data/mekong_above_chiang_saen.gpkg") %>%
    st_union() %>%
    st_sf() %>%
    mutate(riverID = 2,
           basin = "Chiang Saen")
mekong <- rbind(lcg, csn)


# GRUN
grun <- brick("../Runoff data/grun v1/GRUN_v1_GSWP3_WGS84_05_1902_2014.nc") %>%
    crop(mekong, snap="out")
HS <- raster_to_HS(grun,
                   unit = "mm/day",
                   date = as.Date("1902-01-01"),
                   timestep = "month",
                   aoi = mekong,
                   names = "grun")
HS_grun <- interpolate_runoff(HS,
                         river = mekong)
runoff_ts <- HS_grun$runoff_ts

# estimate wet season runoff May-October
seconds <- difftime(as_date("1902-05-01"), as_date("1902-10-31"),
                    units = "secs") %>%
    abs() %>%
    as_units("s")

grun_wet_season <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%    
        as_tibble() %>% 
        mutate(year = year(Date),
               month = month(Date)) %>%
        filter(month %in% 5:10) %>%
        select(-Date) %>% 
        group_by(year) %>%
        summarise(wet_season_runoff = sum(grun)) %>%
        mutate(sec = seconds,
               grun = wet_season_runoff * (sec / 6),
               grun = set_units(grun, "km3"),
               name = HS_grun$basin[i])  %>%
        select(name, year, grun)
}) %>% do.call(rbind, .)
    
# estimate annual runoff
grun_annual <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%
    as_tibble() %>% 
    mutate(year = year(Date)) %>%
    select(-Date) %>% 
    group_by(year) %>%
    summarise(annual_runoff = sum(grun)) %>%
    mutate(sec = as_units(3600*24*365, "s"),
           grun = annual_runoff * (sec/12),
           grun = set_units(grun, "km3"),
           name = HS_grun$basin[i])  %>%
        select(name, year, grun)
}) %>% do.call(rbind, .)

# cumulative wet season runoff
grun_cumul <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%      
        as_tibble() %>% 
        mutate(year = year(Date),
               month = month(Date)) %>%
        filter(month %in% 5:10) %>%
        select(-Date) %>% 
        group_by(year) %>%
        mutate(sec = seconds,
               grun = cumsum(grun),
               grun = grun * (seconds/6)) %>% 
        ungroup() %>% 
        mutate(grun = set_units(grun, "m3"),
               grun = set_units(grun, "km3"),
               name = HS_grun$basin[i]) %>%
        select(name, year, month, grun)
}) %>% do.call(rbind, .)


# -----------------------------------------------------------------------------
# SECOND ESTIMATE LORA

lora <- brick("../Runoff data/lora 1.0/lora combined.tif") %>%
    crop(mekong, snap="out")
HS <- raster_to_HS(lora,
                   unit = "mm/s",
                   date = as.Date("1980-01-01"),
                   timestep = "month",
                   aoi = mekong,
                   names = "lora")
HS_lora <- interpolate_runoff(HS,
                         river = mekong)
runoff_ts <- HS_lora$runoff_ts

# estimate wet season flow may-oct
lora_wet_season <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%    
        as_tibble() %>% 
        mutate(year = year(Date),
               month = month(Date)) %>%
        filter(month %in% 5:10) %>%
        select(-Date) %>% 
        group_by(year) %>%
        summarise(wet_season_runoff = sum(lora)) %>%
        mutate(sec = seconds,
               lora = wet_season_runoff * (sec / 6),
               lora = set_units(lora, "km3"),
               name = HS_lora$basin[i])  %>%
        select(name, year, lora)
}) %>% do.call(rbind, .)

# estimate annual runoff
lora_annual <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%
        as_tibble() %>% 
        mutate(year = year(Date)) %>%
        select(-Date) %>% 
        group_by(year) %>%
        summarise(annual_runoff = sum(lora)) %>%
        mutate(sec = as_units(3600*24*365, "s"),
               lora = annual_runoff * (sec/12),
               lora = set_units(lora, "km3"),
               name = HS_lora$basin[i])  %>%
        select(name, year, lora)
}) %>% do.call(rbind, .)

# estimate cumulative wet season runoff
lora_cumul <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%    
        as_tibble() %>% 
        mutate(year = year(Date),
               month = month(Date)) %>%
        filter(month %in% 5:10) %>%
        select(-Date) %>% 
        group_by(year) %>%
        mutate(sec = seconds,
               lora = cumsum(lora),
               lora = lora * (seconds/6)) %>% 
        ungroup() %>% 
        mutate(lora = set_units(lora, "m3"),
               lora = set_units(lora, "km3"),
               name = HS_lora$basin[i]) %>%
    select(name, year, month, lora)
})%>% do.call(rbind, .)

# ------------------------------------------------------------------------------
# ISIMIP 2a RUNOFF ESTIMATE

# prepare rasters and get model names and forcing data
rasters <- list.files("../RUNOFF DATA/ISIMIP 2a varsoc/", full.names = TRUE)

remove <- grepl("_watch_", rasters)
rasters <- rasters[!remove]
remove <- grepl(".aux.", rasters)
rasters <- rasters[!remove]
#fcast_model <- vector("character", length(rasters))
#fcast_reanalysis <- vector("character", length(rasters))
fcast_names <- vector("character", length(rasters))
fcast_startdate <- vector("character", length(rasters))
for (i in seq_along(rasters)) {
    temp <- basename(rasters[[i]])
    fcast_names[[i]] <- stringr::word(temp,c(1,2),sep = "_") %>% 
        glue::glue_collapse(sep="_")
    fcast_startdate[[i]] <- paste0(stringr::word(temp,9,sep = "_"),"-01-01" )
}

# hydrostreamer
system.time({
    HSgrid <- raster_to_HS(rasters, 
                           unit = "mm/s",
                           date = lubridate::ymd(fcast_startdate),
                           timestep="month",
                           aoi = mekong,
                           names = fcast_names)
}) # 390 sec
HS_isimip <- interpolate_runoff(HSgrid,
                         river = mekong,
                         basins = mekong,
                         verbose = TRUE)
runoff_ts <- HS_isimip$runoff_ts

# estimate wet season flow may-october
isimip_wet_season <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%
        as_tibble() %>% 
        mutate(year = year(Date),
               month = month(Date)) %>%
        filter(month %in% 5:10) %>%
        select(-Date, -month) %>% 
        group_by(year) %>%
        summarise_all(sum) %>%
        mutate(name = HS_isimip$basin[i]) %>%
        select(name, year, everything())
}) %>% do.call(rbind, .)

isimip_wet_season <- lapply(1:ncol(isimip_wet_season), function(i) {
    if(i %in% 1:2) return(pull(isimip_wet_season[,i]))
    
    ts <- pull(isimip_wet_season[,i])
    ts <- ts * seconds / 6
    ts <- set_units(ts, "km3")
    
    return(ts)
}) %>% do.call(cbind, .) %>% 
    as_tibble()
names(isimip_wet_season) <- c("name","year", fcast_names)

# set units
isimip_wet_season[,2] <- as.numeric(pull(isimip_wet_season[,2]))
for(i in 3:ncol(isimip_wet_season)) {
    isimip_wet_season[,i] <- pull(isimip_wet_season[,i]) %>% 
        as.numeric() %>% 
        as_units("km3")
}

# estimate annual runoff
isimip_annual <- lapply(seq_along(runoff_ts), function(i) {
    runoff_ts[[i]] %>%
    as_tibble() %>% 
    mutate(year = year(Date)) %>%
    select(-Date) %>% 
    group_by(year) %>%
    summarise_all(sum) %>%
        mutate(name = HS_isimip$basin[i]) %>%
        select(name, year, everything())
}) %>% do.call(rbind, .)

isimip_annual <- lapply(1:ncol(isimip_annual), function(i) {
    if(i %in% 1:2) return(pull(isimip_wet_season[,i]))
    
    ts <- pull(isimip_annual[,i])
    ts <- ts *  as_units(3600*24*365, "s") / 12
    ts <- set_units(ts, "km3")
    
    return(ts)
}) %>% do.call(cbind, .) %>% 
    as_tibble()
names(isimip_annual) <-  c("name","year", fcast_names)

# set units
isimip_annual[,2] <- as.numeric(pull(isimip_annual[,2]))
for(i in 2:ncol(isimip_annual)) {
    isimip_annual[,i] <- pull(isimip_annual[,i]) %>% 
        as.numeric() %>% 
        as_units("km3")
}

# estimate cumulative wet season runoff
isimip_cumul <- lapply(seq_along(runoff_ts), function(i) {
    x <- runoff_ts[[i]] 
    
    x <- lapply(2:ncol(x), function(ii) {
        nm <- colnames(x)[ii]
        x[,c(1, ii)] %>%
            as_tibble() %>% 
            mutate(year = year(Date),
                   month = month(Date)) %>%
            filter(month %in% 5:10) %>%
            select(-Date) %>% 
            rename(isimip = !!nm) %>% 
            group_by(year) %>% 
            mutate(sec = seconds,
                   isimip_cumul = cumsum(isimip),
                   isimip_cumul = isimip_cumul * (seconds/6)) %>% 
            ungroup() %>% 
            mutate(isimip_cumul = set_units(isimip_cumul, "m3"),
                   isimip_cumul = set_units(isimip_cumul, "km3"),
                   name = HS_isimip$basin[i]) %>%
            select(name, year, month, isimip_cumul) %>% 
            rename(!!nm := isimip_cumul)
    })
    for(ii in seq_along(x)) {
        if(ii == 1) {
            isimip <- x[[1]]
        } else {
            isimip <- left_join(isimip, x[[ii]], by=c("name", "year", "month"))
        }
    }
    return(isimip)
}) %>% do.call(rbind, .)



# ------------------------------------------------------------------------------
# COMBINE ESTIMATES AND ANALYSE

all <- left_join(grun_wet_season, lora_wet_season, by=c("year", "name")) %>% 
    left_join(isimip_wet_season, by=c("year", "name")) 
all <- all[complete.cases(all),]
em <- t(apply(all[,-c(1:2)], 1, meanci))
all <- mutate(all,
              ensemble_mean = em[,1],
              ci_low = em[,2],
              ci_high = em[,3])


all_cumul <- left_join(grun_cumul, lora_cumul, by=c("name", "year", "month")) %>% 
    left_join(isimip_cumul, by=c("year", "name", "month")) 
all_cumul <- all_cumul[complete.cases(all_cumul),]
em <- t(apply(all_cumul[,-c(1:2)], 1, meanci))
all_cumul <- mutate(all_cumul,
              ensemble_mean = em[,1],
              ci_low = em[,2],
              ci_high = em[,3])

all_annual <- left_join(grun_annual, lora_annual, by=c("year", "name")) %>% 
    left_join(isimip_annual, by=c("year", "name"))

all_cumul %>% 
    filter(month == 10) %>% 
    tidyr::gather(model, volume, -year,-name, -month) %>% 
    group_by(name, model) %>%
    summarise(mean_volume = mean(volume, na.rm=TRUE)) %>% 
    arrange(name, mean_volume) %>%
    print(n = Inf)

all %>%
    tidyr::gather(model, volume, -year,-name) %>% 
    group_by(name, model) %>%
    summarise(mean_volume = mean(volume, na.rm=TRUE)) %>% 
    arrange(name, mean_volume) %>%
    print(n = Inf)

p1 <- all %>% 
    filter(name == "Lancang") %>% 
    select(-ci_low, -ci_high) %>% 
    tidyr::gather(model, volume, -year, -name) %>% 
    ggplot() +
    geom_density(aes(x = volume, group = model)) +
    geom_vline(xintercept = active_storage[1], color = "darkred") +
    theme_bw() + 
    gghighlight(model == "ensemble_mean") +
    labs(x = "cubic km",
         title = paste0("Volume of Lancang runoff between months May and Oct"))

ci_data <- all_cumul %>% 
    filter(name == "Lancang") %>% 
    select(name, year, month, ci_low, ci_high) %>%
    group_by(name, month) %>% 
    summarise_all(mean, na.rm=TRUE) %>%
    select(-year) %>% 
    ungroup()
p2 <- all_cumul  %>%
    filter(name == "Lancang") %>% 
    select(-ci_low, -ci_high) %>% 
    group_by(name, month) %>% 
    summarise_all(mean, na.rm=TRUE) %>%
    select(-year) %>% 
    ungroup() %>% 
    tidyr::gather(model, cumul, -name, -month) %>%
    ggplot() +
    geom_ribbon(data = ci_data, aes(month, ymin = ci_low, ymax = ci_high),
                fill = "grey60") +
    geom_linerange(data = ci_data, aes(month, ymin = ci_low, ymax = ci_high)) +
    geom_line(aes(month, cumul, group = model)) +
    gghighlight(model  =="ensemble_mean") +
    geom_hline(yintercept = active_storage[1], color="darkred") +
    theme_bw() +
    labs(x = "Month",
         y = "cubic km",
         title = paste0("Cumulative runoff volume produced upstream of Jinghong"))

p1 + p2

#ggsave(p1+p2, filename = "figures/runoff_estimate_cumulative_update_em_ci.pdf")



# ------------------------------------------------------------------------------
# Compare to Chiang Saen flow

# ------------------------------------------------------------------------------
# Chiang Saen flow
cs_Q <- readxl::read_xlsx("data/Chiang saen daily wl and q 1960-2017.xlsx") %>% 
    mutate(Date = as_date(Date),
           Q = as_units(`Q_[m3/s]`, "m3/s")) %>%
    select(Date, Q)

# wet season mean and confidence interval
cs_wet_mean <- cs_Q %>% 
    filter(Date < as_date("1990-12-31")) %>% 
    mutate(Q = Q * as_units(3600*24, "s"),
           Q = set_units(Q, "km3"),
           year = year(Date),
           month = month(Date)) %>% 
    filter(month %in% 5:10) %>% 
    group_by(year) %>% 
    summarise(Q_wet = sum(Q)) %>% 
    pull(Q_wet) %>% 
    meanci()


cs_ann_mean <- cs_Q %>% 
    filter(Date < as_date("1990-12-31")) %>% 
    mutate(Q = Q * as_units(3600*24, "s"),
           Q = set_units(Q, "km3"),
           year = year(Date),
           month = month(Date)) %>% 
    # filter(month %in% 5:10) %>% 
    group_by(year) %>% 
    summarise(Q_ann = sum(Q)) %>%
    pull(Q_ann) %>% 
    meanci()


hs_wet_mean <- all %>%
    tidyr::gather(model, volume, -year,-name) %>% 
    group_by(name, model) %>%
    summarise(mean_volume = mean(volume, na.rm=TRUE)) %>% 
    arrange(name, mean_volume) %>%
    tidyr::pivot_wider(id_cols = model, 
                       names_from = name, 
                       values_from = mean_volume) %>%
    mutate(Basin = `Chiang Saen` + Lancang) %>% 
    {lapply(.[,-1], function(x) {meanci(x)}) %>% 
            do.call(cbind,.) %>% 
            as_tibble() %>% 
            tibble::add_column(Stat = c("mean", "low", "high"), .before=1)}


hs_ann_mean <- all_annual %>%
    tidyr::gather(model, volume, -year,-name) %>% 
    group_by(name, model) %>%
    summarise(mean_volume = mean(volume, na.rm=TRUE)) %>% 
    arrange(name, mean_volume) %>%
    tidyr::pivot_wider(id_cols = model, 
                       names_from = name, 
                       values_from = mean_volume) %>%
    mutate(Basin = `Chiang Saen` + Lancang) %>% 
    {lapply(.[,-1], function(x) {meanci(x)}) %>% 
            do.call(cbind,.) %>% 
            as_tibble() %>% 
            tibble::add_column(Stat = c("mean", "low", "high"), .before=1)}
