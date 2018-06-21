# original: https://beta.observablehq.com/@mbostock/walmarts-growth

# data wrangling
library(tidyverse)
data <- "https://gist.githubusercontent.com/mbostock/4330486/raw/fe47cd0f43281cae3283a5b397f8f0118262bf55/walmart.tsv"

walmart <- read_csv2(data) %>%
    separate(col = "0\t1\tdate", into = c("lon", "lat", "estab"), sep = "\t") %>%
    mutate(
        lon = as.numeric(lon),
        lat = as.numeric(lat),
        estab = lubridate::mdy(estab)
    ) %>%
    arrange(estab) %>%
    rowid_to_column("number")

# save data and read back in
#saveRDS(walmart, "walmart.rds")
#walmart <- readRDS("walmart.rds")

# first plot
library(maps)
library(ggthemes)
states <- map_data("state")

ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    geom_point(data = walmart, aes(x = lon, y = lat), 
               size = 1.5, alpha = 0.8, shape = 1) +
    theme_map() +
    ggtitle("Walmart's Growth")

# now animate
library(animation)
library(gganimate)

wal_ani <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme_map() +
    # plot first location
    geom_point(data = filter(walmart, number == 1), 
               aes(x = lon, y = lat), color = "blue", size = 1.5) +
    # plot all others
    geom_point(data = filter(walmart, number != 1),
               aes(x = lon, y = lat, frame = estab, cumulative = TRUE), 
               size = 1.5, alpha = 0.8, shape = 1) +
    # plot red point when added
    geom_point(data = filter(walmart, number != 1),
               aes(x = lon, y = lat, frame = estab, cumulative = FALSE), 
               size = 1.5, color = "red") +
    ggtitle("Walmart's Growth") +
    theme(plot.title = element_text(size = 18))

ani.options(ani.width = 1280, ani.height = 720, interval = 0.1)
gganimate(wal_ani, filename = "walmart.gif")

# growth over time
ggplot(walmart, aes(x = estab, y = number)) + 
    geom_line() +
    scale_y_continuous("No. of Locations", labels = scales::comma) +
    scale_x_date("") +
    ggtitle("Growth of Walmart Locations Over Time")

# round dates to nearest month
library(lubridate)
walmart <- walmart %>% 
    mutate(rdate = round_date(estab, unit = "month")) 

# join in empty months
walmart_mon <- walmart %>%
    right_join(
        as_tibble(seq(min(walmart$rdate), max(walmart$rdate), by = 'month')),
        by = c("rdate" = "value")
    ) %>%
    mutate(
        lon = ifelse(is.na(lon), 0, lon),
        lat = ifelse(is.na(lat), 0, lat)
    )

# animate with rounded dates and empty months
wal_ani_mon <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme_map() +
    # plot first location
    geom_point(data = filter(walmart_mon, number == 1), 
               aes(x = lon, y = lat), color = "blue", size = 1.5) +
    # plot all others
    geom_point(data = filter(walmart_mon, number != 1),
               aes(x = lon, y = lat, frame = rdate, cumulative = TRUE), 
               size = 1.5, alpha = 0.8, shape = 1) +
    # plot red point when added
    geom_point(data = filter(walmart_mon, number != 1),
               aes(x = lon, y = lat, frame = rdate, cumulative = FALSE), 
               size = 1.5, color = "red") +
    ggtitle("Walmart's Monthly Growth") +
    theme(plot.title = element_text(size = 18))

ani.options(ani.width = 1280, ani.height = 720, interval = 0.1)
gganimate(wal_ani_mon, filename = "walmart_mon.gif")

# notice a few big flashes
library(DT)
datatable(
    walmart %>%
        mutate(rdate = format(rdate, "%b %Y")) %>%
        count(rdate, sort = TRUE),
    colnames = c("Date", "Locations Opened"),
    options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20),
        columnDefs = list(list(className = 'dt-body-center', targets = '_all'),
                          list(className = 'dt-head-center', targets = '_all'))
    ))

# highlight "1981-07-01"
ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    geom_point(data = filter(walmart, rdate < as.Date("1981-07-01")), 
               aes(x = lon, y = lat), 
               size = 1.5, alpha = 0.8, shape = 1) +
    geom_point(data = filter(walmart, rdate == as.Date("1981-07-01")), 
               aes(x = lon, y = lat), 
               size = 1.5, color = "red", shape = 2) +
    theme_map() +
    labs(title = "Walmart's Monthly Growth up to July 1981", 
         caption = "July 1981 highlighted in red")

# plot new locations per month
mon_counts <- walmart %>%
    count(rdate) 

mon_counts %>%
    ggplot(aes(x = rdate, y = n)) + 
    geom_col() +
    scale_y_continuous("No. of Locations") +
    scale_x_date("", breaks = seq(as.Date("1965-01-01"), as.Date("2005-01-01"), by = "5 years"), labels = scales::date_format("%Y")) +
    ggrepel::geom_label_repel(
        data = filter(mon_counts, rdate == as.Date("1981-07-01")),
        aes(label = format(rdate, "%b %Y"))
    ) +
    ggtitle("Walmart Locations Opened per Month")


