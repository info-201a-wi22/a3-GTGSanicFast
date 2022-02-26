trends <- read.csv("../data/incarceration_trends.csv")

library(dplyr)
library(ggplot2)
library(scales)

sum_info <- list()

trends[trends == 0] <- NA

sub_trends <- trends %>%
  # mutate(black_jail_ratio = black_jail_pop / total_jail_pop,
  #        white_jail_ratio = white_jail_pop / total_jail_pop,
  #        bl_pop_ratio = black_pop_15to64 / total_pop_15to64,
  #        wt_pop_ratio = white_pop_15to64 / total_pop_15to64) %>%
  select(year, state, county_name, total_pop, total_pop_15to64, aapi_pop_15to64,
         black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
         white_pop_15to64, total_jail_pop, white_jail_pop, black_jail_pop,
         aapi_jail_pop, latinx_jail_pop, native_jail_pop)



sum_df <- sub_trends %>%
  filter(year == 2018) %>%
  summarise(mean_bl_jail_pop_2018 = mean(black_jail_pop, na.rm = TRUE),
            mean_wt_jail_pop_2018 = mean(white_jail_pop, na.rm = TRUE),
            mean_aapi_jail_pop_2018 = mean(aapi_jail_pop, na.rm = TRUE),
            mean_nat_jail_pop_2018 = mean(native_jail_pop, na.rm = TRUE),
            mean_lat_jail_pop_2018 = mean(latinx_jail_pop, na.rm = TRUE),
            mean_bl_pop_2018 = mean(black_pop_15to64, na.rm = TRUE),
            mean_wt_pop_2018 = mean(white_pop_15to64, na.rm = TRUE),
            mean_aapi_pop_2018 = mean(aapi_pop_15to64, na.rm = TRUE),
            mean_nat_pop_2018 = mean(native_pop_15to64, na.rm = TRUE),
            mean_lat_pop_2018 = mean(latinx_pop_15to64, na.rm = TRUE))

# ratio of mean pop to jail pop 15-64 in 2018
sum_ratio_2018 <- sum_df %>%
  summarise(bl_jail_ratio = mean_bl_jail_pop_2018 / mean_bl_pop_2018,
            wt_jail_ratio = mean_wt_jail_pop_2018 / mean_wt_pop_2018,
            aapi_jail_ratio = mean_aapi_jail_pop_2018 / mean_aapi_pop_2018,
            nat_jail_ratio = mean_nat_jail_pop_2018 / mean_nat_pop_2018,
            lat_jail_ratio = mean_lat_jail_pop_2018 / mean_lat_pop_2018)
  
sum_info$bl_ratio <- pull(sum_ratio_2018, bl_jail_ratio)
sum_info$wt_ratio <- pull(sum_ratio_2018, wt_jail_ratio)
sum_info$aapi_ratio <- pull(sum_ratio_2018, aapi_jail_ratio)
sum_info$nat_ratio <- pull(sum_ratio_2018, nat_jail_ratio)
sum_info$lat_ratio <- pull(sum_ratio_2018, lat_jail_ratio)


# Trend over time

trend_df <- sub_trends %>%
  group_by(year) %>%
  summarise(mean_bl_jail_pop = mean(black_jail_pop, na.rm = TRUE),
            mean_wt_jail_pop = mean(white_jail_pop, na.rm = TRUE),
            mean_aapi_jail_pop = mean(aapi_jail_pop, na.rm = TRUE),
            mean_nat_jail_pop = mean(native_jail_pop, na.rm = TRUE),
            mean_lat_jail_pop = mean(latinx_jail_pop, na.rm = TRUE),
            mean_bl_pop = mean(black_pop_15to64, na.rm = TRUE),
            mean_wt_pop = mean(white_pop_15to64, na.rm = TRUE),
            mean_aapi_pop = mean(aapi_pop_15to64, na.rm = TRUE),
            mean_nat_pop = mean(native_pop_15to64, na.rm = TRUE),
            mean_lat_pop = mean(latinx_pop_15to64, na.rm = TRUE),
            bl_jail_ratio = mean_bl_jail_pop / mean_bl_pop,
            wt_jail_ratio = mean_wt_jail_pop / mean_wt_pop,
            aapi_jail_ratio = mean_aapi_jail_pop / mean_aapi_pop,
            nat_jail_ratio = mean_nat_jail_pop / mean_nat_pop,
            lat_jail_ratio = mean_lat_jail_pop / mean_lat_pop)

tr_pt_df <- trend_df %>%
  filter(year > 1989) %>%
  select(year, bl_jail_ratio, wt_jail_ratio, aapi_jail_ratio, nat_jail_ratio,
         lat_jail_ratio)
  

trend_plot <- ggplot(data = tr_pt_df) +
  ggtitle("Ratio of Jailed Population Over Time (1990-2018)") +
  xlab("Time (Year)") +
  ylab("Ratio (Jailed:Total Population)") +
  geom_smooth(mapping = aes(x = year, y = bl_jail_ratio, color = "Black")) +
  geom_smooth(mapping = aes(x = year, y = wt_jail_ratio, color = "White")) +
  geom_smooth(mapping = aes(x = year, y = aapi_jail_ratio, color = "Asian American/Pacific Islander")) +
  geom_smooth(mapping = aes(x = year, y = nat_jail_ratio, color = "Native American")) +
  geom_smooth(mapping = aes(x = year, y = lat_jail_ratio, color = "Latinx"))


# Variable Comparison
# white prison adm rate vs black prison adm rate

var_df <- trends %>%
  select(year, state, white_prison_adm_rate, black_prison_adm_rate) %>%
  group_by(state) %>%
  filter(year == 2013) 

var_df[is.na(var_df)] <- 0

var_plot <- ggplot(data = var_df) +
  ggtitle("White vs Black Prison Admission Rate in 2013") +
  xlab("White Prison Admission Rate") +
  ylab("Black Prison Admission Rate") +
  geom_point(mapping = aes(x = white_prison_adm_rate, 
                           y = black_prison_adm_rate))

# Map Chart
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

map_df <- trends %>%
  select(year, state, black_pop_15to64, black_jail_pop, total_pop) %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  mutate(bl_jail_ratio = black_jail_pop / total_pop)

map_df$state <- state.name[match(map_df$state, state.abb)]

map_df <- map_df %>%
  mutate(state = tolower(state))


state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(map_df, by = "state")

map_chart <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bl_jail_ratio),
               color = "white",
               size = .1) +
  coord_map() +
  scale_fill_continuous(labels = percent, low = "#132B43", high = "Red") +
  labs(fill = "Jailed Ratio, Black to Total Population") +
  blank_theme



