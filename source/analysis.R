trends <- read.csv("../data/incarceration_trends.csv")

library(dplyr)
library(ggplot2)


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

