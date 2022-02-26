#Assignment 3
library("stringr")
source <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
inc_data <- read.csv(source)
#View(inc_data)

library(dplyr)

# Which state had the largest amount of black citizens in jail in 2018?

black_highest_state <- inc_data %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  summarize(    
    black_jail_pop = sum(black_jail_pop)
  ) %>%
  select(state, black_jail_pop)
state_highest_black_total <- filter(black_highest_state, black_jail_pop == max(black_jail_pop))
print(state_highest_black_total)

#In which year was the black jail population the highest in US?

year_highest_black <- inc_data %>%
  #tate(total_black = black_jail_pop)%>%
  group_by(year) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  summarize (
    black_jail_pop = sum(black_jail_pop)
  )%>%
  select(year,black_jail_pop)
year_highest_black_total <- filter(year_highest_black, black_jail_pop == max(black_jail_pop))
#print(year_highest_black_total)

#Question: What is the ratio of the black population
# (from ages 15-64) in Georgia in 2018 to the black jail population in
# Georgia in 2018?

black_ratio_2018 <- inc_data %>%
  filter(state == "GA") %>%
  filter(year == 2018) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(black_pop_15to64 == black_pop_15to64) %>%
  summarize(
    total_black_jail = sum(black_jail_pop),
    total_black_pop = sum(black_pop_15to64),
    ratio = (total_black_jail/total_black_pop)*100
  ) %>%
  pull(ratio)
#print(black_ratio_2018)

#Map Question: What is the ratio of the black jail population nationwide in 2018 to
# the total population nationwide in 2018?

black_country_ratio_2018 <- inc_data %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  summarize(
    black_jail = sum(black_jail_pop), 
    total = sum(total_pop),
    ratio = (black_jail/total)*100
  ) %>%
  pull(ratio)
#print(black_country_ratio_2018)

#Which race had been the most incarcerated in 2018?

race_most_incarcerated <- inc_data %>%
  filter(year == 2018) %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(native_jail_pop == native_jail_pop) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  filter(other_race_jail_pop == other_race_jail_pop) %>%
  summarize (
    aapi_total = sum(aapi_jail_pop), black_total = sum(black_jail_pop), 
    latinx_total = sum(latinx_jail_pop), native_total = sum(native_jail_pop),
    other_race_total = sum(other_race_jail_pop), white_total = sum(white_jail_pop),
    totals_data_frame = data_frame(
      race = c("Asian","Black","Latinx","Native American","White","Other"),
      rtotals = c(aapi_total, black_total, latinx_total, native_total, white_total, other_race_total),
    ),
  ) %>%
  pull(totals_data_frame)
race_most_incarcerated_data <-  filter(race_most_incarcerated, rtotals == max(rtotals))
#print(race_most_incarcerated_data)

#Map Question: What is the ratio of the black jail population nationwide in 2018 to
# the total population nationwide in 2018?

black_country_ratio_2018 <- inc_data %>%
  #group_by(year) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  #mutate(total_pop/black_jail_pop) %>%
  summarize(
    black_jail = sum(black_jail_pop), 
    total = sum(total_pop),
    ratio = sum(black_jail_pop/total)
  ) %>%
  pull(ratio)
#print(black_country_ratio_2018)

library(ggplot2)
#----------------Line Chart over time
jail_pop <- inc_data %>%
  group_by(year) %>%
  filter(year > 1985) %>%
  filter(total_jail_pop == total_jail_pop) %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(native_jail_pop == native_jail_pop) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  summarize(
    jail_total = sum(total_jail_pop), total_pop_15_64 =
      sum(total_pop_15to64), aapi_total = sum(aapi_jail_pop), 
    black_total = sum(black_jail_pop),
    latinx_total = sum(latinx_jail_pop),
    native_total = sum(native_jail_pop),
    white_total = sum(white_jail_pop),
  )

line_chart <- ggplot(data = jail_pop) +
  geom_line(mapping = aes(x = year, y = aapi_total, color = "yellow")) +
  geom_line(mapping = aes(x = year, y = black_total, color = "black")) +
  geom_line(mapping = aes(x = year, y = latinx_total, color = "red")) +
  geom_line(mapping = aes(x = year, y = native_total, color = "green")) +
  geom_line(mapping = aes(x = year, y = white_total, color = "blue")) +
  scale_color_manual(
    name = "Races:",
    values = c("red", "yellow", "green", "black", "blue"),
    labels = c("Latinx", "AAPI", "Native", "Black", "White")
  ) +
  scale_fill_manual(
    name = "Races:",
    values = c("red", "yellow", "green", "black", "blue"),
    labels = c("Latinx", "AAPI", "Native", "Black", "White")
  ) +
  ggtitle("Yearly Total Jail Population for each Race") +  labs(x = "Year", y = "Jail Population")
#line_chart

# Scatterplot Chart------------------------------------------------------------

ga_regions <- inc_data %>%
  group_by(year) %>%
  filter(state == "GA") %>%
  filter(black_jail_pop == black_jail_pop) %>%
  summarise(jail = sum(black_jail_pop))

scatter_chart <- ggplot(data= ga_regions) +
  geom_point(mapping = aes(x = year, y = jail),
             color = "red",
             alpha = .3) +
  labs(x = "Year", y = "Black Jail Population in Georgia") +
  ggtitle("Black Jail Population In Georgia Across Years")

#scatter_chart

# Map -------------------------------------------------------------------------

library(usmap)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

black_state <- inc_data %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  mutate(total_pop / black_jail_pop) %>%
  summarize(
    pop = sum(black_jail_pop), total = max(total_pop),
    mutate = sum(total_pop / black_jail_pop)
  )

black_map <- plot_usmap(
  data = black_state, values = "pop", color = "black",
  name = "Black Jail Population"
) +
  coord_fixed(1) +
  blank_theme +
  scale_fill_gradientn(
    colours = c("white", "blue"),
    breaks = c(10, 100, 1000, 10000, 100000),
    trans = "log10", name = "Black Jail Population"
  ) +
  labs(title = "The United States", subtitle = "Black Jail Population in
       2018", name = "Black Jail Population") +
  theme(legend.position = "right")
#black_map

