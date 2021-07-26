rm(list = ls())
graphics.off()
pacman::p_load(ggplot2, dplyr, jsonlite, purrr, patchwork)

# Using API data from GitHub: Our World in Data

url <- "https://covid.ourworldindata.org/data/owid-covid-data.json"
covid <- fromJSON(url)
save(data, file = "./covid.RData")

# typeof(covid)

covid_NPL <- covid$NPL$data
covid_IND <- covid$IND$data
covid_PAK <- covid$PAK$data
covid_BGD <- covid$BGD$data
covid_SLK <- covid$LKA$data
# typeof(covid_NPL)
# str(covid_NPL)

# Convert dates from character to POSIXct
# Add country column

typeof(covid_NPL$date) # character
covid_NPL <- covid_NPL %>% 
  mutate(date = as.Date(date),
         country = "Nepal", .before = date)

covid_IND <- covid_IND %>% 
  mutate(date = as.Date(date),
         country = "India", .before = date)

covid_PAK <- covid_PAK %>% 
  mutate(date = as.Date(date),
         country = "Pakistan", .before = date)

covid_BGD <- covid_BGD %>% 
  mutate(date = as.Date(date),
         country = "Bangladesh", .before = date)

covid_SLK <- covid_SLK %>% 
  mutate(date = as.Date(date),
         country = "Sri Lanka", .before = date)

# Merge all data frames
covid_4c <- bind_rows(covid_NPL, covid_IND, covid_PAK, covid_BGD)
covid_5c <- bind_rows(covid_NPL, covid_IND, covid_PAK, covid_BGD, covid_SLK)
# typeof(covid_plot)

# covid_4c <- covid_4c %>%
#   select(country, date, total_cases, new_cases,
#          total_cases_per_million, new_cases_per_million, total_tests,
#          total_tests_per_thousand, new_tests, new_tests_per_thousand,
#          tests_per_case, total_deaths, new_deaths, total_deaths_per_million,
#          new_deaths_per_million, stringency_index, positive_rate,
#          reproduction_rate) %>% 
#   filter(date >= "2020-03-01")


# PLOT: PARAMETERS

covid_plot <- covid_5c

point_size = 0.6
line_size = 0.5

expand <- c(0.02,0.02)
color_values <- c("limegreen", "orangered", "blue", "grey20", "darkmagenta")

title_new_cases <- "COVID-19: Daily new cases (per million population)"
title_new_deaths <- "Daily new deaths (per million population)"
title_pos_rate <- "Positive rate (the share of tests that are positive)"
title_rep_rate <- "Reproduction rate"
title_str_index <- "Stringency index"

title_21 <- "Since April 1, 2021"
subtitle <- "Pramesh Pudasaini"
caption <- "Time period: April - May, 2021; Data source: OWID"

# CUSTOM THEME

theme <- theme_bw()
TH <- theme(axis.title = element_blank(),
            axis.text = element_text(size = 11),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(3, "pt"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 11, face = "italic", 
                                         hjust = 0.5),
            plot.caption = element_text(size = 10, face = "italic", vjust = 1),
            plot.margin = margin(t=2, r=8, b=2, l=2, unit = "pt"),
            panel.background = element_rect(fill = "grey90"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.2),
            # legend.position = c(0, 0),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key = element_rect(fill = "grey90", color = "grey90"),
            legend.key.width = unit(30, "pt"),
            legend.key.size = unit(1.5, "line"),
            legend.direction = "vertical",
            legend.background = element_blank())

th <- theme(axis.title = element_blank(),
            axis.text = element_text(size = 10),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(2, "pt"),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 11, face = "italic", 
                                         hjust = 0.5),
            plot.caption = element_text(size = 10, face = "italic", vjust = 1),
            plot.margin = margin(t=2, r=8, b=2, l=2, unit = "pt"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.1),
            legend.position = "none")


# ==============================================================================
# PLOT 1: NEW CASES PER MILLION & DAILY NEW CASES SINCE APRIL (INSET)
# ==============================================================================

# data from Mar 1, 2020
p_new_cases <- covid_plot %>% 
  select(country, date, new_cases_per_million) %>% 
  filter(!is.na(new_cases_per_million),
         date >= '2020-03-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_cases_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_cases_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", 
               expand = expand) + 
  scale_y_continuous(breaks = seq(0, 350, 50), expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_new_cases,
       subtitle = subtitle,
       caption = caption) + 
  # theme + 
  TH + 
  theme(legend.position = c(0.7, 0.73))

# data from Apr 1, 2021
p_new_cases_21 <- covid_plot %>% 
  select(country, date, new_cases_per_million) %>% 
  filter(!is.na(new_cases_per_million),
         date >= '2021-04-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_cases_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_cases_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "6 days", date_labels = "%d %b", 
               expand = expand) + 
  scale_y_continuous(breaks = seq(0, 350, 50), expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_21) + 
  theme + 
  th

plot_new_cases <- p_new_cases + 
  inset_element(p_new_cases_21, left = 0.02, right = 0.45, bottom = 0.35, 
                top = 0.96) + 
  theme(legend.position = "none")
plot_new_cases

ggsave(file = "./daily_new_cases_may_29.png", plot = plot_new_cases, 
       units = "cm", width = 38.7, height = 21, dpi = 600)



# ==============================================================================
# PLOT 2: NEW CASES PER MILLION AND STRINGENCY INDEX (INSET)
# ==============================================================================

# data of new cases per million from Mar 1, 2020
p_new_cases <- covid_plot %>% 
  select(country, date, new_cases_per_million) %>% 
  filter(!is.na(new_cases_per_million),
         date >= '2020-03-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_cases_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_cases_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", 
               expand = expand) + 
  scale_y_continuous(breaks = seq(0, 350, 50), expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_new_cases,
       subtitle = subtitle,
       caption = caption) + 
  # theme + 
  TH + 
  theme(legend.position = c(0.7, 0.73))

# data of stringency index from Mar 1, 2020
# Oxford COVID-19 Government Response Tracker, Blavatnik School of Government	
# Government Response Stringency Index: composite measure based on 9 response 
# indicators including school closures, workplace closures, and travel bans, 
# rescaled to a value from 0 to 100 (100 = strictest response)
p_str_index <- covid_plot %>% 
  select(country, date, stringency_index) %>% 
  filter(!is.na(stringency_index),
         date >= '2020-03-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = stringency_index, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = stringency_index, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", 
               expand = expand) + 
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_str_index) + 
  theme + 
  th

plot_cases_str <- p_new_cases + 
  inset_element(p_str_index, left = 0.02, right = 0.48, bottom = 0.3, 
                top = 0.96) + 
  theme(legend.position = "none")
plot_cases_str

ggsave(file = "./daily_new_cases_str_index_may_29.png", plot = plot_cases_str, 
       units = "cm", width = 37.8, height = 21, dpi = 600)



# ==============================================================================
# PLOT 2: NEW DEATHS PER MILLION
# ==============================================================================

# data from Mar 1, 2020
p_new_deaths <- covid_plot %>% 
  select(country, date, new_deaths_per_million) %>% 
  filter(!is.na(new_deaths_per_million),
         date >= '2020-03-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_deaths_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_deaths_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", 
               expand = expand) + 
  # scale_y_continuous(expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_new_deaths,
       subtitle = subtitle,
       caption = caption) + 
  # theme + 
  TH + 
  theme(legend.position = c(0.7, 0.73))

# data from Apr 1, 2021
p_new_deaths_21 <- covid_plot %>% 
  select(country, date, new_deaths_per_million) %>% 
  filter(!is.na(new_deaths_per_million),
         date >= '2021-04-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_deaths_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_deaths_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "6 days", date_labels = "%d %b", 
               expand = expand) + 
  # scale_y_continuous(breaks = seq(0, 350, 50), expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_21) + 
  theme + 
  th
p_new_deaths_21

plot_new_deaths <- p_new_deaths + 
  inset_element(p_new_deaths_21, left = 0.02, right = 0.45, bottom = 0.35, 
                top = 0.96) + 
  theme(legend.position = "none")
plot_new_deaths

ggsave(file = "./daily_new_cases1.png", plot = plot_new_cases, units = "cm", 
       width = 37.8, height = 21, dpi = 1200)



# PLOT: STRINGENCY INDEX

plot_str_index <- covid_plot %>% 
  select(country, date, stringency_index) %>% 
  filter(!is.na(stringency_index)) %>% 
  ggplot() + 
  geom_point(aes(x = date, y = stringency_index, col = country, 
                 fill = country),
             size = 0.6) + 
  geom_line(aes(x = date, y = stringency_index, col = country), 
            size = 0.5)

# PLOT: POSITIVE RATE
# Share of COVID-19 tests that are positive, given as a rolling 7-day average.

plot_pos_rate <- covid_plot %>% 
  select(country, date, positive_rate) %>% 
  filter(!is.na(positive_rate)) %>% 
  ggplot() + 
  geom_point(aes(x = date, y = positive_rate*100, col = country, 
                 fill = country),
             size = 0.6) + 
  geom_line(aes(x = date, y = positive_rate*100, col = country), 
            size = 0.5)


# PLOT: NEW TESTS PER THOUSAND

plot_new_tests <- covid_plot %>% 
  select(country, date, new_tests_per_thousand) %>% 
  filter(!is.na(new_tests_per_thousand))

ggplot(plot_new_tests) + 
  geom_point(aes(x = date, y = new_tests_per_thousand, col = country, 
                 fill = country),
             size = 0.6) + 
  geom_line(aes(x = date, y = new_tests_per_thousand, col = country), 
            size = 0.5)


# PLOT: REPRODUCTION RATE

plot_rep_rate <- covid_plot %>% 
  select(country, date, reproduction_rate) %>% 
  filter(!is.na(reproduction_rate))

ggplot(plot_rep_rate) + 
  geom_point(aes(x = date, y = reproduction_rate, col = country, 
                 fill = country),
             size = 0.6) + 
  geom_line(aes(x = date, y = reproduction_rate, col = country), 
            size = 0.5)


# ==============================================================================
# PLOT: APRIL & MAY 
# ==============================================================================

# new cases
p_new_cases_apr_may <- covid_plot %>% 
  select(country, date, new_cases_per_million) %>% 
  filter(!is.na(new_cases_per_million),
         date >= '2021-04-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_cases_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_cases_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "6 days", date_labels = "%d %b", 
               expand = expand) + 
  scale_y_continuous(breaks = seq(0, 350, 50), expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_new_cases,
       subtitle = subtitle) + 
  th + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white", color = "white"),
        legend.key.width = unit(30, "pt"),
        legend.key.size = unit(0.5, "line"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top")
p_new_cases_apr_may

# new deaths
p_new_deaths_apr_may <- covid_plot %>% 
  select(country, date, new_deaths_per_million) %>% 
  filter(!is.na(new_deaths_per_million),
         date >= '2021-04-01') %>% 
  ggplot() + 
  geom_point(aes(x = date, y = new_deaths_per_million, col = country, 
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = new_deaths_per_million, col = country), 
            size = line_size) + 
  scale_x_date(date_breaks = "6 days", date_labels = "%d %b", 
               expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_new_deaths) + 
  th
p_new_deaths_apr_may

# positive rate
p_pos_rate_apr_may <- covid_plot %>% 
  select(country, date, positive_rate) %>% 
  filter(!is.na(positive_rate),
         date >= "2021-04-01") %>% 
  ggplot() + 
  geom_point(aes(x = date, y = positive_rate*100, col = country,
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = positive_rate*100, col = country),
            size = line_size) + 
  scale_x_date(date_breaks = "6 days", date_labels = "%d %b", 
               expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_pos_rate) + 
  th
p_pos_rate_apr_may

# reproduction rate
p_rep_rate_apr_may <- covid_plot %>% 
  select(country, date, reproduction_rate) %>% 
  filter(!is.na(reproduction_rate),
         date >= "2021-04-01") %>% 
  ggplot() + 
  geom_point(aes(x = date, y = reproduction_rate, col = country,
                 fill = country),
             size = point_size) + 
  geom_line(aes(x = date, y = reproduction_rate, col = country),
            size = line_size) + 
  scale_x_date(date_breaks = "6 days", date_labels = "%d %b", 
               expand = expand) + 
  scale_color_manual(values = color_values) + 
  xlab("") + 
  ylab("") + 
  labs(title = title_rep_rate,
       caption = caption) + 
  th
p_rep_rate_apr_may

plot_apr_may <- p_new_cases_apr_may / p_new_deaths_apr_may / 
  p_pos_rate_apr_may / p_rep_rate_apr_may
plot_apr_may

ggsave(file = "./plot_apr_may.png", plot = plot_apr_may, 
       units = "cm", width = 21, height = 29.7, dpi = 600)

