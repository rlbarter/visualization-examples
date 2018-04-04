# examples for DS421 visualization lecture
library(tidyverse)
# load in gapminder data
gapminder <- read.csv("data/gapminder-FiveYearData.csv")
gapminder_2007 <- gapminder %>% filter(year == 2007)



################### default gapminder scatterplot ###################

ggplot(gapminder_2007) +
  # add scatter points
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  scale_x_log10(limits = c(200, 60000)) 

################### beautiful scatterplot example ####################

gapminder_2007 %>%
  mutate(usa = country == "United States") %>%
  ggplot() +
  # add scatter points
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent, 
                 size = pop, alpha = usa)) +
  # add some text annotations for the very large countries
  geom_text(aes(x = gdpPercap, y = lifeExp - 3, label = country),
            color = "grey40",
            data = filter(gapminder_2007, country %in% c("United States"))) +
  # clean the axes names and breaks
  scale_x_log10(limits = c(200, 60000)) +
  # change labels
  labs(title = "GDP versus life expectancy in 2007",
       x = "GDP per capita (log scale)",
       y = "Life expectancy",
       size = "Popoulation",
       color = "Continent") +
  # change the size scale
  scale_size(range = c(0.1, 10),
             # remove size legend
             guide = "none") +
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  scale_color_manual(values = c("Africa" = rgb(121, 154, 40, maxColorValue = 255),
                                "Americas" = rgb(0, 158, 219, maxColorValue = 255),
                                "Asia" = rgb(242, 124, 150, maxColorValue = 255),
                                "Europe" = rgb(231, 159, 198, maxColorValue = 255),
                                "Oceania" = rgb(255, 212, 0, maxColorValue = 255))) +
  # add a nicer theme
  theme_classic(base_size = 18) +
  # place legend at top and grey axis lines
  theme(legend.position = "top",
        axis.line = element_line(color = "grey85"),
        axis.ticks = element_line(color = "grey85"),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.justification = "left") 



######################## Make panel scatterplots ##########################

# note that this could be done using faceting
gapminder %>%
  # filter to 2007
  filter(year == 2007) %>%
  # Add a variable for africa or not
  mutate(africa = continent == "Africa") %>%
  ggplot() + 
  # plot scatterplot and color by africa and change size by pop
  geom_point(aes(x = gdpPercap, y = lifeExp, col = africa,
                 alpha = africa, size = pop)) +
  # select the colors (red for Africa, grey for all others)
  scale_color_manual(values = c("grey40", "#D8315B"), guide = "none") +
  # set the transparency (no transparency for Africa, some transparency for others)
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  # choose a log scale for the x-axis
  scale_x_log10() +
  # play with the size range and legend
  scale_size_continuous(name = "population", 
                        breaks = c(250000000, 1250000000),
                        range = c(1, 10),
                        labels = c("small", "large")) + 
  # add labels for axes
  ylab("Life Expectancy") +
  xlab("GDP per capita") +
  # increase universal font size and remove grey theme
  theme_classic(base_size = 20) +
  # add title
  ggtitle("Africa") +
  # make the axis lines grey
  theme(axis.line = element_line(color = "grey60"))


########################### bad bar chart ##################

# default bar chart
data.frame(percent_stay_at_home_mother = c(41, 20), year = as.factor(c(1970, 2012))) %>%
  ggplot() +
  geom_bar(aes(x = year, y = percent_stay_at_home_mother), stat = "identity") 


########################### pretty bar chart ##################

# prettier bar chart
data.frame(percent = c(41, 20), year = as.factor(c(1970, 2012))) %>%
  ggplot() +
  # add bars
  geom_bar(aes(x = year, y = percent), stat = "identity") +
  # add text for bars
  geom_text(aes(x = year, y = percent + 3, label = paste0(percent, "%")),
            size = 6) +
  # remove gap between bars and y-axis (expand = c(0, 0))
  scale_y_continuous(expand = c(0, 0), limits = c(0, 48)) +
  # add a nicer theme
  theme_classic(base_size = 22) +
  # play with theme
  theme(axis.line.y = element_blank(), # remove y-axis
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank(), # remove axis ticks
        axis.text.y = element_blank()) + # remove y axis title 
  ggtitle("Children with a 'Traditional'\nStay-at-Home Mother")

  


######################## Default life expectency line plot ####################
# defualt line plot
gapminder %>%
  # filter to rwanda
  filter(country %in% 
           c("Rwanda", "Cambodia")) %>%
  ggplot() +
  # add lines for all gapminder countries (note data = gapminder)
  geom_line(aes(x = year, group = country, y = lifeExp), 
            data = gapminder, color = "grey80") +
  # add line for Rwanda and Cambodia
  geom_line(aes(x = year, color = country, y = lifeExp), 
            size = 1.3) +
  # remove grey grid and increase font size
  theme_classic(base_size = 20) +
  # add title
  ggtitle("The effect of genocies on life expectency")


######################## Cleaner life expectency line plot ####################

# calcualte average life expectency per year
average_lifeExp <- gapminder %>% group_by(year) %>%
  summarise(lifeExp = mean(lifeExp))

# beautiful line plot
gapminder %>%
  filter(country == "Rwanda") %>%
  ggplot() +
  # average life expectancy
  geom_line(aes(x = year, y = lifeExp), 
            col = "grey50", data = average_lifeExp) +
  # add start and end points for average line
  geom_point(aes(x = year, y = lifeExp),
             size = 3, col = "grey50", 
             data = filter(average_lifeExp, year %in% c(1952, 2007))) +
  # add labels at the end for average line
  geom_text(aes(x = year + 1, y = lifeExp, label = round(lifeExp)), 
            data = filter(average_lifeExp, year == 2007),
            hjust = 0, size = 6) +
  # add labels at the beginning of average line
  geom_text(aes(x = year - 1, y = lifeExp, 
                label = paste0("Average, ", round(lifeExp))), 
            data = filter(average_lifeExp, year == 1952),
            hjust = 1, size = 6) +
  # Rwandan life expectancy line
  geom_line(aes(x = year, group = country, y = lifeExp), 
            col = "#508991", size = 1.2) +
  # add important points on the Rwandan line
  geom_point(aes(x = year, group = country, y = lifeExp), 
            col = "#508991", size = 3,
            data = filter(gapminder, year %in% c(1952, 1992, 2007), 
                          country == "Rwanda")) +
  # add label at the end of Rwandan line
  geom_text(aes(x = year + 1, y = lifeExp, label = round(lifeExp)), 
            data = filter(gapminder, year == "2007",
                          country %in% c("Rwanda")),
            hjust = 0, size = 6) +
  # add label at trough of Rwandan line
  geom_text(aes(x = year - 1, y = lifeExp, 
                label = paste0("Rwanda, ", round(lifeExp))), 
            data = filter(gapminder, year %in% c(1952),
                          country %in% c("Rwanda")),
            hjust = 1, size = 6) +
  # add label at beginning of Rwandan line
  geom_text(aes(x = year, y = lifeExp - 2, label = round(lifeExp)), 
            data = filter(gapminder, year %in% c(1992),
                          country %in% c("Rwanda")),
            size = 6) +
  # set the scale for the x-axis 
  scale_x_continuous(breaks = c(1952, 1992, 2007),
                     limits = c(1938, 2012)) +
  # set the limits for the y-axis
  scale_y_continuous(limits = c(20, 70)) +
  # increase the size universally and remove grey background
  theme_classic(base_size = 20) +
  # play with theme
  theme(axis.line.y = element_blank(), # remove y-axis line
        axis.line.x = element_line(color = "grey60"), # make x-axis line grey
        axis.ticks.y = element_blank(), # remove y-axis ticks
        axis.text.y = element_blank(), # remove y-axis text
        axis.title = element_blank()) + # remove all axis titles
  ggtitle("The effect of genocide on life expectency")




############ Line plots for GDP per cap in the UK and Europe ##################

gapminder %>% 
  # filter to countries of interest
  filter(country %in% 
           c("France", "United Kingdom", 
             "Ireland", "Germany")) %>%
  ggplot() +
  # add a line for each country 
  geom_line(aes(x = year, y = gdpPercap, col = country)) +
  # add a y-label
  ylab("GDP per capita") +
  theme_classic(base_size = 17) 
  
############## Line plots for GDP per cap highlighting ireland ###############

gapminder_europe <- gapminder %>% 
  filter(country %in% 
           c("France", "United Kingdom", 
             "Ireland", "Germany")) %>% 
  # change "United Kingdom" to "UK" to save space
  mutate(country = if_else(country == "United Kingdom", 
                           "UK", as.character(country))) %>% 
  # create a variable for whether the country is ireland or not
  mutate(ireland = country == "Ireland") 
gapminder_europe %>%  
  ggplot() +
  # add a line for each country and color by ireland
  geom_line(aes(x = year, y = gdpPercap, group = country, 
                col = ireland, size = ireland)) +
  # add the country names
  geom_text(aes(x = year + 0.5, y = gdpPercap, label = country, col = ireland), 
            data = filter(gapminder_europe, year == 2007),
            hjust = 0, size = 4) +
  # set the colors: dark red for Ireland and grey for others
  scale_color_manual(values = c("grey50", rgb(75, 20, 20, maxColorValue = 255))) +
  # set the size: larger for ireland smaller for others
  scale_size_manual(values = c(1.2, 1.8)) +
  # set the range of the x-axis
  scale_x_continuous(limits = c(1952, 2015)) +
  # universally increase the font size and remove the grey theme
  theme_classic(base_size = 17) +
  # add a y-label
  ylab("GDP per capita") +
  # remove the legend
  theme(legend.position = "none") +
  # add a title
  ggtitle("The rise of Ireland's economy")



############### Default diseases bar plots (entire process) ###################
library(dslabs)
diseases_2000 <- us_contagious_diseases %>%
  # calcualt ethe number of cases per disease in 2000
  filter(year == 2000) %>%
  group_by(disease) %>%
  summarise(count = sum(count)) %>%
  ungroup() 

diseases_2000 %>% 
  ggplot() +
  geom_bar(aes(x = disease, y = count, fill = disease), stat = "identity") 


# remove the color clutter
diseases_2000 %>% 
  ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity")

# change theme to remove grid
diseases_2000 %>% 
  ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity") + 
  theme_classic() 

# increase font size
diseases_2000 %>% 
  ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity") + 
  theme_classic(base_size = 18) 

# order in increasing order
diseases_2000 <- diseases_2000 %>%
  arrange(desc(count)) %>%
  mutate(disease = fct_inorder(droplevels(disease))) 
diseases_2000 %>% ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity") +
  theme_classic(base_size = 18) 

# get rid of y-axis gap
diseases_2000 %>% ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity") +
  theme_classic(base_size = 18) + 
  scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) 

# remove y-axis and put text instead
diseases_2000 %>% ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity") +
  theme_classic(base_size = 18) + 
  scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +
  geom_text(aes(x = disease, y = count + 300, label = count),
            size = 6) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# remove x-label
diseases_2000 %>% ggplot() +
  geom_bar(aes(x = disease, y = count), stat = "identity") +
  theme_classic(base_size = 18) + 
  scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +
  geom_text(aes(x = disease, y = count + 300, label = count),
            size = 6) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# highlight polio
diseases_2000 %>% ggplot() +
  geom_bar(aes(x = disease, y = count, fill = disease), stat = "identity") +
  geom_text(aes(x = disease, y = count + 300, label = count),
            size = 6) +
  theme_classic(base_size = 18) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +
  scale_fill_manual(values = c("grey50", "grey50", "orange", 
                               "grey50", "grey50", "grey50"),
                    guide = "none")

# add a title
diseases_2000 %>% ggplot() +
  geom_bar(aes(x = disease, y = count, fill = disease), stat = "identity") +
  geom_text(aes(x = disease, y = count + 300, label = count),
            size = 6) +
  theme_classic(base_size = 18) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +
  scale_fill_manual(values = c("grey50", "grey50", "orange", 
                               "grey50", "grey50", "grey50"),
                    guide = "none") +
  ggtitle("Incidence of Polio in 2010 in the US\nrelative to other contageous diseases")



################### Bar plots for funding by gender ######################

# combine data for women and men
rbind(transmute(research_funding_rates, 
             discipline, success = success_rates_men, 
             gender = "Male"),
      transmute(research_funding_rates, 
             discipline, success = success_rates_women, 
             gender = "Female")) %>%
  # reorder factor levels
  mutate(gender = fct_inorder(gender)) %>%
  ggplot() +
  # add bar for each discipline separated by gender
  geom_bar(aes(x = discipline, y = success, fill = gender),
           stat = "identity", position = "dodge") +
  # remove gap between bars and y-axis
  scale_y_continuous(expand = c(0, 0)) +
  # remove grey theme
  theme_classic(base_size = 18) +
  # rotate x-axis
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank()) +
  # add title
  ggtitle("Research funding success by discipline and gender")

################### Slope plots for funding by gender ######################

research_funding_rates %>%
  ggplot() +
  # add a segmend that goes from men to women
  geom_segment(aes(x = 1, xend = 2, 
                   y = success_rates_men, 
                   yend = success_rates_women,
                   group = discipline), 
               col = "#3C6997", size = 1.2) +
  # add vertical lines that add as axis for men
  geom_segment(x = 1, xend = 1, 
               y = min(research_funding_rates$success_rates_men) - 2,
               yend = max(research_funding_rates$success_rates_men) + 2,
               col = "grey70", size = 0.5) +
  # add vertical lines that add as axis for women
  geom_segment(x = 2, xend = 2, 
               y = min(research_funding_rates$success_rates_men) - 2,
               yend = max(research_funding_rates$success_rates_men) + 2,
               col = "grey70", size = 0.5) +
  # add the outline for the points at each rate for men
  geom_point(aes(x = 1, 
                 y = success_rates_men), size = 4.5,
             col = "white") +
  # add the outline for the points at each rate for women
  geom_point(aes(x = 2, 
                 y = success_rates_women), size = 4.5,
             col = "white") +
  # add the actual points at each rate for men
  geom_point(aes(x = 1, 
                 y = success_rates_men), size = 4,
             col = "#3C6997") +
  # add the actual points at each rate for men
  geom_point(aes(x = 2, 
                 y = success_rates_women), size = 4,
             col = "#3C6997") +
  # add the label for each discipline next the men axis
  geom_text(aes(x = 1 - 0.02, 
                 y = success_rates_men, 
                label = paste0(discipline, ", ", round(success_rates_men), "%")),
             col = "grey30", hjust = "right", 
            size = 8) +
  # add the label next to each point on the women axis
  geom_text(aes(x = 2 + 0.07, 
                y = success_rates_women, 
                label = paste0(round(success_rates_women), "%")),
            col = "grey30", size = 8) +
  # add the words "men" and "women" above their axes
  geom_text(aes(x = x, y = y, label = label),
            data = data.frame(x = 1:2, 
                              y = 3 + max(research_funding_rates$success_rates_men),
                              label = c("men", "women")),
            col = "grey30",
            size = 10) +
  # set the limits of the x-axis
  scale_x_continuous(limits = c(0.5, 2.1)) +
  # universally increase the size of the text and remove grey background
  theme_classic(base_size = 20) + 
  # remove all axis stuff
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  # add a title
  ggtitle("Research funding success by discipline and gender")



################### Dot plots for funding by gender ######################

research_funding_rates %>%
  # rearrange the factor levels for discipline by rates for women
  arrange(success_rates_women) %>%
  mutate(discipline = fct_inorder(discipline)) %>%
  ggplot() +
  # add a dummy point for scaling purposes
  geom_point(aes(x = 12, y = discipline), 
             size = 0, col = "white") + 
  # add the horizontal grid lines
  geom_hline(yintercept = 1:9, col = "grey80") +
  # add a point for each male success rate
  geom_point(aes(x = success_rates_men, y = discipline), 
             size = 12, col = "#9DBEBB") +
  # add the text (%) for each male success rate
  geom_text(aes(x = success_rates_men, y = discipline, 
                label = paste0(round(success_rates_men, 0), "%")),
            col = "black") +
  # add the border for each point for female success rate
  geom_point(aes(x = success_rates_women, y = discipline),
             size = 12.5, col = "white") +
  # add a point for each female success rate
  geom_point(aes(x = success_rates_women, y = discipline),
             size = 12, col = "#468189") +
  # add the text (%) for each female success rate
  geom_text(aes(x = success_rates_women, y = discipline, 
                label = paste0(round(success_rates_women, 0), "%")),
            col = "white") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label),
            data.frame(x = c(25.6 - 0.7, 26.5 + 0.5), y = 10, 
                       label = c("women", "men")), size = 6) +
  # universally increase size of text and remove grey theme
  theme_classic(base_size = 16) +
  # manually specify the x-axis
  scale_x_continuous(breaks = c(15, 20, 25), 
                     labels = c("15%", "20%", "25%")) +
  # remove gaps in the y-axis
  scale_y_discrete(expand = c(0.2, 0)) +
  # remove axes
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  # add title
  ggtitle("Research funding success by discipline and gender")


