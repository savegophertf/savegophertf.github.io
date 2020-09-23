##
##    Project:    Save Brown Track & Field/Cross Country!!!
##
##    Name:       BTF_cost_analysis.R
##
##    Approach:   Read in and analyze data from Brown varsity sports, including financial data
##                and data on Brown Track and Field and surveyed cost data from Youth Sports
##                
##    Authors:    Calvin J. Munson
##
##    Date:       May 29th, 2020
##
##    Note:       Use Cmd + Shift + O to open RStudio's outline functionality, which will provide
##                better organization for this script


# 1. Set up ---------------------------------------------------------------

## Clean up
rm(list=ls())

## Set working directory
setwd("~/Desktop/savebrowntrack.github.io/analyses_and_visualizations")


# * 1.1 Call to key packages ---------------------------------------------

## Key data visualization packages
library(tidyverse)
library(patchwork)
library(scales)


# * 1.2 Read in data ------------------------------------------------------

## Operating cost data for Brown U. 2019 Varsity teams
cost_raw <- read_csv("data/financial/data_raw/varsity_operating_costs_2019.csv") %>% 
  mutate(is.track = if_else(`Varsity Sport` == "Track/XC", "TRUE", "FALSE"))

cost_raw$`Varsity Sport` %>% unique()


## Family costs data for youth sports (1-18)
## Data taken from Aspen Institute/Utah State University 2019 National Youth Sport Survey
youth_sports <- read_csv("data/financial/data_raw/youth_sport_costs.csv") %>% 
  mutate(is.track = if_else(Sport == "Track and Field" | Sport == "Cross Country", "TRUE", "FALSE"))




# * 1.3 Calculate per capita operating costs ------------------------------

## Divide total team operating cost by number of athletes on rost for each row
cost_perAthlete <- cost_raw %>% 
  mutate(`Cost per Athlete` = `Operating Costs`/Roster)


# * 1.4 Create separate Male/Female dataframes ----------------------------

cost_perAthlete.Men <- cost_perAthlete %>% 
  filter(Gender == "Men")

cost_perAthlete.Women <- cost_perAthlete %>% 
  filter(Gender == "Women")


# 2. Brown University varsity operating costs --------------------------------------------------


# * 2.1 Cost per athlete for each team ------------------------------------


## Calculate cost per roster spot vs cost per athlete
# Explanation: The roster of Brown Men's Track and Field and Cross country is calculated by the
# administration by adding together all registered athletes for Indoor (47), outdoor (44), 
# and Cross Country (17). In reality, almost every athlete overlaps between indoor and outdoor 
# (except for outdoor exclusive events like javelin). In addition, all cross country runners
# run both indoor and outdoor as well. Thus, the number of unique athletes on the Track, Field, 
# and XC roster would be, conservatively, the maximum of any of the three rosters, or 47 
# (though others may not have participated due to injury)
cost_perUnique <- cost_perAthlete.Men %>% 
  filter(`Varsity Sport` == "Track/XC") %>% 
  mutate(`Varsity Sport` = "Track/XC (per unique athlete)",
         Roster = 47,
         `Cost per Athlete` = `Operating Costs`/`Roster`)



# * * 2.1.1 Webpage graph -------------------------------------------------


## Plot Cost per athlete for each varsity team (Men)
cost_perAthlete.Men %>% 
  # Bind the row created above
  bind_rows(cost_perUnique) %>% 
  # Rename the original Track/XC label as Track/XC per roster spot
  mutate(`Varsity Sport` = fct_recode(`Varsity Sport`,
                                      "Track/XC (per roster spot)" = "Track/XC")) %>% 
  # Reorder Sport by cost per athlete (reorders the labels on the axis)
  mutate(`Varsity Sport` = fct_reorder(`Varsity Sport`, `Cost per Athlete`)) %>% 
  # Assemble plot
  # Assign fill to Track/XC
  ggplot(aes(x = `Varsity Sport`, y = `Cost per Athlete`, fill = is.track)) +
  geom_col(color = "black",
           size = 0.35,
           width = 0.75) +
  geom_text(aes(label = dollar(`Cost per Athlete`, 
                               # Divide by 1,000 to report in thousands
                               scale = 1/1000,
                               # Round to the tenths place
                               accuracy = .1,
                               # Add suffix
                               suffix = "K")),
            color = "black",
            size = 4.5,
            nudge_y = 2000,
            hjust = 0) +
  annotate(y = 55000,
           x = "Track/XC (per unique athlete)",
           geom = "text",
           color = "black",
           hjust = 0,
           vjust = 1,
           label = "Data taken from the Brown University\n2019 Equity in Athletics report") +
  #scale_fill_manual(values = c("saddlebrown", "red")) +
  scale_fill_manual(values = c("gray75", "red")) +
  scale_y_continuous(limits = c(0, 100000),
                     labels = dollar) +
  coord_flip() +
  ggtitle("Brown's spending per athlete on Men's varsity teams",
          subtitle = "Based on Brown University's 2018-2019 academic year") +
  labs(y = "Spending per athlete") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "black"),
        axis.text.y = element_text(size = 14,
                                   color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "black"),
        plot.title = element_text(size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 0.75, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        text = element_text(color = "black",
                            #face = "bold",
                            family= "Helvetica"),
        #plot.background = element_rect(fill = "white"),
        legend.position = "none")

# ## Also export as png
# ggsave("figures/financial/costs_per_athlete.webpage.png",
#        height = 6,
#        width = 9,
#        dpi = 640)



# * * 2.1.2 IG graph (different color scheme) -----------------------------

## Create a different color scheme for the team instagram

## Plot Cost per athlete for each varsity team (Men)
cost_perAthlete.Men %>% 
  # Bind the row created above
  bind_rows(cost_perUnique) %>% 
  # Rename the original Track/XC label as Track/XC per roster spot
  mutate(`Varsity Sport` = fct_recode(`Varsity Sport`,
                                      "Track/XC (per roster spot)" = "Track/XC")) %>% 
  # Reorder Sport by cost per athlete (reorders the labels on the axis)
  mutate(`Varsity Sport` = fct_reorder(`Varsity Sport`, `Cost per Athlete`)) %>% 
  # Assemble plot
  # Assign fill to Track/XC
  ggplot(aes(x = `Varsity Sport`, y = `Cost per Athlete`, fill = is.track)) +
  geom_col(color = "black",
           width = 0.75) +
  geom_text(aes(label = dollar(`Cost per Athlete`, 
                               # Divide by 1,000 to report in thousands
                               scale = 1/1000,
                               # Round to the tenths place
                               accuracy = .1,
                               # Add suffix
                               suffix = "K")),
            color = "white",
            size = 4.5,
            nudge_y = 2000,
            hjust = 0) +
  annotate(y = 55000,
           x = "Track/XC (per unique athlete)",
           geom = "text",
           color = "white",
           hjust = 0,
           vjust = 1,
           label = "Data taken from the Brown University\n2019 Equity in Athletics report") +
  #scale_fill_manual(values = c("saddlebrown", "red")) +
  scale_fill_manual(values = c("gray50", "red")) +
  scale_y_continuous(limits = c(0, 100000),
                     labels = dollar) +
  coord_flip() +
  ggtitle("Operating cost per athlete for Men's varsity teams",
          subtitle = "Based on Brown University's 2018-2019 academic year") +
  labs(y = "Cost per athlete") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "white"),
        axis.text.y = element_text(size = 14,
                                   color = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "white"),
        plot.title = element_text(size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 0.75, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray22"),
        text = element_text(color = "white",
                            #face = "bold",
                            family= "Helvetica"),
        plot.background = element_rect(fill = "black"),
        legend.position = "none")


## Also export as png
ggsave("figures/financial/costs_per_athlete.IG.png",
       height = 6,
       width = 9,
       dpi = 640)



# * * 2.1.3 PPT report graph -------------------------------------------------


## Plot Cost per athlete for each varsity team (Men)
cost_perAthlete.Men %>% 
  # Bind the row created above
  bind_rows(cost_perUnique) %>% 
  # Rename the original Track/XC label as Track/XC per roster spot
  mutate(`Varsity Sport` = fct_recode(`Varsity Sport`,
                                      "Track/XC (per roster spot)" = "Track/XC")) %>% 
  # Reorder Sport by cost per athlete (reorders the labels on the axis)
  mutate(`Varsity Sport` = fct_reorder(`Varsity Sport`, `Cost per Athlete`)) %>% 
  # Assemble plot
  # Assign fill to Track/XC
  ggplot(aes(x = `Varsity Sport`, y = `Cost per Athlete`, fill = is.track)) +
  geom_col(color = "black",
           size = 0.35,
           width = 0.75) +
  geom_text(aes(label = dollar(`Cost per Athlete`, 
                               # Divide by 1,000 to report in thousands
                               scale = 1/1000,
                               # Round to the tenths place
                               accuracy = .1,
                               # Add suffix
                               suffix = "K")),
            color = "black",
            size = 4.5,
            nudge_y = 2000,
            hjust = 0) +
  annotate(y = 55000,
           x = "Track/XC (per unique athlete)",
           geom = "text",
           color = "black",
           hjust = 0,
           vjust = 1,
           label = "Data taken from the Brown University\n2019 Equity in Athletics report") +
  #scale_fill_manual(values = c("saddlebrown", "red")) +
  scale_fill_manual(values = c("gray75", "red")) +
  scale_y_continuous(limits = c(0, 100000),
                     labels = dollar) +
  coord_flip() +
  ggtitle("Brown's spending per athlete on Men's varsity teams",
          subtitle = "Based on Brown University's 2018-2019 academic year") +
  labs(y = "Spending per athlete") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "black"),
        axis.text.y = element_text(size = 14,
                                   color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "black"),
        plot.title = element_text(size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 0.75, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        text = element_text(color = "black",
                            #face = "bold",
                            family = "Helvetica"),
        #plot.background = element_rect(fill = "white"),
        legend.position = "none")

# ## Also export as png
# ggsave("figures/financial/costs_per_athlete.webpage.png",
#        height = 6,
#        width = 9,
#        dpi = 640)



# * 2.2 Total operating costs per team ------------------------------------

## Reorder factors for graphing and add abbreviation to be displayed on graph
cost_withLabels <- cost_perAthlete.Men %>% 
  # Reorder Sport by cost per athlete (reorders the labels on the axis)
  mutate(`Varsity Sport` = fct_reorder(`Varsity Sport`, `Operating Costs`)) %>% 
  # Create new column of operating cost, but in dollar format
  mutate(cost_dollar = dollar(`Operating Costs`, accuracy = 100)) %>% 
  # Separate dollar format column into 3 parts based on thousands place
  separate(cost_dollar, 
           into = c("a", "b", "c"), 
           sep = ",",
           remove = FALSE) %>% 
  # Separate the middle part, which will be the value after the decimal place (whether reported in thousands or millions) 
  separate(b, into = c("post_decimal", "other"), sep = 1) %>%
  # if there is data in the last column, c, that means that the cost value is in the millions:
  # assign K vs M suffix as such
  mutate(suffix = if_else(c %>% is.na() == TRUE, "K", "M")) %>% 
  # unite the decorated columns to create the label
  unite(a, post_decimal, col = "front_end", sep = ".") %>% 
  unite(front_end, suffix, col = "dollar_abbreviation", sep = "")


# * * 2.2.1 Webpage graph ---------------------------------------------------


## Plot total operating cost for each varsity team (Men)
cost_withLabels %>% 
  # Assemble plot
  # Assign fill to Track/XC
  ggplot(aes(x = `Varsity Sport`, y = `Operating Costs`, fill = is.track)) +
  geom_col(color = "black",
           size = 0.35,
           width = 0.75) +
  geom_text(aes(label = dollar_abbreviation),
            color = "black",
            size = 4.5,
            nudge_y = 100000,
            hjust = 0) +
  # geom_text(aes(label = dollar(`Operating Costs`,
  #                              # Divide by 1,000 to report in thousands
  #                              scale = 1/1000,
  #                              # Round to the tenths place
  #                              accuracy = .1,
  #                              # Add suffix
  #                              suffix = "K")),
  #           color = "black",
  #           size = 4.5,
  #           nudge_y = 100000,
  #           hjust = 0) +
annotate(y = 2200000,
         x = "Golf",
         geom = "text",
         color = "black",
         hjust = 0,
         vjust = 1,
         label = "Data taken from the Brown University\n2019 Equity in Athletics report") +
  scale_fill_manual(values = c("gray75", "red")) +
  scale_y_continuous(limits = c(0, 4000000),
                     labels = dollar) +
  coord_flip() +
  ggtitle("Brown's total spending on Men's varsity teams",
          subtitle = "Based on Brown University's 2018-2019 academic year") +
  labs(y = "Total spending") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "black"),
        axis.text.y = element_text(size = 14,
                                   color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "black"),
        plot.title = element_text(size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 1, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        text = element_text(color = "black",
                            #face = "bold",
                            family= "Helvetica"),
        #plot.background = element_rect(fill = "black"),
        legend.position = "none")





## Also export as png
ggsave("figures/financial/total_costs_per_team.webpage.png",
       height = 6,
       width = 8.5,
       dpi = 640)



# * * 2.2.2 IG graph ------------------------------------------------------

## Plot total operating cost for each varsity team (Men)
cost_withLabels %>% 
  # Assemble plot
  # Assign fill to Track/XC
  ggplot(aes(x = `Varsity Sport`, y = `Operating Costs`, fill = is.track)) +
  geom_col(color = "black",
           width = 0.75) +
  geom_text(aes(label = dollar_abbreviation),
            color = "white",
            size = 4.5,
            nudge_y = 100000,
            hjust = 0) +
  # geom_text(aes(label = dollar(`Operating Costs`,
  #                              # Divide by 1,000 to report in thousands
  #                              scale = 1/1000,
  #                              # Round to the tenths place
  #                              accuracy = .1,
  #                              # Add suffix
  #                              suffix = "K")),
  #           color = "white",
  #           size = 4.5,
  #           nudge_y = 100000,
  #           hjust = 0) +
annotate(y = 2200000,
         x = "Golf",
         geom = "text",
         color = "white",
         hjust = 0,
         vjust = 1,
         label = "Data taken from the Brown University\n2019 Equity in Athletics report") +
  scale_fill_manual(values = c("gray50", "red")) +
  scale_y_continuous(limits = c(0, 4000000),
                     labels = dollar) +
  coord_flip() +
  ggtitle("Total operating cost for Men's varsity teams",
          subtitle = "Based on Brown University's 2018-2019 academic year") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "white"),
        axis.text.y = element_text(size = 14,
                                   color = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "white"),
        plot.title = element_text(size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 1, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray22"),
        text = element_text(color = "white",
                            #face = "bold",
                            family= "Helvetica"),
        plot.background = element_rect(fill = "black"),
        legend.position = "none")



## Also export as png
ggsave("figures/financial/total_costs_per_team.IG.png",
       height = 6,
       width = 8.5,
       dpi = 640)



# 3. Youth sports costs ---------------------------------------------------


## Data taken from Aspen Institute/Utah State University 2019 National Youth Sport Survey


# * 3.1 Average total cost per sport --------------------------------------

## Average annual family spending on one child (1-18 years of age) per sport


# * * 3.1.1 Webpage graph -------------------------------------------------


youth_sports %>% 
  # Reorder axis by Total Cost
  mutate(Sport = fct_reorder(Sport, `Total Costs`)) %>% 
  # Assign fill to Track/XC
  ggplot(aes(x = Sport, y = `Total Costs`, fill = is.track)) +
  geom_col(color = "black",
           size = 0.25,
           width = 0.7) +
  geom_text(aes(label = dollar(`Total Costs`)),
            color = "black",
            size = 4.5,
            nudge_y = 100,
            hjust = 0) +
  annotate(y = 1200,
           x = "Flag Football",
           geom = "text",
           color = "black",
           hjust = 0,
           vjust = 1,
           size = 3.8,
           label = "Data taken from the Aspen Institute/Utah State\nUniversity 2019 National Youth Sport Survey") +
           #label = "Data taken from the Aspen Institute/Utah State\nUniversity 2019 National Youth Sport Survey\nhttps://www.aspenprojectplay.org/national-youth-sport-survey/1") +
  
  scale_fill_manual(values = c("gray75", "red")) +
  scale_y_continuous(labels = dollar,
                     limits = c(0,3000)) +
  coord_flip() +
  ggtitle("Average annual family spending per child for\nyouth sports (1-18 years old) in the U.S.") +
  labs(y = "Total cost per child") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "black"),
        axis.text.y = element_text(size = 12,
                                   color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "black"),
        plot.title = element_text(size = 15,
                                  #margin = margin(b = 0.5, unit = "cm"),
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(t = 0.1, 0.5, b = 0.1, r = 0.75, unit = "cm"),
        #plot.margin = margin(t = 0.1, 1.5, b = 0.1, r = 1.5, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        text = element_text(color = "black",
                            #face = "bold",
                            family= "Helvetica"),
        #plot.background = element_rect(fill = "black"),
        legend.position = "none")


## Also export as png
ggsave("figures/financial/youthSport_totalCosts.webpage.png",
       height = 6,
       width = 8,
       dpi = 640)

# * * 3.1.2 IG graph ------------------------------------------------------


youth_sports %>% 
  # Reorder axis by Total Cost
  mutate(Sport = fct_reorder(Sport, `Total Costs`)) %>% 
  # Assign fill to Track/XC
  ggplot(aes(x = Sport, y = `Total Costs`, fill = is.track)) +
  geom_col(color = "black",
           width = 0.75) +
  geom_text(aes(label = dollar(`Total Costs`)),
            color = "white",
            size = 4.5,
            nudge_y = 100,
            hjust = 0) +
  annotate(y = 1100,
           x = "Skateboarding",
           geom = "text",
           color = "white",
           hjust = 0,
           vjust = 1,
           size = 3.5,
           label = "Data taken from the Aspen Institute/Utah State\nUniversity 2019 National Youth Sport Survey\nhttps://www.aspenprojectplay.org/national-youth-sport-survey/1") +
  scale_fill_manual(values = c("gray50", "red")) +
  scale_y_continuous(labels = dollar,
                     limits = c(0,3000)) +
  coord_flip() +
  ggtitle("Average annual family spending per child for\nyouth sports (1-18 years old) in the U.S.") +
  labs(y = "Total cost per child") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "white"),
        axis.text.y = element_text(size = 14,
                                   color = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "white"),
        plot.title = element_text(size = 16,
                                  margin = margin(b = 0.5, unit = "cm"),
                                  face = "bold"),
        plot.subtitle = element_text(size = 13,
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 0.75, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray22"),
        text = element_text(color = "white",
                            #face = "bold",
                            family= "Helvetica"),
        plot.background = element_rect(fill = "black"),
        legend.position = "none")


## Also export as png
ggsave("figures/financial/youthSport_totalCosts.IG.png",
       height = 7,
       width = 8.5,
       dpi = 640)


