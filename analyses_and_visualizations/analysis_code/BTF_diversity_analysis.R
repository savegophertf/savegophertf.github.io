##
##    Project:    Save Brown Track & Field/Cross Country!!!
##
##    Name:       BTF_diversity_analysis.R
##
##    Approach:   Read in and analyze data from Brown varsity sports, including financial data
##                and data on Brown Track and Field and surveyed cost data from Youth Sports
##                
##    Authors:    Calvin J. Munson
##
##    Date:       June 6th, 2020
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

## NCAA-wide data
ncaa_div_raw <- read_csv("data/diversity/data_raw/ncaa.csv")

## Just Ivy-League data
Ivy_div_percent <- read_xlsx("data/diversity/data_intermediate/brown_racial_data_mens.IvyLeague.xlsx", sheet = "By Percent")

## NCAA-Ivy totals for comparisons
NCAA_Ivy_comp <- read_csv("data/diversity/data_final/NCAA_vs_Ivy_Totals.csv")


# 2. Clean NCAA data ------------------------------------------------------


# * 2.1 Separate out combined columns -------------------------------------

# What are the unique Divisions in this dataset?
ncaa_div_raw$Division_Subdivision %>% unique()

# What are the unique Gender_Race combinations?
ncaa_div_raw$`Gender_Race/Ethnicity` %>% unique()

# What are the unique Sports?
ncaa_div_raw$Sport %>% unique()


## Separate out the division_subdivision and gender_race columns
ncaa_div_sep <- ncaa_div_raw %>% 
  # Rename the various football categories to all be "Football"
  mutate(Sport = fct_recode(Sport,
                            "Football" = "Football-FBS",
                            "Football" = "Football-FCS")) %>% 
  # Separate by the "_" which separates division and subdivision
  separate(col = Division_Subdivision, 
           into = c("Division", "Subdivision", "Subdivision_2"), 
           sep = "_", 
           remove = FALSE) %>% 
  # Unite subdivisions which had a second "_" in them
  unite(col = "Subdivision", Subdivision, Subdivision_2) %>% 
  # Separate Gender and Race from the Gender_Race column
  separate(col = `Gender_Race/Ethnicity`, into = c("Gender", "Race/Ethnicity"), sep = "-") %>% 
  # Separate Sport column to remove gender from sport (e.g. Men's Golf)
  separate(col = Sport, into = c("Sport_Gender", "Sport"), sep = "'s ") %>%
  # Some sports don't have a gender in front (Football, Baseball), so this separation adds NA's to the Sport column,
  # while the sport itself remains in the Sport_Gender column.
  # Use if_else to re-add the sport to the Sport column if there's an NA there
  mutate(Sport = if_else(Sport %>% is.na() == TRUE, Sport_Gender, Sport))

ncaa_div_sep$Sport %>% unique()
ncaa_div_sep$Sport_Gender %>% unique()


ncaa_div_sep %>% 
  filter(Sport == "Football" |
           Sport == "Football-FBS" |
           Sport == "Football-FCS",
         `Title/Position` == "Student-Athlete")



# * 2.2 Pivot data --------------------------------------------------------

## Pivot data frame so that year is its own column, and the number of people in each category goes
# into its own row
ncaa_div_pivot <- ncaa_div_sep %>% 
  pivot_longer(cols = c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), 
               names_to = "Year",
               values_to = "Number_People")


# * 2.3 Filter data for D1 athletes only, rename sports ---------------------------------

## Filter data frame to only include Division 1, 
## and only Athletes (rather than coaches, assistant coaches)

ncaa_div_pivot$`Title/Position` %>% unique()

ncaa_div_filt <- ncaa_div_pivot %>% 
  filter(Division == "Division I",
         `Title/Position` == "Student-Athlete")



# * 2.4 Calculate percentages  --------------------------------------------


# * * 2.4.1 Percent athletes per race & sport -----------------------------

percent_sport <- ncaa_div_filt %>% 
  filter(Gender == "Male",
         Sport_Gender != "Women",
         Year == "2019") %>% 
  # Calculate number of athletes per sport, year, and race
  group_by(Division, Sport, Gender, `Race/Ethnicity`, Year) %>% 
  summarise(N_Athletes = sum(Number_People, na.rm = TRUE)) %>% 
  # Add column for total number of athletes per sport per year, irrespective of race
  group_by(Division, Sport, Gender, Year) %>% 
  mutate(Total_Athletes = sum(N_Athletes, na.rm = TRUE)) %>% 
  # Calculate percent of each Race/Ethnicity for each sport
  ungroup() %>% 
  mutate(Percent_Athletes = N_Athletes/Total_Athletes*100) %>% 
  # Rename the track categories to remove commas
  mutate(Sport = fct_recode(Sport,
                            "Indoor Track" = "Track, Indoor",
                            "Outdoor Track" = "Track, Outdoor"))


# * * 2.4.2 Percent athletes per race/ethnicity total -----------------------------

percent_total <- percent_sport %>% 
  # Add up all athletes in NCAA per race
  group_by(Division, Year, Gender, `Race/Ethnicity`) %>% 
  summarise(N_Athletes.allSports = sum(N_Athletes, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Calculate total number of athletes, irrespective of race,
  # Calculate percent of each racial group across all NCAA
  mutate(Total_athletes.allSports = sum(N_Athletes.allSports, na.rm = TRUE),
         Percent_Athletes.allSports = N_Athletes.allSports/Total_athletes.allSports*100)




# * 2.5 Export cleaned data -----------------------------------------------

## Pivot so that each value is a column
ncaa_cleaned <- percent_sport %>% 
  pivot_wider(names_from = `Race/Ethnicity`, values_from = c(N_Athletes, Percent_Athletes))

## Export
write_csv(ncaa_cleaned, path = "data/diversity/data_intermediate/ncaa_cleaned.csv")


# 3. NCAA vs Ivy Totals ---------------------------------------------------

NCAA_Ivy_comp


# * 3.1 Calculate percentages per Race/Ethnicity and conference ------------

NCAA_Ivy_perc <- NCAA_Ivy_comp %>% 
  # Calculate total athletes per conference/gender
  group_by(Division, Conference, Year, Gender) %>% 
  mutate(Total_Athletes = sum(N_Athletes)) %>% 
  ungroup() %>% 
  # Caculate percentage race/ethnicity composition per conference/gender
  # Also rename "All" to be "All NCAA"
  # Reorder race factors
  mutate(Percent_group = N_Athletes/Total_Athletes*100,
         Conference = fct_recode(Conference,
                                 "All NCAA D1" = "All"),
         `Race/Ethnicity` = fct_relevel(`Race/Ethnicity`,
                                        "White", "Black", "Other"))

## Seperate out Males
NCAA_Ivy_perc.Male <- NCAA_Ivy_perc %>% 
  filter(Gender == "Male")




# * 3.2 Visualize NCAA vs Ivy race percentages -------------------------------

NCAA_Ivy_perc.Male %>% 
  ggplot(aes(x = `Race/Ethnicity`, y = Percent_group, fill = Conference)) +
  geom_col(position = position_dodge(width = 0.6),
           width = 0.6,
           color = "black") +
  geom_text(aes(label = round(Percent_group, 1) %>% percent(scale = 1),
                y = Percent_group + 7),
            position = position_dodge(width = 0.6),
            fontface = "plain",
            size = 5) +
  scale_y_continuous(limits = c(0,75), 
                     labels = seq(0, 75, by = 25) %>% percent(scale = 1),
                     breaks = seq(0, 75, by = 25)) +
  #scale_fill_manual(values = c("white", "black")) +
  scale_fill_manual(values = c("gray80", "darkgreen")) +
  labs(x = "Race/Ethnicity",
       y = "Percent composition") +
  ggtitle("Demographic composition of Ivy League athletes compared\nto Division I NCAA-wide averages (Men's teams)",
          subtitle = "Source: NCAA Demographics Database, 2019") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "black"),
        axis.text.y = element_text(size = 15,
                                   color = "black"),
        axis.title.y = element_text(size = 15,
                                    margin = margin(r = 0.5, unit = "cm"),
                                    color = "black"),
        axis.title.x = element_text(size = 15,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "black"),
        plot.title = element_text(size = 16,
                                  face = "plain"),
        plot.subtitle = element_text(size = 11,
                                     margin = margin(b = 0.5, unit = "cm"),
                                     face = "italic"),
        plot.margin = margin(0.5, l = 0.75, 0.5, r = 0.75, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(color = "black",
                            family= "Helvetica"),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = c(0.75, .85))

## Also export as png
ggsave("figures/diversity/NCAA_vs_Ivy.png",
       height = 5,
       width = 8,
       dpi = 640) 


NCAA_Ivy_perc.Male %>% 
  ggplot(aes(x = `Race/Ethnicity`, y = Percent_group)) +
  geom_col(position = position_dodge(width = 0.6),
           aes(alpha = Conference, fill = `Race/Ethnicity`),
           width = 0.6,
           color = "black") +
  geom_text(aes(label = round(Percent_group, 1) %>% percent(scale = 1),
                alpha = Conference,
                y = Percent_group + 7),
            position = position_dodge(width = 0.6),
            fontface = "plain",
            color = "black",
            size = 5) +
  scale_y_continuous(limits = c(0,75), 
                     labels = seq(0, 75, by = 25) %>% percent(scale = 1),
                     breaks = seq(0, 75, by = 25)) +
  #scale_fill_manual(values = c("white", "black")) +
  #scale_fill_manual(values = c("white", "green4")) +
  scale_alpha_manual(values = c(.55, 1)) +
  scale_fill_manual(values = c("gray80", "darkred", "gray20")) +
  guides(fill = "none") +
  labs(x = "Race/Ethnicity",
       y = "Percent composition") +
  ggtitle("Demographic composition of Ivy League athletes compared\nto Division I NCAA-wide averages (Men's teams)",
          subtitle = "Source: NCAA Demographics Database, 2019") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16,
                                   margin = margin(t = 0.5, unit = "cm"),
                                   color = "black"),
        axis.text.y = element_text(size = 15,
                                   color = "black"),
        axis.title.y = element_text(size = 15,
                                    margin = margin(r = 0.5, unit = "cm"),
                                    color = "black"),
        axis.title.x = element_text(size = 15,
                                    margin = margin(t = 0.5, unit = "cm"),
                                    color = "black"),
        plot.title = element_text(size = 16,
                                  face = "plain"),
        plot.subtitle = element_text(size = 11,
                                     margin = margin(b = 0.5, unit = "cm"),
                                     face = "italic"),
        plot.margin = margin(0.5, 0.5, 0.5, r = 0.75, unit = "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(color = "black",
                            family= "Helvetica"),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = c(0.75, .85))
