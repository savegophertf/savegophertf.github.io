##
##    Project:    Save Brown Track & Field/Cross Country!!!
##
##    Name:       BTF_improvement_analysis.R
##
##    Approach:   Read in and analyze data from survey given to all current athletes regarding
##                what would be the best financial investments for BTF
##                
##    Authors:    Calvin J. Munson
##
##    Date:       June 24th, 2020
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
library(ggrepel)


# * 1.2 Read in data ------------------------------------------------------

survey_raw <- read_csv("data/improvement_survey/Brown XCTF Improvement (Current) (Responses) - Form Responses 1.csv") %>% 
  dplyr::rename(date_time = 1) %>% 
  filter(date_time %>% is.na() == FALSE) %>% 
  mutate(answer_num = 1:n())



# * 1.3 Assemble labeling function ----------------------------------------

append_suffix <- function(num){
  suff <- case_when(num %in% c(11,12,13) ~ "th",
            num %% 10 == 1 ~ 'st',
            num %% 10 == 2 ~ 'nd',
            num %% 10 == 3 ~'rd',
            TRUE ~ "th")
  
  paste0(num, suff)
}



# 2. Clean data -----------------------------------------------------------


# * 2.1 Separate data by survey question ----------------------------------


# * * 2.1.1 Rank improvements ---------------------------------------------

survey_raw %>% colnames()


## "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [1st-6th]"
rank_improvement <- survey_raw %>% 
  # Select proper columns from this question
  dplyr::select(answer_num, 4, 5, 6, 7, 8, 9) %>% 
  # Rename survey questions to the rank that they coorespond to
  dplyr::rename("1" = "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [1st]",
                "2" = "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [2nd]",
                "3" = "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [3rd]",
                "4" = "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [4th]",
                "5" = "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [5th]",
                "6"= "Please rank the most meaningful near-term improvement to elevate the Brown XC/TF [6th]") %>% 
  # Pivot dataframe so that rank is also its own column
  pivot_longer(c(2:7), names_to = "Rank", values_to = "Improvement") %>% 
  mutate(Rank = as.integer(Rank))

## Count number of responses that categorized each improvement type as each rank
rank_improvement.n <- rank_improvement %>% 
  group_by(Improvement, Rank) %>% 
  summarise(n = n())

## Calculate average rank for each improvement
rank_improvement.mean <- rank_improvement %>% 
  group_by(Improvement) %>% 
  summarise(avg_rank = mean(Rank))



# * * 2.1.2 Rank decision influence ---------------------------------------


survey_raw %>% colnames()


## "Rank the order of importance the following had on your decision to come to Brown (1 to 5)"
rank_influence <- survey_raw %>% 
  # Select proper columns from this question
  dplyr::select(answer_num, 10:14) %>% 
  # Rename survey questions to the rank that they coorespond to
  dplyr::rename("1" = "Rank the order of importance the following had on your decision to come to Brown (1 to 5) [1st]",
                "2" = "Rank the order of importance the following had on your decision to come to Brown (1 to 5) [2nd]",
                "3" = "Rank the order of importance the following had on your decision to come to Brown (1 to 5) [3rd]",
                "4" = "Rank the order of importance the following had on your decision to come to Brown (1 to 5) [4th]",
                "5" = "Rank the order of importance the following had on your decision to come to Brown (1 to 5) [5th]") %>% 
  # Pivot dataframe so that rank is also its own column
  pivot_longer(c(2:6), names_to = "Rank", values_to = "Influence") %>% 
  mutate(Rank = as.integer(Rank))

## Count number of responses that categorized each improvement type as each rank
rank_influence.n <- rank_influence %>% 
  group_by(Influence, Rank) %>% 
  summarise(n = n())

## Calculate average rank for each influence
rank_influence.mean <- rank_influence %>% 
  group_by(Influence) %>% 
  summarise(avg_rank = mean(Rank))



# * * 2.1.3 Rate current aspects of program ---------------------------------------


survey_raw %>% colnames()


##  rate those particular aspects of the program on a 1-5 scale, 1 being terrible and needs immediate improvement, 5 being amazing)
rating_aspect <- survey_raw %>% 
  # Select proper columns from this question
  dplyr::select(answer_num, 15:19) %>% 
  # Pivot dataframe so that rank is also its own column
  pivot_longer(c(2:6), names_to = "Program Aspect", values_to = "Rating") 

## Count number of responses that categorized each improvement type as each rank
rating_aspect.n <- rating_aspect %>% 
  group_by(`Program Aspect`, Rating) %>% 
  summarise(n = n())

## Calculate average rank for each influence
rating_aspect.mean <- rating_aspect %>% 
  group_by(`Program Aspect`) %>% 
  summarise(avg_rating = mean(Rating))





# 3. Visualize ------------------------------------------------------------



# * 3.1 Rank improvements -------------------------------------------------

rank_improvement.n %>% 
  ggplot(aes(x = Improvement, y = Rank, size = n, fill = n)) +
  geom_point(shape = 21) +
  scale_y_reverse() +
  coord_flip() +
  theme_minimal()


rank_improvement %>% 
  merge(rank_improvement.mean) %>% 
  mutate(Improvement = fct_reorder(Improvement, desc(avg_rank))) %>% 
  ggplot(aes(x = Improvement, y = Rank, fill = Rank)) +
  geom_jitter(width = 0.1, 
              height = 0.15,
              fill = "gray50",
              shape = 21,
              size = 2,
              alpha = 0.25) +
  geom_segment(data = rank_improvement.mean,
               inherit.aes = FALSE,
               aes(x = Improvement,
                   xend = Improvement,
                   y = 6,
                   yend = avg_rank),
               size = 0.35) + 
  geom_text_repel(data = rank_improvement.mean,
                  inherit.aes = FALSE,
                  aes(label = paste("Avg. rank:", round(avg_rank, 1), sep = " "),
                      x = Improvement,
                      y = avg_rank),
                  box.padding = 0.5,
                  segment.size = 0.35,
                  segment.color = "gray40",
                  nudge_x = .35,
                  nudge_y = .75) +
  geom_point(data = rank_improvement.mean,
             inherit.aes = FALSE,
             aes(x = Improvement,
                 y = avg_rank),
             size = 6.5,
             shape = 21,
             fill = "white") +
  scale_fill_gradient2(low = "gray88", mid = "steelblue", high = "darkblue", midpoint = 3) +
  # scale_y_continuous(limits = c(0, 7),
  #                    breaks = c(1:6)) +
  scale_y_reverse(breaks = c(1:6)) +
  coord_flip() +
  ggtitle("Student-Athlete rankings in order of needing the\nmost improvement",
          subtitle = "1 = Needs most improvement; 6 = Needs least improvement") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12,
                                   margin = margin(b = 0.25, unit = "cm")),
        axis.title.x = element_text(size = 12),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
        plot.subtitle = element_text(size = 11),
        legend.position = "none")

## Also export as png
ggsave("figures/improvement_survey/program_improvement_ranking.png",
       height = 5,
       width = 7,
       dpi = 640)




# * 3.2 Rank decision influence -------------------------------------------

rank_influence.n %>% 
  ggplot(aes(x = Influence, y = Rank, size = n, fill = n)) +
  geom_point(shape = 21) +
  scale_y_reverse() +
  coord_flip() +
  theme_minimal()


rank_influence %>% 
  merge(rank_influence.mean) %>% 
  mutate(Influence = fct_reorder(Influence, desc(avg_rank))) %>% 
  ggplot(aes(x = Influence, y = Rank, fill = Rank)) +
  geom_jitter(width = 0.1, 
              height = 0.15,
              fill = "gray50",
              shape = 21,
              size = 2,
              alpha = 0.25) +
  geom_segment(data = rank_influence.mean,
               inherit.aes = FALSE,
               aes(x = Influence,
                   xend = Influence,
                   y = 5,
                   yend = avg_rank),
               size = 0.35) + 
  geom_text_repel(data = rank_influence.mean,
                  inherit.aes = FALSE,
                  aes(label = paste("Avg. rank:", round(avg_rank, 1), sep = " "),
                      x = Influence,
                      y = avg_rank),
                  box.padding = 0.5,
                  segment.size = 0.35,
                  segment.color = "gray40",
                  nudge_x = .35,
                  nudge_y = .75) +
  geom_point(data = rank_influence.mean,
             inherit.aes = FALSE,
             aes(x = Influence,
                 y = avg_rank),
             size = 6.5,
             shape = 21,
             fill = "white") +
  scale_fill_gradient2(low = "gray88", mid = "steelblue", high = "darkblue", midpoint = 3) +
  # scale_y_continuous(limits = c(0, 7),
  #                    breaks = c(1:6)) +
  scale_y_reverse(breaks = c(1:5)) +
  coord_flip() +
  ggtitle("Student-Athlete rankings of what had the most influence\non their decisions to attend Brown",
          subtitle = "1 = Most important; 5 = Least important") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12,
                                   margin = margin(b = 0.25, unit = "cm")),
        axis.title.x = element_text(size = 12),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
        plot.subtitle = element_text(size = 11),
        legend.position = "none")

## Also export as png
ggsave("figures/improvement_survey/program_influence_ranking.png",
       height = 5,
       width = 7,
       dpi = 640)



# * 3.3 Rate current aspects of program -----------------------------------


rating_aspect %>% 
  merge(rating_aspect.mean) %>% 
  mutate(`Program Aspect` = fct_reorder(`Program Aspect`, avg_rating)) %>% 
  ggplot(aes(x = `Program Aspect`, y = Rating, fill = Rating)) +
  geom_jitter(width = 0.1, 
              height = 0.15,
              fill = "gray50",
              shape = 21,
              size = 2,
              alpha = 0.25) +
  geom_segment(data = rating_aspect.mean,
               inherit.aes = FALSE,
               aes(x = `Program Aspect`,
                   xend = `Program Aspect`,
                   y = 1,
                   yend = avg_rating),
               size = 0.35) + 
  geom_text_repel(data = rating_aspect.mean,
                  inherit.aes = FALSE,
                  aes(label = paste("Average rating:", round(avg_rating, 1), sep = " "),
                      x = `Program Aspect`,
                      y = avg_rating),
                  box.padding = 0.5,
                  segment.size = 0.35,
                  segment.color = "gray40",
                  nudge_x = .5,
                  nudge_y = .5) +
  geom_point(data = rating_aspect.mean,
             inherit.aes = FALSE,
             aes(x = `Program Aspect`,
                 y = avg_rating),
             size = 6.5,
             shape = 21,
             fill = "white") +
  scale_fill_gradient2(low = "gray88", mid = "steelblue", high = "darkblue", midpoint = 3) +
  scale_y_continuous(limits = c(0.5, 5.5),
                     breaks = c(1:5)) +
  coord_flip() +
  ggtitle("Student-Athlete ratings of each aspect of the\nTrack and Field program at Brown",
          subtitle = "1 = Terrible, needs immediate improvement; 5 = Amazing") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12,
                                   margin = margin(b = 0.25, unit = "cm")),
        axis.title.x = element_text(size = 12),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
        plot.subtitle = element_text(size = 11),
        legend.position = "none")

## Also export as png
ggsave("figures/improvement_survey/program_aspect_rating.png",
       height = 5,
       width = 7,
       dpi = 640)



# 4. Assemble tables ------------------------------------------------------


# * 4.1 Rank improvements -------------------------------------------------

rank_improvement.table <- rank_improvement.n %>% 
  pivot_wider(names_from = Improvement, values_from = n) %>% 
  mutate(Rank = append_suffix(Rank))


# * 4.2 Rank decision influence -------------------------------------------

rank_influence.table <- rank_influence.n %>% 
  pivot_wider(names_from = Influence, values_from = n) %>% 
  mutate(Rank = append_suffix(Rank)) %>% 
  replace(., is.na(.), 0)

# * 4.3 Rate current aspects  --------------------------------------


rating_aspect.table <- rating_aspect.n %>% 
  pivot_wider(names_from = `Program Aspect`, values_from = n) %>% 
  replace(., is.na(.), 0) %>% 
  arrange(desc(Rating))
  
