
library(tidyverse)
library(readxl)
library(wesanderson)

## Get the data

d <- read_excel("./data/Results Oregano Rosemary.xlsx",
                sheet = "for_R") %>%
    mutate(condition = ifelse(condition == "1", "0.33", condition),
           condition = ifelse(condition == "5", "1.66", condition),
           condition = ifelse(condition == "15", "5", condition),
           condition = ifelse(condition == "30", "10", condition)
           )

## Microbial concentrations

d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    group_by(experiment, condition) %>%
    summarize(m_logN = mean(logN, na.rm = TRUE), sd_logN = sd(logN, na.rm = TRUE)) %>%
    mutate(condition = factor(condition, 
                              # levels = c("C+", "W", "Cl", "1", "5", "15", "30")
                              levels = c("C+", "W", "Cl", "0.33", "1.66", "5", "10")
                              )) %>%
    ggplot() +
    geom_col(aes(x = experiment, y = m_logN, fill = condition),
             position = "dodge") +
    geom_errorbar(aes(x = experiment, ymin = m_logN - sd_logN, ymax = m_logN + sd_logN,
                      colour = condition), position = "dodge") +
    xlab("") + ylab("Microbial concentration (log CFU/ml)")

## Figure 1

p <- d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    mutate(experiment = ifelse(condition == "C+", "Untreated", experiment)) %>%
    mutate(experiment = ifelse(condition == "Cl", "Commercial", experiment)) %>%
    mutate(experiment = ifelse(condition == "W", "Water", experiment)) %>%
    mutate(experiment = ifelse(experiment == "oregano Spain", "Oregano solution - Spain", experiment)) %>%
    mutate(experiment = ifelse(experiment == "oregano Romania", "Oregano solution - Romania", experiment)) %>%
    mutate(experiment = ifelse(experiment == "rosemary Spain", "Rosemary solution - Spain", experiment)) %>%
    mutate(experiment = ifelse(experiment == "rosemary Romania", "Rosemary solution - Romania", experiment)) %>%
    mutate(condition = as.numeric(condition),
           condition = as.character(condition),
           condition = ifelse(is.na(condition), "-", 
                              paste0(condition, "%")
           )
    ) %>%
    group_by(experiment, condition) %>%
    summarize(m_logN = mean(logN, na.rm = TRUE), sd_logN = sd(logN, na.rm = TRUE)) %>%
    ungroup() %>%
    # mutate(condition = factor(condition, levels = c("-", "1%", "5%", "15%", "30%"))) %>%
    mutate(condition = factor(condition, levels = c("-", "0.33%", "1.66%", "5%", "10%"))) %>%
    mutate(experiment = factor(experiment,
                               levels = c("Untreated", "Water", "Commercial", 
                                          "Oregano solution - Spain", "Oregano solution - Romania",
                                          "Rosemary solution - Spain", "Rosemary solution - Romania")
                               )
           ) %>%
    ggplot() +
    geom_col(aes(x = condition, y = m_logN),
             position = "dodge",
             fill = "grey45") +
    geom_errorbar(aes(x = condition, ymin = m_logN - sd_logN, ymax = m_logN + sd_logN),
                  position = "dodge", width = .2,
                  colour = "grey45") +
    facet_grid(~experiment, scales = "free",
               space = "free_x") +
    theme_bw(base_size = 14) +
    xlab("") + ylab("Microbial concentration (log CFU/ml)")

ggsave(p, filename = "Figure_1.png",
       width = 18, height = 6)

## Number of reductions

d_control <- d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    filter(condition == "C+") %>%
    group_by(experiment) %>%
    summarize(logN0 = mean(logN, na.rm = TRUE))

d_cl <- d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    filter(condition == "Cl") %>%
    group_by(experiment) %>%
    summarize(logN_Cl = mean(logN, na.rm = TRUE))

d_w <- d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    filter(condition == "W") %>%
    group_by(experiment) %>%
    summarize(logN_w = mean(logN, na.rm = TRUE))

d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    mutate(condition = as.numeric(condition)) %>%
    filter(!is.na(condition)) %>%
    full_join(., d_control) %>%
    full_join(., d_cl) %>%
    full_join(., d_w) %>%
    mutate(red = logN0 - logN,
           red_CL = logN0 - logN_Cl,
           red_w = logN0 - logN_w) %>%
    ggplot(aes(x = condition, y = red, colour = experiment)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(aes(yintercept = red_CL, colour = experiment
                   ), linetype = 2, size = 1) +
    geom_hline(aes(yintercept = red_w, colour = experiment),
                   linetype = 3, size = 1) +
    xlab("Concentration") + ylab("Number of log reductions")

## Figure 2

aa <- d %>%
    pivot_longer(-c(condition, repetition),
                 names_to = "experiment",
                 values_to = "logN") %>%
    mutate(condition = as.numeric(condition)) %>%
    filter(!is.na(condition)) %>%
    full_join(., d_control) %>%
    full_join(., d_cl) %>%
    full_join(., d_w) %>%
    mutate(red = logN0 - logN,
           red_CL = logN0 - logN_Cl,
           red_w = logN0 - logN_w) 

my_cols <- wes_palette("Moonrise2", 4)

p <- aa %>%
    group_by(condition, experiment) %>%
    summarize(m_red = mean(red, na.rm = TRUE), sd_red = sd(red, na.rm = TRUE)) %>%
    separate(experiment, into = c("oil", "origin"), sep =  " ") %>%
    ggplot(aes(x = condition, y = m_red, colour = origin)) +
    geom_smooth(method = "lm", se = FALSE, size = 1,
                aes(linetype = oil)) +
    geom_point(aes(shape = oil), size = 3) +
    geom_errorbar(aes(ymin = m_red - sd_red, ymax = m_red + sd_red),
                  width = .1) +
    geom_hline(aes(yintercept = mean(aa$red_CL)), linetype = 4, size = 1,
               colour = my_cols[1]) +
    geom_hline(aes(yintercept = mean(aa$red_w)),
               linetype = 3, size = 1,
               colour = my_cols[4]) +
    xlab("Concentration of EO (%)") + 
    ylab("Number of reductions (log CFU/g)") +
    scale_shape_manual(values = c(16, 17)) +
    scale_color_manual(values = my_cols[c(3,2)]) +
    theme_classic(base_size = 14) +
    theme(legend.position = "top",
          legend.title = element_blank())

ggsave(p, filename = "Figure_2.png",
       width = 9, height = 6)

## Table 2

aa %>%
    select(condition, experiment, red) %>%
    split(.$experiment) %>%
    map(.,
        ~ lm(red ~ condition, data = .)
        ) %>%
    map(summary)














