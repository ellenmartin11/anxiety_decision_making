---
title: "Summary Plots Digging"
author: "Ellen Martin"
date: "`r Sys.Date()`"
output: html_document
---

Read in the play 2 data ahead of time using other script. (DiggingCPC_5_play2)

Play 2 has 1922 unique participants' worth of data.

### Risk-Taking Behaviour in the Gain Domain, Based on Future Information

-   participants take the riskier option around 50% of the time in gain domains when the future is positive.
-   when the future is negative, participants take the riskier option less.

```{r Import Data from RDS and Libraries}

happyData1 <- readRDS("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/data/happyData/happyData1.rds")
happyData1_diag <- readRDS("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/data/happyData/happyData1_diag.rds")
happyData1_past <- readRDS("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/data/happyData/happyData1_past.rds")

library(readr)
library(NatParksPalettes)
library(ggplot2)
library(Hmisc) #for computing correlation stuff
library(knitr) 
library(tidyverse, warn.conflict=F)
library(corrplot)
library(dplyr)
library(MASS)
library(sandwich)
library(lmtest)
library(broom)
library(dplyr)
library(gt)
library(lme4)
library(dunn.test)

source("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/poster_theme.R") #for figure themes
source("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/functions/process_initquiz.R")
source("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/functions/process_digging_long.R")
source("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/functions/process_params.R")
source("~/Desktop/Rutledge Lab/ApathyDepression/updatedData/functions/softmax.R")
```

```{r Summary Plot % Risk-Taking Based on Future in Gain Domain}

sample_size = length(unique(happyData1$userKey))

gamb_future <- happyData1 %>%
  mutate(Future = case_when(NextIsland == 2 ~ "Positive",
                            NextIsland == 1 ~ "Negative")) %>% 
  filter(Trial=="Gain") %>% # selecting only gain domain
  group_by(userKey,Future) %>%
  summarise(total_choices = n(),
            risky_choices = sum(Choice == 2, na.rm = TRUE),
            percent_risky = (risky_choices / total_choices) * 100,
            .groups = 'drop')

summary_gamb_future <- gamb_future %>%
  group_by(Future) %>%
  summarise(mean_gambling = mean(percent_risky),
            sem_gambling = sd(percent_risky)/sqrt(n())) %>%
  na.omit()

fig1a <- ggplot(summary_gamb_future, aes(x=Future, y=mean_gambling, color=Future)) +
    geom_point(position = position_dodge(0.3), width = 0.3) +
    geom_errorbar(aes(ymin = mean_gambling - sem_gambling, ymax = mean_gambling + sem_gambling),
                  position = position_dodge(0.3), width = 0.1) + 
    geom_hline(yintercept = 50, linetype = "dotted", color = "black") +  
    scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
    labs(
      title = paste0("% Gambling based on Future (Gain Domain) (N=", sample_size, ")"),
      x = "Future",
      y = "% Risky Choices"
    ) +
    ylim(40,70) + 
    theme_minimal() +
    poster_theme 

print(fig1a)

# summary: increased gambling when the future is positive vs negative in gain domain. 

```

### Risk-Taking in the Gain Domain Based on Future Information and Anxiety Diagnosis

-   Participants without anxiety particularly show the above effect of taking the riskier option more in the gain domain when the future is positive vs negative.
-   Participants with anxiety show similar behaviour irrespective of future information.

```{r Summary Plot % Risk-Taking Based on Future in Gain Domain and Diagnosis}

sample_size = length(unique(happyData1_diag$userKey))

# Prepare data for plotting
gamb_future <- happyData1_diag %>%
  mutate(Future = case_when(NextIsland == 2 ~ "Positive",
                            NextIsland == 1 ~ "Negative")) %>% 
  filter(Trial == "Gain") %>%  # selecting only gain domain
  group_by(userKey, Future, diagnosis_anxiety) %>%
  summarise(total_choices = n(),
            risky_choices = sum(Choice == 2, na.rm = TRUE),
            percent_risky = (risky_choices / total_choices) * 100,
            .groups = 'drop')

# Summarize the data for plotting
summary_gamb_future <- gamb_future %>%
  group_by(Future, diagnosis_anxiety) %>%
  summarise(mean_gambling = mean(percent_risky, na.rm = TRUE),
            sem_gambling = sd(percent_risky, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop')

# Create the plot with facet_wrap for "anx" and "no anx" groups
fig1b <- ggplot(summary_gamb_future, aes(x = Future, y = mean_gambling, color = Future)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean_gambling - sem_gambling, ymax = mean_gambling + sem_gambling),
                position = position_dodge(0.3), width = 0.1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
  labs(
    title = paste0("% Gambling based on Future (Gain Domain) (N = ", sample_size, ")"),
    x = "Future",
    y = "% Risky Choices"
  ) +
  ylim(40, 70) +
  theme_minimal() +
  poster_theme + 
  facet_wrap(~ diagnosis_anxiety, labeller = labeller(diagnosis_anxiety = c("anx" = "Anxiety", "no anx" = "No Anxiety")))

# Display the plot
print(fig1b)


```

### Risk-Taking Behaviour in Gain Domain Based on Future Information and Depression Diagnosis

-   Reversed patten to anxiety diagnosis, where participants with depression exhibit different risk-taking behaviour when the future is positive vs negative in the gain domain.
-   Participants with depression diagnosis take more risks when the future is positive vs negative
-   Participants without depression also do this, but there is less of a difference in the % of risk-taking based on future information.

```{r Summary Plot % Risk-Taking Based on Future in Gain Domain and Depression Diagnosis}

sample_size = length(unique(happyData1_diag$userKey))

# Prepare data for plotting
gamb_future <- happyData1_diag %>%
  mutate(Future = case_when(NextIsland == 2 ~ "Positive",
                            NextIsland == 1 ~ "Negative")) %>% 
  filter(Trial == "Gain") %>%  # selecting only gain domain
  group_by(userKey, Future, diagnosis_depression) %>%
  summarise(total_choices = n(),
            risky_choices = sum(Choice == 2, na.rm = TRUE),
            percent_risky = (risky_choices / total_choices) * 100,
            .groups = 'drop')

# Summarize the data for plotting
summary_gamb_future <- gamb_future %>%
  group_by(Future, diagnosis_depression) %>%
  summarise(mean_gambling = mean(percent_risky, na.rm = TRUE),
            sem_gambling = sd(percent_risky, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop') %>%
  na.omit()

# Create the plot with facet_wrap for "dep" and "no dep" groups
fig1b2 <- ggplot(summary_gamb_future, aes(x = Future, y = mean_gambling, color = Future)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean_gambling - sem_gambling, ymax = mean_gambling + sem_gambling),
                position = position_dodge(0.3), width = 0.1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
  labs(
    title = paste0("% Gambling based on Future (Gain Domain) (N = ", sample_size, ")"),
    x = "Future",
    y = "% Risky Choices"
  ) +
  ylim(40, 70) +
  theme_minimal() +
  poster_theme + 
  facet_wrap(~ diagnosis_depression, labeller = labeller(diagnosis_depression = c("dep" = "Depression", "no dep" = "No Depression")))

# Display the plot
print(fig1b2)


```

### Risk-Taking Behaviour in Gain Domain Based on Past Information and Anxiety Diagnosis

-   As with future information, participants without anxiety exhibit different behaviour based on the past island, gambling more on the current gain trial if they previously experienced a loss domain.
-   Participants with anxiety show similar behaviour based on the past island domains.

```{r Summary Plot % Risk-Taking Based on Past in Gain Domain and Anxiety Diagnosis}

happyData1_past_diag <- happyData1_diag %>%
  arrange(userKey, TrialNumber) %>%
  group_by(userKey) %>%
  mutate(
    # Create an indicator for valid happiness ratings
    happyind = !is.na(zHappy),
    
    # Identify if the current trial is on the same island as the previous valid happiness rating
    prev_happyind_trial = lag(Trial, order_by = TrialNumber),  # Previous trial type (Gain/Loss)
    
    # Set PastIsland based on the previous valid happiness rating's trial type
    PastIsland = case_when(
      lag(happyind, order_by = TrialNumber) & prev_happyind_trial == "Gain" ~ 2,
      lag(happyind, order_by = TrialNumber) & prev_happyind_trial == "Loss" ~ 1,
      TRUE ~ NA_real_  # NA for the first island or no previous valid row
    )
  ) %>%
  ungroup()

happyData1_past_diag <- happyData1_past_diag %>%
  fill(PastIsland, .direction = "up")



sample_size = length(unique(happyData1_past_diag$userKey))

# Prepare data for plotting
gamb_past <- happyData1_past_diag %>%
  mutate(Past = case_when(PastIsland == 2 ~ "Positive",
                            PastIsland == 1 ~ "Negative")) %>% 
  filter(Trial == "Gain") %>%  # selecting only gain domain
  group_by(userKey, Past, diagnosis_anxiety) %>%
  summarise(total_choices = n(),
            risky_choices = sum(Choice == 2, na.rm = TRUE),
            percent_risky = (risky_choices / total_choices) * 100,
            .groups = 'drop')

# Summarize the data for plotting
summary_gamb_past <- gamb_past %>%
  group_by(Past, diagnosis_anxiety) %>%
  summarise(mean_gambling = mean(percent_risky, na.rm = TRUE),
            sem_gambling = sd(percent_risky, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop') %>%
  na.omit()

# Create the plot with facet_wrap for "anx" and "no anx" groups
fig1c <- ggplot(summary_gamb_past, aes(x = Past, y = mean_gambling, color = Past)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean_gambling - sem_gambling, ymax = mean_gambling + sem_gambling),
                position = position_dodge(0.3), width = 0.1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
  labs(
    title = paste0("% Gambling based on Past (Gain Domain) (N = ", sample_size, ")"),
    x = "Past",
    y = "% Risky Choices"
  ) +
  ylim(40, 70) +
  theme_minimal() +
  poster_theme + 
  facet_wrap(~ diagnosis_anxiety, labeller = labeller(diagnosis_anxiety = c("anx" = "Anxiety", "no anx" = "No Anxiety")))

# Display the plot
print(fig1c)


```

# Happiness Plots

### Happiness with Future (in Loss Domain)

-   z-scored happiness shows decreased current happiness (in the loss domain) when the future is positive vs negative

```{r Happiness with Future (Loss Domain)}
sample_size = length(unique(happyData1$userKey))

happy_future <- happyData1 %>%
  mutate(Future = case_when(NextIsland == 2 ~ "Positive",
                            NextIsland == 1 ~ "Negative")) %>% 
  filter(Trial=="Loss") %>% # selecting only gain domain
  group_by(userKey,Future) %>%
  summarise(happiness = mean(zHappy, na.rm=TRUE),
                        .groups = 'drop') %>%
  na.omit()


summary_happy_future <- happy_future %>%
  group_by(Future) %>%
  summarise(mean_happiness = mean(happiness),
            sem_happiness = sd(happiness)/sqrt(n())) %>%
  na.omit()

fig2a <- ggplot(summary_happy_future, aes(x=Future, y=mean_happiness, color=Future)) +
    geom_point(position = position_dodge(0.3), width = 0.3) +
    geom_errorbar(aes(ymin = mean_happiness - sem_happiness, ymax = mean_happiness + sem_happiness),
                  position = position_dodge(0.3), width = 0.1) + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
    scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
    labs(
      title = paste0("zHappy based on Future (Loss Domain) (N=", sample_size, ")"),
      x = "Future",
      y = "z-scored Happiness"
    ) +
    ylim(-0.3,0.3) + 
    theme_minimal() +
    poster_theme 

print(fig2a)

# summary: decreased happiness when future is positive in loss domain
```

### Happiness based on Future Information and Past Experience in the Gain Domain

-   In the current gain domain, participants show increased happiness when the past island was a loss domain and when the future island is a loss domain
-   Happieness is similar regardless of whether the future is a loss or gain island if the past island was a gain domain.
-   So, if the past island was negative, future information seems to matter in affecting happiness.

```{r Future and Past Interaction in Happiness in Gain Domain}
sample_size = length(unique(happyData1_past$userKey))

happy_future_past <- happyData1_past %>%
  mutate(Future = case_when(NextIsland == 2 ~ "Positive",
                            NextIsland == 1 ~ "Negative"),
         Past = case_when(PastIsland == 2 ~ "Positive",
                          PastIsland == 1 ~ "Negative")) %>% 
  filter(Trial=="Gain") %>% # selecting only gain domain
  group_by(userKey,Future, Past) %>%
  summarise(happiness = mean(zHappy, na.rm=TRUE),
                        .groups = 'drop') %>%
  na.omit()


summary_happy_future_past <- happy_future_past %>%
  group_by(Future, Past) %>%
  summarise(mean_happiness = mean(happiness),
            sem_happiness = sd(happiness)/sqrt(n())) %>%
  na.omit()

fig2b <- ggplot(summary_happy_future_past, aes(x=Future, y=mean_happiness, color=Past)) +
    geom_point(position = position_dodge(0.3), width = 0.3) +
    geom_line(position = position_dodge(0.3), aes(group = Past)) +  # Add lines connecting points within groups
    geom_errorbar(aes(ymin = mean_happiness - sem_happiness, ymax = mean_happiness + sem_happiness),
                  position = position_dodge(0.3), width = 0.1) + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
    scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
    labs(
      title = paste0("zHappy based on Future (Gain Domain) (N=", sample_size, ")"),
      x = "Future",
      y = "z-scored Happiness"
    ) +
    ylim(-0.5,0.5) + 
    theme_minimal() +
    poster_theme 

print(fig2b)

# summary: increased gambling when the future is positive vs negative in gain domain. 
```

### Happiness based on Future Information and Anxiety Diagnosis in the Loss Domain

-   In the loss domain, people without anxiety show similar happiness levels regardless of whether the future is positive or negative.
-   People with anxiety are happier when the future is negative vs positive if they are currently in the loss domain.
-   Perhaps there is a preference for persistence - and participants with anxiety do not like having to switch policies when the island changes if they're in a loss domain.

```{r Happiness with Future (Loss Domain) and Anxiety}
sample_size = length(unique(happyData1_diag$userKey))

happy_future <- happyData1_diag %>%
  mutate(Future = case_when(NextIsland == 2 ~ "Positive",
                            NextIsland == 1 ~ "Negative")) %>% 
  filter(Trial=="Loss") %>% # selecting only gain domain
  group_by(userKey,Future,diagnosis_anxiety) %>%
  summarise(happiness = mean(zHappy, na.rm=TRUE),
                        .groups = 'drop') %>%
  na.omit()


summary_happy_future <- happy_future %>%
  group_by(Future, diagnosis_anxiety) %>%
  summarise(mean_happiness = mean(happiness),
            sem_happiness = sd(happiness)/sqrt(n())) %>%
  na.omit()

fig2c <- ggplot(summary_happy_future, aes(x=Future, y=mean_happiness, color=Future)) +
    geom_point(position = position_dodge(0.3), width = 0.3) +
    geom_errorbar(aes(ymin = mean_happiness - sem_happiness, ymax = mean_happiness + sem_happiness),
                  position = position_dodge(0.3), width = 0.1) + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
    scale_color_manual(values = c("Positive" = "#FFD700", "Negative" = "#FF6347")) +
    labs(
      title = paste0("zHappy based on Future (Loss Domain) (N=", sample_size, ")"),
      x = "Future",
      y = "z-scored Happiness"
    ) +
    ylim(-0.3,0.3) + 
    theme_minimal() +
    poster_theme +
facet_wrap(~ diagnosis_anxiety, labeller = labeller(diagnosis_anxiety = c("anx" = "Anxiety", "no anx" = "No Anxiety")))

print(fig2c)

# summary: decreased happiness when future is positive in loss domain
```
