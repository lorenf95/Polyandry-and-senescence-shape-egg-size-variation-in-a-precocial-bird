#### Things to check in the data in response to reviews

library(RSQLite)
library(tidyverse)
library(lme4)
library(ggplot2)
library(gtable)

Ceuta_OPEN <- 
  dbConnect(SQLite(), dbname = "../Ceuta_Open/Ceuta_OPEN/data/Ceuta_OPEN_version_releases/Ceuta_OPEN_v1-5.sqlite")

plover_date_convert <- 
  function(df, input = "mdd"){
    if(input == "mdd"){
      
      if(sum(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df))) > 1){
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
                 function(x) 
                   paste(df$year, 
                         ifelse(nchar(x) == 3, 
                                substring(x, first = 1, last = 1), 
                                substring(x, first = 1, last = 2)), 
                         ifelse(nchar(x) == 3, 
                                substring(x, first = 2, last = 3), 
                                substring(x, first = 3, last = 4)), 
                         sep = "-"))
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <-
          lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
                 function(x) as.Date(x, format = "%Y-%m-%d"))
      }
      else{
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          paste(df$year, 
                ifelse(nchar(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))]) == 3, 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 1, last = 1), 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 1, last = 2)), 
                ifelse(nchar(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))]) == 3, 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 2, last = 3), 
                       substring(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], first = 3, last = 4)), 
                sep = "-")
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          as.Date(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], format = "%Y-%m-%d")
      }
    }
    if(input == "Rdate"){
      if(sum(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df))) > 1){
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          lapply(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))],
                 function(x) 
                   as.Date(x, origin = "1970-01-01"))
      }
      else{
        df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))] <- 
          as.Date(df[, which(grepl(paste(c("date", "alive", "manip"), collapse = "|"), names(df)))], origin = "1970-01-01")
      }
    }
    return(df)
  }

luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.5, colour = "grey40"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

# set the ggplotting theme
theme_set(luke_theme)

#### Response 3 ####
# "I don’t know if I missed the argument, but what is the average (and range) 
# number of nests event in a same season a female can do?"
# Answer: This is a two-pronged question. First, look at the distribution of 
# nests that females lay in a single season (i.e., matter of their biological
# limits). Second, look at the distribution of successful nests a female
# can have in a single season (i.e., matter of timing for multiple matings).

nest_dates_F <- 
  dbReadTable(Ceuta_OPEN, "Nests") %>% 
  mutate(nest_initiation_date = as.numeric(nest_initiation_date),
         end_date = as.numeric(end_date),
         found_date = as.numeric(found_date)) %>% 
  mutate(date = ifelse(nest_initiation_date == "NANA",
                       NA, nest_initiation_date)) %>%
  mutate(date = as.numeric(ifelse(!is.na(date), date,
                                  ifelse(!is.na(found_date), found_date, date))),
         year = as.factor(year)) %>%
  plover_date_convert(input = "Rdate") %>%
  mutate(lay_date = 
           as.Date(
             ifelse(!is.na(nest_initiation_date), nest_initiation_date,
                    ifelse((!is.na(end_date) & fate == "Hatch"), end_date - 25, 
                           ifelse((!is.na(found_date) & float1 == "F"), found_date - 11,
                                  ifelse(!is.na(found_date), found_date, NA)))), 
             origin = "1970-01-01")) %>% 
  mutate(lay_date =
           as.Date(ifelse(!is.na(found_date), found_date, NA), 
                   origin = "1970-01-01"))

successful_nest_tally <-
  dbReadTable(Ceuta_OPEN, "BirdRef") %>% 
  filter(!is.na(female)) %>% 
  dplyr::select(year, ID, male, female) %>%
  left_join(., dplyr::select(nest_dates_F, ID, fate, nest_initiation_date, end_date), by = "ID") %>% 
  filter(fate == "Hatch") %>% 
  group_by(female, year) %>%
  summarise(n_nests = n_distinct(ID, na.rm = TRUE),
            first_lay_date = nest_initiation_date[which.min(nest_initiation_date)],
            first_end_date = end_date[which.min(end_date)],
            second_lay_date = nest_initiation_date[which.max(nest_initiation_date)],
            second_end_date = end_date[which.max(end_date)]) %>%
  ungroup() %>% 
  summarise(mean_n_nests_per_year = mean(n_nests),
            median_n_nests_per_year = median(n_nests),
            max_n_nests_per_year = max(n_nests),
            min_n_nests_per_year = min(n_nests))

all_nest_tally <-
  dbReadTable(Ceuta_OPEN, "BirdRef") %>% 
  filter(!is.na(female)) %>% 
  dplyr::select(year, ID, male, female) %>%
  left_join(., dplyr::select(nest_dates_F, ID, fate, nest_initiation_date, end_date), by = "ID") %>% 
  group_by(female, year) %>%
  summarise(n_nests = n_distinct(ID, na.rm = TRUE),
            first_lay_date = nest_initiation_date[which.min(nest_initiation_date)],
            first_end_date = end_date[which.min(end_date)],
            second_lay_date = nest_initiation_date[which.max(nest_initiation_date)],
            second_end_date = end_date[which.max(end_date)]) %>%
  ungroup() %>% 
  summarise(mean_n_nests_per_year = mean(n_nests),
            median_n_nests_per_year = median(n_nests),
            max_n_nests_per_year = max(n_nests),
            min_n_nests_per_year = min(n_nests))

#### Response 4 ####
# Is there a difference of number of reproductive events between monogamous 
# and polyandrous females?
load(file = "data/egg_volume_data_2006_2019.rds")

# wrangle data
dbReadTable(Ceuta_OPEN, "BirdRef") %>% 
dplyr::select(year, ID, male, female) %>%
arrange(female) %>%
group_by(female, year) %>%
summarise(n_mates = n_distinct(male, na.rm = TRUE),
          n_nests = n_distinct(ID, na.rm = TRUE)) %>% 
mutate(polyandry = ifelse(n_mates > 1, "poly", "mono"),
       year = as.integer(year),
       n_mates = ifelse(n_mates == 0, 1, n_mates),
       multi_clutch = ifelse(n_nests > 1, 1, 0)) %>% 
rename(ring = female) %>% 
mutate(polyandry = as.factor(polyandry)) %>% 
Rmisc::summarySEwithin(.,
                       measurevar = "n_nests",
                       withinvars = "polyandry",
                       idvar = "ring",
                       conf.interval = 0.95) %>% 
mutate(lcl = n_nests - ci,
       ucl = n_nests + ci) %>% 
ggplot(.) +
geom_bar(aes(x = polyandry, y = n_nests, fill = polyandry),
         colour = "grey40", stat = "identity", alpha = 0.4,
         width = 0.5) +
geom_errorbar(width = 0.1, aes(x = polyandry, y = n_nests, 
                               ymin = lcl, ymax = ucl)) +
ylab("Number of nests per female per year (mean ± 95% CI)") +
xlab("Mating tactics of a given female in a given year") +
scale_x_discrete(labels = c("mono" = "Monogamous",
                            "poly" = "Polyandrous")) +
scale_fill_manual(values = c("black", "#f03b20")) +
theme(legend.position = "none")

#### Response 28 ####
# What about testing a quadratic age effect?
age_summary <- 
  function(df){
    
    # extract the average, first, and last age for each individual
    ring_Age <- 
      df %>%
      dplyr::select(ring, est_age, conservative_age, 
                    est_age_lower, est_age_upper) %>%
      distinct() %>% 
      group_by(ring) %>% 
      summarise(firstage = min(est_age) - 1,
                conservative_firstage = min(conservative_age),
                firstage_lower = min(est_age_lower),
                firstage_upper = min(est_age_upper),
                lastage = max(est_age) - 1,
                conservative_lastage = max(conservative_age),
                lastage_lower = max(est_age_lower),
                lastage_upper = max(est_age_upper))
    
    # merge with dataframe
    df2 <- 
      left_join(df, ring_Age, by = "ring") %>% 
      mutate(est_age = est_age - 1,
             conservative_age = ifelse(age_first_cap == "J", 
                                       conservative_age - 1, 
                                       conservative_age),
             conservative_firstage = ifelse(age_first_cap == "J", 
                                            conservative_firstage - 1, 
                                            conservative_firstage),
             conservative_lastage = ifelse(age_first_cap == "J", 
                                           conservative_lastage - 1, 
                                           conservative_lastage))
    
    
    return(df2)
    
  }

eggdf <- 
  age_summary(df = eggdf) %>% 
  mutate(year = as.factor(year),
         ID = as.factor(ID),
         ring = as.factor(ring)) %>% 
  distinct()

agedf <-
  eggdf %>%
  dplyr::select(ring, year, est_age, firstage, lastage, polyandry, jul_std_date) %>%
  distinct() %>% 
  mutate(polyandry = as.factor(polyandry))

poly_age_linear <- 
  glmer(polyandry ~ est_age + firstage + lastage + 
          (1|ring) + (1|year), data = agedf, family = "binomial")

poly_age_quadratic <- 
  glmer(polyandry ~ poly(est_age, 2) + firstage + lastage + 
          (1|ring) + (1|year), data = agedf, family = "binomial")

poly_age_null <- 
  glmer(polyandry ~  firstage + lastage + 
          (1|ring) + (1|year), data = agedf, family = "binomial")

summary(poly_age_linear)
summary(poly_age_quadratic)

poly_age_model_list <- 
  list(poly_age_linear, poly_age_quadratic, poly_age_null)
poly_age_R2_list <- lapply(poly_age_model_list, function(x) r2_nakagawa(x))
poly_age_R2_list <- lapply(poly_age_R2_list, function(x) unlist(x))
poly_age_R2_list <- lapply(poly_age_R2_list, function(x) as.data.frame(x))

R2_estimates <- 
  data.frame(condR2 = c(unlist(lapply(poly_age_R2_list, function(x) x$x[1]))),
             margR2 = c(unlist(lapply(poly_age_R2_list, function(x) x$x[2]))),
             model = c("poly_age_linear", "poly_age_quadratic", "poly_age_null"))

poly_age_mod_names_fixef <- 
  data.frame(fixeffect = c("Null",
                           "Quadratic age effect",
                           "Linear age effect"),
             model = c("poly_age_null", "poly_age_quadratic", "poly_age_linear"))

AICc(poly_age_linear, poly_age_quadratic, poly_age_null) %>% 
  mutate(model = row.names(.),
         deltaAICc = AICc - min(AICc),
         AICcWt = Weights(AICc)) %>% 
  arrange(AICc) %>% 
  mutate(ER = AICcWt[1]/AICcWt) %>% 
  collect() %>%
  left_join(., poly_age_mod_names_fixef, by = "model") %>%
  left_join(., R2_estimates, by = "model") %>%
  dplyr::select(fixeffect, df, AICcWt, deltaAICc, condR2, margR2) %>% 
  gt() %>% 
  cols_label(fixeffect = "Predictors of Polyandry",
             df = md("***k***"),
             AICcWt = md("***w***"),
             deltaAICc = md("\U0394*AIC*"),
             condR2 = md("*R<sup>2</sup><sub>conditional</sub>*"),
             margR2 = md("*R<sup>2</sup><sub>marginal</sub>*")) %>% 
  fmt_number(columns = vars(deltaAICc, AICcWt,
                            condR2, margR2),
             decimals = 2,
             use_seps = FALSE) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.width = pct(80),
              column_labels.font.size = 14,
              table.font.size = 12,
              data_row.padding = 5) %>% 
  cols_align(align = "left",
             columns = vars(fixeffect))
