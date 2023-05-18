# set working directory 

# load packages
library(tidyr)
library(tidymodels)
library(tidyverse)
library(janitor)
library (here)
library(readxl)
library(reshape2)
library(car)
library(corrr)
library(tidytext)

# read in data 
data <- read_excel(here("king_air_critical_task_anthropometry.xlsx"))

# check data 
head(data)
tail(data)
str(data)
dim(data)

# tidy data
data <- data %>%
  mutate(bmi = body_mass/((stand_height/100)*(stand_height/100)),
         combo_leg = buttock_knee_length + sit_knee_height) 

view(data)
str(data)

data$visor_clear_dist <- as.numeric(data$visor_clear_dist)


# PRINCIPLE COMPONENT ANALYSIS #######################################

data2 <- data %>%
  select(id, sex, age, body_mass:sit_height, buttock_knee_length:combo_leg)

view(data2)

# assess correlation between data 
data2 %>%
  select(body_mass:combo_leg) %>%
  correlate() %>%
  shave() %>%
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan")) 

pca_data <- data2 %>% 
  rename("Body mass" = body_mass,
         "Standing height" = stand_height,
         "Sitting height" = sit_height,
         "Sitting buttock-knee length" = buttock_knee_length,
         "Sitting knee height" = sit_knee_height,
         "Thigh circumference" = thigh_circum,
         "Waist circumference" = waist_circum,
         "Thumbtip reach" = thumbtip_reach,
         "Armspan" = arm_span,
         "BMI" = bmi,
         "Combo-leg" = combo_leg)

view(pca_data)

pca_rec <- recipe(~., data = pca_data) %>%
  update_role(id, sex, age, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") 

pca_prep <- prep(pca_rec)

pca_prep

tidied_pca <- tidy(pca_prep, id = "pca")

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

pca_contribution <- tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  scale_fill_manual(values=c("chocolate", "dark green")) +
  geom_col() +
  scale_x_continuous(limits = c(0,1), breaks = c(0.0, 0.5, 1.0), labels = c(0.0, 0.5, 1.0)) +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?")

pca_contribution

# create theme 
my_theme <- theme_classic() + 
  theme(
    text = element_text(family = "Times New Roman"),
    strip.text.x = element_text(size = 15, colour = "black", face = "bold"),
    strip.background = element_rect(colour = "black", size = 0, linetype = "solid"),
    axis.title.y = element_text(colour = "black", size = 15, 
                                vjust = 3.5, face = "bold"),
    axis.title.x = element_text(colour = "black", size = 15, 
                                vjust = -1.5, face = "bold"),
    axis.line.y = element_line(colour = "black", size = 1),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(.2, "cm"),
    axis.text = element_text(size = 12, face = "bold", colour = "black"),
    strip.text = element_blank(),
    plot.margin = unit(c(0.1,0.1,0.3,0.1),"cm"))

pca_contribution2 <- pca_contribution + my_theme

pca_contribution3 <- pca_contribution2 + 
  theme(legend.title = element_text(size = 15, face = "bold"), 
        legend.title.align = 0.5,
        legend.text = element_text(size = 12, face = "bold")) 

pca_contribution3

ggsave("Figure PCA Contribution", device='jpg', dpi = 300)

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = id)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL)
# colour fill based on identifiers 
# contribution low (i.e. ~2), suggestive of no extremes 

# calculate variation explained by each PC
sdev <- pca_prep$steps[[2]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

tibble(
  component = unique(tidied_pca$component),
  percent_var = percent_variation ## use cumsum() to find cumulative, if you prefer
) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_var)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent variance explained by each PCA component")

juice(pca_prep)

# can also do this for variance explained 

pca_variances <- tidy(pca_prep, id = "pca", type = "variance") %>% view()

pca_variances %>%
  filter(terms == "percent variance") %>%
  ggplot(aes(component, value)) +
  geom_col(fill="#372F60") +
  labs(x = "Principal Components", y = "Variance explained (%)") +
  theme_minimal()

cumulative_pca <- pca_variances %>%
  filter(terms == "cumulative percent variance") %>%
  ggplot(aes(component, value)) +
  geom_col(fill = "black") +
  labs(x = "Principal Components", y = "Cumulative variance explained (%)") +
  theme_minimal()

cumulative_pca2 <- cumulative_pca + my_theme + 
  theme(axis.title.y = element_text(colour = "black", size = 20, 
                                    vjust = 3.5, face = "bold"),
        axis.title.x = element_text(colour = "black", size = 20),
        axis.text = element_text(size = 15, face = "bold", colour = "black"),
        plot.margin = unit(c(0.2,0.1,0.5,0.5),"cm"))

ggsave("Figure Cumulative PCA", device='jpg', dpi = 300)

# create figure again and include vairation explained in axes titles 
juice(pca_prep) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(color = NULL) + 
  labs(x = paste0("Principal component 1 (",  percent(percent_variation[1]), ")"), 
       y = paste0("Principal component 2 (",  percent(percent_variation[2]),")"))

ggsave("Figure PC1 versus PC2", device='jpg', dpi = 300)

# understand https://juliasilge.com/blog/stack-overflow-pca/


# SITTING HEIGHT ############################################################

# sitting height is a product of seat position and sitting eye height
# sitting eye height and sitting height are strongly correlated

data %>% summarise(mean(sit_height),
                   sd(sit_height),
                   median(sit_height),
                   min(sit_height),
                   max(sit_height))

# check if there is a risk with headset-visor clearance
# one NA due to being behind the sun visor
data %>% summarise(mean(visor_clear_dist, na.rm = T),
            median(visor_clear_dist, na.rm = T),
            min(visor_clear_dist, na.rm = T),
            max(visor_clear_dist, na.rm = T))
# no conflict but potentially greater risk select individuals and when turning head

# visualise data
ggplot(data, aes(sit_height, seat_pos_perc_up)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.95, fullrange = TRUE) + 
  ylim(0, 100) +
  xlim(80,104) 
  
ggplot(data, aes(sit_height, seat_pos_perc_forward)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.95, fullrange = TRUE) + 
  ylim(0, 100) +
  xlim(85,104) 

# linear relationship
# create a linear model to determine ranges 

lm_mod_sit_height <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lm_fit_sit_height <- lm_mod_sit_height %>%
  fit(sit_height ~ seat_pos_perc_up + seat_pos_perc_forward , data = data)

lm_fit_sit_height

summary(lm_fit_sit_height$fit)
tidy(lm_fit_sit_height)
glance(lm_fit_sit_height)


model <- lm(sit_height ~ seat_pos_perc_up * seat_pos_perc_forward , data = data)


new.data <- data.frame(
  seat_pos_perc_up = c(0, 0, 100, 100),
  seat_pos_perc_forward = c(100, 0, 100, 0)
)

predict(model, newdata = new.data)

sit_height_predict <- data.frame(predict(model, newdata = new.data, 
                                         interval = "confidence"))

sit_height_predict

predict(model, newdata = new.data, interval = "prediction")

a <- sit_height_predict$lwr[3] # 100% up, 100% forward, lower CL 
b <- sit_height_predict$upr[3] # 100% up, 100% forward, upper CL
c <- sit_height_predict$lwr[2] # 0% up, 0% back lower CL
d <- sit_height_predict$upr[2] # 0% up, 0% back, upper CL
min <- sit_height_predict$fit[3]
max <- sit_height_predict$fit[2]

# create new plot with limits and borderline ranges 

plot <- ggplot(data) +
  coord_cartesian(ylim = c(0, 100), xlim = c(78,107)) +
  geom_rect(aes(xmin = a, xmax = b, ymin = 0, ymax = 100), 
            fill = "orange", alpha = 0.05) +
  geom_rect(aes(xmin = c, xmax = d, ymin = 0, ymax = 100), 
            fill = "orange", alpha = 0.05) +
  geom_rect(aes(xmin = b, xmax = c, ymin = 0, ymax = 100), 
            fill = "green", alpha = 0.05) + 
  geom_rect(aes(xmin = -Inf, xmax = a, ymin = 0, ymax = 100), 
            fill = "red", alpha = 0.05) + 
  geom_rect(aes(xmin = d, xmax = Inf, ymin = 0, ymax = 100), 
            fill = "red", alpha = 0.05) + 
  geom_hline(yintercept = c(0, 100), linetype = "dashed", colour = "black", size = 0.8) +
  geom_vline(xintercept = c(a, b, c, d, min, max), linetype = "dashed", colour = "black", size = 0.8) +
  geom_smooth(aes(sit_height, seat_pos_perc_forward), 
              method = "lm", level = 0.95, fullrange = TRUE, 
              colour = "black", size = 1) + 
  geom_smooth(aes(sit_height, seat_pos_perc_up), 
              method = "lm", level = 0.95, fullrange = TRUE, 
              colour = "black", size = 1) 

+ 
  geom_point(aes(sit_height, seat_pos_perc_forward, data))

+ 
  geom_point(aes(sit height)) 

+
  ylab("Seat position (% up)") + 
  xlab("Sitting height (cm)") 

plot

# create theme 
my_theme <- theme_classic() + 
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.y = element_text(colour = "black", size = 15, 
                                vjust = 3.5, face = "bold"),
    axis.title.x = element_text(colour = "black", size = 15, 
                                vjust = -1.5, face = "bold"),
    axis.line.y = element_line(colour = "black", size = 1),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(.2, "cm"),
    axis.text = element_text(size = 12, face = "bold", colour = "black"),
    strip.text = element_blank(),
    plot.margin = unit(c(1,1,1,1),"cm"))

plot2 <- plot + 
  my_theme


plot2

ggsave("Figure Sitting Height", device='jpg', dpi = 300)











