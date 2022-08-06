
# Qn 1. Do gender make a difference when come to environmental awareness ?
# Qn 2. Do education impact towards sustainability ?
# Qn 3. Do income drive away sustainability ?

setwd("C:\\Users\\joyce\\OneDrive\\Desktop\\dasr2022")

pacman::p_load(tidyverse, lubridate, 
               gtrendsR, rvest, glue, rtweet,
               gridExtra, scales, ggthemes, sandwich,interaction
                )

sessionInfo()

install.packages("haven") # STATA, SPSS datafile
library(haven)



green_malaysia <- read_sav("C:\\Users\\joyce\\OneDrive\\Desktop\\dasr2022\\green consumption data.sav")
       
class(green_malaysia)

view(green_malaysia)

glimpse(green_malaysia)

str(green_malaysia_Original)

head(green_malaysia_Original)

as_tibble(green_malaysia)

names(green_malaysia_Original)

library(data.table)

clean_data <-green_malaysia     # Remove unwanted column
setDT(clean_data)[, c("ID",
                      "Marital_status","No_household","Occupation",
                      "Work_category",
                      "TotalEC",
                      "TotalSI",
                      "TotalPBC",
                      "TotalCNS",
                      "TotalGC")
                  := NULL]

glimpse(clean_data)                        

# Environment Concern (EC) 

clean_data$EC1 <- as.numeric(clean_data$EC1)
clean_data$EC2 <- as.numeric(clean_data$EC2)
clean_data$EC3 <- as.numeric(clean_data$EC3)
clean_data$EC4 <- as.numeric(clean_data$EC4)
clean_data$EC5 <- as.numeric(clean_data$EC5)
clean_data$EC6 <- as.numeric(clean_data$EC6)

# Social Influence

clean_data$SI1 <- as.numeric(clean_data$SI1)
clean_data$SI2 <- as.numeric(clean_data$SI2)
clean_data$SI3 <- as.numeric(clean_data$SI3)
clean_data$SI4 <- as.numeric(clean_data$SI4)


# Perceive Behavioural Control (PBC) 
clean_data$PBC1 <- as.numeric(clean_data$PBC1)
clean_data$PBC2 <- as.numeric(clean_data$PBC2)
clean_data$PBC3 <- as.numeric(clean_data$PBC3)
clean_data$PBC4 <- as.numeric(clean_data$PBC4)
clean_data$PBC5 <- as.numeric(clean_data$PBC5)


# Green Consumption Behaviour (GC) 
clean_data$GC1 <-  as.numeric(clean_data$GC1)
clean_data$GC2 <-  as.numeric(clean_data$GC2)
clean_data$GC3 <-  as.numeric(clean_data$GC3)
clean_data$GC4 <-  as.numeric(clean_data$GC4)
clean_data$GC5 <-  as.numeric(clean_data$GC5)
clean_data$GC6 <-  as.numeric(clean_data$GC6)
clean_data$GC7 <-  as.numeric(clean_data$GC7)
clean_data$GC8 <-  as.numeric(clean_data$GC8)
clean_data$GC9 <-  as.numeric(clean_data$GC9)
clean_data$GC10 <- as.numeric(clean_data$GC10)
clean_data$GC11 <- as.numeric(clean_data$GC11)
clean_data$GC12 <- as.numeric(clean_data$GC12)
clean_data$GC13 <- as.numeric(clean_data$GC13)


# Consumer Novelty Seeking

clean_data$CNS1 <- as.numeric(clean_data$CNS1)
clean_data$CNS2 <- as.numeric(clean_data$CNS2)
clean_data$CNS3 <- as.numeric(clean_data$CNS3)
clean_data$CNS4 <- as.numeric(clean_data$CNS4)
clean_data$CNS5 <- as.numeric(clean_data$CNS5)
clean_data$CNS6 <- as.numeric(clean_data$CNS6)
clean_data$CNS7 <- as.numeric(clean_data$CNS7)
clean_data$CNS8 <- as.numeric(clean_data$CNS8)

# Media Preference
clean_data$PM1 <- as.numeric(clean_data$PM1)
clean_data$PM2 <- as.numeric(clean_data$PM2)
clean_data$PM3 <- as.numeric(clean_data$PM3)
clean_data$PM4 <- as.numeric(clean_data$PM4)
clean_data$PM5 <- as.numeric(clean_data$PM5)

# View if the variable is normally distribute 
install.packages("skimr")
library(skimr)

skim(clean_data)

## Reliability test ----
# Cronbach's alpha (cut-off > 0.80)

install.packages("psych")
library(psych)

clean_data%>%                      
  select(starts_with("GC") # Green Consumption Behaviour 0.90
        ) %>% 
  psych::alpha(.)

clean_data %>%                      
  select(starts_with("PBC") # Perceived behavioural control 0.80
  ) %>% 
  psych::alpha(.)

clean_data %>%                      
  select(starts_with("EC") # Environment Concern (EC) 0.80
  ) %>% 
  psych::alpha(.)

clean_data %>%                      
  select(starts_with("SI") # Environment Concern (EC) 0.80
  ) %>% 
  psych::alpha(.)

clean_data %>%                      
  select(starts_with("PM") # Media < 0.80 
  ) %>% 
  psych::alpha(.)

clean_data %>%                      
  select(starts_with("CNS") # Consumer Novelty Seeking (CNS) > 0.80
  ) %>% 
  psych::alpha(.)

# ## Correlation Matrix on original data ----
install.packages("DT")
library(DT)

clean_data %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(.) %>% 
  broom::tidy(.) %>% 
  rename(corr = estimate) %>% 
  mutate(abs_corr = abs(corr)
  ) %>% 
  arrange(desc(abs_corr)
  ) %>% 
  datatable()

tibble(clean_data)
tibble(green_data)




# Green Data

green_data <- clean_data %>% 
  mutate(green_attitude = rowMeans(clean_data %>% select(starts_with("PBC") 
  )
  ),
  green_behaviour = rowMeans(clean_data %>% select(starts_with("GC") 
  )
  ),
  green_concern = rowMeans(clean_data %>% select(starts_with("EC") 
  )
  ),
  
  green_Infulence = rowMeans(clean_data %>% select(starts_with("SI")  
  )
  ),
  
  green_seeking = rowMeans(clean_data %>% select(starts_with("CNS")
  )
  )
  ) 


# Data will not be able to reflect out if it were to amend to as.factor,
# But during the preparation of ggplot, all demographic variable will be amend to "as.factor "

# Gender  # Female = 2 , Male 1 > Categorical Variable
green_data$Gender <- as.numeric(green_data$Gender)

# Age > Categorical Variable
green_data$Age <- as.numeric(green_data$Age)

# Education > Categorical Variable
green_data$Education  <- as.numeric(green_data$Education)

# Income > Categorical Variable
green_data$Income  <- as.numeric(green_data$Income)

# Ethnicity > Categorical Variable

green_data$Ethnicity   <- as.numeric(green_data$Ethnicity)




# Cannot include gender in data, need rename , # Original Data : 1 = Male , 2 = Female
green_data <- green_data %>% 
  mutate(female = ifelse(Gender == 2, 1, 0) 
  ) %>% 
  select(-Gender) 
  


green_data <- green_data[ , -(1:41)]

glimpse(green_data)

as.tibble(green_data)


## Correlation Matrix after performing Means(Average) rating for the variable----

install.packages("DT")
library(DT)  

green_data %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(.) %>% 
  broom::tidy(.) %>% 
  rename(corr = estimate) %>% 
  mutate(abs_corr = abs(corr)
  ) %>% 
  datatable(options = list(scrollX = T),
  ) %>%
  formatRound(columns = c("corr", "p.value", "abs_corr"), 
              digits = 3)


# Mean Centering

## Cutting-edge assumption check ----
install.packages("gvlma")
library(gvlma)

library(car)

#------------------------------------------------------------------------------------

Centered_data_m1 <- green_data %>% 
  mutate_at(vars(-c(green_behaviour,female) # mutate_at() acts on multiple columns
  ), 
  ~(. - mean(., na.rm = T)
  )
  )

# Model 1 Main Effect :  Green Behavior x Green attitude 

model1 <- Centered_data_m1 %>% 
  lm(formula = green_behaviour ~ green_attitude + Education + Income + female + Age + Ethnicity,   
     data = .) 

summary(model1)

gvlma(model1)
vif(model1)
anova(model1)

# Model 2 Interaction : Green Behavior x Green attitude * Age + Education + female + Ethnicity

model2 <- Centered_data_m1 %>% 
  lm(formula = green_behaviour ~ green_attitude *( Age + Education + female + Ethnicity +Income),
     data = .) 

summary(model2)
gvlma(model2)
vif(model2)

anova(model2)

anova(model1,model2)

library(broom)

tidy(model1) %>% 
  filter(abs(estimate) > 2 * abs(std.error))
  

tidy(model2) %>% 
  filter(abs(estimate) > 2 * abs(std.error))
  

## model2_WO_center based on Original Data

model2_WO_center <- green_data %>% 
  lm(formula = green_behaviour ~ green_attitude * (Income + Age +  Education + female+ Ethnicity) ,
     data = .)


library(modelr)

# Perform the analysis for Model 1 & 2  Green Behaviour X Income

# Income is a continuous variable

green_data %>% summarise(sd(Income ))

glimpse(green_data)



grid_group3_Model2 <- Centered_data_m1 %>% 
  data_grid(green_attitude, Income = c(-1.13, 0.00, 1.13),
            female = 1, Age = 0, Education = 0, green_belief = 0,Ethnicity = 0) %>% 
  add_predictions(model2) %>% 
  rename(predicted_green_behaviour = pred)


# We undid the centering of variable (`green attitude`). 

grid_Model2 <- grid_group3_Model2 %>% 
  mutate(green_attitude= green_attitude + mean(green_data$green_attitude), Income = factor(as.double(factor(Income))))



# GGplot to Visualise

## ggplot on predicted_green_behaviour

ggplot(grid_Model2, aes(x = green_attitude , y = predicted_green_behaviour, color = factor(Income))) +
  geom_point(size=3) + 
  scale_color_discrete(breaks = c(1,2,3), label=c("Low", "Average","High")) +
  geom_segment(aes(x=green_attitude, 
                   xend=green_attitude, 
                   y=2.5, 
                   yend=predicted_green_behaviour)) + 
  coord_cartesian(ylim = c(2.5, 3.4),
                  xlim = c(2.4, 3.0)) +
  theme_bw() +                 
  labs(title="Green Decsion X Income", 
       subtitle="Green Consumption perceive Behaviour", 
       caption="Source: Mendeley Data") 
      

pacman::p_load(jtools, huxtable, ggstance, interactions,grid.Extra,sandwich)


export_summs(model1, model2, 
             error_format = "(t = {statistic}, p = {p.value})",
             align = "right",
             model.names = c("Main Effects Only", "with Interactions"),
             digits = 3)
dev.off() 
plot_summs(model1, model2,
           scale = T,
           plot.distributions = T,
           model.names = c("Main Effects Only", "with Interactions")) +
  theme(legend.position = "top")
           
# Step 1: Plot interaction using interact_plot(interval = T) ----

summ(model2,
     digits = 3)


summ(model2_WO_center,
     center = T)

# How to address heteroscedasticity
summ(model2, robust = "HC3", cluster = "firm")

# Step 1: Plot interaction using interact_plot(interval = T) ----

interact_plot(model = model2_WO_center, # plug-in your model
              pred = green_attitude, # pred = predictor = X1 = Explanatory
              modx = Income, # modx = moderator = X2
              modx.labels = c("Low Income",
                              "Average Income",
                              "High Income"),
              legend.main = "Income",
              colors = c("sandybrown", 
                         "deepskyblue3",
                         "orangered1"),
              interval = T # display confidence intervals
              ) +
  theme(legend.position = "top")

# Step 2: Run simple slopes analysis  

sim_slopes(model2_WO_center,
           pred = green_attitude, 
           modx = Income,
           johnson_neyman = F)

# Step 3: Spotlight analysis 

sim_slopes(model2_WO_center,
           pred = Income, 
           modx = green_attitude,
           johnson_neyman = T)

set.seed(20210427)

# Step 4: Run interaction_plot() again by adding benchmark for regions of significance

interact_plot(model = model2_WO_center, # plug-in your model
              pred = green_attitude, # pred = predictor = X1 = Explanatory
              modx = Income, # modx = moderator = X2
              modx.labels = c("Low Income",
                              "Average Income",
                              "High Income"),
              legend.main = "Income",
              colors = c("sandybrown", 
                         "deepskyblue3",
                         "orangered1"),
              interval = T # display confidence intervals
              ) +
  theme(legend.position = "top") +
  geom_vline(xintercept = 4.73,
             color = "red",
             size = 1.5) + 
  annotate("rect",
           fill = "yellow",
           xmin = 4.73,
           xmax = 5.0,
           ymin = 2.4,
           ymax = 5.0,
           alpha = 0.10) + 
  annotate("text",
           x = 4.0,
           y = 5.0,
           label = "The shaded area denotes\nthe regions of significance\nbased on alpha at 5%")
 
# Step 5: Finalize the figure by adding necessary information

interact_plot(model = model2_WO_center, # plug-in your model
              pred = green_attitude, # pred = predictor = X1 = Explanatory
              modx = Income, # modx = moderator = X2
              modx.labels = c("Low Income",
                              "Average Income",
                              "High Income"),
              legend.main = "Income",
              colors = c("sandybrown", 
                         "deepskyblue3",
                         "orangered1"),
              interval = T ,# display confidence intervals
              plot.points = T, # reveal your data points
              jitter = 0.1) +
  theme(legend.position = "top") +   
  labs(y = "Green Consumption",
       x = "Green Attitude",
       title = "The Green Awareness on Income Level and Attitude \non Green Consumption",
       subtitle = "Income Level does not drive away green consumption",
       caption = "Source: Mendeley Data") +
  geom_vline(xintercept = 4.73,
             color = "black",
             size = 1.0) + 
  annotate("rect",
           fill = "yellow",
           xmin = 4.73,
           xmax = 5.0,
           ymin = 2.4,
           ymax = 5.0,
           alpha = 0.10) + 
  annotate("text",
           x = 4.0,
           y = 5.2,
           label = "The shaded area denotes\nthe regions of significance\nbased on alpha at 5%")
  theme_fivethirtyeight() + 
  theme(legend.position = "top",
        axis.title = element_text(),
        text = element_text(family = "Courier")
        )

#--- MOdel 3 & 4 
  
Centered_data_m2 <- green_data %>% 
    mutate_at(vars(-c(green_behaviour,female)
    ), 
    ~(. - mean(., na.rm = T)
    )
    )
  
library(gvlma)
library(car)
  
# Model 3 Main Effect  
  
model3 <- Centered_data_m2 %>% 
    lm(formula = 	green_behaviour ~ green_seeking + Age + Ethnicity + Income + Education + female + green_concern,
       data = .)

gvlma(model3)
  
vif(model3)
  
# Model 4 Interactions
  
model4 <- Centered_data_m2 %>% 
    lm(formula = green_behaviour ~ green_seeking *(Ethnicity + Income + Education  + Age + female)+ green_concern,
       data = .)  
  
gvlma(model4)
  
vif(model4)

anova(model3,model4)

library(broom) 

model3_model_info <- tidy(model3)
model3_model_info 

model4_var_info <- tidy(model4)
model4_var_info

library(ggfortify)

autoplot(gvlma(model4))


export_summs(model3,
             model4,
             model.names = c ("Main Effeects only",
                              "with Interaction"),
             error_format = "CIs = [{conf.low} , {conf.high}]",
             dights = 3
            )


predicted_Y_value <- Centered_data_m2 %>% 
  modelr::data_grid(green_seeking, 
                    female, 
                    Age = 0 ,
                    Education  = 0,
                    Income = 0,
                    Ethnicity = 0 ,
                    green_concern = 0,
                    ) %>% 
  add_predictions(model4) %>% 
  rename(predicted_green_behaviour = pred)



predicted_Y_value %>% 
  print(n = nrow(.))
       

predicted_Y_value <- predicted_Y_value %>% 
  mutate(green_seeking = green_seeking + mean(green_data$green_seeking))
  


predicted_Y_value %>% 
  print(n = nrow(.))
  

estimated <- predicted_Y_value %>% 
  ggplot(aes(x = green_seeking,
             y = predicted_green_behaviour,
             color = factor(female)
            )
          ) + 
  geom_line(size = 2) +
  scale_color_discrete(breaks = c(0, 1),
                       label = c("Male",
                                 "Female")
                        ) + 
  coord_cartesian(ylim = c(2.4, 3.0),
                  xlim = c(1.1, 1.6)
                  ) + 
  theme_bw() + 
  theme(legend.position = "top")

estimated 



estimated_vs_observed <- ggplot(data = green_data, 
                                aes(x = green_seeking,
                                    y = green_behaviour)
                                ) + 
  geom_point(aes(color =factor(female)
            ),
  alpha = 0.25) + 
  geom_line(data = predicted_Y_value, 
            aes(y = predicted_green_behaviour,
                color = factor(female)
            ),
            size = 1.5
            ) +
  facet_wrap(. ~ factor(female),
             labeller = as_labeller(c("0" = "MALE",
                                      "1" = "FEMALE")
             )
            ) +
  coord_cartesian(ylim = c (2.4, 3.0),
                  xlim = c (1.1, 1.6)
                  ) +
  theme_bw() +
  theme(legend.position = "none")


grid.arrange(estimated, estimated_vs_observed, ncol = 2)


model4_WO_center <- green_data %>% 
  lm(formula = green_behaviour ~ green_seeking *(Ethnicity + Income + Education  + Age + female)+ green_concern,
     data = .) 



# Interpretion for Model 4

# Step 1: Plot interaction using interact_plot(interval = T)


interact_plot(model = model4_WO_center, 
              pred = green_seeking,
              modx = female, 
              modx.labels = c("0" = "Male",
                              "1" = "Female"),
              legend.main = "Gender",
              colors = c("tomato3",
                         "deepskyblue3"),
              interval = T 
              ) +
  theme(legend.position = "top")

# Step 2: Run simple slopes analysis

sim_slopes(model = model4_WO_center,
           pred = green_seeking,
           modx = female,
           johnson_neyman = F)

# Step 3: Spotlight analysis


sim_slopes(model = model4_WO_center,
           pred = female,
           modx = green_seeking,
           johnson_neyman = T)



# Step 4: Run interaction_plot() again by adding benchmark for regions of significance

interact_plot(model = model4_WO_center, 
              pred = green_seeking,
              modx = female,
              modx.labels = c("0" = "Male",
                              "1" = "Female"),
              legend.main = "Gender",
              colors = c("tomato3",
                         "deepskyblue3"),
              interval = T 
              ) +
  theme(legend.position = "top") + 
  geom_vline(xintercept = 6.57,
             color = "red",
             size = 1.5) + 
  annotate("rect",
           fill = "yellow",
           xmin = 6.57,
           xmax = 5.00,
           ymin = 1,
           ymax = 5,
           alpha = 0.10) + 
  annotate("text",
           x = 4.3,
           y = 4.3,
           label = "The shaded area denotes\nthe regions of significance\nbased on alpha at 5%")



# Step 5: Finalize the figure by adding necessary information

interact_plot(model = model4_WO_center, 
              pred = green_seeking, 
              modx = female, 
              modx.labels = c("0" = "Male",
                              "1" = "Female"),
              legend.main = "Gender",
              colors = c("tomato3",
                         "deepskyblue3"),
              interval = T,
              plot.points = T,
              jitter = 0.1) +
  theme(legend.position = "top") + 
  labs(y = "Green Consumption Behaviour",
       x = "Green Seeking",
       title = "Green Consumption Perception among Male and Female",
       subtitle = "Male had higher green consumption behaviour than female ",
       caption = " Source :https: //data.mendeley.com/submissions/ees/edit/r5tfv3pp8k?
       submission _ id= DIB _  46952&token=d217d726- 718b- 417a- 9d6c- 8941fe29687f") +
  geom_vline(xintercept = 6.57,
             color = "red",
             size = 1.5) + 
  annotate("rect",
           fill = "yellow",
           xmin = 6.57,
           xmax = 5.00,
           ymin = 1,
           ymax = 5,
           alpha = 0.10) + 
  annotate("text",
           x = 5.0,
           y = 5.5,
           label = "The shaded area denotes\nthe regions of significance\nbased on alpha at 5%") +
  theme_fivethirtyeight() +
  theme(legend.position = "top",
        axis.title = element_text(),
        text = element_text(family = "Courier")
        )














