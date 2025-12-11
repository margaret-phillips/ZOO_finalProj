
#install.packages("dataRetrieval")
library(dataRetrieval)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

# Vector of USGS site numbers (8–15 digits). Example sites:
site_numbers <- c("04067500", "04087000", "04085200", "04087000", "040871475",
                  "040871476", "04063700", "04087234", "04101500", "04108660",
                  "04118564"
)  # replace with your gages

start_date <- "2015-01-01"
end_date   <- "2025-12-03"

# Pull daily water temperature (°C) for multiple sites
dv_raw <- readNWISdv(
  siteNumbers = site_numbers,
  parameterCd = c("00010", "00020"),  # water temperature
  startDate   = start_date,
  endDate     = end_date,
  statCd      = "00003"   # mean daily, default; explicit here for clarity
)

dv<- dv_raw %>% 
  rename(daily_temp= X_00010_00003)

#now plotting to visualize
ggplot(dv, aes(Date, daily_temp, color = site_no)) +
  geom_line() +
  labs(x = "Date", y = "Water temperature (°C, daily mean)",
       color = "USGS site") +
  theme_minimal()

#pull in air temp data for sites w long-term data from PRISM
#need to skip merged metadata in first 10 rows in order for columns to show up
at_04063700<- read.csv("04063700_airtemp.csv", skip=10)
at_04101500<- read.csv("04101500_airtemp.csv", skip= 10)
at_040637500<- read.csv("040637500_airtemp.csv", skip= 10)
at_04108660<- read.csv("04108660_airtemp.csv", skip=10)

#next, subset the dfs by month and combine to make airtemp= x, stream=y
#also convert water temp to deg F
#add interaction for summer and winter
#questions I would like to ask: is relationship linear?
# does season matter (it should)? anything in the data to indicate there is another variable(s) to use?

dv<- dv %>% 
  mutate(water_temp_f= daily_temp * (9/5) +32) #converted to deg F

#now subset the df by date and four sites
# first pull out months
dv$month <- format(dv$Date, "%m")

dv_at_wt<- dv %>% 
  #filter(month== "07" | month== "01") %>% 
  select(site_no, Date, daily_temp, water_temp_f, month) %>% 
  filter(site_no== "04063700" | site_no== "040637500" | site_no== "04101500" | site_no== "04108660")

#now site_no column for easy df combining
at_04063700$site_no<- "04063700"
at_040637500$site_no<- "040637500"
at_04101500$site_no<- "04101500" #agricultural
at_04108660$site_no<- "04108660"
df_all_at<- rbind(at_04063700, at_040637500, at_04101500, at_04108660)


df_all_at <- df_all_at %>%
  mutate(
    site_no = as.character(site_no),
    Date    = as.Date(Date)
  )

#combining air and water temp data into one df
df_combined <- dv_at_wt %>%
  left_join(df_all_at, by = c("site_no", "Date"))

df_combined<- df_combined %>% 
  rename(tw= water_temp_f, t_air= tmean..degrees.F.)

###### THIS IS WHEN IT GETS GOOD

analysis_3_df <- df_combined %>%
  filter(site_no %in% c("04063700", "04101500", "04108660")) %>%
  filter(month == "07") %>%
  select(site_no, Date, t_air, tw) %>%
  mutate(
    land_use = case_when(
      site_no == "04063700" ~ "forested",
      site_no == "04108660" ~ "mixed",        # last site gets "mixed"
      TRUE                  ~ "agricultural"  # everything else
    ),
    drainage_area = case_when(
      site_no == "04101500" ~ "large",
      site_no == "04063700"     ~ "small",
      TRUE ~ "medium"
    )
  )


#now plot to visualize
ggplot(analysis_3_df, mapping= aes(x = t_air, y = tw, color = drainage_area, group = drainage_area)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "Air temperature (F)",
    y = "Water temperature (F)",
    color = "Drainage Area",
    title = "Water temperature vs. Air temperature by watershed drainage size"
  ) +
  theme_minimal()


#make sure drainage area and land_use are factors
analysis_3_df <- analysis_3_df %>%
  mutate(drainage_area= factor(drainage_area,
                               levels= c("small", "medium", "large")))
# choose your baseline

# Fit ANCOVA with interaction (tests homogeneity of slopes)

# Full factorial with t_air: includes 2-way & 3-way interactions

# Different slopes for each factor separately
lm_interaction3 <- lm(tw ~ t_air * drainage_area, data = analysis_3_df)
summary(lm_int)

#run simple linear model
lm_simple3<- lm(tw ~ t_air, data= analysis_3_df)
summary (lm_simple3)

#assumption checking:
autoplot(lm_simple3)

#AIC table

# Put models in a named list for convenience
lm_intercept3<- lm(tw ~ 1, data=analysis_3_df)
models <- list(
  parallel    = lm_simple3,
  interaction = lm_interaction3,
  air_only    = lm_intercept3
)

# Compute AIC for each model
aic_vals <- sapply(models, AIC)

# ΔAIC relative to the best (lowest AIC)
delta_aic <- aic_vals - min(aic_vals)

# Akaike weights
#akaike_weights <- exp(-0.5 * delta_aic)
#akaike_weights <- akaike_weights / sum(akaike_weights)

# Compose a table
aic_table <- data.frame(
  model = names(models),
  AIC = aic_vals,
  deltaAIC = delta_aic,
  #weight = akaike_weights,
  row.names = NULL
)

# Order by AIC
aic_table <- aic_table[order(aic_table$AIC), ]
print(aic_table)

## kalamazoo: 5,200 sq km, poppler: 360km2, st. joeseph's: 12,000 km2
