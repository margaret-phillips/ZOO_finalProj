# Maggie Philllips
# ZOO 800 final project

#long list of libraries used in code below!!
library(dataRetrieval) #package for pulling USGS data from NWIS
library(tidyverse)
library(lubridate) #dealing with dates
library(broom)
library(purrr)
library(ggfortify) #assumption checking
library(car)
library(emmeans) 
library(sf) #dealing with geospatial data and mapping
library(tigris) #get geospatial data    
library(ggtext) #help formatting figure captions
library(viridisLite) #colorblind friendly color palettes 
library(lmtest) #for ratio test in ANCOVA

##--------------- downloading data from USGS ---------------------------####

# Vector of USGS site numbers:
site_numbers <- c("04101500", "04108660", "04121660", "04121944", "04124000", "04137020")

start_date <- "2015-01-01"
end_date   <- "2025-12-03"

# Pull daily water temperature (°C) for all sites
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

##----- pull in csv files donwloaded from PRISM website for air temp-------####

#pull in air temp data for sites w long-term data from PRISM
#need to skip merged metadata in first 10 rows in order for columns to show up
at_04121660<- read.csv("at_04121660.csv", skip=10)
at_04101500<- read.csv("at_04101500.csv", skip= 10)
at_04121944<- read.csv("at_04121944.csv", skip= 10)
at_04108660<- read.csv("at_04108660.csv", skip=10)
at_04124000<- read.csv("at_04124000.csv", skip=10)
at_04137020<- read.csv("at_04137020.csv", skip=10)


##---------- wrangling and organizing data ----------------------------####
#next, subset the dfs by month and combine to make airtemp= x, stream=y
#also convert water temp to deg F
#add interaction for drainage area
#questions I would like to ask: is relationship linear?
# does drainage area matter (it should)?

dv<- dv %>% 
  mutate(water_temp_f= daily_temp * (9/5) +32) #converted to deg F

#now subset the df by date and four sites
# first pull out months
dv$month <- format(dv$Date, "%m")

dv_at_wt<- dv %>% 
  select(site_no, Date, daily_temp, water_temp_f, month) 
  #filter(site_no== "04063700" | site_no== "040637500" | site_no== "04101500" | site_no== "04108660")

  ##FIX THIS UP FOR NEW SITE NOs and download data from PRISM
#now site_no column for easy df combining
at_04121660$site_no<- "04121660" # 1,834 mi²
at_04101500$site_no<- "04101500" #3,666 mi²
at_04121944$site_no<- "04121944" #345 square miles
at_04108660$site_no<- "04108660" #1,950 square miles
at_04124000$site_no<- "04124000" # 857 mi²
at_04137020$site_no<- "04137020" #1,689 mi²
df_all_at<- rbind(at_04121660, at_04101500, at_04121944, at_04108660, at_04124000, at_04137020)

## date is in format d/m/yyyy
#first formatting the column as a date just in case
#df_all_at$Date <- as.Date(df_all_at$Date, format = "%m/%d/%Y")
#now changing format to yyy-mm-dd to match nwis data 
df_all_at$Date <- as.Date(df_all_at$Date, format = "%m/%d/%Y")

## air temp not showing up for 1500 and 8660 (issue with date prob)

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


##-- adding interaction and visualizing data from 6 sites with 10 yr record-----####

analysis_3_df <- df_combined %>%
  filter(month == "07") %>%
  select(site_no, Date, t_air, tw) %>%
  mutate(
    drainage_area = case_when(
      site_no == "04101500"                      ~ "large",
      site_no %in% c("04121944", "04124000")     ~ "small",
      TRUE                                       ~ "medium"
    )
  )

#now plot to visualize
ggplot(analysis_3_df, mapping= aes(x = t_air, y = tw, color = drainage_area, group = drainage_area)) +
  geom_point() +
  geom_smooth() +
  scale_color_viridis_d() +
  labs(
    x = "Air temperature (F)",
    y = "Water temperature (F)",
    color = "Drainage Area"
  ) +
  labs(
    color = "Drainage Area",
    caption = "<span style='line-height:0.9'><b>Figure 3.</b> Mean July daily stream temperature (F) vs. mean daily air temperature<br>at Michigan streamgages with different drainage areas from 2015 through 2025.</span>"
  )+
  theme_classic()+ 
  theme(
    plot.caption = element_markdown( #using markdown so that I can more easily bold "figure 1" and format caption
      size= 12, #caption text size
      hjust= 0, #justify left
    ),
    legend.title = element_text(size = 10), #increasing legend title and text size
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = c(0.045, 1),  # bottom left corner (tweak as needed)
    legend.justification = c(0, 1),  # anchor legend to bottom-left corner  
    axis.title = element_blank() #getting rid of axis title
  )


#make sure drainage area and land_use are factors
analysis_3_df <- analysis_3_df %>%
  mutate(drainage_area= factor(drainage_area,
                               levels= c("small", "medium", "large")))
# choose your baseline level (optional)

# Fit ANCOVA with interaction (tests homogeneity of slopes)

# Full factorial with t_air: includes 2-way and 3-way interactions

# Different slopes for each factor separately
lm_interaction3 <- lm(tw ~ t_air * drainage_area, data = analysis_3_df)
summary(lm_interaction3)

#run simple linear model
lm_simple3<- lm(tw ~ t_air, data= analysis_3_df)
summary (lm_simple3)

# plot the mean of July temp over 10 yrs at each site to see if there is a trend
analysis_3_df$year <- format(analysis_3_df$Date, "%Y")

#new smaller df with means per site and year
mean_temp_df<- analysis_3_df %>% 
  group_by(site_no, year) %>% 
  summarize(mean_temp= mean(tw), .groups= "drop")


#now plot to visualize
ggplot(mean_temp_df, mapping= aes(x = year, y = mean_temp, color = site_no, group = site_no)) +
  geom_point() +
  geom_smooth() +
  scale_color_viridis_d() + #colorblind friendly palette
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05), add = c(0, 2.9))  #adding room at the top of plot for legend
  )+
  labs(
    color = "Site ID",
    caption = "<span style='line-height:0.9'><b>Figure 2.</b> Mean July stream temperature (F) at Michigan streamgages from years<br>2015 through 2025.</span>"
  )+
  theme_classic()+ 
  theme(
    plot.caption = element_markdown( #using markdown so that I can more easily bold "figure 1" and format caption
      size= 12, #caption text size
      hjust= 0, #justify left
    ),
    legend.title = element_text(size = 10), #increasing legend title and text size
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = c(0.045, 1),  #top left corner
    legend.justification = c(0, 1),  # anchor legend  
    axis.title = element_blank() #getting rid of axis title
  )


#####---------linear regression, conf intervals, p values for sites over the decade----####

trend_tbl <- mean_temp_df %>%
  mutate(year = as.integer(year)) %>%                 # make sure year is numeric
  filter(!is.na(year), !is.na(mean_temp)) %>%         #get rid of any missing/NA data
  group_by(site_no) %>%
  filter(n_distinct(year) >= 3) %>%                   # need ≥3 distinct years for CI
  group_split() %>%
  map_df(function(df_site) { #I got this code to make a tibble of change per decade, conf interval, and p values from copilot
    # fit
    m <- lm(mean_temp ~ year, data = df_site)
    # tidy with CI; if CI fails, catch and return without CI
    td <- tryCatch(
      broom::tidy(m, conf.int = TRUE),
      error = function(e) broom::tidy(m) %>% mutate(conf.low = NA_real_, conf.high = NA_real_)
    )
    td %>%
      filter(term == "year") %>%
      transmute(
        site_no  = df_site$site_no[1],
        change_per_decade = estimate * 10,
        lower_decade      = conf.low * 10,
        upper_decade      = conf.high * 10,
        p.value
      )
  })

trend_tbl

##----------------assumption checking:-----------------------------####

autoplot(lm_simple3)
hist(lm_simple3$residuals)

# residuals are normally distributed--better than I expected!

# scale-location has a slight trend, but nothing to indicate that residuals change a bunch
# depending on fitted values. heteroscedasticity assumption seems okay!

# residuals vs. fitted seems okay too--no apparent trends, so relationship seems linear

# residuals vs. leverage also seems okay. Most points have very low leverage

# I am concerned about the possibility of colinearity, so I'm doing a more detailed check
#that I found online, just to make sure!

#####---------------------check for colinearity:---------------------------#### 

#Main-effects ANCOVA (parallel slopes assumption)
m_main <- lm(tw ~ t_air + drainage_area, data = analysis_3_df)

#Interaction ANCOVA (different slopes by drainage_area)
m_int <- lm(tw ~ t_air * drainage_area, data = analysis_3_df)
summary(m_int)

#now looking at the model matrix

X_int <- model.matrix(m_int)
colnames(X_int)
# Examine correlations among numeric columns of the model matrix (excluding intercept)
cor_mat <- cor(X_int[, -1])
round(cor_mat, 2)

#there actually are not large r values between t_air and t_air:drainagearea,
#so I think I'm okay to proceed to the more detailed check

## more detailed check below using car package:

v_main <- vif(m_main)
v_int  <- vif(m_int)

v_main
v_int

# Adjusted GVIF for factor (drainage area) in the interaction model
df_attr <- attr(v_int, "df")  # df for multi-df terms (factors)
adj_gvif <- if (!is.null(df_attr)) v_int^(1/(2*df_attr)) else NULL

adj_gvif

#my GVIF is high for the model with interaction, but that is natural with categorical predictors, 
#so I will just "interpret cautiously"

##---------- additional analysis: AIC table ------------------------####

# Put models in a named list for convenience
lm_intercept3<- lm(tw ~ 1, data=analysis_3_df)
models <- list(
  parallel    = lm_simple3,
  interaction = lm_interaction3,
  air_only    = lm_intercept3
)

#calculate AIC for each model
aic_vals <- sapply(models, AIC)

# delta AIC relative to the best (lowest AIC)
delta_aic <- aic_vals - min(aic_vals)

# Akaike weights (closer to 1 is better)
akaike_weights <- exp(-0.5 * delta_aic)
akaike_weights <- akaike_weights / sum(akaike_weights)

#make the table!
aic_table <- data.frame(
  model = names(models),
  AIC = aic_vals,
  deltaAIC = delta_aic,
  weight = akaike_weights,
  row.names = NULL
)

# Order by AIC
aic_table <- aic_table[order(aic_table$AIC), ]
print(aic_table)

#making sure that model with interaction is significantly better than model without
ratio_test<- lmtest::lrtest(lm_simple3, lm_interaction3)
print(ratio_test)

## testing slope differences ##############################################

m <- lm(tw ~ t_air * drainage_area, data = analysis_3_df)
b <- coef(m)

slope_small  <- b["t_air"]
slope_medium <- b["t_air"] + b["t_air:drainage_areamedium"]
slope_large  <- b["t_air"] + b["t_air:drainage_arealarge"]

intercept_small  <- b["(Intercept)"]
intercept_medium <- b["(Intercept)"] + b["drainage_areamedium"]
intercept_large  <- b["(Intercept)"] + b["drainage_arealarge"]

emtr <- emtrends(m, ~ drainage_area, var = "t_air")
pairs(emtr)  # pairwise slope contrasts and p-values
summary(emtr)  # slope estimates per class with SE and CI

## slopes differ, but are not statistically different at the 95% confidence interval
## would need more sites with a range in drainage area to untangle this effect!

##---------------------- making a site map ---------------------------------####

#loading in MI site coords
mi_coords<- readxl::read_excel("mi_sites.xlsx", 
                               col_types = c("text", "numeric", "numeric")  # adjust order to match your sheet: Site_ID, Latitude, Longitude
)

#csv of site lat/longs
sf_coords<- data.frame(mi_coords) #sf obj conversion didn't work til I made this a df

#need to make coords df into a sf obj for mapping
sf_coords<- st_as_sf(mi_coords, 
                     coords= c("Longitude", #long NEEDS to come first!
                               "Latitude"),
                     crs= 4326) #this is for WGS84 for lat/long

GL<- st_read("Great_Lakes.shp") #downloaded from MI DNR arcgis website

options(tigris_use_cache = TRUE)

#getting Michigan state polygon (cb = TRUE gives generalized boundary; set FALSE for full detail)
mi <- states(cb = TRUE, year = 2022) |>
  st_as_sf() |>
  st_transform(4326) |>
  dplyr::filter(STUSPS == "MI")

#dissolve multiparts and extract exterior boundary only
mi_boundary <- st_cast(st_union(mi), "MULTILINESTRING")

#Plotting over my Great Lakes shapefile so you can see both
ggplot() +
  geom_sf(data = GL, fill = "grey80", color = "grey40", linewidth = 0.6) +
  geom_sf(data = mi_boundary, color = "black", linewidth = 0.9) +
  theme_minimal()

#### new map version adding site points

ggplot() +
  geom_sf(data = GL, fill = "grey80", color = "grey40", linewidth = 0.6) +
  geom_sf(data = sf_coords, aes(color = Site_ID), size = 3) +
  scale_color_viridis_d() +
  #using markdown syntax to insert line breaks, control line height, and bold the figure caption
  labs(
    color= "Site ID",
    caption = "<span style='line-height:0.9'><b>Figure 1.</b> Selection of Michigan USGS streamgages with 10 or more years<br>of daily stream temperature data.</span>"
  )+
  theme_minimal()+ 
  theme(
    plot.caption = element_markdown( #using markdown so that I can more easily bold "figure 1" and format caption
      size= 12, #caption text size
      hjust= 0, #justify left
    ),
    legend.title = element_text(size = 12), #increasing legend title and text size
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = c(0, 0.025),  # bottom left corner (tweak as needed)
    legend.justification = c(0, 0),  # anchor legend to bottom-left corner  
    axis.text = element_blank(), #googled this. getting rid of gridlines and lat/long   
    axis.ticks = element_blank(),  #getting rid of tick marks         
    axis.title = element_blank() #getting rid of axis title
  )
  theme_classic()
