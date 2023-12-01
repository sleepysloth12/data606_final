library(httr)
library(rvest)
library(xml2)
library(tidyverse)
library(ggplot2)
library("RSocrata")
library(ggfortify)
library(onewaytests)
library(ggbeeswarm)
library(DATA606)
library(psych)
library(RColorBrewer)

# Importing  Data -------------------------------------------------------


## Opioid Data -------------------------------------------------------------

#OPIOID DEATH DATA
nyh_url = "https://health.data.ny.gov/resource/sn5m-dv52.csv"
nyh_app_token = "kOhfbw9lv6itO4zCkPMDQR1cz"

opioid_data = read.socrata(
 url=nyh_url,
  app_token = nyh_app_token,
)



# Substance Use Disorder Treatment Center Data ----------------------------


sud_treat_data = read.socrata(
  url="https://data.ny.gov/resource/ngbt-9rwf.csv",
  app_token = nyh_app_token
)

## fips code for cencus data FIPS DATA -------------------------------------


fips_url = "https://unicede.air-worldwide.com/unicede/unicede_new-york_fips_3.html"


fips_webpage = read_html(fips_url)

rfips_tables = fips_webpage %>% html_table(fill = TRUE)


fips_tables = rfips_tables[[1]]

fips_heads=c("fips","county")

names(fips_tables)=fips_heads

print(head(fips_tables))



## cencus data -------------------------------------------------------------
#cencus data from https://seer.cancer.gov/popdata/download.html

census_url = "https://raw.githubusercontent.com/sleepysloth12/data606_final/main/ny_census_1990_2020.txt"

census_response = GET(census_url)
census_data_text = content(census_response, "text")

census_lines = unlist(strsplit(census_data_text, "\n"))

census_lengths = c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8)

col_names = c("year", "state_abb", "state_fips", "county_fips", "reg", "race", "origin", "sex", "age", "population")


split_line = function(line, lengths) {
  start <- 1
  values <- vector("list", length(lengths))
  for (i in seq_along(lengths)) {
    len <- lengths[i]
    values[[i]] <- substr(line, start, start + len - 1)
    start <- start + len
  }
  return(unlist(values))
}

census_data_list = lapply(census_lines, split_line, census_lengths)


census_matrix= do.call(rbind, census_data_list)



census_df = as.data.frame(census_matrix, stringsAsFactors = FALSE)
colnames(census_df) = col_names
census_df$year = as.numeric(census_df$year)
census_df$population = as.numeric(census_df$population)


print(head(census_df))



# data preprocessing ------------------------------------------------------


## join opioid data and census and clean ---------------------------------------------


### add fips code -----------------------------------------------------------


opioid_data_fps = opioid_data %>%
  left_join(fips_tables, by = "county")

class(opioid_data_fps$fips)

clean_census_df= census_df %>%
  select(year, county_fips, population) %>%
  group_by(year, county_fips) %>%
  summarise(total_population = sum(population))


clean_census_df$county_fips=as.integer(clean_census_df$county_fips)

clean_census_df$fips=clean_census_df$county_fips


### add census data and clean ------------------------------------------------


opioid_data_comp = opioid_data_fps %>%
  inner_join(clean_census_df, by = c("fips","year"))

opioid_data_clean=opioid_data_comp %>%
  select(year,county,total_population,opioid_poisoning_deaths) %>%
  arrange(county,year)%>%
  mutate(death_prop=opioid_poisoning_deaths/total_population)

counties = opioid_data_county %>%
  select(county) 

ny_counties= counties$county

ny_counties_df=as.data.frame(ny_counties)

ny_counties_df=ny_counties_df %>%
  arrange(ny_counties)



## Cleaning and adding SUD treatment data ----------------------------------

### cleaning sud data -------------------------------------------------------

str(sud_treat_data)

unique(sud_dat$primary_substance_group)

sud_dat=sud_treat_data %>%
  select(year, county_of_program_location, age_group,primary_substance_group,admissions) %>%
  filter(primary_substance_group %in% c("Heroin","Other Opioids"))

sud_dat_pro=sud_dat %>%
  group_by(year,county_of_program_location) %>%
  summarise(n_programs_avail=n())

sud_dat_age=sud_dat %>%
  group_by(year, county_of_program_location, age_group)%>%
  summarise(adm_per_grp= sum(admissions))

sud_dat_county=sud_dat_age %>%
  group_by(year,county_of_program_location) %>%
  summarise(total_admissions= sum(adm_per_grp))

names(sud_dat_county)=c("year","county","total_admissions")
names(sud_dat_age)=c("year","county","age_group","adm_per_age_grp")
names(sud_dat_pro)=c("year","county","n_avilable_programs")

unique(sud_dat_county$year)

unique(opioid_data_clean$year)

sud_dat_county=sud_dat_county%>%
  filter(year != 2021)

sud_dat_age=sud_dat_age%>%
  filter(year != 2021)

sud_dat_pro=sud_dat_pro%>%
  filter(year != 2021)

###  merging sud data with our opioid data -----------------------------------

opioid_data_clean=opioid_data_clean %>%
  filter(year >= 2007 )

opioid_data_clean=opioid_data_clean %>%
  inner_join(sud_dat_county, by=c("year","county"))


opioid_data_clean=opioid_data_clean %>%
  inner_join(sud_dat_pro, by=c("year","county"))

opioid_data_clean=opioid_data_clean %>%
  select(year, county, total_population, n_avilable_programs, total_admissions, opioid_poisoning_deaths, death_prop)

opioid_data_clean=opioid_data_clean %>%
  mutate(prog_per_cap=n_avilable_programs /total_population,
         admissions_per_cap=total_admissions/total_population) %>%
  select(year, county, total_population, n_avilable_programs,prog_per_cap, total_admissions,admissions_per_cap, opioid_poisoning_deaths, death_prop)

## add county tiers --------------------------------------------------------

ny_counties_df=read.csv(url("https://raw.githubusercontent.com/sleepysloth12/data606_final/main/ny_counties.csv"))
ny_counties_df = ny_counties_df %>%
  mutate(county=ny_counties) %>%
  select(county,tier)

opioid_data_clean = opioid_data_clean %>%
  inner_join(ny_counties_df, by="county")%>%
  select(year, county,tier, total_population, n_avilable_programs,prog_per_cap, total_admissions,admissions_per_cap, opioid_poisoning_deaths, death_prop)%>%
  arrange(county,year,tier)


# exploratory data analysis -----------------------------------------------

summary(opioid_data_clean)


## by county ---------------------------------------------------------------


opioid_data_county = opioid_data_clean %>%
  group_by(county) 

mean_opioid_data_county=opioid_data_county%>%
  summarise(mean_deaths = mean(opioid_poisoning_deaths, na.rm = TRUE),
            mean_pop=mean(total_population, na.rm=TRUE),
            mean_prog=mean(n_avilable_programs),
            mean_admissions=mean(total_admissions)) %>%
  mutate(mean_prop=mean_deaths/mean_pop,
         mean_prog_percap=mean_prog/mean_pop,
         mean_admissions_percap=mean_admissions/mean_pop) %>%
  select(county,mean_pop,mean_prog_percap,mean_admissions_percap,mean_prop) %>%
  arrange(desc(mean_prop))

summary(opioid_data_county)

ggplot(mean_opioid_data_county, aes(x = mean_prop)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Mean Deaths by County")



## by region ---------------------------------------------------------------
opioid_data_tier = opioid_data_clean %>%
  group_by(tier) 

mean_opioid_data_tier=opioid_data_tier%>%
  summarise(mean_deaths = mean(opioid_poisoning_deaths, na.rm = TRUE),
            mean_pop=mean(total_population, na.rm=TRUE),
            mean_prog=mean(n_avilable_programs),
            mean_admissions=mean(total_admissions)) %>%
  mutate(mean_prop=mean_deaths/mean_pop,
         mean_prog_percap=mean_prog/mean_pop,
         mean_admissions_percap=mean_admissions/mean_pop) %>%
  select(tier,mean_pop,mean_prog_percap,mean_admissions_percap,mean_prop) %>%
  arrange(desc(mean_prop))



summary(mean_opioid_data_tier)

ggplot(mean_opioid_data_tier, aes(x = reorder(tier, -mean_prop), y = mean_prop)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Mean Proportion of Opioid Deaths by Tier", x = "Tier", y = "Mean Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


anova_tier = aov(mean_prop ~ tier, data = mean_opioid_data_tier)
summary(anova_tier)

anova_tier = aov(mean_prop ~ mean_prog_percap, data = mean_opioid_data_tier)
summary(anova_tier)

cor_test_result_tier = cor.test(mean_opioid_data_tier$mean_prog_percap, mean_opioid_data_tier$mean_prop, method = "pearson")
cor_test_result_tier

## by year -----------------------------------------------------------------



opioid_data_year = opioid_data_clean %>%
  group_by(year) %>%
  summarise(mean_deaths = mean(opioid_poisoning_deaths, na.rm = TRUE),
            mean_pop=mean(total_population, na.rm=TRUE)) %>%
  mutate(mean_prop=mean_deaths/mean_pop) %>%
  arrange(desc(mean_prop))

summary(opioid_data_year)






# Comparing Change btween years per county --------------------------------


opioid_data_county_an=opioid_data_county %>%
  select(year,county,total_population,prog_per_cap,admissions_per_cap,death_prop)

unique(opioid_data_county_an$year)

opioid_death_county_percent_change = opioid_data_county_an %>%
  group_by(county) %>%
  arrange(county, year) %>%
  mutate(percent_change = (death_prop - lag(death_prop)) / lag(death_prop)) %>%
  select(-total_population, -prog_per_cap, -admissions_per_cap, -death_prop) %>%
  spread(key = year, value = percent_change)


opioid_death_county_percent_change$`2007`=NULL

names(opioid_death_county_percent_change)=c("county","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13")



population_county_percent_change = opioid_data_county_an %>%
  group_by(county) %>%
  arrange(county, year) %>%
  mutate(percent_change = (total_population - lag(total_population)) / lag(total_population)) %>%
  select(-total_population, -prog_per_cap, -admissions_per_cap, -death_prop) %>%
  spread(key = year, value = percent_change)


population_county_percent_change$`2007`=NULL

names(population_county_percent_change)=c("county","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13")


prog_county_percent_change = opioid_data_county_an %>%
  group_by(county) %>%
  arrange(county, year) %>%
  mutate(percent_change = (prog_per_cap - lag(prog_per_cap)) / lag(prog_per_cap)) %>%
  select(-total_population, -prog_per_cap, -admissions_per_cap, -death_prop) %>%
  spread(key = year, value = percent_change)


prog_county_percent_change$`2007`=NULL

names(prog_county_percent_change)=c("county","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13")


admit_county_percent_change = opioid_data_county_an %>%
  group_by(county) %>%
  arrange(county, year) %>%
  mutate(percent_change = (admissions_per_cap  - lag(admissions_per_cap )) / lag(admissions_per_cap )) %>%
  select(-total_population, -prog_per_cap, -admissions_per_cap, -death_prop) %>%
  spread(key = year, value = percent_change)


admit_county_percent_change$`2007`=NULL

names(admit_county_percent_change)=c("county","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13")


opioid_death_county_percent_change = opioid_death_county_percent_change %>%
  rowwise()%>%
  mutate(net_death_change=sum(c_across(where(is.numeric)),na.rm=TRUE)) %>%
  ungroup()


population_county_percent_change = population_county_percent_change %>%
  rowwise()%>%
  mutate(net_pop_change=sum(c_across(where(is.numeric)),na.rm=TRUE)) %>%
  ungroup()


prog_county_percent_change = prog_county_percent_change %>%
  rowwise()%>%
  mutate(net_prog_change=sum(c_across(where(is.numeric)),na.rm=TRUE)) %>%
  ungroup()

admit_county_percent_change = admit_county_percent_change %>%
  rowwise()%>%
  mutate(net_admit_change=sum(c_across(where(is.numeric)),na.rm=TRUE)) %>%
  ungroup()

opioid_death_county_percent_change = opioid_death_county_percent_change %>%
  filter(net_death_change != Inf) %>%
  arrange(desc(net_death_change)) %>%
  select(county,net_death_change)

population_county_percent_change = population_county_percent_change %>%
  filter(net_pop_change != Inf) %>%
  arrange(desc(net_pop_change))%>%
  select(county,net_pop_change)

prog_county_percent_change = prog_county_percent_change %>%
  filter(net_prog_change != Inf) %>%
  arrange(desc(net_prog_change))%>%
  select(county,net_prog_change)

admit_county_percent_change = admit_county_percent_change %>%
  filter(net_admit_change != Inf) %>%
  arrange(desc(net_admit_change))%>%
  select(county,net_admit_change)

final_opioid_data= population_county_percent_change %>%
  group_by(county)%>%
  right_join(prog_county_percent_change)

final_opioid_data= final_opioid_data %>%
  group_by(county)%>%
  right_join(opioid_death_county_percent_change)

final_opioid_data= final_opioid_data %>%
  group_by(county)%>%
  right_join(admit_county_percent_change)

final_opioid_data = final_opioid_data %>% 
  arrange(desc(net_death_change))

mean(final_opioid_data$net_prog_change)

final_opioid_data = final_opioid_data %>%
  mutate(more_prog = ifelse(net_prog_change >= mean(net_prog_change),1,0))
#multiple regression model

mreg_1=lm(net_death_change ~ net_admit_change + net_prog_change, data=final_opioid_data)
summary(mreg_1)
autoplot(mreg_1)

mreg_2=lm(net_death_change ~ net_admit_change , data=final_opioid_data)
summary(mreg_2)
autoplot(mreg_2)

mreg_3=lm(net_death_change ~  net_prog_change, data=final_opioid_data)
summary(mreg_3)
autoplot(mreg_3)


ggplot(final_opioid_data, aes(x = net_admit_change, y = net_death_change)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlab('Percent Change admission per capita') +
  ylab('Percent Change Opioid Death Rate') +
  ggtitle('Relationship between Opioid admission rate and death rate')





td=opioid_data_clean %>%
  group_by(tier) %>%
  select(county,tier) %>%
  distinct()


final_opioid_data = final_opioid_data %>%
  group_by(county)%>%
  right_join(td)



tier_final_dat=final_opioid_data %>%
  group_by(tier) %>%
  summarise(mean_pop_rate_change=mean(net_pop_change, na.rm = TRUE),
            mean_death_rate_change=mean(net_death_change, na.rm = TRUE))


region_anova=aov(mean_death_rate_change ~ tier, data=tier_final_dat)
summary(region_anova)


tier_2_dat=opioid_data_tier %>%
  filter(!is.na(death_prop)) %>%
  select(tier,death_prop)

tier_2_dat %>%
  ggplot(aes(x= tier, y=death_prop))+
  geom_boxplot()+
  labs(
    title = "Opioid Death Per Capita by NYS Region 2007-2020",
    x = "Region",
    y = "Opioid Deaths Per Capita"
  )



tier_2_dat %>%
  group_by(tier) %>%
  summarise(varience=var(death_prop))

tier_2_dat %>%
  group_by(tier) %>%
  summarise(lengths=n())

leveneTest(death_prop ~ tier, data = tier_2_dat, center = mean)

owt = oneway.test(death_prop ~ tier, data = tier_2_dat)

anova_111=aov(death_prop ~ tier, data = tier_2_dat)
summary(anova_111)
#Null Hypothesis (H0): Mean opioid deaths per capita accross all nys regeions  will be the same.

#Alternative Hypotheses (HA): Mean opioid deaths per capita between at least 1 of the nys regeion would be significantly different.

no_hv = tier_2_dat %>%
  filter(tier != "Hudson Valley")%>%
  group_by(tier)

leveneTest(death_prop ~ tier, data = no_hv, center = mean)

owt2 = oneway.test(death_prop ~ tier, data = no_hv)

anova_222 = aov(death_prop ~ tier, data = no_hv)
summary(anova_222)








#SPARC Data
sparcs_url = "https://health.data.ny.gov/resource/rxm6-fp54.csv"


sparcs_raw = read.socrata(
  url=sparcs_url,
  app_token = nyh_app_token,
)

# start here --------------------------------------------------------------



sparcs_dat_1= sparcs_raw %>%
  select(year, patient_county_name, er_rateper1000) %>%
  group_by(year, patient_county_name) %>%
  summarise(mean_er_rate=mean(er_rateper1000, na.rm = TRUE))



names(sparcs_dat_1)=c("year","county","mean_er_rate")

sparcs_dat_2 = sparcs_dat_1 %>%
  mutate(mean_er_n=1000*mean_er_rate)


opioid_er_raw= opioid_data_county %>%
  group_by(year, county) %>%
  right_join(sparcs_dat_2)

opioid_er_1=opioid_er_raw %>%
  select(year, county, opioid_poisoning_deaths, mean_er_n) %>%
  mutate(opioid_poisoning_deaths=as.numeric(opioid_poisoning_deaths)) %>%
  filter(!is.na(opioid_poisoning_deaths) & ! is.na(mean_er_n))

max(opioid_er_1$opioid_poisoning_deaths)


ggplot(opioid_er_1, aes(x =mean_er_n , y = opioid_poisoning_deaths)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  xlab('Number of ER Opioid Visits') +
  ylab('Opioid Deaths') +
  ggtitle('Relationship between ER Opioid Visits and Opioid Deaths')

er_model=lm(death_prop ~ rat_in_out, data =  opioid_er_1)
summary(er_model)

summary(opioid_er_1$opioid_poisoning_deaths)

ci_upper(opioid_er_1$opioid_poisoning_deaths)





sparcs_dat_5= sparcs_raw %>%
  select(year, patient_county_name, er_rateper1000, payer)


 # group_by(payer) %>%
 # summarise(mean_er_rate=mean(er_rateper1000, na.rm = TRUE))



oer= opioid_data_county %>%
    group_by(year, county) %>%
    right_join(sparcs_dat_5) %>%
  select(death_prop, er_rateper1000, payer)%>%
  ungroup()

oer2=oer %>%
  group_by(payer) %>%
   summarise(mean_er_rate=mean(er_rateper1000, na.rm = TRUE))

oer3 =oer2 %>%
  mutate(mean_er_n=1000*mean_er_rate)

ins_type=c("Other","Commercial")

pvp1= oer %>%
  mutate(is_priv= ifelse(payer %in% ins_type, 1, 0),
         n_dead= death_prop * er_rateper1000 * 1000)

pvp2=pvp1%>%
  select(n_dead, is_priv) %>%
  filter(!is.na(n_dead)&!is.na(is_priv))


log_mod=glm(is_priv ~ n_dead, data=pvp2, family=binomial)
summary(log_mod)

confint(log_mod, level=0.95)

pvp2$predictions = predict.glm( type = "response", newdata = pvp2)

pvp2_sorted <- pvp2[order(pvp2$n_dead), ]

ggplot(pvp2, aes(x = n_dead, y = predictions)) +
  geom_point(aes(color = factor(is_priv)), alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "n_dead", y = "Predicted Probability", color = "is_priv") +
  theme_minimal()


pvp2$pearson_residuals <- residuals(log_mod, type = "pearson")

# Plot the residuals
ggplot(pvp2, aes(x = predictions, y = pearson_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicted Probability", y = "Pearson Residuals") +
  theme_minimal()

autoplot(log_mod)

payer_count=pvp2 %>%
  group_by(is_priv)%>%
  summarise(n())



ggplot(payer_count, aes(x = factor(is_priv), y = `n()`, fill = factor(is_priv))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Payer Type", y = "Count") +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Public", "Private"),
                    name = "Payer Type") +
  theme_minimal()


ggplot(pvp2, aes(x = n_dead, fill = factor(is_priv))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ factor(is_priv)) +
  labs(x = "Opioid Death Rate", y = "Density", fill = "Payer Type") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), labels = c("Public", "Private"))+
  coord_cartesian(c(0,0.75))

pvp2 %>%
  group_by(is_priv) %>%
  summarise(mean= mean(n_dead),
            med=median(n_dead),
            std=sd(n_dead))


sparcs_dat_9= sparcs_raw %>%
  select(year, patient_county_name, er_rateper1000, payer, rural_urban)

rr1= opioid_data_county %>%
  group_by(year, county) %>%
  right_join(sparcs_dat_9) %>%
  select(death_prop, er_rateper1000, payer,rural_urban)%>%
  ungroup()


rr2=rr1 %>%
  filter(rural_urban != "State") %>%
  group_by(rural_urban) %>%
  summarise(mean_er_rate=mean(er_rateper1000, na.rm = TRUE))

rr3 =rr2 %>%
  mutate(mean_er_n=1000*mean_er_rate)



zzz1= rr1 %>%
  mutate(is_rural= ifelse(rural_urban=="Rural", 1, 0),
         n_dead= death_prop * er_rateper1000 * 1000)

zzz2=zzz1%>%
  select(n_dead, is_rural) %>%
  filter(!is.na(n_dead)&!is.na(is_rural))


rur_mod=glm(is_rural ~ n_dead, data=zzz2, family=binomial)
summary(rur_mod)



zzz2$predictions = predict.glm(rur_mod , newdata = pvp2)

zzz2_sorted <- zzz2[order(zzz2$n_dead), ]

ggplot(zzz2, aes(x = n_dead, y = predictions)) +
  geom_point(aes(color = factor(is_rural)), alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "n_dead", y = "Predicted Probability", color = "is_rural") +
  theme_minimal()

# Plot the residuals
ggplot(zzz2, aes(x = predictions, y = pearson_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicted Probability", y = "Pearson Residuals") +
  theme_minimal()

autoplot(rur_mod)

rural_urban_count=zzz2 %>%
  group_by(is_rural)%>%
  summarise(n())



ggplot(rural_urban_count, aes(x = factor(is_rural), y = `n()`, fill = factor(is_rural))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "County Type", y = "Count") +
  scale_fill_manual(values = c("green", "purple"),
                    labels = c("Urban", "Rural"),
                    name = "County Type") +
  theme_minimal()


ggplot(zzz2, aes(x = n_dead, fill = factor(is_rural))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ factor(is_rural)) +
  labs(x = "Opioid Death Rate", y = "Density", fill = "County Type") +
  theme_minimal() +
  scale_fill_manual(values = c("green", "purple"), labels = c("Urban", "Rural"))+
  coord_cartesian(c(0,0.75))

zzz2 %>%
  group_by(is_rural) %>%
  summarise(mean= mean(n_dead),
            std=sd(n_dead),
            med=median(n_dead))




ggplot(pvp2, aes(x=as.character(is_priv), y=n_dead))+ geom_boxplot() 


ir_payer = pvp2 %>%
  select(n_dead, is_priv) %>%
  mutate(mortality_1k=n_dead * 1000)

ir_payer = ir_payer %>%
  select(mortality_1k, is_priv)

ggplot(ir_payer, aes(x=as.character(is_priv), y=mortality_1k))+ geom_boxplot()

ggplot(ir_payer, aes(x = mortality_1k, fill = factor(is_priv))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ factor(is_priv)) +
  labs(x = "Opioid Mortality Rate per 1000", y = "Density", fill = "Payer Type") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), labels = c("Public", "Private"))+
  coord_cartesian(c(0,500))






# last day changes --------------------------------------------------------


t_payer=ir_payer %>%
  mutate(is_priv=ifelse(is_priv==1, TRUE, FALSE)) %>%
  mutate(mortality_1k=ceiling(mortality_1k)) 

count_under_100=t_payer %>%
  filter(mortality_1k < 100) %>%
  count(mortality_1k) %>%
  summarise(total_obs=sum(n))

count_over_100=t_payer %>%
  filter(mortality_1k > 100) %>%
  count(mortality_1k) %>%
  summarise(total_obs=sum(n))

t_payer_100 = t_payer %>%
  filter(mortality_1k <= 100)

t_payer_100_plus = t_payer %>%
  filter(mortality_1k > 100)

t_payer_100_count = t_payer_100 %>%
  group_by(is_priv)%>%
  summarise(n())

ggplot(t_payer_100, aes(x = is_priv, y = mortality_1k)) + geom_boxplot() +
  geom_beeswarm(aes(color = is_priv)) + theme(legend.position = 'none')


set.seed(606) 
sampled_payer <- t_payer %>% sample_frac(0.1)

ggplot(sampled_payer, aes(x = is_priv, y = mortality_1k)) + geom_boxplot() +
  geom_beeswarm(aes(color = is_priv)) + theme(legend.position = 'none')


boxplot_payer <- ggplot(t_payer_100, aes(x = is_priv, y = mortality_1k)) +
  geom_boxplot() +
  geom_jitter(aes(color = is_priv), width = 0.2) + 
  theme(legend.position = 'none')

# Display the plot
print(boxplot_payer)





# new way summary stats ---------------------------------------------------

#preprocessing
sparcs_final= sparcs_raw %>%
  select(year, patient_county_name, overall_rateper1000, rural_urban, payer)

joint_data_final= opioid_data_county %>%
  group_by(year, county) %>%
  right_join(sparcs_final) %>%
  ungroup()

names(joint_data_final)

joint_data_final=joint_data_final %>%
  select(rural_urban,payer,overall_rateper1000) %>%
  filter(payer %in% c("Commercial" ,"Medicaid" , "Medicare"),
         rural_urban != "State")




#summary stats

payer_stats= joint_data_final %>%
  group_by(payer) %>%
  summarise(min= min(overall_rateper1000),
            mean= mean(overall_rateper1000),
             med=median(overall_rateper1000),
             std=sd(overall_rateper1000),
            max=max(overall_rateper1000))

payer_tab=describeBy(joint_data_final$overall_rateper1000,
                            group=joint_data_final$payer,
                            mat=TRUE, skew=FALSE)
write.csv(payer_tab, 'payerstats.csv')

rural_tab=describeBy(joint_data_final$overall_rateper1000,
                     group=joint_data_final$rural_urban,
                     mat=TRUE, skew=FALSE)

write.csv(rural_tab, 'ruralstats.csv')

payer_tab$variance=payer_tab$sd^2
rural_tab$variance=rural_tab$sd^2
payer_tab$contrast=payer_tab$mean - mean(payer_tab$mean)
rural_tab$contrast=rural_tab$mean - mean(rural_tab$mean)

n_rows_final=nrow(joint_data_final)
grand_mean= mean(joint_data_final$overall_rateper1000)
grand_var= var(joint_data_final$overall_rateper1000)
pooled_var_payer= mean(payer_tab$variance)
pooled_var_rural= mean(rural_tab$variance)

 boxpayer <- ggplot(joint_data_final, aes(x=payer, y=overall_rateper1000)) +
  geom_boxplot() +
   scale_fill_brewer(palette="BuPu")+
  geom_point(data = payer_tab, aes(x=group1 , y=mean),
             color='red', size=4)
 
 density_payer <- ggplot(joint_data_final, aes(x = overall_rateper1000, fill = factor(payer))) +
   geom_density(alpha = 0.5) +  # Set transparency to allow overlap visibility
   labs(x = "Deaths Per 1000 People", y = "Frequency", fill = "Payer Type") +
   theme_minimal() +
   scale_fill_brewer(palette = "Set1")
 
 violin_player <- ggplot(joint_data_final, aes(x = payer, y = overall_rateper1000)) +
   geom_violin(trim = TRUE) +  # Draw violin plot
   geom_jitter(width = 0.1, alpha = 0.3, color = "blue") +  # Add jittered points for data visualization
   labs(title = "Violin Plot of Overall Rate per 1000 by Insurance Payer Type",
        x = "Commercal vs. Medicaid vs. Medicare",
        y = "Overall Rate per 1000") +
   theme_minimal() 


 boxrural <- ggplot(joint_data_final, aes(x=rural_urban, y=overall_rateper1000)) +
   geom_boxplot() +
   scale_fill_brewer(palette="BuPu")+
   geom_point(data = rural_tab, aes(x=group1 , y=mean),
              color='red', size=4)
 density_rural <- ggplot(joint_data_final, aes(x = overall_rateper1000, fill = factor(rural_urban))) +
   geom_density(alpha = 0.5) +  # Set transparency to allow overlap visibility
   labs(x = "Deaths Per 1000 People", y = "Frequency", fill = "County Type") +
   theme_minimal() +
   scale_fill_brewer(palette = "Set1")
 
 violin_rural<- ggplot(joint_data_final, aes(x = rural_urban, y = overall_rateper1000)) +
   geom_violin(trim = TRUE) +  # Draw violin plot
   geom_jitter(width = 0.1, alpha = 0.3, color = "blue") +  # Add jittered points for data visualization
   labs(title = "Violin Plot of Overall Rate per 1000 by County Type",
        x = "rural vs. Urban",
        y = "Overall Rate per 1000") +
   theme_minimal() 
 
 #Anova
 
aov_payer= aov(overall_rateper1000 ~ payer, data = joint_data_final) |> summary()
 
aov_rural= aov(overall_rateper1000 ~ rural_urban, data = joint_data_final) |> summary()

# Perform Tukey HSD test
tukey_payer <- TukeyHSD(aov(overall_rateper1000 ~ payer, data = joint_data_final))
tukey_rural <- TukeyHSD(aov(overall_rateper1000 ~ rural_urban, data = joint_data_final))

# Convert to a data frame (for ggplot)
tukey_df_payer <- as.data.frame(tukey_payer$`payer`)
tukey_df_rural <- as.data.frame(tukey_rural$`rural_urban`)


tukey_df_payer$comparison <- rownames(tukey_df_payer)
tukey_df_rural$comparison <- rownames(tukey_df_rural)


ggplot(tukey_df_payer, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(title = "Tukey HSD Test Results",
       x = "Group Comparison",
       y = "Mean Differences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


ggplot(tukey_df_rural, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(title = "Tukey HSD Test Results",
       x = "Group Comparison",
       y = "Mean Differences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(joint_data_final, aes(x = rural_urban, y = overall_rateper1000)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Violin Plot of Overall Rate per 1000 by Rural vs Urban",
       x = "Rural vs Urban",
       y = "Overall Rate per 1000") +
  theme_minimal()

group_summary <- joint_data_final %>%
  group_by(rural_urban) %>%
  summarise(mean = mean(overall_rateper1000),
            se = sd(overall_rateper1000) / sqrt(n()),
            ci_upper = mean + qt(0.975, df=n()-1) * se,
            ci_lower = mean - qt(0.975, df=n()-1) * se)

# Create the plot
ggplot(group_summary, aes(x = rural_urban, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mean Overall Rate per 1000 with 95% CI by Rural vs Urban",
       x = "Rural vs Urban",
       y = "Mean Overall Rate per 1000") +
  theme_minimal()
