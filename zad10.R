library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
#names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric)]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
tbl_numeric <- drop_na(tbl_numeric)
names(tbl_numeric)

cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula1 = as.formula(paste("co2_flux", paste(vars,collapse = "+"), sep=""))
teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7))
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3))
tbl_numeric = filter(tbl_numeric, DOY > 151)
tbl_numeric = filter(tbl_numeric, DOY < 243)
mod = lm(co2_flux ~ (Tau+rand_err_Tau+H+rand_err_H+LE+qc_LE+rand_err_LE+co2_flux+h2o_flux+qc_h2o_flux+rand_err_h2o_flux+H_strg+co2_molar_density+h2o_time_lag+sonic_temperature+air_temperature+air_density+air_molar_volume+es+RH+VPD+max_speed+u_star_+TKE+T_star_+un_Tau+un_H+un_LE+un_co2_flux+un_h2o_flux+u_var+v_var+w_var+h2o_var+w_div_ts_cov+w_div_co2_cov+w_div_h2o_cov+flowrate)^2, data = tbl_numeric)
summary(mod)
resid(mod)
coef(mod)
names(tbl_numeric)
qplot(co2_flux, DOY, data = tbl_numeric, alpha = I(1/10)) + theme_bw() + geom_line(aes(y = predict(mod)))
qplot(co2_flux, predict(mod), data = tbl_numeric, geom = "line")
qplot(flowrate, co2_flux, data = tbl_numeric, alpha = I(1/10)) + theme_bw() + geom_line(aes(y = predict(mod)))
#lm(earn ~ . - age, data = wages)
anova(mod)

