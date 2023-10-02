library(readr)
library(tidyverse)
library(kableExtra)

options(digits = 8)

#read data

background1 <- read_table("background1.lst", skip = 9) %>%
  rename(channel = `0`, count = `4095`) %>%
  mutate(type = "Background")

background2 <- read_table("background2.lst", skip = 9) %>%
  rename(channel = `0`, count = `4095`) %>%
  mutate(type = "Background")

silver1 <- read_table("silver.lst", skip = 9) %>%
  rename(channel = `0`, count = `4095`) 

#function

least_mean <- function(data_input) {
  s = data_input
  Σw = sum(s$w)
  Σwt = sum(s$w * s$t)
  Σwy = sum(s$w * s$y)
  Σwty = sum(s$w * s$t * s$y)
  Σwtt = sum(s$w * s$t * s$t)
  
  Δ = Σw*Σwtt-(Σwt)^2
  a = (Σwtt*Σwy - Σwt*Σwty)/Δ
  b = (Σw*Σwty - Σwt*Σwy)/Δ
  σ_a = sqrt(Σwtt/Δ)
  σ_b = sqrt(Σw/Δ) 
  
  return(c(a,b,σ_a,σ_b))}

#extract channels

type <- c("Background before measurement", "Background after measurement",
          "Silver measurement", "Long lived regression",
          "Short lived regression")

channels <-c("1:698", "0:623", "25:670", "149:670", "25:134")

#make table of channels

channel_table <- cbind.data.frame(type, channels)
channel_table %>%
  rename(Type = type, Channels = channels) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("bordered", "striped"),
                full_width = FALSE) 

# change to 2 second intervalls
silver_ <- silver1[c(26:671), c(1:2)]%>%
  mutate(ranges = cut(channel, seq(25, 671, 2), right = FALSE)) %>%
  group_by(ranges) %>%
  summarize(count = sum(count)) %>%
  mutate(time = ((row_number()-1)*2 +1))

background_pre <- background1[c(2:699), c(1:2)] %>%
  mutate(ranges = cut(channel, seq(1, 699, 2), right = FALSE)) %>%
  group_by(ranges) %>%
  summarize(count = sum(count)) %>%
  mutate(time = ((row_number()-1)*2 +1))

background_after <- background2[c(1:624), c(1:2)]%>%
  mutate(ranges = cut(channel, seq(0, 624, 2), right = FALSE)) %>%
  group_by(ranges) %>%
  summarize(count = sum(count)) %>%
  mutate(time = ((row_number()-1)*2 +1), type = "Background")


#activies and average

activity_pre = sum(background_pre$count)/(nrow(background_pre)*2)
activity_pre_error = sqrt(sum(background_pre$count))/(nrow(background_pre)*2)

activity_after = sum(background_after$count)/(nrow(background_after)*2)
activity_after_error = sqrt(sum(background_after$count))/(nrow(background_after)*2)

mean_after = mean(background_after$count)
mean_after_error = sqrt(mean_after)

avg_activity =  (activity_pre + activity_after)/2 

stdm_avg_activity = sqrt(((activity_pre-avg_activity)^2 + (activity_after-avg_activity)^2)/2)

mean_after
mean_after_error

name <- c("Background before", "Background after", "Combined")
activity <- c(activity_pre, activity_after, avg_activity)
errors <- c(activity_pre_error, activity_after_error, stdm_avg_activity)


# make table of mean activity

activites <- cbind.data.frame(name, activity, errors)
activites %>%
  mutate(activity = format(activity, digits = 3), errors = format(errors, digits = 2)) %>%
  unite("Mean Activity (decay/s)", activity, errors, sep = ' ± ') %>%
  rename(Measurement = name) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("bordered", "striped"),
                full_width = FALSE) 


silver <- silver_ %>%
  mutate(activity = count/2, activity_error = sqrt(count)/2,
         activity_corr = activity - avg_activity, 
         activity_corr_error = sqrt(activity_error^2+stdm_avg_activity^2))

# histogram
ggplot() +
  geom_histogram(data = background_after, aes(x = count, fill = "Background"), color="#F8766D", binwidth = 1) +
  geom_line(aes(x = c(0, 1,2,3,4),
                y = sum(background_after$count)*(dpois(c(0,1,2,3,4),
                lambda = mean_after)), color = "Poisson"))+
  geom_point(aes(x = c(0, 1,2,3,4),
                y = sum(background_after$count)*(dpois(c(0,1,2,3,4),
                lambda = mean_after)), color = "Poisson"))+
  scale_fill_manual(name='Type',
                     breaks=c( 'Background'),
                     values=c( 'Background'="grey")) +
  scale_color_manual(name = "",
                     breaks=c( 'Poisson'),
                     values=c( 'Poisson'="#619CFF")) +
  labs(y = "Amount of times", x = "Decays",
       title = "Background matches modeled Poisson distribution", color = "Type")


#table for activity corr

silver[c(1:5),]%>%
  select(time, activity_corr, activity_corr_error)%>%
  mutate(activity_corr = format(activity_corr, digits = 4),
         activity_corr_error = format(activity_corr_error, digits=2)) %>%
  rename('Time (s)' = time) %>%
  unite('Corrected Activity (decay/s)', activity_corr, activity_corr_error, sep = ' ± ') %>%
  kable() %>%
  kable_styling(bootstrap_options = c("bordered", "striped"),
                full_width = FALSE) 

#diagram of ln(activity_corr)

silver%>%
  select(time, activity_corr, activity_corr_error)%>%
  mutate(log = log(activity_corr), error_log = activity_corr_error/activity_corr, type = "Silver") %>%
  ggplot(aes(x = time, y = log, color = type, alpha = 0.8)) +
  geom_point() +
  geom_errorbar(aes(xmin= time , xmax = time, ymin = log - error_log, ymax = log + error_log)) +
  labs(title= "Logarithmic scale shows two disctict processes",
       x = "Time (s)", y = "Logarithm of corrected activity (ln(decay)/s)")+
  guides(alpha = "none", color = "none")

#weighted least sqaure

#s is silver cutted
s1 <- silver[c(63:323),] %>%
  mutate(log = log(activity_corr), 
         error_log = activity_corr_error/activity_corr,
         w = 1/error_log^2) %>%
  select(ranges, time, log, error_log,w) %>%
  rename(t = time, y = log) 

# y = a + bt
# y = log(n_corr)
# t = time
# a = ln(n_1(0)) assumption
# b = -λ_1

least_mean_n_1 <- least_mean(data_input = s1)
least_mean_n_1  #a,b,σ_a, σ_b

#parameters
λ_1 = -least_mean_n_1[2]
σ_λ_1 = least_mean_n_1[4]
n_1 = exp(least_mean_n_1[1])
σ_n_1 = exp(least_mean_n_1[1])*least_mean_n_1[3]

# control
model <- lm(y ~ t, s, weights=(w)) 
summary(model)

#Parameters:
least_mean_n_1[4]
λ_1
σ_λ_1
n_1 
σ_n_1 


#plot cut part

s %>%
  ggplot(aes(x = t , y = y, alpha=0.8)) +
  geom_point() +
  geom_errorbar(aes(xmin= t , xmax = t ,
                    ymin = y - error_log,
                    ymax = y + error_log, alpha = 0.8)) +
  geom_line(aes(x = t, y = a + b*(t)))


#plot linear regression on top

silver%>%
  select(time, activity_corr, activity_corr_error)%>%
  mutate(log = log(activity_corr), error_log = activity_corr_error/activity_corr) %>%
  ggplot(aes(x = time, y = log, color = "Data points", alpha = 0.7)) +
  geom_point() +
  geom_errorbar(aes(xmin= time , xmax = time, ymin = log - error_log, ymax = log + error_log)) +
  geom_line(aes(x = time, y = least_mean_n_1[1] + least_mean_n_1[2]*(time), linetype = "Regression"), color = "#619CFF") +
  labs(title= "Logarithmic scale shows two disctict processes",
       x = "Time (s)", y = "Logarithm of corrected activity (ln(decay)/s)",
       linetype = " ", color = " Type ")+
  guides(alpha = "none") +
  annotate(geom = "text",
    label=(" y = a + bt
           a = 3.81 ± 0.04 
                     b = - 0.0048 ± 0.0001") ,
    x = 50, y = 2, color = "#619CFF")

least_mean_n_1[1]
least_mean_n_1[3]

least_mean_n_1[2]
least_mean_n_1[4]

#short isotope

silver_long_short <- silver%>%
  select(time, activity_corr, activity_corr_error)%>%
  mutate(n_1_t = n_1 * exp(-λ_1*time), 
         σ_n_1_t = sqrt( (exp(-λ_1 *time)*σ_n_1)^2 + ((-1)*n_1*time*exp(-λ_1 *time)*σ_λ_1)^2),
         n_2_t = activity_corr - n_1_t,
         σ_n_2_t = sqrt(activity_corr_error^2 + σ_n_1_t^2)) %>%view()

#table
silver_long_short[c(1:5),]%>%
  mutate(activity_corr = format(activity_corr, digits = 4),
         activity_corr_error = format(activity_corr_error, digits = 2),
         n_1_t = format(round(n_1_t,digits = 3), digits = 3),
         σ_n_1_t = format(σ_n_1_t, digits = 2),
         n_2_t = format(round(n_2_t, digits = 4), digits = 4),
         σ_n_2_t = format(σ_n_2_t, digits = 2)) %>% 
  unite('Corrected Activity (decay/s)', activity_corr, activity_corr_error, sep = ' ± ') %>%
  unite('Long Lived Activity (decay/s)', n_1_t, σ_n_1_t, sep = ' ± ') %>%
  unite('Short Lived Activity (decay/s)', n_2_t, σ_n_2_t, sep = ' ± ') %>%
  rename('Time (s)' = time)%>%
  kable() %>%
  kable_styling(bootstrap_options = c("bordered", "striped"),
                full_width = FALSE) 
  
  

# log plot for n_2
#obs rest is negative, can´t take logaritm 

silver_long_short[c(1:55),]%>% 
  select(time, n_2_t, σ_n_2_t)%>%
  mutate(log = log(n_2_t), error_log = σ_n_2_t/n_2_t, type = "short") %>%
  ggplot(aes(x = time, y = log, alpha=0.8, color = type)) +
  geom_point() +
  geom_errorbar(aes(xmin= time, xmax = time , ymin = log - error_log, ymax = log + error_log, alpha = 0.8)) +
  labs(title= "Logarithmic scale shows a linear process",
       x = "Time (s)", y = "Logarithm of short lived activity (ln(particle/s))") +
  guides(color = "none", alpha = "none")


# plot with regression
silver_long_short[c(1:55),]%>% 
  select(time, n_2_t, σ_n_2_t)%>%
  mutate(log = log(n_2_t), error_log = σ_n_2_t/n_2_t) %>%
  ggplot(aes(x = time, y = log, alpha=0.8, color = "Data points")) +
  geom_point() +
  geom_errorbar(aes(xmin= time, xmax = time , ymin = log - error_log, ymax = log + error_log, alpha = 0.8)) +
  geom_line(aes(x = time, y = least_mean_n_2[1] + least_mean_n_2[2]*(time), linetype = "Regression"), color = "#619CFF") +
  labs(linetype = " ", color = "Type",
       title= "Logarithmic scale shows a linear process",
       x = "Time (s)", y = "Logarithm of short lived activity (ln(particle/s))") +
  annotate(geom = "text",
           label=(" y = a + bt
          a = 4.99 ± 0.03
                     b = - 0.0283 ± 0.0009") ,
           x = 20, y = 2, color = "#619CFF") +
  guides(alpha = "none")


least_mean_n_2[1]
least_mean_n_2[3]

least_mean_n_2[2]
least_mean_n_2[4]

#least mean square

s2 <- silver_long_short[c(1:55),] %>%
  mutate(log = log(n_2_t), error_log = σ_n_2_t/n_2_t, w = 1/error_log^2) %>%
  rename(t = time, y = log)

least_mean_n_2 <- least_mean(data_input = s2)
least_mean_n_2  #a,b,σ_a, σ_b
least_mean_n_1
#parameters
λ_2 = -least_mean_n_2[2]
σ_λ_2 = least_mean_n_2[4]
n_2 = exp(least_mean_n_2[1])
σ_n_2 = exp(least_mean_n_2[1])*least_mean_n_2[3]


λ_2 
σ_λ_2 
n_2 
σ_n_2

  
#plot on top

silver_long_short[c(1:60),]%>% 
  select(time, n_2_t, σ_n_2_t)%>%
  mutate(log = log(n_2_t), error_log = σ_n_2_t/n_2_t) %>%
  ggplot(aes(x = time, y = log, alpha=0.8), color = "blue") +
  geom_point() +
  geom_errorbar(aes(xmin= time , xmax = time , ymin = log - error_log, ymax = log + error_log, alpha = 0.8)) +
  geom_line(aes(x = time, y = least_mean_n_2[1] + least_mean_n_2[2]*(time)), color = "green")


# half times

T_1 = log(2)/λ_1
T_2 = log(2)/λ_2

σ_T_1 = σ_λ_1 / λ_1 * T_1
σ_T_2 = σ_λ_2 / λ_2 * T_2

  
T_1 
T_2 
σ_T_1 
σ_T_2 

#plot measured data and models

silver %>%
  ggplot() + 
  geom_point(aes(x=time, y = activity, shape = "Data points"), alpha = 0.5) +
  geom_errorbar(aes(x = time, y = activity,
                    xmax = time, xmin = time,
                    ymax = activity + activity_error, ymin = activity - activity_error), alpha = 0.5) +
  geom_line(aes(x = time, y = n_1*exp(-λ_1 * time) + avg_activity, color = "Long lived", linetype = "Long lived"), size = 0.8) +
  geom_line(aes(x = time, y = n_2*exp(-λ_2 * time) + avg_activity, , color = "Short lived", linetype = "Short lived"), size = 0.8) +
  geom_line(aes(x = time, y = n_1*exp(-λ_1 * time) + n_2*exp(-λ_2 * time) + avg_activity,  color = "Total", linetype = "Total"), size = 0.8) +
  scale_color_manual(name='Model of isotope',
                     breaks=c( 'Long lived', 'Short lived', 'Total'),
                     values=c( 'Long lived'='#F8766D', 'Short lived'='#619CFF', 'Total'='seagreen2'))+
  scale_linetype_manual(name='Model of isotope',
                        breaks=c( 'Long lived', 'Short lived', 'Total'),
                        values=c('Long lived'='dashed', 'Short lived'='dotted', 'Total'='solid'))+
  labs(color ="Model of isotope", linetype = "Model of isotope", shape= "Measured",
       title = "Measured data is described by models",
       x = "Time (s)", y = "Activity (decay/s)")



silver_chi <- silver %>%
  select(time, activity) %>%
  mutate(expectation = n_1*exp(-λ_1 * time) + n_2*exp(-λ_2 * time),
         total_expectation = expectation + avg_activity,
         σ_total_expectation = sqrt(total_expectation), 
         deviation = (activity - total_expectation)^2 / σ_total_expectation^2)


silver_chi$σ_expectation^2
stdm_avg_activity^2

chi_squared = sum(silver_chi$deviation)
df = nrow(silver) - 6
df
chi_squared_tilde = chi_squared/df


p_value = pchisq(chi_squared, df, lower.tail=FALSE)
p_value

chi_squared
chi_squared_tilde

p_value

#create table of chi and p values
Calculation <- c("Chi Squared", "Reduced Chi Squared", "P value")
Value <- c(format(chi_squared, digits = 3), format(chi_squared_tilde, digits = 3), "1.00")

cbind.data.frame(Calculation, Value) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("bordered", "striped"),
                full_width = FALSE) 


#table of results
options(digits = 6)
Isotope <- c("Long lived", "Short lived")
constant <- c(n_1, n_2)
constant_error <- c(σ_n_1, σ_n_2)
decay <- c(λ_1*10^3, λ_2*10^3)
decay_error <- c(σ_λ_1*10^3, σ_λ_2*10^3)
half_time <- c(format(T_1, digits = 4), format(T_2, digits = 4))
half_time_error <- c(format(σ_T_1, digits = 2), format(σ_T_2, digits = 2))
literature <- c(142.92, 24.56)

cbind.data.frame(Isotope,constant, constant_error, 
                 decay, decay_error, 
                 half_time, half_time_error, literature) %>%
  mutate(constant = format(constant, digits = 3),
         constant_error = format(constant_error,digits = 2),
         decay = format(decay, digits = 3),
         decay_error = format(decay_error, digits = 2)) %>%
  unite("Activity constant n(0) (decay/s)", constant, constant_error, sep = ' ± ') %>%
  unite("Decay constant λ (1/ms)", decay, decay_error, sep = ' ± ') %>%
  unite("Half time (s)", half_time, half_time_error, sep = ' ± ') %>%
  rename("Literature half time (s)" = literature) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("bordered", "striped"),
                full_width = FALSE) 


