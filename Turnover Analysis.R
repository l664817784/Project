library(dplyr)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(survival)
library(stringr)

d = read_csv("MAS646 Final Project/turnover.csv")


#DATA ANALYSIS SECTION

# turnover in 2022Q1
turnover_22 = d %>%
  mutate(termination_date = coalesce(termination_date, as.Date("2022-03-30"))) %>%
  filter(termination_date > "2022-01-01") %>%
  summarise(left = sum(left), quit = sum(Quit), fired = sum(Fired), 
            total = n(), year = '2022Q1')

# turnoverin 2021
turnover_21 = d %>%
  mutate(termination_date = coalesce(termination_date, as.Date("2022-03-30")),
         left = ifelse(termination_date > "2022-01-01", 0, left),
         Quit = ifelse(termination_date > "2022-01-01", 0, Quit),
         Fired = ifelse(termination_date > "2022-01-01", 0, Fired)) %>%
  filter(termination_date > "2021-01-01" & hire_date < "2022-01-01") %>%
  summarise(left = sum(left), quit = sum(Quit), fired = sum(Fired),
            total = n(), year = '2021')
  
# turnover in 2020  
turnover_20 = d %>%
  mutate(termination_date = coalesce(termination_date, as.Date("2022-03-30")),
         left = ifelse(termination_date > "2021-01-01" , 0, left),
         Quit = ifelse(termination_date > "2021-01-01", 0, Quit),
         Fired = ifelse(termination_date > "2021-01-01", 0, Fired)) %>%
  filter(termination_date > "2020-01-01" & hire_date < "2021-01-01") %>%
  summarise(left = sum(left), quit = sum(Quit), fired = sum(Fired),
            total = n(), year = '2020')

# turnover in 2019
turnover_19 = d %>%
  mutate(termination_date = coalesce(termination_date, as.Date("2022-03-30")),
         left = ifelse(termination_date > "2020-01-01" , 0, left),
         Quit = ifelse(termination_date > "2020-01-01", 0, Quit),
         Fired = ifelse(termination_date > "2020-01-01", 0, Fired)) %>%
  filter(termination_date > "2019-01-01" & hire_date < "2020-01-01") %>%
  summarise(left = sum(left), quit = sum(Quit), fired = sum(Fired),
            total = n(), year = '2019')

# turnover combine 2022Q1, 2021, 2020, 2019
turnover = bind_rows(turnover_22, turnover_21, turnover_20, turnover_19) %>%
  select(year, total, left, quit, fired)%>%
  mutate(turnover_rate = left/total,
         quit_percentage = quit/left,
         fired_percentage = fired/left)

# Create plot of turnover rate over time
ggplot(data = filter(turnover, year %in% c('2019', '2020', '2021')), 
       aes(x = year, y = turnover_rate)) +
  geom_bar(stat = "identity", fill = "#528c98") +
  labs(x = "Year", y = "Turnover Rate") +
  ggtitle("Turnover Rate over Time (2019-2021)") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(data = turnover, 
       aes(x = year, y = quit_percentage)) +
  geom_bar(stat = "identity", fill = "#dc7c61") +
  geom_smooth(method = "lm", se = FALSE, color = "#4C4C4C") +
  labs(x = "Year", y = "Quit Percentage") +
  ggtitle('Quit Proportion over Time (2019-2022Q1)') +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#SURVIVAL ANALYSIS

#split d to two group - quit, fired
d_q = d %>%
  filter(Fired == 0)%>%
  select(Time, Quit, age, gender, degree_level, workerhasdegree, segment, rating)

d_f = d %>%
  filter(Quit == 0)%>%
  select(Time, Fired, age, gender, degree_level, workerhasdegree, segment, rating)


#Survival Analysis in group quit
m_q = coxph(Surv(Time, Quit) ~ age + rating + degree_level, data=d_q)
summary(m_q)

#Survival Analysis in group fired
m_f = coxph(Surv(Time, Fired) ~ age + rating, data=d_f)
summary(m_f)




