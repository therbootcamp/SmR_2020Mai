

exp1 = readxl::read_excel('~/Downloads/exp1.xlsx')
names(exp1)

exp1 = exp1 %>% select(-Session, -Date,-Session.Length) %>%
  mutate(StartTime = hour(as_datetime(StartTime,format="%I:%M%p"))) %>%
  rename(hour_of_day = StartTime,
         gender = Participant.Sex,
         age = Participant.Age,
         depicted_gender  = ExpSex,
         stimulus_seeking = Stimulus.Seeking,
         erotic = Erotic.Hits.PC,
         control = Control.Hits.PC) %>%
  gather(condition, hit_rate, c(erotic, control)) %>%
  select(gender, age, stimulus_seeking, hour_of_day,condition,depicted_gender,hit_rate)


exp2 = readxl::read_excel('~/Downloads/exp2.xlsx')

exp2 = exp2 %>% select(-Session, -Date,-Session.Length) %>%
  mutate(StartTime = hour(as_datetime(StartTime,format="%I:%M%p"))) %>%
  rename(hour_of_day = StartTime,
         gender = Participant.Sex,
         age = Participant.Age,
         depicted_gender  = ExpSex,
         stimulus_seeking = Stimulus.Seeking,
         hit_rate = Hits.PC) %>%
  select(gender, age, stimulus_seeking, hour_of_day,hit_rate)


t.test(exp2$hit_rate,mu = 50)


write_csv(exp1,'_sessions/NewStats/1_Data/psi_exp1.csv')
write_csv(exp2,'_sessions/NewStats/1_Data/psi_exp2.csv')

dim(exp1)
dim(exp2)
