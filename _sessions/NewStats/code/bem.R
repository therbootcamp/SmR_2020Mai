

exp1 = readxl::read_excel('~/Downloads/exp1.xlsx')
names(exp1)

exp1 = exp1 %>% select(-Session, -Date,-Session.Length) %>%
  mutate(StartTime = hour(as_datetime(exp1$StartTime,format="%I:%M%p"))) %>%
  rename(hour_of_day = StartTime,
         gender = Participant.Sex,
         age = Participant.Age,
         depicted_gender  = ExpSex,
         stimulus_seeking = Stimulus.Seeking,
         erotic = Erotic.Hits.PC,
         control = Control.Hits.PC) %>%
  gather(condition, hit_rate, c(erotic, control)) %>%
  select(gender, age, stimulus_seeking, hour_of_day,condition,depicted_gender,hit_rate)


a = readxl::read_excel('~/Downloads/exp2.xlsx')
names(a)


