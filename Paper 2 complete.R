# FOR Correlation and Regression   ------------------------------------------------

## Load necessary package -------
library(pwr)
library(tidyverse)
library(reshape2)
library(broom)
library(kableExtra)

## RAW 2023 --------------------------------------------------------------------

raw_survey_data_2023 <- read.csv("Raw_Survey_Results_2023.csv")

raw_survey_data_2023 <- raw_survey_data_2023 %>% rename( Language = Start.language,
                                                         Consent = I.GIVE.MY.CONSENT.to.participate.in.this.study.and.allow.the.use.of.data.generated.in.Mosquito.Alert.on.my.device.to.be.re.used.in.this.research.project..,
                                                         User_ID = user_UUID,
                                                         Age = How.old.are.you.,
                                                         Gender = What.is.your.gender.,
                                                         Country = What.is.the.country.you.currently.reside.in.,
                                                         Participation_Date = In.what.year.did.you.first.participate.in.Mosquito.Alert.,
                                                         Network = How.many.people.do.you.personally.know..acquaintances..friends..family.members.etc...who.are.participating.in.Mosquito.Alert..not.including.yourself..,
                                                         Other_Citi_Sci = Are.you.currently.engaged.in.other.citizen.science.projects.,
                                                         Self_Direction = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.am.interested.in.the.topic.of.this.project.,
                                                         Stimulation = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.challenge.myself.and.do.something.new.,
                                                         Hedonism = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.a.fun.activity.,
                                                         Achievement = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.an.opportunity.to.perform.better.than.others.,
                                                         Face = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.enhance.my.reputation.,
                                                         Security = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.live.in.safer.surroundings.,
                                                         Conformity = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....Other.people.I.know.are.participating..,
                                                         Benevolence = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.my.community.,
                                                         Universalism_Social = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.make.the.world.a.better.place.,
                                                         Universalism_Nature = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.protect.the.environment.,
                                                         Routine = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.is.part.of.my.routine.,
                                                         Social_Expansion = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.be.part.of.this.volunteers..community.,
                                                         Power =On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.gain.recognition.,
                                                         Help_Science = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.science.,
                                                         Teaching = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.use.it.to.teach.others.about.the.topic.,
                                                         Dislike = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.dislike.mosquitos..,
                                                         Env_Change = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.noticed.an.increase.or.change.in.mosquitos.in.my.surroundings.,
                                                         Prom_1 = I.prefer.to.work.without.instructions.from.others.,
                                                         Prev_1 = Rules.and.regulations.are.helpful.and.necessary.for.me,
                                                         Prev_2 = For.me..it.is.very.important.to.carry.out.the.obligations.placed.on.me.,
                                                         Prom_2 = I.generally.solve.problems.creatively.,
                                                         Prev_3 = I.m.not.bothered.about.reviewing.or.checking.things.really.closely.,
                                                         Prom_3 = I.like.to.do.things.in.a.new.way.,
                                                         Prev_4 = I.always.try.to.make.my.work.as.accurate.and.error.free.as.possible.,
                                                         Prom_4 = I.like.trying.out.lots.of.different.things..and.am.often.successful.in.doing.so.,
                                                         Prom_5 = It.is.important.to.me.that.my.achievements.are.recognized.and.valued.by.other.people.,
                                                         Prev_5 = I.often.think.about.what.other.people.expect.of.me.)





raw_survey_data_2023 <- raw_survey_data_2023 %>% mutate(Country = if_else(Country == "", "I prefer not to say", Country))

raw_survey_data_2023$User_ID <- sub(" target=", "", raw_survey_data_2023$User_ID) #removed target= which was appearing at the end of some uuid's



raw_survey_data_2023 <- raw_survey_data_2023 %>%
  mutate_at(vars(starts_with("Prom_"), starts_with("Prev_")), list(~as.numeric(str_extract(., "\\d+")))) #turning the Reg Focus Responses to Numeral


raw_survey_data_2023 <- raw_survey_data_2023 %>%
  mutate(Other_Citi_Sci = ifelse(Other_Citi_Sci == "Yes", 1, 0)) # Converting Other_Citi_Sci to a binary variable


raw_survey_data_2023 <- raw_survey_data_2023 %>% 
  dplyr::select(Response.ID, Last.page, Language, Consent, User_ID, Age,
                Gender, Country, Participation_Date, Network, 
                Other_Citi_Sci, Security, Teaching, Self_Direction, Stimulation, Hedonism, 
                Achievement, Face, Conformity, Benevolence, Universalism_Social, 
                Universalism_Nature, Routine, Social_Expansion, Power, 
                Help_Science, Prom_1, Prom_2, Prom_3, 
                Prom_4, Prom_5,  Prev_1, Prev_2, Prev_3, Prev_4, Prev_5) %>% 
  mutate( Gender = as.factor(Gender),
          Country = as.factor(Country),
          Network = as.numeric(Network),
          Participation_Date = as.factor(Participation_Date))


## RAW 2024 --------------------------------------------------------------------

raw_survey_data_2024 <- read.csv("Raw_Survey_Results_2024.csv")

raw_survey_data_2024 <- raw_survey_data_2024 %>% rename( Language = Start.language,
                                                         Consent = I.GIVE.MY.CONSENT.to.participate.in.this.study.and.allow.the.use.of.data.generated.in.Mosquito.Alert.on.my.device.to.be.re.used.in.this.research.project..,
                                                         User_ID = user_UUID,
                                                         Age = How.old.are.you.,
                                                         Gender = What.is.your.gender.,
                                                         Country = What.is.the.country.you.currently.reside.in.,
                                                         Participation_Date = In.what.year.did.you.first.participate.in.Mosquito.Alert.,
                                                         Network = How.many.people.do.you.personally.know..acquaintances..friends..family.members.etc...who.are.participating.in.Mosquito.Alert..not.including.yourself..,
                                                         Other_Citi_Sci = Are.you.currently.engaged.in.other.citizen.science.projects.,
                                                         Self_Direction = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.am.interested.in.the.topic.of.this.project.,
                                                         Stimulation = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.challenge.myself.and.do.something.new.,
                                                         Hedonism = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.a.fun.activity.,
                                                         Achievement = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.an.opportunity.to.perform.better.than.others.,
                                                         Face = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.enhance.my.reputation.,
                                                         Security = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.live.in.safer.surroundings.,
                                                         Conformity = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....Other.people.I.know.are.participating..,
                                                         Benevolence = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.my.community.,
                                                         Universalism_Social = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.make.the.world.a.better.place.,
                                                         Universalism_Nature = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.protect.the.environment.,
                                                         Routine = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.is.part.of.my.routine.,
                                                         Social_Expansion = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.be.part.of.this.volunteers..community.,
                                                         Power =On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.gain.recognition.,
                                                         Help_Science = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.science.,
                                                         Teaching = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.use.it.to.teach.others.about.the.topic.,
                                                         Dislike = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.dislike.mosquitos..,
                                                         Env_Change = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.noticed.an.increase.or.change.in.mosquitos.in.my.surroundings.,
                                                         Prom_1 = I.prefer.to.work.without.instructions.from.others.,
                                                         Prev_1 = Rules.and.regulations.are.helpful.and.necessary.for.me,
                                                         Prev_2 = For.me..it.is.very.important.to.carry.out.the.obligations.placed.on.me.,
                                                         Prom_2 = I.generally.solve.problems.creatively.,
                                                         Prev_3 = I.m.not.bothered.about.reviewing.or.checking.things.really.closely.,
                                                         Prom_3 = I.like.to.do.things.in.a.new.way.,
                                                         Prev_4 = I.always.try.to.make.my.work.as.accurate.and.error.free.as.possible.,
                                                         Prom_4 = I.like.trying.out.lots.of.different.things..and.am.often.successful.in.doing.so.,
                                                         Prom_5 = It.is.important.to.me.that.my.achievements.are.recognized.and.valued.by.other.people.,
                                                         Prev_5 = I.often.think.about.what.other.people.expect.of.me.)





raw_survey_data_2024 <- raw_survey_data_2024 %>% mutate(Country = if_else(Country == "", "I prefer not to say", Country))

raw_survey_data_2024$User_ID <- sub(" target=", "", raw_survey_data_2024$User_ID) #removed target= which was appearing at the end of some uuid's



raw_survey_data_2024 <- raw_survey_data_2024 %>%
  mutate_at(vars(starts_with("Prom_"), starts_with("Prev_")), list(~as.numeric(str_extract(., "\\d+")))) #turning the Reg Focus Responses to Numeral


raw_survey_data_2024 <- raw_survey_data_2024 %>%
  mutate(Other_Citi_Sci = ifelse(Other_Citi_Sci == "Yes", 1, 0)) # Converting Other_Citi_Sci to a binary variable


raw_survey_data_2024 <- raw_survey_data_2024 %>% 
  dplyr::select(Response.ID, Last.page, Language, Consent, User_ID, Age,
                Gender, Country, Participation_Date, Network, 
                Other_Citi_Sci, Security, Teaching, Self_Direction, Stimulation, Hedonism, 
                Achievement, Face, Conformity, Benevolence, Universalism_Social, 
                Universalism_Nature, Routine, Social_Expansion, Power, 
                Help_Science, Prom_1, Prom_2, Prom_3, 
                Prom_4, Prom_5,  Prev_1, Prev_2, Prev_3, Prev_4, Prev_5) %>% 
  mutate( Gender = as.factor(Gender),
          Country = as.factor(Country),
          Network = as.numeric(Network),
          Participation_Date = as.factor(Participation_Date))


## Survey 2023 --------------------------------------------------------------------
survey_data_2023 <- raw_survey_data_2023 %>%
  filter(Consent == "Yes" & nzchar(User_ID) > 0)     #Last page = 5  removed

#removed the entries by users that have filled the survey twice, maintaining the results of their first attempt and deleting repeats.
survey_data_2023 <- survey_data_2023 %>%
  group_by(User_ID) %>%
  filter(row_number() == 1) %>%
  mutate(Complt_Survey = TRUE) %>% 
  ungroup()                                          

#cleaning unnecessary columns
survey_data_2023 <- survey_data_2023 %>%
  select(-Response.ID, -Last.page, -Language, -Consent, -Complt_Survey) 


#replacing NA's with Median (median imputation is less sensitive to outliers and max 2 missing for each column )
#survey_data_2023 <- survey_data_2023 %>%
# mutate(across(Security:Prev_5, ~replace(., is.na(.), median(., na.rm = TRUE))))


#creating an average individual Reg Focus
survey_data_2023 <- survey_data_2023 %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))


mean(survey_data_2023$Reg_Orientation, na.rm = TRUE) 


# Adding categorical version of Reg_Orientation
survey_data_2023 <- survey_data_2023 %>%
  mutate(Reg_Orientation_Cat = case_when(
    Reg_Orientation < -1 ~ "Prevention",
    Reg_Orientation >= -1 & Reg_Orientation <= 1 ~ "Neutral",
    Reg_Orientation > 1 ~ "Promotion",
    TRUE ~ as.character(NA)  # For NAs
  ))

table(survey_data_2023$Reg_Orientation_Cat)

survey_data_2023 <- survey_data_2023 %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))

survey_data_2023 <- survey_data_2023 %>%
  mutate(Promotion =  (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5),
         Prevention = (Prev_1+Prev_2+Prev_3+Prev_4+Prev_5))

#survey_data_2023 <- survey_data_2023 %>%
 # select(-Prom_1,-Prom_2,-Prom_3,-Prom_4,-Prom_5,-Prev_1,-Prev_2,-Prev_3,-Prev_4,-Prev_5)

survey_data_2023 <- survey_data_2023 %>% 
                      mutate(Year_Survey_Taken = 2023)  

## Survey 2024 --------------------------------------------------------------------



survey_data_2024 <- raw_survey_data_2024 %>%
  filter(Consent == "Yes" & nzchar(User_ID) > 0)     #385  #338  #Last page = 5  removed

#removed the entries by users that have filled the survey twice, maintaining the results of their first attempt and deleting repeats.
survey_data_2024 <- survey_data_2024 %>%
  group_by(User_ID) %>%
  filter(row_number() == 1) %>%
  mutate(Complt_Survey = TRUE) %>% 
  ungroup()                                          #364  #327

#cleaning unnecessary columns
survey_data_2024 <- survey_data_2024 %>%
  select(-Response.ID, -Last.page, -Language, -Consent, -Complt_Survey) 


#replacing NA's with Median (median imputation is less sensitive to outliers and max 2 missing for each column )
#survey_data_2024 <- survey_data_2024 %>%
 # mutate(across(Security:Prev_5, ~replace(., is.na(.), median(., na.rm = TRUE))))



#creating an average individual Reg Focus
survey_data_2024 <- survey_data_2024 %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))


mean(survey_data_2024$Reg_Orientation, na.rm = TRUE) 

#Last Page 5  Imputed -2.08
#Last Page 5  NOT Imputed -2.08
#not last page not imputed -2.07


# Adding categorical version of Reg_Orientation
survey_data_2024 <- survey_data_2024 %>%
  mutate(Reg_Orientation_Cat = case_when(
    Reg_Orientation < -1 ~ "Prevention",
    Reg_Orientation >= -1 & Reg_Orientation <= 1 ~ "Neutral",
    Reg_Orientation > 1 ~ "Promotion",
    TRUE ~ as.character(NA)  # For NAs
  ))

table(survey_data_2024$Reg_Orientation_Cat)
# Last Page 5  Imputed  N 66       PRE 186       PRO  75
# Last Page 5  NOT Imputed  N 64       PRE 184       PRO  75
# NOT Last Page 5  NOT Imputed  N 64       PRE 183       PRO  75  CHOSEN 

survey_data_2024 <- survey_data_2024 %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))

survey_data_2024 <- survey_data_2024 %>%
  mutate(Promotion =  (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5),
         Prevention = (Prev_1+Prev_2+Prev_3+Prev_4+Prev_5))

#survey_data_2024 <- survey_data_2024 %>%
#  select(-Prom_1,-Prom_2,-Prom_3,-Prom_4,-Prom_5,-Prev_1,-Prev_2,-Prev_3,-Prev_4,-Prev_5)

survey_data_2024 <- survey_data_2024 %>% 
  mutate(Year_Survey_Taken = 2024)  



## Combined Survey Data ----------------------------------------------------

survey_data <- rbind(survey_data_2023, survey_data_2024)


## User Data --------------------------------------------------------------------
raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)


# AGUSTI MADE A MISTAKE AND SWITCHED COLUMN NAMES THIS TIME HE SENT THE REPORT
# THE CODE BELOW IS ONLU FOR THIS TIME NOT TO BE USED FOR OTHER VERSIONS OF THE RAW DATA

raw_user_data <- raw_user_data %>%
  rename(temp_name = registration_time)

raw_user_data <- raw_user_data %>%
  rename(registration_time = n, 
         n = temp_name)

### again do not use what is above for other versions of the data


#since the beginning of the project, there have been 464,408 registration/downloads
raw_user_data <- raw_user_data %>% 
  rename( User_ID = user_UUID,
          Registered_Participation_Date = registration_time,
          Registered_Total_Reports = n) %>% 
  mutate(Registered_Participation_Date = as.POSIXct(Registered_Participation_Date, format = "%Y-%m-%d %H:%M:%S"),
         Registered_Total_Reports = as.integer(Registered_Total_Reports)) %>%
  replace_na(list(Registered_Total_Reports = 0)) 


# Count of unique users that have submitted at least 1 report
nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1))

#  85550 of the registered users have  filled a report until end of 2024 up from 70,799 at the end of 2023


nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
                Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2023-12-31")))

# 55213 registered users from Update cutoff until the end of 2023


nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
                Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2024-12-31")))

# now its 70,525 until the end of 2024, meaning 15312 new users registered in 2024 that have filled a report in 2024





user_data <- raw_user_data %>%
  filter(User_ID %in% survey_data$User_ID)




user_data <- user_data %>%
  mutate(
    accuracy = ((hits_adult + hits_site + maybes_adult) / (total_adult + total_site)) * 100)


## Reports Data ------------------------------------------------------------

repor <- read.csv(file="full_reports_table.csv", header = TRUE)

repor <- repor %>% 
  mutate(creation_time = as.POSIXct(creation_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(creation_time >= as.POSIXct("2020-10-02") & creation_time <= as.POSIXct("2023-12-31"))

rm(repor)

#173,147 reports filled from update til end of 2024




reports_data <- read.csv(file="full_reports_table.csv", header = TRUE)%>% 
  filter(user_id %in% survey_data$User_ID) %>% 
  select(user_id, creation_time) %>%
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d")) %>% 
  rename(User_ID = user_id,
         Report_Date = creation_time) %>% 
group_by(User_ID) %>%
mutate(Active_Duration = as.integer(difftime(max(Report_Date, na.rm = TRUE), min(Report_Date, na.rm = TRUE), units = "days"))) %>% 
  slice(1L) %>% 
  ungroup() %>%
  select(-Report_Date)



## Creating main data set --------------------------------------------------


data  <- full_join(survey_data , user_data, by = "User_ID")

data  <- full_join(data , reports_data, by = "User_ID")

data <- data %>% 
  mutate(Network = as.factor(Network),
         Other_Citi_Sci = as.factor(Other_Citi_Sci))

## Demographic Analysis ----------------------------------



summary(data)

## Normality (Shapiro-Wilk) ------------------------------------------------

shapiro_test <- shapiro.test(data$Reg_Orientation)
print(shapiro_test)

rm(shapiro_test)
## Reg Focus Mean and T.test/ Wilcox.test & Plot -----------------------------------------------

wilcox.test(data$Reg_Orientation, mu = 0, na.rm = TRUE)
t.test(data$Reg_Orientation, mu = 0, na.rm = TRUE)

ggplot(data, aes(x = Reg_Orientation)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "#b4464b") +
  geom_vline(xintercept = -1.799625, color = "#82b446", linetype = "dashed") +                                    # Make sure to change numbers based on means and confidence intervals
  geom_vline(xintercept = -2.238212, color = "#b47846", linetype = "dashed") +
  geom_vline(xintercept = -1.361039, color = "#b47846", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  annotate("text", x = -1.799625, y = Inf, label = "Mean (-1.79)", vjust = 0.8, color = "#82b446", size = 3) +
  annotate("text", x = -2.238212, y = Inf, label = "Lower CI (-2.24)", vjust = 2,hjust = 1.2, color = "#b47846", size = 3) +
  annotate("text", x = -1.361039, y = Inf, label = "Upper CI (-1.36)", vjust = 2, hjust = -0.3, color = "#b47846", size = 3) +
  labs(title = "Distribution of Dispositional Reg-Focus Scores",
       x = "Reg Orientation Value",
       y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 7))

## Reg Focus Indv. -----------------------------------------------


oriented_data <- subset(data, Reg_Orientation_Cat %in% c('Prevention', 'Promotion'))
oriented_counts <- table(oriented_data$Reg_Orientation_Cat)
oriented_counts <- oriented_counts[names(oriented_counts) != "Neutral"]
oriented_counts  



#285  Prevention to 125 Promotion

# Since we expect the counts to be equal, the expected frequency for each is half of the total count
total_expected_count <- sum(oriented_counts)
expected_counts <- rep(total_expected_count / 2, 2)

chi_squared_test <- chisq.test(oriented_counts, p = rep(1/2, 2))
print(chi_squared_test) 


rm(chi_squared_test, oriented_data, oriented_counts, total_expected_count, expected_counts)


## Relation Between Dispositional Goal Orientations and User Motivation --------


### Creating a Table for top Motivators per Group ---------------------------


Motivator_Means_By_Reg_Focus <- data %>%
  filter(!is.na(Reg_Orientation_Cat))  %>%
  group_by(Reg_Orientation_Cat) %>%
  summarise(across(Security:Help_Science, mean, na.rm = TRUE))%>%
  pivot_longer(-Reg_Orientation_Cat, names_to = "Motivator", values_to = "Mean") %>%
  pivot_wider(names_from = Reg_Orientation_Cat, values_from = Mean) 

Motivator_SDs_By_Reg_Focus <- data %>%
  filter(!is.na(Reg_Orientation_Cat)) %>%
  group_by(Reg_Orientation_Cat) %>%
  summarise(across(Security:Help_Science, sd, na.rm = TRUE)) %>%
  pivot_longer(-Reg_Orientation_Cat, names_to = "Motivator", values_to = "SD") %>%
  pivot_wider(names_from = Reg_Orientation_Cat, values_from = SD) 



table_data <- Motivator_Means_By_Reg_Focus %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(across(where(is.numeric), ~paste0(.x, " (", 
                                           sprintf("%.2f", Motivator_SDs_By_Reg_Focus[[cur_column()]]), ")")))


style_values <- function(data, column) {
  sorted_values <- sort(data[[column]], decreasing = TRUE)
  top_values <- sorted_values[1:5]  # Highest values
  bottom_values <- sorted_values[(length(sorted_values) - 4):length(sorted_values)]  # Lowest values (to make it the 6 lowest values replace 4 by 5)
  
  data[[column]] <- ifelse(data[[column]] %in% top_values,
                           cell_spec(data[[column]], "html", bold = TRUE),
                           data[[column]])
  data[[column]] <- ifelse(data[[column]] %in% bottom_values,
                           cell_spec(data[[column]], "html", italic = TRUE),
                           data[[column]])
  data
}



for (col in names(table_data)[-1]) {
  table_data <- style_values(table_data, col)
}

# Create a table with kable and apply additional styling
kable(table_data, "html", escape = FALSE, caption = "Table 2<br>Mean scores (SD) of motivational categories for participation for the three
      distinct regulatory focus groups. Each sample’s five top-rated motivational categories are in bold; the bottom four are in grey italic.") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE) 

rm(Motivator_SDs_By_Reg_Focus, col, style_values, table_data)




### ANOVA to check if any motivator is better than another ------------------


motivator_columns <- c("Security","Teaching", "Self_Direction","Stimulation", "Hedonism", "Achievement","Face","Conformity","Benevolence",
                       "Universalism_Social" ,"Universalism_Nature" ,"Routine",            
                       "Social_Expansion" ,"Power","Help_Science")


for (motivator in motivator_columns) {
  formula <- as.formula(paste(motivator, "~ Reg_Orientation_Cat"))
  anova_result <- aov(formula, data = survey_data)
  cat("\nANOVA results for", motivator, ":\n")
  print(summary(anova_result))
  cat("\n")
}

rm(formula, motivator, motivator_columns, shapiro_test, anova_result)


### Tukey --------

#Power
anova_result <- aov(Power ~ Reg_Orientation_Cat, data = data)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result) 
# Significant difference between Pre and Pre

#Benevolence
anova_result <- aov(Benevolence ~ Reg_Orientation_Cat, data = data)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result) 

#Achievement 
anova_result <- aov(Achievement  ~ Reg_Orientation_Cat, data = data)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result) 

## Correlation Analysis ----------------------------------------------------

predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism",
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social", 
"Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science", "Promotion", "Prevention")

outcomes <-  c("Active_Duration", "accuracy", "accuracy2", "accuracy3", "Registered_Total_Reports", "score")


# Calculate Spearman's correlation for motivators and engagement metrics
correlation_motivators <- cor(data[, c(predictors, outcomes)], 
                              method = "spearman",
                              use = "pairwise.complete.obs")

cor(data$Prevention, data$Active_Duration, method = "spearman",  use = "pairwise.complete.obs")


# Extract the relevant part of the correlation matrix for motivators
correlation_motivators_results <- correlation_motivators[length(predictors) + (1:length(outcomes)), 1:length(predictors)]
print(correlation_motivators_results)


Correlation <- as.data.frame(t(correlation_motivators_results))
Correlation <- round(Correlation, 2)





#make this into a graph 


melted_corr_matrix <- melt(correlation_motivators_results)

ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="Correlation")


# Go with Active Duration, Accuracy as the outcome variable

rm(predictors, outcomes, correlation_motivators, correlation_motivators_results, Correlation, melted_corr_matrix)


## Regression --------------------------------------------------------------

### Define Predictors and Function for Linear Regression --------------------


predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism",
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social",
                "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science",
                "Promotion", "Prevention")



# Define a function to run simple linear regressions for each predictor
run_simple_regressions <- function(data, dv, predictors) {
  results <- tibble(                                                        # Initialize an empty results table
    Predictor = character(),
    Beta = numeric(),
    SE = numeric(),
    t_value = numeric(),
    p_value = numeric()
  )
  for (pred in predictors) {                                               # Loop through each predictor
    formula <- as.formula(paste(dv, "~", pred))
    model <- lm(formula, data = data)
    tidy_model <- tidy(model)[2, ]
    results <- results %>%                                                # Append results to the table
      add_row(
        Predictor = pred,
        Beta = as.numeric(tidy_model$estimate),
        SE = as.numeric(tidy_model$std.error),
        t_value = as.numeric(tidy_model$statistic),
        p_value = as.numeric(tidy_model$p.value)
      )
  }
  summary_table <- results %>%                                             # Create a summary table with absolute Beta and p-values
    mutate(Abs_Beta = abs(Beta)) %>%
    arrange(desc(Abs_Beta)) %>%
    mutate(Beta_Rank = ifelse(row_number() <= 5, "**bold**", "")) %>%
    arrange(p_value) %>%
    mutate(p_value_Rank = ifelse(row_number() <= 5, "**bold**", ""))
  return(list(                                                             # Return both raw results and summary in a list
    results = results,
    summary_table = summary_table
  ))
}

  
 


 
### Active Duration -------------------------------------------------------- 

regression_results <- run_simple_regressions(
  data = data, 
  dv = "Active_Duration", 
  predictors = predictors
)

print(regression_results$results)

print(regression_results$summary_table)

#both promotion and prevention had the only significant p-values showing the most reliable 
#observed relationship with active participation duration

### Accuracy ---------------------------------------------------------------

regression_results <- run_simple_regressions(
  data = data, 
  dv = "accuracy", 
  predictors = predictors
)

print(regression_results$results)

print(regression_results$summary_table)


# non of the variables seemed to be significant except for hedonism, prevention in 5th place
