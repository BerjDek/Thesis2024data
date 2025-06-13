# Paper 3 Complete -----------
## Load necessary package -------

library(tidyverse)
library(ggpubr)
library(lme4)




## Load necessary Data sets (skip till ) -------
reports_data <- read.csv("nudge_reports_data.csv")
reports_data_all <- read.csv("nudge_reports_data_all.csv")

survey_data_2023 <- read.csv("nudge_survey_data_2023.csv")
survey_data_2024 <- read.csv("nudge_survey_data_2024.csv")
message_data_2023 <- read.csv("CleanMessageData_2023.csv")
message_data_2024 <- read.csv("CleanMessageData_2024.csv")
Data <- read.csv("nudge_data_clean.csv")
reg_data <- read.csv("reg_data.csv")
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



#creating an average individual Reg Focus
survey_data_2023 <- survey_data_2023 %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))


# Adding categorical version of Reg_Orientation
survey_data_2023 <- survey_data_2023 %>%
  mutate(Reg_Orientation_Cat = case_when(
    Reg_Orientation < -1 ~ "Prevention",
    Reg_Orientation >= -1 & Reg_Orientation <= 1 ~ "Neutral",
    Reg_Orientation > 1 ~ "Promotion",
    TRUE ~ as.character(NA)  # For NAs
  ))



survey_data_2023 <- survey_data_2023 %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))

survey_data_2023 <- survey_data_2023 %>%
  mutate(Promotion =  (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5),
         Prevention = (Prev_1+Prev_2+Prev_3+Prev_4+Prev_5))



survey_data_2023 <- survey_data_2023 %>% 
  mutate(Year_Survey_Taken = 2023)  


survey_data_2023 <- survey_data_2023 %>%
  select( -Network,- Other_Citi_Sci, - Security, - Teaching, - Self_Direction, - Stimulation, 
          - Hedonism, - Achievement, - Face, - Conformity, - Benevolence, - Universalism_Social, 
          - Universalism_Nature, - Routine, - Social_Expansion, - Power, - Help_Science) 



write.csv(survey_data_2023, "nudge_survey_data_2023.csv", row.names = FALSE)

rm(raw_survey_data_2023)

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


#creating an average individual Reg Focus
survey_data_2024 <- survey_data_2024 %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))



# Adding categorical version of Reg_Orientation
survey_data_2024 <- survey_data_2024 %>%
  mutate(Reg_Orientation_Cat = case_when(
    Reg_Orientation < -1 ~ "Prevention",
    Reg_Orientation >= -1 & Reg_Orientation <= 1 ~ "Neutral",
    Reg_Orientation > 1 ~ "Promotion",
    TRUE ~ as.character(NA)  # For NAs
  ))



survey_data_2024 <- survey_data_2024 %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))

survey_data_2024 <- survey_data_2024 %>%
  mutate(Promotion =  (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5),
         Prevention = (Prev_1+Prev_2+Prev_3+Prev_4+Prev_5))



survey_data_2024 <- survey_data_2024 %>% 
  mutate(Year_Survey_Taken = 2024)  


survey_data_2024 <- survey_data_2024 %>%
  select(-Network, - Other_Citi_Sci, - Security, - Teaching, - Self_Direction, - Stimulation, 
         - Hedonism, - Achievement, - Face, - Conformity, - Benevolence, - Universalism_Social, 
         - Universalism_Nature, - Routine, - Social_Expansion, - Power, - Help_Science) 


#Note: After creating the message data, 2 users apparently joined very late and did not receive the treatment, so their entries are removed

survey_data_2024 <- survey_data_2024 %>%
  filter(!(User_ID %in% c("6c7e2143-1c46-42df-9d65-dd78b6fce431", 
                          "4272eff3-4fef-4235-a34a-2254c7e0143d")))

write.csv(survey_data_2024, "nudge_survey_data_2024.csv", row.names = FALSE)

rm(raw_survey_data_2024)





## Combined Survey Data ----------------------------------------------------

survey_data <- rbind(survey_data_2023, survey_data_2024)
write.csv(survey_data, "nudge_survey_data.csv", row.names = FALSE)

## Messages from 2023 ------------------------------------------------------


raw_message_data_2023 <- read.csv(file="all_messages_2023.csv", header = TRUE)
#separate notification column to type language and msg_nmbr
raw_message_data_2023 <- raw_message_data_2023 %>% separate(notification_label, c('type','language','msg_nmbr'))
#change the message date to simple date
raw_message_data_2023 <- raw_message_data_2023 %>% 
  rename(User_ID = user_uuid, Msg_Date = date_sent, Msg_Lang = language, Msg_Type = type) %>% 
  mutate(Msg_Nmbr = as.integer(msg_nmbr),
         Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S"),
         Year = format(Msg_Date, "%Y"))%>%
  group_by(User_ID) %>%
  mutate(Repeat_User = n_distinct(Year) > 1)%>%
  mutate(Msg_Type = tools::toTitleCase(Msg_Type)) %>% 
  mutate(Msg_Type = recode(Msg_Type, "Prevention" = "Vigilant", "Promotion" = "Eager"))%>% 
  ungroup() 





message_data_2023 <- raw_message_data_2023 %>%
  filter(year(Msg_Date) == 2023) %>% 
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(Msg_Type),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(format(min(Msg_Date), "%Y-%m-%d")),
    Last_Msg_Date = as.POSIXct(format(max(Msg_Date), "%Y-%m-%d")),
    Nmbr_Msgs_Sent = n(),
    Nmbr_Msgs_Seen = sum(read_notification == "t"),
    Msg_Duration_Days = as.integer(Last_Msg_Date - First_Msg_Date, units = "days")
  ) %>%
  ungroup() %>%
  mutate(Msg_Type = as.factor(Msg_Type))




#creating Messaging group based on date of first message received.
message_data_2023 <- message_data_2023 %>%
  mutate(
    Message_Group = case_when(
      month(First_Msg_Date) == 6 & day(First_Msg_Date) <= 14 ~ "A-June1",
      month(First_Msg_Date) == 6 & day(First_Msg_Date) > 14 ~ "B-June2",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) <= 14 ~ "C-July1",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) > 14 ~ "D-July2",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) <= 14 ~ "E-Aug1",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) > 14 ~ "F-Aug2",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) <= 14 ~ "G-Sept1",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) > 14 ~ "H-Sept2",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) <= 14 ~ "I-Oct1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "J-Oct2",
      month(First_Msg_Date) == 11 & day(First_Msg_Date) <= 14 ~ "K-Nov1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "L-Nov2",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(Message_Group = as.factor(Message_Group)) 


write.csv(message_data_2023, "CleanMessageData_2023.csv", row.names = FALSE)



## Messages from2024 --------------------------------------------------------------------

#load message Data
raw_message_data_2024 <- read.csv(file="all_messages_2024.csv", header = TRUE)

raw_message_data_2024 <- raw_message_data_2024 %>% separate(notification_label, c('type','language','msg_nmbr'))

raw_message_data_2024 <- raw_message_data_2024 %>% 
  rename(User_ID = user_uuid, Msg_Date = date_sent, Msg_Lang = language) %>% 
  mutate(Msg_Nmbr = as.integer(msg_nmbr),
         Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S"),
         Year = format(Msg_Date, "%Y")) %>%
  group_by(User_ID) %>%
  mutate(Repeat_User = n_distinct(Year) > 1)%>%
  mutate(type = tools::toTitleCase(type)) %>% 
  mutate(type = recode(type, "Prevention" = "Vigilant", "Promotion" = "Eager"))%>% 
  ungroup() 




message_data_2024 <- raw_message_data_2024 %>%
  filter(!is.na(Msg_Nmbr)) %>%                       #this column changed from 2023 code, since instead of haveing messages from multile years, we have a poll for those in St.Luis that need to be exluded
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(type),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(format(min(Msg_Date), "%Y-%m-%d")),
    Last_Msg_Date = as.POSIXct(format(max(Msg_Date), "%Y-%m-%d")),
    Nmbr_Msgs_Sent = n(),
    Nmbr_Msgs_Seen = sum(read == "t"),                                             # changed from 2023 column read_notifications to read
    Msg_Duration_Days = as.integer(Last_Msg_Date - First_Msg_Date, units = "days")
  ) %>%
  ungroup() %>%
  mutate(Msg_Type = as.factor(Msg_Type))




#creating Messaging group based on date of first message received.
message_data_2024 <- message_data_2024 %>%
  mutate(
    Message_Group = case_when(
      month(First_Msg_Date) == 5 ~ "A-June1",                                #added since there is one message at end of May
      month(First_Msg_Date) == 6 & day(First_Msg_Date) <= 14 ~ "A-June1",
      month(First_Msg_Date) == 6 & day(First_Msg_Date) > 14 ~ "B-June2",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) <= 14 ~ "C-July1",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) > 14 ~ "D-July2",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) <= 14 ~ "E-Aug1",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) > 14 ~ "F-Aug2",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) <= 14 ~ "G-Sept1",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) > 14 ~ "H-Sept2",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) <= 14 ~ "I-Oct1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "J-Oct2",
      month(First_Msg_Date) == 11 & day(First_Msg_Date) <= 14 ~ "K-Nov1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "L-Nov2",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(Message_Group = as.factor(Message_Group))



write.csv(message_data_2024, "CleanMessageData_2024.csv", row.names = FALSE)





## Combined Message Data ------------------------------------------------------------
message_data <- rbind(message_data_2023, message_data_2024)
write.csv(message_data_2024, "CleanMessageData_2024.csv", row.names = FALSE)

## User Data ------------------------------------------------        
raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)


raw_user_data <- raw_user_data %>%
  rename(temp_name = registration_time)

raw_user_data <- raw_user_data %>%
  rename(Registered_Participation_Date = n, 
         n = temp_name)


raw_user_data <- raw_user_data %>% 
  rename( User_ID = user_UUID) %>% 
  mutate(Registered_Participation_Date = as.Date(Registered_Participation_Date)) %>% 
  select(User_ID, Registered_Participation_Date)

user_data <- raw_user_data %>%
  filter(User_ID %in% survey_data$User_ID)


## Reports Data ------------------------------------------------------------

raw_reports_data <-  read.csv(file="full_reports_table.csv", header = TRUE)


reports_data <- read.csv(file="full_reports_table.csv", header = TRUE)%>% 
  filter(user_id %in% survey_data$User_ID) %>% 
  select(user_id, creation_time, type) %>%
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d")) %>% 
  rename(User_ID = user_id,
         Report_Date = creation_time,
         Report_Type = type) %>%
  left_join(user_data, by = "User_ID") 



write.csv(reports_data, "nudge_reports_data.csv", row.names = FALSE)




reports_data_all <-  read.csv(file="full_reports_table.csv", header = TRUE) %>% 
  filter(!user_id %in% survey_data$User_ID) %>% 
  select(user_id, creation_time) %>%  
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d")) %>% 
  rename(User_ID = user_id,
         Report_Date = creation_time) %>%   
  group_by(User_ID) %>%  
  mutate(Total_Rprts_Filled = n(),
         Rprts_Filled_2024 = sum(Report_Date >= "2024-01-01" & Report_Date <= "2024-12-31"),  
         Rprts_Filled_2023 = sum(Report_Date >= "2023-01-01" & Report_Date <= "2023-12-31"),
         Rprts_Filled_2022 = sum(Report_Date >= "2022-01-01" & Report_Date <= "2022-12-31"),
         Rprts_Filled_2021 = sum(Report_Date >= "2021-01-01" & Report_Date <= "2021-12-31"),
         Season_Rprts_Filled_2024 = sum(Report_Date >= "2024-05-01" & Report_Date <= "2024-10-30"),
         Season_Rprts_Filled_2023 = sum(Report_Date >= "2023-05-01" & Report_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Report_Date >= "2022-05-01" & Report_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Report_Date >= "2021-05-01" & Report_Date <= "2021-10-30")) %>% 
  slice(1L) %>% 
  ungroup() 




reports_data_all <- reports_data_all %>%
  select(-Report_Date) %>%
  left_join(raw_user_data, by = "User_ID")



write.csv(reports_data_all, "nudge_reports_data_all.csv", row.names = FALSE)

rm(raw_user_data)
rm(user_data)


## Creating Main ------------------------------------------------ 

Data <- full_join(survey_data, message_data, by = "User_ID")

Data  <- full_join(Data , reports_data, by = "User_ID")


Data <- Data %>%
  group_by(User_ID) %>%
  mutate(Total_Rprts_Filled = n(),
         Season_Rprts_Filled_2024 = sum(Report_Date >= "2024-05-01" & Report_Date <= "2024-10-30"),
         Season_Rprts_Filled_2023 = sum(Report_Date >= "2023-05-01" & Report_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Report_Date >= "2022-05-01" & Report_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Report_Date >= "2021-05-01" & Report_Date <= "2021-10-30"),
         Rprts_During_Msging = sum(Report_Date >= First_Msg_Date & Report_Date <= Last_Msg_Date, na.rm = TRUE),
         Rprts_Before_Msging = sum(Report_Date >= (First_Msg_Date - days(Msg_Duration_Days)) & Report_Date < First_Msg_Date, na.rm = TRUE),
         Rprts_After_Msging = sum(Report_Date > Last_Msg_Date & Report_Date <= (Last_Msg_Date + days(Msg_Duration_Days)), na.rm = TRUE)) %>% 
  slice(1L) %>%
  ungroup() %>% 
  select(- Report_Date )


write.csv(Data, "nudge_data_clean.csv", row.names = FALSE)

## Creating Regression data set-----


### 2023 ----------

# Creating date sequence for 2023 
dates_2023  <- seq.Date(as.Date("2023-05-01"), as.Date("2023-10-31"), by = "day")



# Filtering Message data to keep only those that were part of the survey
filtered_2023_message_data <- raw_message_data_2023 %>%
  filter(User_ID %in% survey_data_2023$User_ID) %>%
  mutate(Date = as.Date(Msg_Date)) %>%  
  filter(format(Date, "%Y") == "2023") %>% 
  select(-Msg_Date)


# Creates Data frame from survey data set, containing every combination of UUID and Date 
reg_msg_data_2023 <- expand.grid(
  User_ID = unique(survey_data_2023$User_ID),
  Date = dates_2023
)




reg_msg_data_2023 <- reg_msg_data_2023 %>%
  left_join(filtered_2023_message_data, by = c("User_ID", "Date"))


reg_msg_data_2023 <- reg_msg_data_2023 %>%
  mutate(
    Msg_Received = ifelse(!is.na(Msg_Type), 1, 0),  # 1 if message exists, otherwise 0
    Msg_Type = ifelse(!is.na(Msg_Type), Msg_Type, "None"),  # Message type or "None"
    Msg_Seen = ifelse(read_notification == "t", 1, 0)  # 1 if read, 0 otherwise
  ) %>%
  select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen)




#  Processing Report Data for 2023

filtered_2023_reports <- reports_data %>%    
  filter(User_ID %in% survey_data_2023$User_ID)

# Aggregating Reports by Date, User, and Report Type
aggregated_2023_reports <- filtered_2023_reports %>%
  filter(Report_Date >= as.Date("2023-05-01") & Report_Date <= as.Date("2023-10-31")) %>%
  group_by(User_ID, Report_Date, Report_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Report_Type, values_from = count, values_fill = list(count = 0)) %>%
  rename(Date = Report_Date)


# Creating a Regular Grid for Reports and Merging
reg_rprt_data_2023 <- expand.grid(
  User_ID = unique(survey_data_2023$User_ID),
  Date = dates_2023
)

reg_rprt_data_2023 <- reg_rprt_data_2023 %>%
  left_join(aggregated_2023_reports, by = c("User_ID", "Date")) %>%
  mutate(
    adult = coalesce(adult, 0),  # Fill missing values with 0
    bite = coalesce(bite, 0),
    site = coalesce(site, 0),
    total_reports = adult + bite + site,  # Sum of all report types
    Report = ifelse(total_reports > 0, 1, 0)  # Binary flag for report sent
  ) %>%
  select(User_ID, Date, adult, bite, site, total_reports, Report)  # Keep required columns only


# Merging Message and Report Data 


reg_data_2023 <- full_join(reg_msg_data_2023, reg_rprt_data_2023, by = c("User_ID", "Date"))


# Adding Survey Information and Filtering 

reg_data_2023 <- reg_data_2023 %>% 
  left_join(survey_data_2023 %>% dplyr::select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))


# Creating the Orientation Nudge Agreement Variable and Final Transformations 
reg_data_2023 <- reg_data_2023 %>%
  mutate(Orientation_Nudge_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Eager" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Vigilant" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Msg_Type = as.factor(Msg_Type),
         Orientation_Nudge_Agreement = as.factor(Orientation_Nudge_Agreement),
         Msg_Received = as.factor(Msg_Received),
         User_ID  = as.factor(User_ID),
         Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2023-05-01")) 


#   creating a column for centered day of intervention divided to month


reg_data_2023$Seasonality <- (yday(reg_data_2023$Date)- 211)/7

reg_data_2023 <- reg_data_2023 %>%
  left_join(message_data_2023 %>% select(User_ID, First_Msg_Date), by = "User_ID")

reg_data_2023$First_Msg_Date <- as.Date(reg_data_2023$First_Msg_Date)


reg_data_2023$Start_of_Intervention <- (pmax(as.numeric(reg_data_2023$Date - reg_data_2023$First_Msg_Date), 0))/7


reg_data_2023 <- reg_data_2023 %>% rename(Nudge =  Msg_Received,
                                          Nudge_Type = Msg_Type )


summary(reg_data_2023)


write.csv(reg_data_2023, "reg_data_2023.csv", row.names = FALSE)


rm(dates_2023, filtered_2023_message_data, reg_msg_data_2023,filtered_2023_reports,aggregated_2023_reports, reg_rprt_data_2023 )

### 2024 ----------

# Creating date sequence for 2024 
dates_2024  <- seq.Date(as.Date("2024-05-01"), as.Date("2024-10-31"), by = "day")

# Preparing data on messaging/nudging 

# Filtering Message data to keep only those that were part of the survey 
filtered_2024_message_data <- raw_message_data_2024 %>%
  filter(User_ID %in% survey_data_2024$User_ID) %>%
  mutate(Date = as.Date(Msg_Date)) %>%  
  filter(format(Date, "%Y") == "2024") %>% 
  select(-Msg_Date) %>% 
  rename(read_notification = read)



# Creates Data frame from survey dataset, containing every combination of UUID and Date 
reg_msg_data_2024 <- expand.grid(
  User_ID = unique(survey_data_2024$User_ID),
  Date = dates_2024
)

reg_msg_data_2024 <- reg_msg_data_2024 %>%
  left_join(filtered_2024_message_data, by = c("User_ID", "Date"))

reg_msg_data_2024 <- reg_msg_data_2024 %>%
  mutate(
    Msg_Received = ifelse(!is.na(type), 1, 0),  # 1 if message exists, otherwise 0
    Msg_Type = ifelse(!is.na(type), type, "None"),  # Message type or "None"
    Msg_Seen = ifelse(read_notification == "t", 1, 0)  # 1 if read, 0 otherwise
  ) %>%
  select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen)

# Processing Report Data 

filtered_2024_reports <- reports_data %>%
  filter(User_ID %in% survey_data_2024$User_ID)

# Aggregating Reports by Date, User, and Report Type
aggregated_2024_reports <- filtered_2024_reports %>%
  filter(Report_Date >= as.Date("2024-05-01") & Report_Date <= as.Date("2024-10-31")) %>%
  group_by(User_ID, Report_Date, Report_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Report_Type, values_from = count, values_fill = list(count = 0)) %>%
  rename(Date = Report_Date)

# Creating a Regular Grid for Reports and Merging
reg_rprt_data_2024 <- expand.grid(
  User_ID = unique(survey_data_2024$User_ID),
  Date = dates_2024
)


reg_rprt_data_2024$Date <-as.Date(reg_rprt_data_2024$Date)
aggregated_2024_reports$Date <-as.Date(aggregated_2024_reports$Date)


reg_rprt_data_2024 <- reg_rprt_data_2024 %>%
  left_join(aggregated_2024_reports, by = c("User_ID", "Date")) %>%
  mutate(
    adult = coalesce(adult, 0),  # Fill missing values with 0
    bite = coalesce(bite, 0),
    site = coalesce(site, 0),
    total_reports = adult + bite + site,  # Sum of all report types
    Report = ifelse(total_reports > 0, 1, 0)  # Binary flag for report sent
  ) %>%
  select(User_ID, Date, adult, bite, site, total_reports, Report)  # Keep required columns only




# Merging Message and Report Data 

reg_data_2024 <- full_join(reg_msg_data_2024, reg_rprt_data_2024, by = c("User_ID", "Date"))

# Adding Survey Information and Filtering

reg_data_2024 <- reg_data_2024 %>% 
  left_join(survey_data_2024 %>% dplyr::select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))

# Creating the Orientation Nudge Agreement Variable and Final Transformations  
reg_data_2024 <- reg_data_2024 %>%
  mutate(Orientation_Nudge_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Eager" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Vigilant" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Msg_Type = as.factor(Msg_Type),
         Orientation_Nudge_Agreement = as.factor(Orientation_Nudge_Agreement),
         Msg_Received = as.factor(Msg_Received),
         User_ID  = as.factor(User_ID),
         Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2024-05-01")) 



# creating a column for centered day of intervention divided to month 

reg_data_2024$Seasonality <- (yday(reg_data_2024$Date)- 211)/7

reg_data_2024 <- reg_data_2024 %>%
  left_join(message_data_2024 %>% select(User_ID, First_Msg_Date), by = "User_ID")

reg_data_2024$First_Msg_Date <- as.Date(reg_data_2024$First_Msg_Date)


reg_data_2024$Start_of_Intervention <- (pmax(as.numeric(reg_data_2024$Date - reg_data_2024$First_Msg_Date), 0))/7



reg_data_2024 <- reg_data_2024 %>% rename( Nudge =  Msg_Received,
                                           Nudge_Type = Msg_Type )

write.csv(reg_data_2024, "reg_data_2024.csv", row.names = FALSE)


rm(dates_2024, filtered_2024_message_data, reg_msg_data_2024,filtered_2024_reports,aggregated_2024_reports, reg_rprt_data_2024 )

### Joint Regression Data ---------------------------------------------------

reg_data <- bind_rows(reg_data_2023, reg_data_2024)
write.csv(reg_data, "reg_data.csv", row.names = FALSE)

## Deleting Unnecessary Objects --------------------------------------------


rm(raw_message_data_2023,raw_message_data_2024)



## Demographic Variables ------------------------------------------------ 

Data <- Data %>%
  mutate(
    Gender = as.factor(Gender),
    Country = as.factor(Country),
    Participation_Date = as.factor(Participation_Date),
    Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat),
    Year_Survey_Taken = as.factor(Year_Survey_Taken),
    Msg_Type = as.factor(Msg_Type),
    Msg_Lang = as.factor(Msg_Lang),
    Message_Group = as.factor(Message_Group),
    Registered_Participation_Date = as.POSIXct(Registered_Participation_Date, format = "%Y"),
    Registered_Participation_Year = as.factor(Registered_Participation_Date),
    First_Msg_Date = as.POSIXct(First_Msg_Date, format = "%Y-%m-%d"),
    Last_Msg_Date = as.POSIXct(Last_Msg_Date, format = "%Y-%m-%d"))



summary(Data)

# Percentage of Male participants ,Average Age, year of participation and Reg Orientation in each Nudge Group



Data %>%
  group_by(Msg_Type) %>%
  summarise(
    n_total = n(),
    n_male = sum(Gender == "Male", na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    avg_participation_date = mean(as.numeric(as.character(Participation_Date)), na.rm = TRUE),
    avg_reg_orientation = mean(Reg_Orientation, na.rm = TRUE)
  ) %>%
  mutate(
    perc_male = round(n_male / n_total * 100, 1),
    avg_age = round(avg_age, 1),
    avg_participation_date = format(round(avg_participation_date, 2), nsmall = 2),
    avg_reg_orientation = round(avg_reg_orientation, 2)
  )





## Paired t-test comparison  Seasonal reporting behavior change  -------------------------------------

summary(Data)

### 2023 Comparison for survey takers  --------------------------------------------


# Difference between  2022  &  2023 
Data %>%
  filter(Year_Survey_Taken == 2023,
         Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))

# mean difference 12.14184 t = 3.0093, df = 140, p-value = 0.003106 Significant




# Difference between  2021 & 2022  

Data %>%
  filter(Year_Survey_Taken == 2023,
         Registered_Participation_Date < as.Date('2022-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2022, Season_Rprts_Filled_2021, paired = TRUE))

# mean difference -0.5352113; t = -0.13508, df = 70, p-value = 0.8929 Less impressive, reduction no significance



### 2024 Comparison for survey takers--------------------------------------------

# Difference between  2023  &  2024 

Data %>%
  filter(Year_Survey_Taken == 2024,
         Registered_Participation_Date < as.Date('2024-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2024, Season_Rprts_Filled_2023, paired = TRUE))

# mean difference 0.785 ; t = 0.778, df = 297, p-value = 0.4372  Slight increase, which is NON SIGNIFICANT


# Difference between  2023 & 2022  ---

Data %>%
  filter(Year_Survey_Taken == 2024,
         Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))

# mean difference -2.04918 ; t = -2.8053, df = 60, p-value = 0.006765  heavier reduction than before that IS significant


Data %>%
  filter(Year_Survey_Taken == 2024)

### No Intervention  ----------

#  Difference between  2022  &  2023 

reports_data_all %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))

# mean difference -0.7272123  ; t = -27.369, df = 30081, p-value < 2.2e-16  Significant Reduction





# Difference between  2023  &  2024 
reports_data_all %>%
  filter(Registered_Participation_Date < as.Date('2024-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2024, Season_Rprts_Filled_2023, paired = TRUE))

# mean difference -1.087651   ; t = -53.946, df = 54773, p-value < 2.2e-16  Significant Reduction





## Plots for Paired t-test comparison  Seasonal reporting behavior change  -------------------------------------

### 2023 --------

data_1 <- Data %>%
  filter(Year_Survey_Taken == 2023,
         Registered_Participation_Date < as.Date('2023-01-01')) %>% 
  select(User_ID, Season_Rprts_Filled_2023,  Season_Rprts_Filled_2022) %>%
  pivot_longer(
    cols = c(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022),
    names_to = "Year",
    values_to = "Reports"
  ) %>%
  mutate(Year = gsub("Season_Rprts_Filled_", "", Year),
         Comparison_Group = "2022 vs 2023")  # Label this group

data_3 <- Data %>%
  filter(Year_Survey_Taken == 2023,
         Registered_Participation_Date < as.Date('2022-01-01')) %>% 
  select(User_ID, Season_Rprts_Filled_2021,  Season_Rprts_Filled_2022) %>%
  pivot_longer(
    cols = c(Season_Rprts_Filled_2021, Season_Rprts_Filled_2022),
    names_to = "Year",
    values_to = "Reports"
  ) %>%
  mutate(Year = gsub("Season_Rprts_Filled_", "", Year),
         Comparison_Group = "2021 vs 2022")  


data_combined <- bind_rows(data_1, data_3) 


data_filtered <- data_combined %>%
  filter((Comparison_Group == "2021 vs 2022" & Year %in% c("2021", "2022")) |
           (Comparison_Group == "2022 vs 2023" & Year %in% c("2022", "2023")))

# Create the combined violin plot
ggplot(data_filtered, aes(x = Year, y = Reports, fill = Year)) +
  geom_violin(alpha = 0.8, trim = FALSE, color = "black") +  
  stat_summary(fun = mean, geom = "crossbar", width = 1, fatten = 2, color = "#00204d") + # Mean line in the center
  labs(
    x = "Year",
    y = "Reports per Participant Per Year",
    caption = expression(bold("Fig.1") ~ italic("Year on Year Change in Average Number of Reports of 2023 Cohort"))
  ) +
  
  # Custom Colors for APA Consistency
  scale_fill_manual(name = "Year",
                    values = c("2021" = "#bb3754",  # Deep Purple
                               "2022" = "#f5b14c",  # Magenta-Red
                               "2023" = "#00509d")) +  # Ensuring consistent color mapping
  scale_y_log10(limits = c(-1, 1000)) +
  facet_wrap(~ Comparison_Group, scales = "free_x") +  # Prevents extra years from appearing
  theme_bw(base_size = 14) + 
  theme(
    # Title formatting 
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    panel.border = element_blank()
  )

### 2024 ----------


data_1 <- Data %>%
  filter(Year_Survey_Taken == 2024,
         Registered_Participation_Date < as.Date('2024-01-01')) %>% 
  select(User_ID, Season_Rprts_Filled_2024,  Season_Rprts_Filled_2023) %>%
  pivot_longer(
    cols = c(Season_Rprts_Filled_2024, Season_Rprts_Filled_2023),
    names_to = "Year",
    values_to = "Reports"
  ) %>%
  mutate(Year = gsub("Season_Rprts_Filled_", "", Year),
         Comparison_Group = "2023 vs 2024")  

data_3 <- Data %>%
  filter(Year_Survey_Taken == 2024,
         Registered_Participation_Date < as.Date('2023-01-01')) %>% 
  select(User_ID, Season_Rprts_Filled_2022,  Season_Rprts_Filled_2023) %>%
  pivot_longer(
    cols = c(Season_Rprts_Filled_2022, Season_Rprts_Filled_2023),
    names_to = "Year",
    values_to = "Reports"
  ) %>%
  mutate(Year = gsub("Season_Rprts_Filled_", "", Year),
         Comparison_Group = "2022 vs 2023")  


data_combined <- bind_rows(data_1, data_3) 


data_filtered <- data_combined %>%
  filter((Comparison_Group == "2022 vs 2023" & Year %in% c("2022", "2023")) |
           (Comparison_Group == "2023 vs 2024" & Year %in% c("2023", "2024")))

# Create the combined violin plot
ggplot(data_filtered, aes(x = Year, y = Reports, fill = Year)) +
  geom_violin(alpha = 0.8, trim = FALSE, color = "black") +  
  stat_summary(fun = mean, geom = "crossbar", width = 1, fatten = 2, color = "#00204d") + # Mean line in the center
  labs(
    x = "Year",
    y = "Reports per Participant Per Year",
    caption = expression(bold("Fig.2") ~ italic("Year on Year Change in Average Number of Reports of 2024 Cohort"))
  ) +
  
  # Custom Colors for APA Consistency
  scale_fill_manual(name = "Year",
                    values = c("2022" = "#bb3754",  # Deep Purple
                               "2023" = "#f5b14c",  # Magenta-Red
                               "2024" = "#00509d")) +  # Ensuring consistent color mapping
  scale_y_log10(limits = c(-1, 1000)) +
  facet_wrap(~ Comparison_Group, scales = "free_x") +  # Prevents extra years from appearing
  theme_bw(base_size = 14) + 
  theme(
    # Title formatting 
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    panel.border = element_blank()
  )


rm(data_1, data_3, data_combined, data_filtered, Plot1)


## Reporting Difference During, Before, and After Intervention Period --------------

# Before/During

with(Data, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))

# mean difference 1.6 ; t = 3.0282, df = 599, p-value = 0.002566 Significant increase during intervention than before 


#During/After

with(Data, t.test(Rprts_During_Msging, Rprts_After_Msging,  paired = TRUE))

#mean difference 0.8766667 ; t = 1.6264, df = 599, p-value = 0.1044 Insignificant decrease

# Putting all Participants in both  years together, during intervention period they experience a significant increase in amount of reporting
# this average goes down again after the intervention stops but the drop is insignificant  


## Plot for  before and after analysis ------
data_4 <- Data %>%
  select(User_ID, Rprts_Before_Msging,  Rprts_During_Msging) %>%
  pivot_longer(
    cols = c(Rprts_Before_Msging, Rprts_During_Msging),
    names_to = "Intervention",
    values_to = "Reports") %>%
  mutate(Intervention = recode(Intervention, 
                               "Rprts_Before_Msging" = "Before", 
                               "Rprts_During_Msging" = "During"))




ggplot(data_4, aes(x = as.factor(Intervention), y = Reports, fill = Intervention)) +
  geom_boxplot(alpha = 0.8, color = "black", outlier.shape = NA) + 
  geom_line(aes(group = User_ID), alpha = 0.3, color = "black", linetype = "dashed") +
  geom_jitter(width = 0.1, alpha = 0.6, color = "black") +
  scale_fill_manual(name = "Condition",
                    values = c("Before" = "#bb3754",   # Deep Purple
                               "During" = "#f5b14c")) +  # Magenta-Red
  scale_y_continuous(trans = "log10") +
  labs(
    x = "Intervention Phase",
    y = "Number of Reports",
    caption = expression(bold("Fig.3") ~ italic("Paired comparison of reports before and during intervention phases"))
  ) +
  theme_bw(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    panel.border = element_blank()
  )

rm(data_4)

## Regression Analysis ---------

reg_data <- reg_data %>% mutate(
  Nudge_Type = as.factor(Nudge_Type),
  Gender = as.factor(Gender),
  Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat)
)




### Does Nudging (general) increase the chance of reporting?  ------------------------------
summary(glmer(
  Report ~ Nudge + Seasonality + (1 | User_ID),
  family = binomial, data = reg_data
)) 


#NOTE if you do it as a single day seasonality is still significant at  -0.0010457  0.0002821  -3.707  0.00021 *** per day
#and nudge is significant as  0.3526865  0.0538798   6.546 5.92e-11 ***

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Nudge + Seasonality + (1 | User_ID)
# Data: reg_data
# 
# AIC      BIC   logLik deviance df.resid 
# 34371.7  34410.2 -17181.9  34363.7   110158 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.6435 -0.2064 -0.1273 -0.0835 12.1839 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 2.428    1.558   
# Number of obs: 110162, groups:  User_ID, 600
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.971501   0.070450 -56.373  < 2e-16 ***
#   Nudge  0.352687   0.053882   6.546 5.93e-11 ***
#   Seasonality  -0.007320   0.001975  -3.707 0.000209 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Msg_Rc
# Msg_Receivd -0.067       
# Seasonality  0.002  0.010




### Does Nudging (general) increase the chance of reporting?  ------------------------------

summary(glmer(
  Report ~ Nudge + Start_of_Intervention + (1 | User_ID),
  family = binomial, data = reg_data
)) 


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Nudge + Start_of_Intervention + (1 | User_ID)
# Data: reg_data
# 
# AIC      BIC   logLik deviance df.resid 
# 34290.2  34328.6 -17141.1  34282.2   110158 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7312 -0.2052 -0.1262 -0.0798 13.0615 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 2.475    1.573   
# Number of obs: 110162, groups:  User_ID, 600
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -3.839644   0.072107 -53.249  < 2e-16 ***
#   Nudge1                 0.297000   0.054185   5.481 4.22e-08 ***
#   Start_of_Intervention -0.026091   0.002661  -9.806  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Nudge1
# Nudge1      -0.082       
# Strt_f_Intr -0.175  0.098



### Does nudging increase the total number (intensity) of reporting ------------------------------


summary(glmer(total_reports ~ Nudge + Seasonality +  (1 |User_ID), 
              data = reg_data, 
              family = poisson)) 

# > summary(glmer(total_reports ~ Nudge + Seasonality + (1 |User_ID), 
#                 +               data = reg_data, 
#                 +               family = poisson)) 
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: total_reports ~ Nudge + Seasonality + (1 | User_ID)
# Data: reg_data
# 
# AIC      BIC   logLik deviance df.resid 
# 61389.9  61428.3 -30690.9  61381.9   110158 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -2.006 -0.265 -0.149 -0.086 40.978 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 2.925    1.71    
# Number of obs: 110162, groups:  User_ID, 600
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.660237   0.075319 -48.596  < 2e-16 ***
#   Nudge  0.179679   0.034400   5.223 1.76e-07 ***
#   Seasonality   0.004551   0.001234   3.689 0.000225 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Msg_Rc
# Msg_Receivd -0.036       
# Seasonality -0.011  0.053
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?
#   
#   Warning message:
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model is nearly unidentifiable: very large eigenvalue
#                - Rescale variables?




### Does the framing of  the nudge matter ----------------------

levels(reg_data$Nudge_Type)
reg_data$Nudge_Type <- relevel(reg_data$Nudge_Type, ref = "None") #set the level to none




summary(glmer(Report ~ Nudge_Type + Seasonality  + (1 | User_ID),
              data = reg_data,
              family = binomial(link = "logit")))


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Nudge_Type + Seasonality + (1 | User_ID)
# Data: reg_data
# 
# AIC      BIC   logLik deviance df.resid 
# 34372.9  34430.6 -17180.5  34360.9   110156 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7720 -0.2058 -0.1272 -0.0835 12.1907 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 2.427    1.558   
# Number of obs: 110162, groups:  User_ID, 600
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -3.971328   0.070447 -56.373  < 2e-16 ***
#   Nudge_TypeEager     0.452877   0.093056   4.867 1.13e-06 ***
#   Nudge_TypeNeutral   0.220182   0.100834   2.184 0.028991 *  
#   Nudge_TypeVigilant  0.369782   0.087185   4.241 2.22e-05 ***
#   Seasonality        -0.007329   0.001975  -3.712 0.000206 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Ndg_TE Ndg_TN Ndg_TV
# Nudg_TypEgr -0.042                     
# Ndg_TypNtrl -0.038  0.002              
# Ndg_TypVgln -0.036  0.002  0.001       
# Seasonality  0.002  0.007  0.009  0.002

### Does agreement between framing and regulatory orientation impact reporting ---------------------

reg_data_nudged<- subset(
  reg_data, 
  Nudge == 1  # or however you mark "a message was received"
)

summary(glmer(
  Report ~ Orientation_Nudge_Agreement   + (1 | User_ID),
  data = reg_data_nudged,
  family = binomial
))

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Orientation_Nudge_Agreement + (1 | User_ID)
# Data: reg_data_nudged
# 
# AIC      BIC   logLik deviance df.resid 
# 2936.4   2957.0  -1465.2   2930.4     7202 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.2453 -0.2336 -0.0859 -0.0859  4.2813 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 4.819    2.195   
# Number of obs: 7205, groups:  User_ID, 600
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -4.4866     0.2287 -19.615   <2e-16 ***
#   Orientation_Nudge_Agreement   0.5534     0.2604   2.125   0.0336 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Ornttn_Nd_A -0.435

summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = subset(reg_data_nudged, Reg_Orientation_Cat == "Prevention"),
  family = binomial
))


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Orientation_Nudge_Agreement + (1 | User_ID)
# Data: subset(reg_data_nudged, Reg_Orientation_Cat == "Prevention")
# 
# AIC      BIC   logLik deviance df.resid 
# 1433.9   1452.3   -713.9   1427.9     3402 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.1823 -0.2336 -0.0859 -0.0859  4.2815 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 4.813    2.194   
# Number of obs: 3405, groups:  User_ID, 284
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 -4.4850241  0.0004862   -9225   <2e-16 ***
#   Orientation_Nudge_Agreement  0.6464821  0.0004861    1330   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Ornttn_Nd_A 0.000 
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model failed to converge with max|grad| = 0.145446 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?

summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = subset(reg_data_nudged, Reg_Orientation_Cat == "Neutral"),
  family = binomial
))


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Orientation_Nudge_Agreement + (1 | User_ID)
# Data: subset(reg_data_nudged, Reg_Orientation_Cat == "Neutral")
# 
# AIC      BIC   logLik deviance df.resid 
# 700.5    716.4   -347.2    694.5     1466 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.2246 -0.2531 -0.1463 -0.1383  4.1948 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 2.071    1.439   
# Number of obs: 1469, groups:  User_ID, 123
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -3.3231     0.2866 -11.596   <2e-16 ***
#   Orientation_Nudge_Agreement  -0.1677     0.3959  -0.423    0.672    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Ornttn_Nd_A -0.461

summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = subset(reg_data_nudged, Reg_Orientation_Cat == "Promotion"),
  family = binomial
))


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Report ~ Orientation_Nudge_Agreement + (1 | User_ID)
# Data: subset(reg_data_nudged, Reg_Orientation_Cat == "Promotion")
# 
# AIC      BIC   logLik deviance df.resid 
# 617.8    633.8   -305.9    611.8     1507 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5605 -0.0685 -0.0578 -0.0578  3.9494 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# User_ID (Intercept) 9.325    3.054   
# Number of obs: 1510, groups:  User_ID, 125
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -5.3303     0.8186  -6.511 7.46e-11 ***
#   Orientation_Nudge_Agreement   0.4926     0.7246   0.680    0.497    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Ornttn_Nd_A -0.314

## Plots For Regression -----------

reg_data$Nudge <- as.factor(reg_data$Nudge)
reg_data$Nudge_Type <- factor(reg_data$Nudge_Type, levels = c("None", "Eager", "Neutral", "Vigilant"))
reg_data$Orientation_Nudge_Agreement <- as.factor(reg_data$Orientation_Nudge_Agreement)



# Plot 1: Effect of Nudge on Reporting


ggplot(reg_data, aes(x = Nudge, y = Report, fill = Nudge)) +
  
  # Bar plot with dodge positioning
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.5), 
               color = "black", width = 0.5, alpha = 0.8) +
  
  # Error bars for confidence intervals
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.5), 
               width = 0.15, size = 1, color = "#00204d") +
  
  # Labels & Titles
  labs(
    x = "Experimental Condition",
    y = "Reporting Probability (%)",
    title = "Impact of Receiving a Nudge on Reporting",
    caption = expression(bold("Fig.4") ~ italic("Mean reporting probability of a Citizen Scientist with 95% confidence intervals"))
  ) +
  
  # X-axis Formatting
  scale_x_discrete(labels = c("No Nudge", "Nudge Received")) +
  
  # Custom Color Palette (Matching APA Colors)
  scale_fill_manual(name = "Experimental Condition",
                    values = c("0" = "#00509d",  # Deep Blue
                               "1" = "#bb3754")) +  # Magenta-Red
  
  # Y-axis formatted as percentages
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  
  # APA-Style Theme Adjustments
  theme_bw(base_size = 14) + 
  theme(
    # Title and subtitle formatting
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    
    # Axes formatting
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    
    # Y-axis gridline formatting (dotted, light gray)
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    
    # Legend Formatting (APA Style)
    legend.position = "none",
    
    # Removing panel border for clean APA look
    panel.border = element_blank()
  )



# Effect of Messages over dates  

# 2023

ggplot(reg_data_2023, aes(x = as.Date(Date), y = Report * 100, color = Nudge, fill = Nudge)) +
  
  # Mean line with enhanced visibility
  stat_summary(fun = mean, geom = "line", size = 1) +
  
  # Confidence interval ribbon
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  
  # Custom APA-compatible color palette
  scale_color_manual(name = "Experimental Phase",
                     values = c("0" = "#00509d",  
                                "1" = "#bb3754"),
                     labels = c("0" = "Outside Intervention Period", "1" = "During Intervention Period")) +  # Warm Red
  scale_fill_manual(name = "Experimental Phase",
                    values = c("0" = "#00509d",  
                               "1" = "#bb3754"),
                    labels = c("0" = "Outside Intervention Period", "1" = "During Intervention Period")) +
  
  # Formatting date axis (monthly intervals)
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  
  # Highlighting message intervention (adjust the date accordingly)
  geom_vline(xintercept = as.Date("2023-06-12"), linetype = "dashed", color = "black", size = 0.8) +
  annotate("text", x = as.Date("2023-06-12"), y = max(reg_data$Report) * 0.9, 
           label = "Start of Intervention", angle = 90, vjust = -0.5, hjust = -3, size = 3, fontface = "bold") +
  
  # Labels & Title
  labs(
    x = "Date",
    y = "Proportion of Reports (%)",
    title = "Changes in Reporting Behavior With and Without Nudges (2023)",
    caption = "Fig. 5: Proportion of participants who submitted reports over time in 2023.\nRed line =  articipants during their designated intervention period 
(i.e., when they received behavioral nudges; \nBlue line = participants outside of their intervention period."
  ) +
  
  # APA-Style Theme Adjustments
  theme_bw(base_size = 14) +
  theme(
    # Title and subtitle formatting
    plot.title = element_text(face = "bold", size = 16, hjust = 0),  # Left-aligned per APA guidelines
    plot.subtitle = element_text(size = 12, hjust = 0),
    
    # Axes formatting
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    
    # Y-axis gridline formatting (dotted, light gray)
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    
    # Legend Formatting (APA Style, placed inside plot for compact design)
    legend.position = c(0.65, 0.95),  # Inside the plot (top-left)
    legend.justification = c(0, 1),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    
    # Removing panel border for a clean APA look
    panel.border = element_blank()
  )



# 2024

# Effect of Messages over dates  NOTE: did not use this in the thesis since i changed the code i was using from date to seasonality
# if you are going to usi it just cut the data off for specific years either 2023 or 2024

ggplot(reg_data_2024, aes(x = as.Date(Date), y = Report * 100, color = Nudge, fill = Nudge)) +
  
  # Mean line with enhanced visibility
  stat_summary(fun = mean, geom = "line", size = 1) +
  
  # Confidence interval ribbon
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  
  # Custom APA-compatible color palette
  scale_color_manual(name = "Intervention Period",
                     values = c("0" = "#00509d",  
                                "1" = "#bb3754"),
                     labels = c("0" = "Outside", "1" = "Within")) +  # Warm Red
  scale_fill_manual(name = "Intervention Period",
                    values = c("0" = "#00509d",  
                               "1" = "#bb3754"),
                    labels = c("0" = "Outside", "1" = "Within")) +
  
  # Formatting date axis (monthly intervals)
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  
  # Highlighting message intervention (adjust the date accordingly)
  geom_vline(xintercept = as.Date("2024-05-29"), linetype = "dashed", color = "black", size = 0.8) +
  annotate("text", x = as.Date("2024-05-29"), y = max(reg_data$Report) * 0.9, 
           label = "Start of Intervention", angle = 90, vjust = -0.5, hjust = -3, size = 3, fontface = "bold") +
  
  # Labels & Title
  labs(
    x = "Date",
    y = "Proportion of Reports (%)",
    title = "Impact of Recieving a Nudge on Reporting Over Time",
    caption = expression(bold("Fig.4") ~ italic("Reporting behavior over time, comparing participants within or outside intervention period"))
  ) +
  
  # APA-Style Theme Adjustments
  theme_bw(base_size = 14) +
  theme(
    # Title and subtitle formatting
    plot.title = element_text(face = "bold", size = 16, hjust = 0),  # Left-aligned per APA guidelines
    plot.subtitle = element_text(size = 12, hjust = 0),
    
    # Axes formatting
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    
    # Y-axis gridline formatting (dotted, light gray)
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    
    # Legend Formatting (APA Style, placed inside plot for compact design)
    legend.position = c(0.65, 0.95),  # Inside the plot (top-left)
    legend.justification = c(0, 1),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    
    # Removing panel border for a clean APA look
    panel.border = element_blank()
  )



# Plot 2: Effect of Nudge on Total Reports


ggplot(reg_data, aes(x = Nudge, y = total_reports, fill = Nudge)) +
  
  # Bar plot for mean values with dodge positioning
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.5), 
               color = "black", width = 0.5, alpha = 0.8) +
  
  # Error bars for confidence intervals
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.5), 
               width = 0.15, size = 1, color = "#00204d") +
  
  # Labels & Titles
  labs(
    x = "Experimental Condition",
    y = "Mean Total Reports per Day",
    title = "Impact of Nudging on Reporting Intensity",
    caption = expression(bold("Fig.6") ~ italic("Average number of reports per day between participants who did or did not receive a nudge"))
  ) +
  
  # X-axis Formatting
  scale_x_discrete(labels = c("No Nudge", "Nudge Recieved")) +
  
  # Custom Color Scheme (APA-Style Blue)
  scale_fill_manual(name = "Condition", values = c("0" = "#00509d",  # High-contrast blue
                                                   "1" = "#bb3754")    # Magenta-Red
  ) +
  
  # Y-axis formatted appropriately
  scale_y_continuous(expand = c(0, 0)) +
  
  # APA-Style Theme Adjustments
  theme_bw(base_size = 14) + 
  theme(
    # Title formatting (left-aligned for APA style)
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    
    
    # Axes formatting
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    
    # Y-axis gridline formatting (dotted, light gray)
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    
    # Legend Formatting (APA Style)
    legend.position = "none",  # Inside the plot (top-left)
    
    
    # Removing panel border for clean APA look
    panel.border = element_blank()
  )



# Plot 3: Effect of Nudge_Type on Reporting


ggplot(subset(reg_data, Nudge_Type != "None"), aes(x = Nudge_Type, y = Report, fill = Nudge_Type)) +
  # Bar plot for mean values
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6, alpha = 0.8) +
  
  # Error bars for confidence intervals
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, size = 1, color = "#00204d") +
  
  # Labels & Titles
  labs(
    x = "Nudge Type",
    y = "Chance of Reporting (%)",
    title = "Impact of Differently Framed Nudges",
    caption = expression(bold("Fig.7") ~ italic("Average chance of reporting after being nudged, compared by framing"))
  ) +
  
  # Y-axis formatted as percentages (if applicable)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  
  # Custom Color Scheme
  scale_fill_manual(
    name = "Nudge Type",  # Legend title
    values = c(
      "Vigilant" = "#bb3754",  # Deep Purple
      "Eager" = "#f5b14c",   # Magenta-Red
      "Neutral" = "#1f78b4"     # Soft Orange
    )
  ) +
  
  # Clean, professional theme
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    
    # Adjusting the legend
    legend.position = "top",  # Moves the legend above the plot
    legend.title = element_text(face = "bold", size = 10),  # Makes the title bold
    legend.text = element_text(size = 8)
  )



# Plot 4: Effect of Orientation-Nudge Agreement on Reporting 


ggplot(reg_data %>% filter(!is.na(Reg_Orientation_Cat)), 
       aes(x = Orientation_Nudge_Agreement, y = Report, fill = Reg_Orientation_Cat)) +
  
  # Bar plot for mean values with dodge positioning
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.5), 
               color = "black", width = 0.5 , alpha = 0.8) +
  
  # Error bars for confidence intervals
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.5), 
               width = 0.15, size = 1, color = "#00204d") +
  
  # Labels & Titles
  labs(
    x = "Nudge/Orientation Agreement",
    y = "Mean Reporting Probability (%)",
    title = "Impact of Matching Regulatory Orientation with Nudge Framing",
    caption = expression(bold("Fig.8") ~ italic("Effect of nudge framing alignment with participant orientation on reporting probability"))
  ) +
  
  # X-axis Formatting
  scale_x_discrete(labels = c("No Match", "Match")) +
  
  # Grayscale for APA Style (use if printing in grayscale)
  scale_fill_manual(name = "Regulatory Orientation", values = c("Prevention" = "#bb3754",  # Deep Purple
                                                                "Promotion" = "#f5b14c",  # Magenta-Red
                                                                "Neutral" = "#00509d")   # Soft Orange
  ) +
  
  # Y-axis formatted as percentages
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  
  # APA-Style Theme Adjustments
  theme_bw(base_size = 14) + 
  theme(
    # Title and subtitle formatting
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    
    # Axes formatting
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    
    # Y-axis gridline formatting (dotted, light gray)
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    
    # Legend Formatting (APA Style)
    legend.position = c(0.05, 0.95),  # Inside the plot (top-left)
    legend.justification = c(0, 1),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    
    # Removing panel border for clean APA look
    panel.border = element_blank()
  )
