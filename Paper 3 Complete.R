# Paper 3 Complete -----------
## Load necessary package -------

library(tidyverse)


library(pwr)

library(reshape2)
library(broom)
library(kableExtra)


## Load necessary Data sets (skip till ) -------


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

rm(raw_message_data_2023)

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


rm(raw_message_data_2024)


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
  select(user_id, creation_time) %>%
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d")) %>% 
  rename(User_ID = user_id,
         Report_Date = creation_time) %>%
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
         
   Data <- full_join(survey_data_2024, message_data_2024, by = "User_ID")
         
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
           ungroup() 
   
rm(Data1)
         
write.csv(Data, "nudge_data_clean.csv", row.names = FALSE)
   