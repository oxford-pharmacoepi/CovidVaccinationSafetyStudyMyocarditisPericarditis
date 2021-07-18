# packages ----
library(dplyr)
library(tidyr)
library(here)
library(stringr)
library(epiR)
library(ggplot2)
library(purrr)
library(popEpi)
library(Epi)
library(splines)
library(epitools)
library(scales)


# patient characteristics ----- 
data.files<-list.files(here("data"))
# dbs
db.names<-data.files[str_detect(data.files, "Patient.characteristcis_")]
db.names<-str_replace(db.names, "Patient.characteristcis_","")
db.names<-str_replace(db.names, ".RData","" )

Network.patient.characteristcis<-list()
for(i in 1:length(db.names)){
 load(paste0(here("data"),"/Patient.characteristcis_", db.names[i], ".RData"))

for(l in 1:length(Patient.characteristcis)){
    Patient.characteristcis[[l]]$db<- db.names[i]
  }
 Network.patient.characteristcis[[db.names[i]]]<-bind_rows(Patient.characteristcis, .id = "id") 
 rownames(Network.patient.characteristcis[[db.names[i]]])<-1:nrow(Network.patient.characteristcis[[db.names[i]]])
 rm(Patient.characteristcis)
}
Network.patient.characteristcis<-bind_rows(Network.patient.characteristcis)
# 
# # add patient characteristics from background population
# load(paste0(here("data", "background"),"/Patient.characteristcis_", "CPRD", ".RData"))
# for(l in 1:length(Patient.characteristcis)){
#     Patient.characteristcis[[l]]$db<- "CPRD"
#   }
#  Background.Patient.characteristcis<-bind_rows(Patient.characteristcis, .id = "id") 
#  rownames(Background.Patient.characteristcis)<-1:nrow(Background.Patient.characteristcis)
#  rm(Patient.characteristcis)
# 
#  Background.Patient.characteristcis<- Background.Patient.characteristcis %>% 
#                rename(pop=pop.type) %>% 
#     filter(pop!="general.pop.with.visit.28.days") %>% 
#     mutate(pop=ifelse(pop=="general.pop.all", 
#                       "Background population 2017 to 2019",
#                ifelse(pop=="general.pop.with.visit", 
#                       "Background population with visit 2017 to 2019",      
#       NA)))
#  
#  Network.patient.characteristcis<-bind_rows(Network.patient.characteristcis, 
#             Background.Patient.characteristcis )
# rm(Background.Patient.characteristcis)





#
names(Network.patient.characteristcis)<-str_replace(names(Network.patient.characteristcis),
                                                    "Overall","Study population")
names(Network.patient.characteristcis)
Network.patient.characteristcis <-  Network.patient.characteristcis %>% 
  select(-"Myocarditis wo cleanwindow", -"Myocarditis_Narrow wo cleanwindow", -"Pericarditis wo cleanwindow", -"Pericarditis_narrow wo cleanwindow", -"thrombocyt", -"VTE narrow")


table(Network.patient.characteristcis$pop.type)

# Network.patient.characteristcis<-Network.patient.characteristcis %>% 
#  mutate(pop.type=ifelse(pop.type=="All" ,
#                         "Overall population",  
#                         pop.type))  %>% 
#  mutate(pop.type=ifelse(pop.type=="general.pop.with.visit" ,
#                         "Population with visit after January 1st 2017",  
#                         pop.type))  %>% 
#  mutate(pop.type=ifelse(pop.type=="general.pop.with.visit.28.days" ,
#                         "Population with visit after January 1st 2017, 28 day follow-up",  
#                         pop.type)) 

# Network.patient.characteristcis<-Network.patient.characteristcis %>% 
#   select(-Death)
# Network.patient.characteristcis<-Network.patient.characteristcis %>% 
#  filter(pop.type!="Population with visit after January 1st 2017, 28 day follow-up")

# Network.patient.characteristcis<-
#   Network.patient.characteristcis[,which(str_detect(names(Network.patient.characteristcis),
#            "with thrombocytopenia 42",
#            negate = TRUE)==TRUE)]
# names(Network.patient.characteristcis)<-str_replace_all(names(Network.patient.characteristcis),
#                 "with thrombocytopenia 10 days pre to 10 days post",
#                       "with thrombocytopenia")

save(Network.patient.characteristcis, file = here("data", "Network.patient.characteristcis.RData"))


  

# summarise overall study populations -----
table<-Network.patient.characteristcis %>%
  filter(prior.obs.required=="No") %>% 
  filter(pop.type=="All") %>% 
  filter(age_gr2=="All") %>% 
  select("var", 
         "Study population",
         "pop") 
table<-table  %>%
  pivot_wider(names_from = pop,
             names_glue = "{pop}: {.value}",
              values_from = "Study population")
 
  
  table<-bind_rows(
     table[c(1:4),],
    table[c(1),]  %>%
      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table[c(5:17),],
    table[c(1),]  %>%
      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table[c(18:26),]
    )  %>%
     mutate(var=ifelse(var=="Copd", "COPD", var)) %>%
     mutate(var=ifelse(var=="Antiinflamatory and antirheumatic", "Non-steroidal anti-inflammatory drugs ", var)) %>%
     mutate(var=ifelse(var=="Coxibs", "Cox2 inhibitors ", var)) %>%
     mutate(var=ifelse(var=="Corticosteroids", "Systemic corticosteroids ", var)) %>%
     mutate(var=ifelse(var=="Antithrombotic", "Antithrombotic and anticoagulant therapies", var)) %>%
     mutate(var=ifelse(var=="Lipid modifying", "Lipid modifying agents ", var)) %>%
     mutate(var=ifelse(var=="Antineoplastic immunomodulating", "Antineoplastic and immunomodulating agents ", var)) %>%
     mutate(var=ifelse(var=="Hormonal contraceptives", "Hormonal contraceptives for systemic use ", var)) %>%
     mutate(var=ifelse(var=="Sex hormones modulators", "Sex hormones and modulators of the genital system", var))


  
write.csv(table,
          here("data", "table1.csv"), 
            row.names = FALSE)



# incidence rates -----

db.names<-data.files[str_detect(data.files, "IR.summary_")]
db.names<-str_replace(db.names, "IR.summary_","")
db.names<-str_replace(db.names, ".RData","" )

Network.IR<-list()
for(i in 1:length(db.names)){
load(paste0(here("data"),"/IR.summary_", db.names[i], ".RData"))
Network.IR[[db.names[i]]]<-IR.summary %>%
  select(-outcome) %>% 
  filter(events!="<5") %>% 
  mutate(events=as.numeric(events))
 rm(IR.summary)
}
Network.IR<-bind_rows(Network.IR)

## XL

unique(Network.IR$outcome.name)
Network.IR <-  Network.IR %>% 
  filter(!outcome.name %in%  c("Myocarditis wo cleanwindow", "Myocarditis_Narrow wo cleanwindow", "Pericarditis wo cleanwindow", "Pericarditis_narrow wo cleanwindow", "Thrombocytopenia", "Venous thromboembolism - narrow") )


# add IRS from background population
# load(paste0(here("data", "background"),"/IR.summary_", "CPRD", ".RData"))
# IR.summary<-IR.summary %>%
#   select(-outcome) %>% 
#   filter(events!="<5") %>% 
#   mutate(events=as.numeric(events))
# 
#  IR.summary<- IR.summary %>% 
#                rename(pop=pop.type) %>% 
#     filter(pop!="general.pop.with.visit.28.days") %>% 
#     mutate(pop=ifelse(pop=="general.pop.all", 
#                       "Background population 2017 to 2019",
#                ifelse(pop=="general.pop.with.visit", 
#                       "Background population with visit 2017 to 2019",      
#       NA)))
#  
#  Network.IR<-bind_rows(Network.IR, 
#             IR.summary )
# rm(IR.summary)


# add CIs
IR.conf<-epi.conf(as.matrix(cbind(Network.IR$events, Network.IR$years)),
         ctype = "inc.rate", method = "exact", conf.level = 0.95)
Network.IR$ir_100000_lower<-IR.conf$lower* 100000
Network.IR$ir_100000_upper<-IR.conf$upper* 100000
rm(IR.conf)


table(Network.IR$time.window)
# Network.IR<-Network.IR %>%
#  mutate(time.window=ifelse(time.window=="0_28", "0 to 28 days",
#                     ifelse(time.window=="0_7", "0 to 7 days",
#                     ifelse(time.window=="7_14", "7 to 14 days",
#                     ifelse(time.window=="14_21", "14 to 21 days",     
#                     ifelse(time.window=="21_28", "21 to 28 days",    
#                         time.window)))))) %>% 
#   mutate(time.window=factor(time.window,
#                             levels=c("0 to 28 days","0 to 7 days","7 to 14 days",
#                                      "14 to 21 days","21 to 28 days")))

table(Network.IR$pop)
Network.IR<-Network.IR %>%
 mutate(pop=ifelse(pop=="General population 2017" ,
                        "General population",
                        pop))  %>%
 mutate(pop=ifelse(pop=="General population with visit 2017 to 2019" ,
                        "General population with visit",
                        pop))

table(Network.IR$pop)

table(Network.IR$pop.type)
# 
# Network.IR<-Network.IR %>%
#  mutate(pop.type=ifelse(pop.type=="general.pop.all" ,
#                         "Overall population as of January 1st 2017",
#                         pop.type))  %>%
#  mutate(pop.type=ifelse(pop.type=="general.pop.with.visit" ,
#                         "Population with visit after January 1st 2017",
#                         pop.type))  %>%
#  mutate(pop.type=ifelse(pop.type=="general.pop.with.visit.28.days" ,
#                         "Population with visit after January 1st 2017, 28 day follow-up",
#                         pop.type))
# 
# #
# Network.IR<-Network.IR %>% 
#  filter(pop.type!="Population with visit after January 1st 2017, 28 day follow-up")
# Network.IR<-Network.IR %>% 
#   filter(outcome.name!="Death")
# 
# Network.IR<-Network.IR %>% 
#   mutate(events=ifelse(events<5, "<5",events))
# Network.IR<-Network.IR %>% 
#   mutate(ir_100000=ifelse(events=="<5",NA,ir_100000)) %>% 
#   mutate(ir_100000_lower=ifelse(events=="<5",NA,ir_100000_lower)) %>% 
#   mutate(ir_100000_upper=ifelse(events=="<5",NA,ir_100000_upper))
#   
#  Network.IR<-Network.IR%>%   
#   mutate(db=ifelse(db=="France LPD", "LPD France", db)) %>% 
#   mutate(db=ifelse(db=="Italy LPD", "LPD Italy", db)) 
#  
# Network.IR<- Network.IR %>%
#    filter(!str_detect(outcome.name,"with thrombocytopenia 42 ")) %>%
#    mutate(outcome.name=str_replace(outcome.name,"with thrombocytopenia 10 days pre to 10 days post",
#                       "with thrombocytopenia"))
# 
# 
# 
# 
 save(Network.IR, file = here("data", "Network.IR.RData"))
# 
# 
# 
# 
# 
# load(here("data", "Network.patient.characteristcis.RData"))
# load(here("data", "Network.IR.RData"))
# 
# 
#   
 
# incidence ------
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
         big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
         big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
         big.mark=",", nsmall = 0, digits=0, scientific=FALSE))}


# dbs
db.names<-data.files[str_detect(data.files, "Survival.summary_")]
db.names<-str_replace(db.names, "Survival.summary_","")
db.names<-str_replace(db.names, ".RData","")


Network.Survival.summary<-list()
for(i in 1:length(db.names)){
 load(paste0(here("data"),"/Survival.summary_", db.names[i], ".RData"))
 Network.Survival.summary[[db.names[i]]]<-Survival.summary 
 rm(Survival.summary)
}
Network.Survival.summary<-bind_rows(Network.Survival.summary) %>% 
  mutate(total_events=nice.num.count(cum.n.event)) %>% 
  mutate(inc.est=
           ifelse(!is.na(surv),
           paste0(nice.num2((1-surv)*100),
                    "% (",
                    nice.num2((1-upper)*100),
                    "% to ",
                    nice.num2((1-lower)*100), "%)"
                    ), NA))
Network.Survival.summary<-Network.Survival.summary  %>% 
  mutate(total_events=ifelse(time==0, NA, total_events)) %>% 
  mutate(inc.est=ifelse(time==0, NA, inc.est))

Network.Survival.summary<-Network.Survival.summary%>% 
   mutate(group=str_replace(group, "age_gr2=", "Age: " )) %>% 
   mutate(group=str_replace(group, "age_gr=", "Age: " )) %>% 
   mutate(group=str_replace(group, "age_gr3=", "Age: " )) %>% 
   mutate(group=str_replace(group, ", gender=", "; Sex: " )) %>% 
   mutate(group=str_replace(group, "gender=", "Sex: " )) %>% 
   mutate(n.risk=nice.num.count(n.risk)) %>% 
   mutate(total_events=
            ifelse(total_events=="<5", 
                   total_events,
            nice.num.count(as.numeric(total_events)))) %>% 
   rename("Strata"="group")%>% 
   rename("Number at risk"="n.risk")%>% 
   rename("Events"="total_events")%>% 
   rename("Cumulative incidence"="inc.est") 

Network.Survival.summary<-Network.Survival.summary %>%
  filter(!outcome.name %in%  c("Myocarditis wo cleanwindow", "Myocarditis_Narrow wo cleanwindow", "Pericarditis wo cleanwindow", "Pericarditis_narrow wo cleanwindow", "thrombocyt", "VTE narrow") )

unique(Network.Survival.summary$outcome.name)


 save(Network.Survival.summary, file = here("data", "Network.Survival.summary.RData"))

# IRRs ----  
get.IRR.df<-function(target.name, comparator.name){
target<-Network.IR %>% 
  filter(pop=={{target.name}}) %>%  
  select(pop, pop.type, time.window, n, years, events, 
         strata, outcome.name,
         ir_100000, ir_100000_lower,ir_100000_upper,
         prior.obs.required, gender, age_gr, age_gr2, age_gr3)  %>%
  rename("target.pop"="pop") %>% 
  rename("target.pop.type"="pop.type") %>% 
  rename("target.time.window"="time.window") %>% 
   rename("n_target"="n") %>% 
   rename("years_target"="years") %>% 
   rename("events_target"="events") %>% 
   rename("ir_100000_target"="ir_100000")%>% 
   rename("ir_100000_lower_target"="ir_100000_lower")%>% 
   rename("ir_100000_upper_target"="ir_100000_upper")

comparator<-Network.IR %>% 
  filter(pop=={{comparator.name}}) %>%
  select(pop, n, years, events, strata, outcome.name,
         ir_100000, ir_100000_lower,ir_100000_upper,
         prior.obs.required, gender, age_gr, age_gr2, age_gr3) %>% 
   rename("comparator"="pop") %>% 
   rename("n_comparator"="n") %>% 
   rename("years_comparator"="years") %>% 
   rename("events_comparator"="events") %>% 
   rename("ir_100000_comparator"="ir_100000")%>% 
   rename("ir_100000_lower_comparator"="ir_100000_lower")%>% 
   rename("ir_100000_upper_comparator"="ir_100000_upper")

IRR<-left_join(target, comparator)
IRR$rrr.est<-NA
IRR$rrr.lower<-NA
IRR$rrr.upper<-NA

for(i in 1:nrow(IRR)){
if(!is.na(IRR$events_comparator[i]) &
   !is.na(IRR$events_target[i])){
r<-rateratio(x=c(IRR$events_comparator[i],
              IRR$events_target[i]), 
          y = c(IRR$years_comparator[i],
                IRR$years_target[i]),
          conf.level = 0.95)
IRR$rrr.est[i]<-r$measure[2,1]
IRR$rrr.lower[i]<-r$measure[2,2]
IRR$rrr.upper[i]<-r$measure[2,3]  
}}

IRR}

 
COVID_PCR_background.pop.IRR<-get.IRR.df(target.name="SARS-CoV-2 PCR positive test",
                                  comparator.name="General population") 
COVID_PCR_background.pop.visit.IRR<-get.IRR.df(target.name ="SARS-CoV-2 PCR positive test",
                                  comparator.name="General population with visit") 
 
 
AZ_background.pop.IRR<-get.IRR.df(target.name="Vaccinated with AstraZeneca",
                                  comparator.name="General population") 
AZ_background.pop.visit.IRR<-get.IRR.df(target.name="Vaccinated with AstraZeneca",
                                  comparator.name="General population with visit") 
Pf_background.pop.IRR<-get.IRR.df(target.name="Vaccinated with Pfizer-Biontech",
                                  comparator.name="General population") 
Pf_background.pop.visit.IRR<-get.IRR.df(target.name="Vaccinated with Pfizer-Biontech",
                                  comparator.name="General population with visit") 
 
save(COVID_PCR_background.pop.IRR, file = here("data", "COVID_PCR_background.pop.IRR.RData"))
save(COVID_PCR_background.pop.visit.IRR, file = here("data", "COVID_PCR_background.pop.visit.IRR.RData"))
save(AZ_background.pop.IRR, file = here("data", "AZ_background.pop.IRR.RData"))
save(AZ_background.pop.visit.IRR, file = here("data", "AZ_background.pop.visit.IRR.RData"))
save(Pf_background.pop.IRR, file = here("data", "Pf_background.pop.IRR.RData"))
save(Pf_background.pop.visit.IRR, file = here("data", "Pf_background.pop.visit.IRR.RData"))
 
 
# IRS -----

get.IRS.df<-function(target.name, comparator.name){ 

target<-Network.IR %>% 
  filter(pop=={{target.name}}) %>%  
  filter(strata=="age_gr3") %>% 
  select(pop, pop.type, n, time.window, years, events, strata, outcome.name,
         prior.obs.required, age_gr3) %>% 
  rename("target.pop"="pop") %>% 
  rename("target.pop.type"="pop.type") %>% 
  rename("target.time.window"="time.window") %>% 
   rename("n_target"="n") %>% 
   rename("years_target"="years") %>% 
   rename("events_target"="events") 

comparator<-Network.IR %>% 
  filter(pop=={{comparator.name}}) %>%
  filter(strata=="age_gr3") %>% 
  select(pop, pop.type, n, time.window, years, events, strata, outcome.name,
         prior.obs.required, age_gr3) %>% 
   rename("comparator"="pop") %>% 
   rename("n_comparator"="n") %>% 
   rename("years_comparator"="years") %>% 
   rename("events_comparator"="events")




# for each outcome
# for each target.pop.type
# for each time window
# for each prior.obs.required

outcome.names.to.get<- unique(target$outcome.name)
target.pop.type.to.get<- unique(target$target.pop.type)
target.time.window.to.get<- unique(target$target.time.window)
prior.obs.required.to.get<- unique(target$prior.obs.required)

# o<-1
# pt<-1
# tw<-1
# po<-1

ISR<-list()
for(o in 1:length(outcome.names.to.get)){ 
for(pt in 1:length(target.pop.type.to.get)){ 
for(tw in 1:length(target.time.window.to.get)){ 
for(po in 1:length(prior.obs.required.to.get)){ 


a<-target %>% 
  filter(target.pop.type==target.pop.type.to.get[pt]) %>% 
  filter(outcome.name==outcome.names.to.get[o])  %>% 
  filter(target.time.window==target.time.window.to.get[tw]) %>% 
  filter(prior.obs.required==prior.obs.required.to.get[po])
  
b<-comparator %>% 
  filter(outcome.name==outcome.names.to.get[o]) %>% 
  filter(prior.obs.required==prior.obs.required.to.get[po])

isr<-ageadjust.indirect(count=a$events_target,
                        pop=a$years_target,
                        stdcount =b$events_comparator,
                        stdpop = b$years_comparator)


ISR[[paste(target.pop.type.to.get[pt],
           outcome.names.to.get[o],
           target.time.window.to.get[tw],
           prior.obs.required.to.get[po], sep = ";")]]<-
  data.frame(
    n_target=sum(a$n_target),
   years_target= sum(a$years_target),
   events_target= sum(a$events_target),
   n_comparator= sum(b$n_comparator),
    years_comparator=sum(b$years_comparator),
   events_comparator= sum(b$events_comparator),
data.frame(isr.observed=data.frame(isr$sir)[1,1]),
data.frame(isr.expected=data.frame(isr$sir)[2,1]),
data.frame(isr.sir=data.frame(isr$sir)[3,1]),
data.frame(isr.sir_lower=data.frame(isr$sir)[4,1]),
data.frame(isr.sir_upper=data.frame(isr$sir)[5,1]),
data.frame(ir_100000.crude=data.frame(100000*isr$rate)[1,1]),
data.frame(ir_100000.stand=data.frame(100000*isr$rate)[2,1]),
data.frame(ir_100000_lower.stand=data.frame(100000*isr$rate)[3,1]),
data.frame(ir_100000_upper.stand=data.frame(100000*isr$rate)[4,1])
) %>% 
  mutate(target.pop.type=target.pop.type.to.get[pt]) %>% 
  mutate(outcome.name=outcome.names.to.get[o])  %>% 
  mutate(target.time.window=target.time.window.to.get[tw]) %>% 
  mutate(prior.obs.required=prior.obs.required.to.get[po])
}}}}

ISR<-bind_rows(ISR)

ISR
}

COVID_PCR_background.pop.IRS<-get.IRS.df(target.name="SARS-CoV-2 PCR positive test",
                                  comparator.name="General population") 
COVID_PCR_background.pop.visit.IRS<-get.IRS.df(target.name ="SARS-CoV-2 PCR positive test",
                                  comparator.name="General population with visit") 
 
AZ_background.pop.IRS<-get.IRS.df(target.name="Vaccinated with AstraZeneca",
                                  comparator.name="General population") 
AZ_background.pop.visit.IRS<-get.IRS.df(target.name="Vaccinated with AstraZeneca",
                                  comparator.name="General population with visit") 
Pf_background.pop.IRS<-get.IRS.df(target.name="Vaccinated with Pfizer-Biontech",
                                  comparator.name="General population") 
Pf_background.pop.visit.IRS<-get.IRS.df(target.name="Vaccinated with Pfizer-Biontech",
                                  comparator.name="General population with visit") 
 
save(COVID_PCR_background.pop.IRS, file = here("data", "COVID_PCR_background.pop.IRS.RData"))
save(COVID_PCR_background.pop.visit.IRS, file = here("data", "COVID_PCR_background.pop.visit.IRS.RData"))
save(AZ_background.pop.IRS, file = here("data", "AZ_background.pop.IRS.RData"))
save(AZ_background.pop.visit.IRS, file = here("data", "AZ_background.pop.visit.IRS.RData"))
save(Pf_background.pop.IRS, file = here("data", "Pf_background.pop.IRS.RData"))
save(Pf_background.pop.visit.IRS, file = here("data", "Pf_background.pop.visit.IRS.RData"))
 

# # export as csv -----
# library(openxlsx)
# wb <- createWorkbook()
# addWorksheet(wb=wb, sheetName = "All")
# Network.IR.for.xlsx<-Network.IR %>% 
#   select(db, n,years,events , strata,outcome.name,
#          prior.obs.required,
#          pop.type,
#          gender,
#          age_gr,
#          age_gr2,
#          age_gr3,
#          ir_100000,
#          ir_100000_lower,
#          ir_100000_upper) %>% 
#   filter(strata %in% c("overall", "age_gr2_gender","age_gr_gender", "age_gr3_gender")) %>% 
#   arrange(db, outcome.name)
# writeData(wb, sheet = "All", Network.IR.for.xlsx,rowNames = FALSE)
# saveWorkbook(wb, 
#              here("data", paste0("Network.IR", ".xlsx")), overwrite = TRUE) 


library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb=wb, sheetName = "All")
a<-Network.IR %>% 
  filter(strata=="age_gr3_gender") %>% 
  select(pop, pop.type,
         n, years, events, ir_100000, age_gr3, gender, outcome.name,
         time.window, 
         prior.obs.required)
writeData(wb, sheet = "All", a,rowNames = FALSE)
saveWorkbook(wb,
             here("data", paste0("IncidenceRatesToStandardise", ".xlsx")), overwrite = TRUE)


wb <- createWorkbook()
addWorksheet(wb=wb, sheetName = "All")
a<-Network.IR
# %>% 
#   select(pop, pop.type,
#          n, years, events, ir_100000, age_gr3, gender, outcome.name,
#          time.window, 
#          prior.obs.required)
writeData(wb, sheet = "All", a,rowNames = FALSE)
saveWorkbook(wb,
             here("data", paste0("IncidenceRates", ".xlsx")), overwrite = TRUE)


## save standardized IRR to excel, all vaccines and covid. 


SIR_all <-  COVID_PCR_background.pop.IRS %>% 
  mutate(general.pop.visit = "NO", exposure.group = "SARS-CoV-2 PCR positive test" ) %>%
  bind_rows( mutate(COVID_PCR_background.pop.visit.IRS,general.pop.visit = "YES", exposure.group = "SARS-CoV-2 PCR positive test")) %>%
  bind_rows( mutate(AZ_background.pop.IRS,general.pop.visit = "NO", exposure.group = "Vaccinated with AstraZeneca")) %>%
  bind_rows( mutate(AZ_background.pop.visit.IRS,general.pop.visit = "YES", exposure.group = "Vaccinated with AstraZeneca")) %>%
  bind_rows( mutate(Pf_background.pop.IRS,general.pop.visit = "NO", exposure.group = "Vaccinated with Pfizer-Biontech")) %>%
  bind_rows( mutate(Pf_background.pop.visit.IRS,general.pop.visit = "YES", exposure.group = "Vaccinated with Pfizer-Biontech")) 
  
wb <- createWorkbook()
addWorksheet(wb=wb, sheetName = "All")
a<-SIR_all
writeData(wb, sheet = "All", a, rowNames = FALSE)
saveWorkbook(wb,
             here("data", paste0("SIR_ALL", ".xlsx")), overwrite = TRUE)

                                        