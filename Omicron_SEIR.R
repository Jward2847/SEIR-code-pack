#Dependencies 
library(rmarkdown)
library(devtools)
devtools::install_github("cbdrh/covoid",build_vignettes = TRUE)
library(covoid)
library(ggplot2)
library(scales)
library(readxl)
library(ggpubr)

        
#####################################################################################################
#Importing population demographics

#import contact matrix and age distribution
cm_oz <- import_contact_matrix("United Kingdom of Great Britain","general")  #Contact matrix for GB
nJ <- ncol(cm_oz)

p_age_oz <- import_age_distribution("United Kingdom") #Age distribution for GB

# Contact rates separated into General, school and work 
cm_oz_all <- import_contact_matrix("United Kingdom of Great Britain","general") #General 
cm_oz_sch <- import_contact_matrix("United Kingdom of Great Britain","school")  #School
cm_oz_wk <- import_contact_matrix("United Kingdom of Great Britain","work")     #Workplace

#Graph for contact matrix

#General
a <- plot(cm_oz_all) +
  xlab("Individual Age")+
  ylab("Age of Contacts") 

#School
b <- plot(cm_oz_sch) +
  xlab("Individual Age")+
  ylab("Age of Contacts") 
  
#Work
 c <- plot(cm_oz_wk) +
  xlab("Individual Age")+
  ylab("Age of Contacts")
 
#Arrange into graph (fig.2)
gridExtra::grid.arrange(a,b,c, nrow=2, ncol=2)


#Age distribution figure (fig.3)
plot(p_age_oz) +
  xlab("Age")+
  ylab("Proportion of population")+
  theme_bw() +
  theme(text = element_text(size = 20), axis.text = element_text(size = 15)) 

########################################################################################################

#Effect of vaccination

#  reduce susceptibility in different age groups to simulate vaccine booster 
im <- matrix(1,ncol=16,nrow=16)

im[,15:16] <- 0.55 # over 70s less infectious 
im[15:16,] <- 0.25 #over 70s less susceptible

im[,5:14] <- 0.58 #20-69 less infectious
im[5:14,] <- 0.9 #20-69 less susceptible 


#####################################################################################################
# initial conditions 
S <- p_age_oz*5.96e7 #starting population
E <- rep(0,length(S)) #Starting exposed
I <- c(0,0,0,0,ceiling(81453.3*(1)^(1:12))) #Starting infected
R <- rep(0,length(S)) #Starting recoverd

#####################################################################################################
#Scenario 01- No intervention

#Simulation
param <- seir_c_param(R0 = 2.4 ,gamma = 0.17,sigma=0.31,cm=cm_oz,dist=p_age_oz, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res01 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 02 - Plan B

#Mask mandate
mask <- transmission_intervention(start = 11,stop = 61, reduce = 0.83)

#Seperating contact matrix into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#Plotting contact matrix for general and work
p_all_wk <- plot(cm_oz_all_wk) + labs(title = "General") +
  theme(axis.text.x = element_text(size=6, angle=0),
        axis.text.y = element_text(size=6))
p_wk <- plot(cm_oz_wk) + labs(title = "Work") +
  theme(axis.text.x = element_text(size=6, angle=0),
        axis.text.y = element_text(size=6))
gridExtra::grid.arrange(p_all_wk,p_wk,ncol=2)


#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Interventions
#"Optional" work from home (WFH)
planB<- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75)) #Work from home

#Simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= planB,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res02 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 03 - Plan B+

#Extend mask wearing 
mask_full <- transmission_intervention(start = 11,stop = 175, reduce = 0.83)

#Seperating contact settings
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting

cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Compulsory work from home
planB_no_wk <- list(wk=contact_intervention(start = 11,stop = 175,reduce = 0.2)) #Work from home

#Simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= planB_no_wk,
                      transmission_intervention = mask_full, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res03 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 04 - Plan B + 25% social distancing

#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Optional WFM and 25% social distancing 
B_mild_SD <- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
            all=contact_intervention(start = 11,stop = 61,reduce = 0.75))

#Simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= B_mild_SD,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res04 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 05 -Plan B and imposition of 50% social distancing measures 


#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk


#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

##Optional WFM and 50% social distancing 
B_mod_SD <- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
                  all=contact_intervention(start = 11,stop = 61,reduce = 0.5))

#Simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= B_mod_SD,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res05 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 06 - Plan B and imposition of school closures until the end of the school holidays

#Separate contact into general, school and work
cm_oz_all_sch <- cm_oz_all - cm_oz_sch - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
#contact matrix
cm_sch <- list(all = cm_oz_all_sch, sch = cm_oz_sch, wk = cm_oz_wk)

##Optional WFM and school closures 
noskool <- list(sch=contact_intervention(start = 11,stop = 38,reduce = 0.2),
                wk=contact_intervention(start = 11,stop = 61,reduce = 0.75))

#Simulation
param <- seir_c_param(R0 = 2.4 ,gamma = 0.17,sigma=0.31,cm=cm_sch,dist=p_age_oz,
                      contact_intervention = noskool,
                      transmission_intervention = mask)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res06 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 07 - Plan B and 2 week circuit breaker lockdown before New Year

#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Optional WFM and circuit breaker
CB_LD_1 <- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
                 all=contact_intervention(start = 20,stop = 34,reduce = 0.25))

#simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= CB_LD_1,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res07 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 08 - Plan B and 2 week circuit breaker lockdown after New Year

#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Optional WFM and circuit breaker
CB_LD_2 <- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
                all=contact_intervention(start = 35,stop = 49,reduce = 0.25))

#Simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= CB_LD_2,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res08 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#####################################################################################################
#Scenario 09 - Plan B and four week lockdown in the new year

#masking wearing + LD
mask_LD_1 <- transmission_intervention(start = 11,stop = 71, reduce = 0.83)

#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Optional WFM and Lockdown
LD_1 <- list(wk=contact_intervention(start = 11,stop = 71,reduce = 0.75),
                all=contact_intervention(start = 35,stop = 71,reduce = 0.25))

#Run simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= LD_1,
                      transmission_intervention = mask_LD_1, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res09 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

########################################################################################################
#Scenario 10 -Plan B and four week lockdown with continuous plan B measures

#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)

#Optional WFM and Lockdown
LD_2 <- list(wk=contact_intervention(start = 11,stop = 175,reduce = 0.75),
             all=contact_intervention(start = 35,stop = 71,reduce = 0.25))

#Run simulation 
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= LD_2,
                      transmission_intervention = mask_full, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res10 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)



########################################################################################################
#Circuit breaker timing

#Separate contact into general and work
cm_oz_all_wk <- cm_oz_all - cm_oz_wk

#add in time varying interventions using create_intervention to each setting
cm_wk <- list(all = cm_oz_all_wk, wk = cm_oz_wk)


#Circuit breakers

#day 25-39
CB_1<- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
                all=contact_intervention(start = 25,stop = 39,reduce = 0.25))

#day 30-44
CB_2<- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
            all=contact_intervention(start = 30,stop = 44,reduce = 0.25))

#day 35-49
CB_3<- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
            all=contact_intervention(start = 35,stop = 49,reduce = 0.25))

#day 40-54
CB_4<- list(wk=contact_intervention(start = 11,stop = 61,reduce = 0.75),
            all=contact_intervention(start = 40,stop = 54,reduce = 0.25))


#Simulations 

#day 25-39 sim (11)
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= CB_1,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res11 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#day 30-44 sim (12)
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= CB_2,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res12 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#day 35-49 sim (13)
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= CB_3,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res13 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)

#day 40-54 sim (14)
param <- seir_c_param(R0 = 2.4,gamma = 0.17,sigma=0.31,cm=cm_wk,dist=p_age_oz,
                      contact_intervention= CB_4,
                      transmission_intervention = mask, im = im)
state0 <- seir_c_state0(S = S,E =E,I = I,R = R)
res14 <- simulate_seir_c(t = 175,state_t0 = state0,param = param)


########################################################################################################
#Figures 

# 1) No Intervention (baseline model) - Fig.4
ggplot(res01$epi, aes(x=t)) + 
  geom_line(aes(y = S), color = "Purple", size = 2) + 
  geom_line(aes(y = E), color="blue", size = 2) +
  geom_line(aes(y = I), color="green", size = 2) +
  geom_line(aes(y = R), color="orange", size = 2) +
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Number of people") +
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))

# 2) Plan B vs No intervention vs IRL  - Fig.5

#Import incidence data
setwd("") 
incidence1 <- read_excel("incidence.xlsx")

ggplot(NULL, aes(t, I)) + 
  geom_line(data = res01$epi,color="Red", size = 2) + #no intervention
  geom_line(data=res02$epi, color="Dark Blue", size = 2) +  #Plan B
  geom_line(data = incidence1,color="Black", linetype = "dashed") + #IRL data 
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))+
  annotate("rect", xmin = 11, xmax = 61, ymin = 0, ymax = 6811105,
           alpha = .2)



# 3) Plan B vs Plan B + - Fig.6
ggplot(NULL, aes(t, I)) + 
  geom_line(data = res02$epi,color="Dark blue", size = 2) + #Plan B
  geom_line(data = res03$epi, color= "#87CEEB", size = 2) + #Plan B +
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))


# 4) Social distancing - Fig.7
ggplot(NULL, aes(t, I)) + 
  geom_line(data = res02$epi,color="Dark blue", size = 2) + #Plan B
  geom_line(data = res04$epi,color="green", size = 2) + #25% SD
  geom_line(data = res05$epi,color="#bcbc73", size = 2) + #50% SD
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))+
  annotate("rect", xmin = 11, xmax = 61, ymin = 0, ymax = 4068836,
           alpha = .2)


# 5) School closures- Fig.8
ggplot(NULL, aes(t, I)) + 
  geom_line(data = res02$epi,color="Dark blue", size = 2) + #Plan B
  geom_line(data = res06$epi,color="#b11c72", size = 2) + #School closures
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA)) +
  annotate("rect", xmin = 11, xmax = 38, ymin = 0, ymax = 4068836,
           alpha = .2)

# 6) Circuit breaker lockdown - Fig.9

ggplot(NULL, aes(t, I)) + 
  geom_line(data = res02$epi,color="Dark blue", size = 2) + #Plan B
  geom_line(data = res07$epi,color="magenta", size = 2) + #B4 NY
  geom_line(data = res08$epi,color="red", size = 2) + #After NY
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))+
  annotate("rect", xmin = 20, xmax = 34, ymin = 0, ymax = 4068836,
             alpha = .1, fill = "magenta")+
  annotate("rect", xmin = 35, xmax = 49, ymin = 0, ymax = 4068836,
           alpha = .1, fill = "red")
  

# 7) Lockdown - Fig.10
ggplot(NULL, aes(t, I)) + 
  geom_line(data = res02$epi,color="Dark blue", size = 2) + #Plan B
  geom_line(data = res09$epi,color="gray", size = 2) + #LD
  geom_line(data = res10$epi,color="Dark Orange", size = 2) + #LD + Plan B extened
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))+
  annotate("rect", xmin = 35, xmax = 71, ymin = 0, ymax = 4068836,
           alpha = .2)


#Comparison between different interventions

 #Percentage of population infected - Fig.11a


#Name of intervention
all <- c("PlanB", "Plan B+", "25% social distancing","50% social distancing","School closures",
         "Circuit breaker before NY","Circuit breaker after NY","Full lockdown","Full lockdown & extended plan B")

#Percentage of population infected
num_all <- c((max(res02$epi$R)/59600000)*100,(max(res03$epi$R)/59600000)*100,
             (max(res04$epi$R)/59600000)*100,(max(res05$epi$R)/59600000)*100,(max(res06$epi$R)/59600000)*100,
             (max(res07$epi$R)/59600000)*100,(max(res08$epi$R)/59600000)*100,(max(res09$epi$R)/59600000)*100,
             (max(res10$epi$R)/59600000)*100)

#Create data frame
all_int<- data.frame(all, num_all)


#Create graph 
G<- ggdotchart(all_int, x = "all", y = "num_all",
               color = "all",                                # Color by groups
               palette = c("Dark blue", "#87CEEB", "dark green",
                           "#bcbc73","#b11c72","magenta","red","gray","dark orange"), # Custom color palette
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # Add segments from y = 0 to dots
               rotate = TRUE,                                # Rotate vertically
               group = "all",                                # Order by groups
               dot.size = 15,                                 # Large dot size
               label = round(all_int$num_all),                        # Add values as dot labels
               font.label = list(color = "white", size = 14, 
                                 vjust = 0.5),               # Adjust label parameters
               ggtheme = theme_pubr()                        # ggplot2 theme
)


#Add labs
 G + xlab("Intervention") + ylab("Population infected (%)")+
  theme(legend.position="none",text = element_text(size = 20), 
        axis.text = element_text(size = 15))


#Percentage reduction in peak Fig.11b  

#Name of intervention
peak <- c("Plan B+", "25% social distancing","50% social distancing","School closures",
         "Circuit breaker before NY","Circuit breaker after NY","Full lockdown","Full lockdown & extended plan B")

#Percentage reduction in peak
num_peak <- c(36,37,19,11,20,8,8,8)


#Create data frame
all_peak<- data.frame(peak, num_peak)

#Graph 
P<- ggdotchart(all_peak, x = "peak", y = "num_peak",
               color = "peak",                                # Color by groups
               palette = c( "#87CEEB", "dark green",
                           "#bcbc73","#b11c72","magenta","red","gray","dark orange"), # Custom color palette
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # Add segments from y = 0 to dots
               rotate = TRUE,                                # Rotate vertically
               group = "peak",                                # Order by groups
               dot.size = 15,                                 # Large dot size
               label = round(all_peak$num_peak),                        # Add  values as dot labels
               font.label = list(color = "black", size = 14, 
                                 vjust = 0.5),               # Adjust label parameters
               ggtheme = theme_pubr()                        # ggplot2 theme
)

#Add labs
 P + xlab("Intervention") + ylab("Reduction in epidemic peak compared to plan B (%)")+
  theme(legend.position="none",text = element_text(size = 20), 
        axis.text = element_text(size = 15))

################ Scenarios 11-14 figs #########

#Circuit breaker timing graph - Fig.12
ggplot(NULL, aes(t, I)) + 
  geom_line(data = res02$epi,color="Dark blue", size = 2) +
  geom_line(data = res11$epi,color="gold", size = 2) + #day 25
  geom_line(data = res12$epi,color="green", size = 2) + #day 30
  geom_line(data = res13$epi,color="red", size = 2) + #day 35
  geom_line(data = res14$epi,color="black", size = 2) + #day 40
  scale_y_continuous(labels = comma,expand = c(0, 0), limits = c(0, NA))+
  xlab("Time (day)") + ylab("Infected (n)")+
  theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 15)) + 
  scale_x_continuous(breaks = scales::breaks_width(10),expand = c(0, 0), limits = c(0, NA))+
  annotate(geom="segment", x = 25, xend = 25, y = 0, yend = 500000, 
           colour = "gold", size=1.5, linetype="dotted")+ #day 25 Start line
  annotate(geom="segment", x = 39, xend = 39, y = 0, yend = 500000,
           colour = "gold", size=1.5, linetype="dotted")+  #day 25 End line
  annotate(geom="segment", x = 30, xend = 30, y = 0, yend = 500000, 
           colour = "green", size=1.5, linetype="dotted")+ #day 30 Start line
  annotate(geom="segment", x = 44, xend = 44, y = 0, yend = 500000, 
           colour = "green", size=1.5, linetype="dotted")+ #day 30 End line
  annotate(geom="segment", x = 35, xend = 35, y = 3568836, yend = 4068836, 
           colour = "red", size=1.5, linetype="dotted")+ #day 35 Start line
  annotate(geom="segment", x = 49, xend = 49, y = 3568836, yend = 4068836, 
           colour = "red", size=1.5, linetype="dotted")+ #day 35 End line
  annotate(geom="segment", x = 40, xend = 40, y = 3568836, yend = 4068836, 
           colour = "black", size=1.5, linetype="dotted")+ #day 40 Start line
  annotate(geom="segment", x = 54, xend = 54, y = 3568836, yend = 4068836, 
           colour = "black", size=1.5, linetype="dotted") #day 40 End line



#Circuit breaker total infected graph Fig.13a
 
#Name of intervention
dayCB <- c("25", "30","35","40")

#Percentage of population infected
numCB <- c((max(res11$epi$R)/59600000)*100,
           (max(res12$epi$R)/59600000)*100,(max(res13$epi$R)/59600000)*100,(max(res14$epi$R)/59600000)*100)

#Create data frame
CBdf2 <- data.frame(dayCB, numCB)

#Graph
CBt <- ggdotchart(CBdf2, x = "dayCB", y = "numCB",
               color = "dayCB",                                # Color by groups
               palette = c("gold","green","red","black"), # Custom color palette
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # Add segments from y = 0 to dots
               rotate = TRUE,                                # Rotate vertically
               group = "dayCB",                                # Order by groups
               dot.size = 15,                                 # Large dot size
               label = round(CBdf2$numCB),                        # Add  values as dot labels
               font.label = list(color = "white", size = 14, 
                                 vjust = 0.5),               # Adjust label parameters
               ggtheme = theme_pubr()                        # ggplot2 theme
)

#Adding labs
CBt + xlab("Start of circuit breaker (day)") + ylab("Population infected (%)")+
  theme(legend.position="none",text = element_text(size = 20), 
        axis.text = element_text(size = 15))


#reduction in peak for CB timings - Fig.13b

#Name of intervention
peakCB <- c("25", "30","35","40")

#% reduction in peak
CB_num_P <- c(30,19,8,1)

#Create data frame
CB_peak<- data.frame(peakCB, CB_num_P)


#Graph
PeakG <- ggdotchart(CB_peak, x = "peakCB", y = "CB_num_P",
                  color = "peakCB",                                # Color by groups
                  palette = c("gold","green","red","black"), # Custom color palette
                  sorting = "descending",                       # Sort value in descending order
                  add = "segments",                             # Add segments from y = 0 to dots
                  rotate = TRUE,                                # Rotate vertically
                  group = "peakCB",                                # Order by groups
                  dot.size = 15,                                 # Large dot size
                  label = round(CB_peak$CB_num_P),                        # Add  values as dot labels
                  font.label = list(color = "white", size = 14, 
                                    vjust = 0.5),               # Adjust label parameters
                  ggtheme = theme_pubr()                        # ggplot2 theme
)

#Adding labs
PeakG  + xlab("Start of circuit breaker (day)") + ylab("Reduction in epidemic peak compared to plan B (%)")+
  theme(legend.position="none",text = element_text(size = 20), 
        axis.text = element_text(size = 15))




