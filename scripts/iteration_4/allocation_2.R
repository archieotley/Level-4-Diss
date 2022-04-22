
pacman::p_load(dplyr,tidyr,lubridate,purrr)
n.rooms <- 7
room.conc <- data.frame(conc.P=runif(n=7,min=0,max=700),
                        conc.C=runif(n=7,min=0,max=700),
                        room.letter=LETTERS[1:7],
                        occupied=rep("No"),
                        occupied.until=rep("00:00:00"))

clinicians <- data.frame(clinicians=c("Ian","Dan"),is.free=rep("Yes"),binary.is.free=1)


patient_roster<- read.csv("patient_roster.csv")
patient_roster<-patient_roster %>%
  filter(Patient!="Fake Patient")

patient.time.in <- patient_roster$Appointment.Start.Time



patient.date.in <- patient_roster$Appointment.start.date #%>%

patient.time.out <- patient_roster$Appointment.End.Time

appointment.length <- patient_roster$Appointment.length #%>%


patient.infection <-patient_roster$Infection.type #%>%

patient.roster <- data.frame(
  ID=seq(1:NROW(patient_roster)),
  patient.time.in=lubridate::dmy_hms(paste(patient.date.in,patient.time.in)),
  patient.time.out=lubridate::dmy_hms(paste(patient.date.in,patient.time.out)),
  patient.infection=patient.infection,
  seen.yet=rep("No"),
  binary.seen.yet=0,
  room=0)

#Check if rooms have clashes
patient.roster <- patient.roster %>%
  ungroup() %>% 
  mutate(clinician=rep(c("Ian","Dan"),5)) %>% 
  group_by(clinician) %>% 
  mutate(clash=case_when(patient.time.in < lag(patient.time.out,1)~"Wait",
                         TRUE~"No Clash")) %>% 
  ungroup()

#Main for loop
#TODO need to track if clinician is available or not (current model assumes no break for clinicians
#TODO incorporate ODE model
#TODO incorporate variable fallow time #Just add a patient to the roster with NA.
#TODO Allow multiple clinicians to work simultaneously - Currently only assumes 1 which makes the wait/clash in patient.roster. 
#TODO incorporate variability in output for Monte Carlo
#TODO add room variability
#TODO change numbers to reflect csv. file size (18 not 12)

# set up a data.frame to store room concentrations after each patient has left.
room.conc.storage <- data.frame()

while(sum(patient.roster$binary.seen.yet,na.rm=TRUE)<10){
  
  
  for(i in 1:NROW(patient.roster)){
    
    print(paste("Allocation for patient",i))
    
    if(patient.roster[i,]$seen.yet=="No"){ #DMight be a more efficient way of doing this
      
      
      if(patient.roster[i,]$patient.infection=="P"){ #Patients with P
        
        #Arrange room concentrations and filter occupied rooms or conc.C<300
        
        room.conc %>% 
          # filter(occupied=="No") %>% #Don't filter for occupied room as we don't unoccupy yet
          filter(conc.C<300) %>% 
          arrange(desc(conc.C))->r.temp.conc
        
        #FIXME forcing patient into next available room if the best room is occupied
        if(i>1 & NROW(r.temp.conc)>0){
          if(patient.roster$room[i-1]==r.temp.conc %>% slice() %>% pull(room.letter) & patient.roster$clash[i]=="Wait"){
            
            #next #Don't need to skip, just need to put patient in next best room
            if(NROW(r.temp.conc)==0|sum(clinicians$binary.is.free)<1){
              #TODO add in an error message that tells the scheduler what time it has moved the patient to
              
              print("No room or clinician available") 
              #Don't need a next extraction here because it just doesn't put a 1 in the been.seen column
            }else{
              
              # if(i>1){
              #   if(patient.roster$patient.time.in[i]<patient.roster$patient.time.out[i-1]){
              #     
              #   }else{}
              # }else{}
              
              print(paste("Allocated room:",r.temp.conc %>% slice(n()-1) %>% pull(room.letter) ))
              r.temp.conc %>% slice(n()-1) %>% pull(room.letter)->temp.letter
              
              #Update the data.frame with an occupied room
              #This is redundant but could be used for tracking occupancy time
              room.conc %>% 
                mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) %>% 
                mutate(occupied.until=replace(occupied,room.letter==temp.letter,patient.roster[i,] %>% pull(patient.time.out)))->room.conc
              
              #Upadte the patient roster if the they've been seen
              patient.roster %>% 
                mutate(seen.yet=replace(seen.yet,ID==i,"Yes")) %>% 
                mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1)) %>% 
                mutate(room=replace(room,ID==i,temp.letter))->patient.roster
            }
            
            #FIXME Placeholder for the ODE but for the moment C just decrease by some random value
            #FIXME Placeholder for the ODE but for the moment P just increases by some random value
            
            V<- 31.9+(runif(1)*11.8)
            lambda<- runif(1, min = 30, max = 90)
            logE <- runif(1,min = 8, max = 13)
            E <- (10^(logE))/3600
            
            room.conc %>% 
              if(patient.roster[i,]$patient.infection=="C"){
                conc.C->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
                conc.P->C01*exp(-lambda*appointment.length*3600)
              } 
            else if (patient.roster[i,]$patient.infection=="P"){
              conc.P->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
              conc.C->C01*exp(-lambda*appointment.length*3600)
            }
            else {}
            
            # mutate(across(conc.C,~.x*runif(1,min=0,max=1))) %>% #reduces C but increases P. Currently reduces all the rooms even if they are used concurrently
            #   mutate(across(conc.P,~.x/runif(1,min=0,max=1)))->room.conc
            
            #Stores final concentrations of the room after each patient
            room.conc.storage %>% 
              bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
            
            #Update the data.frame with an un.occupied room
            # room.conc %>%
            #   mutate(occupied.until=patient.time.out)->room.conc
            # mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
            
            # appointment.time=appointment.time+minutes(5)
            
          }else{
            print("I'm allocating because no clash")
            #next #Don't need to skip, just need to put patient in next best room
            if(NROW(r.temp.conc)==0|sum(clinicians$binary.is.free)<1){
              print("No room or clinician available") 
              #Don't need a next extraction here because it just doesn't put a 1 in the been.seen column
            }else{
              
              # if(i>1){
              #   if(patient.roster$patient.time.in[i]<patient.roster$patient.time.out[i-1]){
              #     
              #   }else{}
              # }else{}
              
              print(paste("Allocated room:",r.temp.conc %>% slice(n()) %>% pull(room.letter) ))
              r.temp.conc %>% slice(n()) %>% pull(room.letter)->temp.letter
              
              #Update the data.frame with an occupied room
              #This is redundant but could be used for tracking occupancy time
              room.conc %>% 
                mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) %>% 
                mutate(occupied.until=replace(occupied,room.letter==temp.letter,patient.roster[i,] %>% pull(patient.time.out)))->room.conc
              
              #Upadte the patient roster if the they've been seen
              patient.roster %>% 
                mutate(seen.yet=replace(seen.yet,ID==i,"Yes")) %>% 
                mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1)) %>% 
                mutate(room=replace(room,ID==i,temp.letter))->patient.roster
            }
            
            #FIXME Placeholder for the ODE but for the moment C just decrease by some random value
            #FIXME Placeholder for the ODE but for the moment P just increases by some random value
            
            room.conc %>% 
              if(patient.roster[i,]$patient.infection=="C"){
                conc.C->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
                conc.P->C01*exp(-lambda*appointment.length*3600)
              }
            else if (patient.roster[i,]$patient.infection=="P"){
              conc.P->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
              conc.C->C01*exp(-lambda*appointment.length*3600)
            }
            else {}
            # mutate(across(conc.C,~.x*runif(1,min=0,max=1))) %>% #reduces C but increases P. Currently reduces all the rooms even if they are used concurrently
            # mutate(across(conc.P,~.x/runif(1,min=0,max=1)))->room.conc
            # 
            #Stores final concentrations of the room after each patient
            room.conc.storage %>% 
              bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
            
            #Update the data.frame with an un.occupied room
            # room.conc %>%
            #   mutate(occupied.until=patient.time.out)->room.conc
            # mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
            
            # appointment.time=appointment.time+minutes(5)
            
          }
        }else{print("I'm allocating because i=1")
          
          if(NROW(r.temp.conc)==0|sum(clinicians$binary.is.free)<1){
            print("No room or clinician available") 
            
          }else{
            
            print(paste("Allocated room:",r.temp.conc %>% slice(n()) %>% pull(room.letter) ))
            r.temp.conc %>% slice(n()) %>% pull(room.letter)->temp.letter
            
            #Update the data.frame with an occupied room
            #This is redundant but could be used for tracking occupancy time
            room.conc %>% 
              mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) %>% 
              mutate(occupied.until=replace(occupied,room.letter==temp.letter,patient.roster[i,] %>% pull(patient.time.out)))->room.conc
            
            #Upadte the patient roster if the they've been seen
            patient.roster %>% 
              mutate(seen.yet=replace(seen.yet,ID==i,"Yes")) %>% 
              mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1)) %>% 
              mutate(room=replace(room,ID==i,temp.letter))->patient.roster
            
            #FIXME Placeholder for the ODE but for the moment C just decrease by some random value
            #FIXME Placeholder for the ODE but for the moment P just increases by some random value
            
            room.conc %>% 
              if(patient.roster[i,]$patient.infection=="C"){
                conc.C->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
                conc.P->C01*exp(-lambda*appointment.length*3600)
              } 
            else if (patient.roster[i,]$patient.infection=="P"){
              conc.P->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
              conc.C->C01*exp(-lambda*appointment.length*3600)
            }
            else {}
            # mutate(across(conc.C,~.x*runif(1,min=0,max=1))) %>% #reduces C but increases P. Currently reduces all the rooms even if they are used concurrently
            # mutate(across(conc.P,~.x/runif(1,min=0,max=1)))->room.conc
            
            #Stores final concentrations of the room after each patient
            room.conc.storage %>% 
              bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
            
            #Update the data.frame with an un.occupied room
            # room.conc %>%
            #   mutate(occupied.until=patient.time.out)->room.conc
            # mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
            
            # appointment.time=appointment.time+minutes(5)
          }
          
          
        }
        
        
        
        
      }
      else if(patient.roster[i,]$patient.infection=="C"){ #Patients with C
        
        #Arrange room concentrations and filter occupied rooms or conc.C<300
        
        room.conc %>% 
          # filter(occupied=="No") %>% #Don't filter for occupied room as we don't unoccupy yet
          filter(conc.P<500) %>% 
          arrange(desc(conc.P))->r.temp.conc
        
        #FIXME forcing patient into next available room if the best room is occupied
        if(i>1& NROW(r.temp.conc)>0){
          if(patient.roster$room[i-1]==r.temp.conc %>% slice(n()) %>% pull(room.letter) & patient.roster$clash[i]=="Wait"){
            ##AGO{stop("Schedule is unviable, please reschedule appointments")}#########
            
            #next #Don't need to skip, just need to put patient in next best room
            if(NROW(r.temp.conc)==0|sum(clinicians$binary.is.free)<1){
              print("No room or clinician available") 
              #Don't need a next extraction here because it just doesn't put a 1 in the been.seen column
            }else{
              
              # if(i>1){
              #   if(patient.roster$patient.time.in[i]<patient.roster$patient.time.out[i-1]){
              #     
              #   }else{}
              # }else{}
              
              print(paste("Allocated room:",r.temp.conc %>% slice(n()-1) %>% pull(room.letter) ))
              r.temp.conc %>% slice(n()-1) %>% pull(room.letter)->temp.letter
              
              #Update the data.frame with an occupied room
              #This is redundant but could be used for tracking occupancy time
              room.conc %>% 
                mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) %>% 
                mutate(occupied.until=replace(occupied,room.letter==temp.letter,patient.roster[i,] %>% pull(patient.time.out)))->room.conc
              
              #Upadte the patient roster if the they've been seen
              patient.roster %>% 
                mutate(seen.yet=replace(seen.yet,ID==i,"Yes")) %>% 
                mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1)) %>% 
                mutate(room=replace(room,ID==i,temp.letter))->patient.roster
            }
            
            #FIXME Placeholder for the ODE but for the moment C just decrease by some random value
            #FIXME Placeholder for the ODE but for the moment P just increases by some random value
            
            room.conc %>% 
              if(patient.roster[i,]$patient.infection=="P"){
                conc.P->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
                conc.C->C01*exp(-lambda*appointment.length*3600)
              } 
            else if (patient.roster[i,]$patient.infection=="C"){
              conc.C->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
              conc.P->C01*exp(-lambda*appointment.length*3600)
            }
            else {}
            # mutate(across(conc.P,~.x*runif(1,min=0,max=1))) %>% #reduces C but increases P. Currently reduces all the rooms even if they are used concurrently
            # mutate(across(conc.C,~.x/runif(1,min=0,max=1)))->room.conc
            # 
            #Stores final concentrations of the room after each patient
            room.conc.storage %>% 
              bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
            
            #Update the data.frame with an un.occupied room
            # room.conc %>%
            #   mutate(occupied.until=patient.time.out)->room.conc
            # mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
            
            # appointment.time=appointment.time+minutes(5)
            
          }else{
            print("I'm allocating because no clash")
            #next #Don't need to skip, just need to put patient in next best room
            if(NROW(r.temp.conc)==0|sum(clinicians$binary.is.free)<1){
              print("No room or clinician available") 
              #Don't need a next extraction here because it just doesn't put a 1 in the been.seen column
            }else{
              
              # if(i>1){
              #   if(patient.roster$patient.time.in[i]<patient.roster$patient.time.out[i-1]){
              #     
              #   }else{}
              # }else{}
              
              print(paste("Allocated room:",r.temp.conc %>% slice(n()) %>% pull(room.letter) ))
              r.temp.conc %>% slice(n()) %>% pull(room.letter)->temp.letter
              
              #Update the data.frame with an occupied room
              #This is redundant but could be used for tracking occupancy time
              room.conc %>% 
                mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) %>% 
                mutate(occupied.until=replace(occupied,room.letter==temp.letter,patient.roster[i,] %>% pull(patient.time.out)))->room.conc
              
              #Upadte the patient roster if the they've been seen
              patient.roster %>% 
                mutate(seen.yet=replace(seen.yet,ID==i,"Yes")) %>% 
                mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1)) %>% 
                mutate(room=replace(room,ID==i,temp.letter))->patient.roster
            }
            
            #FIXME Placeholder for the ODE but for the moment C just decrease by some random value
            #FIXME Placeholder for the ODE but for the moment P just increases by some random value
            
            room.conc %>% 
              if(patient.roster[i,]$patient.infection=="P"){
                conc.P->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
                conc.C->C01*exp(-lambda*appointment.length*3600)
              } 
            else if (patient.roster[i,]$patient.infection=="C"){
              conc.C->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
              conc.P->C01*exp(-lambda*appointment.length*3600)
            }
            else {}
            # mutate(across(conc.P,~.x*runif(1,min=0,max=1))) %>% #reduces C but increases P. Currently reduces all the rooms even if they are used concurrently
            # mutate(across(conc.C,~.x/runif(1,min=0,max=1)))->room.conc
            
            #Stores final concentrations of the room after each patient
            room.conc.storage %>% 
              bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
            
            #Update the data.frame with an un.occupied room
            # room.conc %>%
            #   mutate(occupied.until=patient.time.out)->room.conc
            # mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
            
            # appointment.time=appointment.time+minutes(5)
            
          }
        }else{print("I'm allocating because i=1")
          
          if(NROW(r.temp.conc)==0|sum(clinicians$binary.is.free)<1){
            print("No room or clinician available") 
            
          }else{
            
            print(paste("Allocated room:",r.temp.conc %>% slice(n()) %>% pull(room.letter) ))
            r.temp.conc %>% slice(n()) %>% pull(room.letter)->temp.letter
            
            #Update the data.frame with an occupied room
            #This is redundant but could be used for tracking occupancy time
            room.conc %>% 
              mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) %>% 
              mutate(occupied.until=replace(occupied,room.letter==temp.letter,patient.roster[i,] %>% pull(patient.time.out)))->room.conc
            
            #Upadte the patient roster if the they've been seen
            patient.roster %>% 
              mutate(seen.yet=replace(seen.yet,ID==i,"Yes")) %>% 
              mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1)) %>% 
              mutate(room=replace(room,ID==i,temp.letter))->patient.roster
            
            #FIXME Placeholder for the ODE but for the moment C just decrease by some random value
            #FIXME Placeholder for the ODE but for the moment P just increases by some random value
            
            room.conc %>% 
              if(patient.roster[i,]$patient.infection=="P"){
                conc.P->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
                conc.C->C01*exp(-lambda*appointment.length*3600)
              } 
            else if (patient.roster[i,]$patient.infection=="C"){
              conc.C->E/(V*lambda)*(1-exp(-lambda*(appointment.length*3600)))
              conc.P->C01*exp(-lambda*appointment.length*3600)
            }
            else {}
            # mutate(across(conc.P,~.x*runif(1,min=0,max=1))) %>% #reduces C but increases P. Currently reduces all the rooms even if they are used concurrently
            # mutate(across(conc.C,~.x/runif(1,min=0,max=1)))->room.conc
            
            #Stores final concentrations of the room after each patient
            room.conc.storage %>% 
              bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
            
            #Update the data.frame with an un.occupied room
            # room.conc %>%
            #   mutate(occupied.until=patient.time.out)->room.conc
            # mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
            
            # appointment.time=appointment.time+minutes(5)
          }
          
          
        }
        
        
        
        
        
      }else{ #Patients with no previous infection
        
        room.conc %>% 
          filter(occupied=="No")->r.temp.conc
        
        print(paste("Allocated room:",r.temp.conc %>% slice(n()) %>% pull(room.letter) ))
        r.temp.conc %>% slice(n()) %>% pull(room.letter)->temp.letter
        
        #Update the data.frame with an occupied room
        room.conc %>% 
          mutate(occupied=replace(occupied,room.letter==temp.letter,"Yes")) ->room.conc
        
        #Update the patient roster if the they've been seen
        patient.roster %>% 
          mutate(seen.yet=replace(seen.yet,ID==i,"Yes"))%>% 
          mutate(binary.seen.yet=replace(binary.seen.yet,ID==i,1))%>% 
          mutate(room=replace(room,ID==i,temp.letter))->patient.roster
        
        #FIXME Placeholder for the ODE but for the moment just decrease by random factor
        room.conc %>% 
          if(patient.roster[i,]$patient.infection=="P"){
            conc.P->C01*exp(-lambda*appointment.length*3600)
          } 
        else if (patient.roster[i,]$patient.infection=="C"){
          conc.C->C01*exp(-lambda*appointment.length*3600)
        }
        else {}
        # mutate(across(conc.C:conc.P,~.x*runif(1,min=0,max=1)))->room.conc
        
        #Stores final concentrations of the room after each patient
        room.conc.storage %>% 
          bind_rows(room.conc %>% filter(room.letter==temp.letter) %>% select(-occupied))->room.conc.storage
        
        #Update the data.frame with an occupied room
        room.conc %>% 
          mutate(occupied=replace(occupied,room.letter==temp.letter,"No")) ->room.conc
        
      }
    }else{
      print("Still trying to fit patients in")
    }
    
  }
  
}
patient.roster
room.conc.storage
# Check infection status of patient P, C or NA
# if P then rank rooms by conc.C # equivalent to filtering by occupied=="NO" & conc.C<10
# 
#   Check if lowest conc.C room is allocated & conc.C<10
#     If not allocated then allocate and update room.allocation status to "Yes"
#   if allocated then check next lowest
# 
# else  if C then rank by conc.C
#     Check if lowest room & conc.C < 2 & is allocated 
#     If not allocated then allocate and update room.allocation status to "Yes"
#     if allocated then check next lowest
# else allocate any free
# end
# end
