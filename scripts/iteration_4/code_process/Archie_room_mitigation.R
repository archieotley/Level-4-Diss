

s_f = 1.2 ## Susecptible factor
a_f = 1.5 ## Time factor



### Susepctability factors per pateint
s_p1 = 10
s_p2 = 10
s_p3 = 10
s_p4 = 10
s_p5 = 10
s_p6 = 10



### code will need to know what pathogen type the patient has to better allocate
### patients



### Appointment per patient, in minutes
a_p1 = 30
a_p2 = 100
a_p3 = 20
a_p4 = 70
a_p5 = 60
a_p6 = 120
### these will take place at different points during the day, need code to take
### this in cos other wise not enough rooms for patients. code atm thinks taking
### place at same time



### level of risk
risk_pat=function(s_f, a_f, s_p, a_p ) {
  r_p <-s_p*s_f + a_p*a_f
  return(r_p)
}



r<-data.frame(s_f=s_f, a_f=a_f, s_p=runif(6,0,15), a_p=c(a_p1, a_p2,a_p3, a_p4,a_p5,a_p6))


purrr::pmap_dbl(
  .l = r,
  .f = risk_pat) ->patient_risks

patient_risks  %>% 
  as_tibble() %>% 
  mutate(pat_num=row_number()) %>% 
  arrange(value)%>% 
  mutate(tempCol=row_number())->patient_risks


# Room risk

ts_f=1.2 # Time safe factor
v_f=1.4 # Volume factor



### Room factors, time since room has been deemed safe, in seconds
ts_r1 = 1234
ts_r2 = 2444
ts_r3 = 0
ts_r4 = 0
ts_r5 = 0
ts_r6 = 3455
ts_r7 = 4564



### need code that says if value is zero or less than zero than the mitigation
### level is zero, so it won"t be used. room will also change time till safe, so
### will need code to update



#### volume of room, in m^3
v_r1 = 14
v_r2 = 15
v_r3 = 16
v_r4 = 12
v_r5 = 20
v_r6 = 26
v_r7 = 10



### need term to gauge what previous infection was in the room



#### level of mitigation
room_mit=function(ts_f, v_f, ts_r,v_r ) {
  m_r <- v_r*v_f + ts_r*ts_f
  return(m_r)
}


m<-data.frame(ts_f=ts_f, v_f=v_f, ts_r=runif(7,0,5500), v_r=runif(7,50,150))


purrr::pmap_dbl(
  .l = m,
  .f = room_mit) ->room_mit_df

room_mit_df %>% 
  as_tibble() %>% 
  mutate(room_num=row_number()) %>% 
  arrange(value) %>% 
  mutate(Temp_col=row_number())->room_mit_df



patient_risks %>% 
  right_join(
    #Doesn't allow a room to be used if 0
    room_mit_df %>% filter(value>0),by = "tempCol") 

# TODO Think about whether room safety depends on dose.
# 

#If waiting patient Infection == “P”

#room_Capaccia_conc%>%
  
 # filter(Occupied==“No”)%>%
  
 # arrange(conc)%>%
  
#  filter(row_number()==n())->room_patient_2



#room_roster update with occupied with patient_2





#“No available room”
