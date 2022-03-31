library(dplyr)
### Room Mitigations ################################
### Room factors weightings

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
  m_r <-  v_r*v_f + ts_r*ts_f
  return(m_r)
}  

m_r1 <-room_mit(ts_f, v_f, ts_r1,v_r1)
m_r2 <-room_mit(ts_f, v_f, ts_r2,v_r2)
m_r3 <-room_mit(ts_f, v_f, ts_r3,v_r3)
m_r4 <-room_mit(ts_f, v_f, ts_r4,v_r4)
m_r5 <-room_mit(ts_f, v_f, ts_r5,v_r5)
m_r6 <-room_mit(ts_f, v_f, ts_r6,v_r6)
m_r7 <-room_mit(ts_f, v_f, ts_r7,v_r7)

#m_r1 = v_r1*v_f + ts_r1*ts_f
#m_r2 = v_r2*v_f + ts_r2*ts_f
#m_r3 = v_r3*v_f + ts_r3*ts_f
#m_r4 = v_r4*v_f + ts_r4*ts_f
#m_r5 = v_r5*v_f + ts_r5*ts_f
#m_r6 = v_r6*v_f + ts_r6*ts_f
#m_r7 = v_r7*v_f + ts_r7*ts_f

### Patient risk ######################################
### Patient risk factors weightings
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

r<-data.frame(s_f=s_f, a_f=a_f, s_p=c(s_p1, s_p2, s_p3, s_p4, s_p5,s_p6 ), a_p=c(a_p1, a_p2,a_p3, a_p4,a_p5,a_p6))
r%>%purrr::map_df(.f=risk_pat)

purrr::map(
  .x = 1:nrow(my_tibble),
  .f = ~

###change values 
r_p1 <-risk_pat(s_f, a_f, s_p1, a_p1)
r_p2 <-risk_pat(s_f, a_f, s_p2, a_p2)
r_p3 <-risk_pat(s_f, a_f, s_p3, a_p3)
r_p4 <-risk_pat(s_f, a_f, s_p4, a_p4)
r_p5 <-risk_pat(s_f, a_f, s_p5, a_p5)
r_p6 <-risk_pat(s_f, a_f, s_p6, a_p6)

#ris <- c(r_p)
#mit <- c(m_r)

allocation<-data.frame( r=c(   r_p1,
                           r_p2,
                           r_p3,
                           r_p4,
                           r_p5,
                           r_p6,
                           NA),
                        m=c(
                           m_r1,
                           m_r2,
                           m_r3,
                           m_r4,
                           m_r5,
                           m_r6,
                           m_r7)
)

return(allocation)

allocation_order<- allocation[order(allocation$r_p,allocation$m_r)]

risk<-data.frame(    r_p1,
                     r_p2,
                     r_p3,
                     r_p4,
                     r_p5,
                     r_p6
)

mit<-data.frame(
                     m_r1,
                     m_r2,
                     m_r3,
                     m_r4,
                     m_r5,
                     m_r6,
                     m_r7
                     
)
#randomVals <-  dont know how to work this button
return(risk)
return(mit)


risk_order <- risk[order(risk$r_p)]
mit_order <- mit[order(mit$m_r)]



#r_p1 = s_p1*s_f + a_p1*a_f
#r_p2 = s_p2*s_f + a_p2*a_f
#r_p3 = s_p3*s_f + a_p3*a_f
#r_p4 = s_p4*s_f + a_p4*a_f
#r_p5 = s_p5*s_f + a_p5*a_f
#r_p6 = s_p6*s_f + a_p6*a_f

### function to match highest r_p value to the highest m_r value, need to store
### calculated values of r_p and m_r in order and match values 

### store values as a vector

#ris <- c(r_p1,r_p2,r_p3,r_p4,r_p5,r_p6)

#mit <- d(m_r1,m_r2,m_r3,m_r4,m_r5,m_r6,m_r7)

### order vector values
### potentially could order here with function remove minus numbers, then give 
###value if room not ready a really large negative, no. so it wont get picked

#ris[order(ris)]

#mit[order(mit)]

#x = readline();

# we are converting data from string to
# int and storing in x.
#x = as.integer(x);

# we are sorting vector and printing
# nth element.As this vector is in sorted
# way we will get xth largest number
#print(sort(a,TRUE)[x])



#which.max(ris)
#which.min(ris)

#which.max(mit)
#which.min(mit)

## need to get code to recognise terms, so that function is 100% working, then store values as vector??
## then match highest values and print the values




