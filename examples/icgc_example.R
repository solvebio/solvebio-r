
library(solvebio)

icgc_donor = Dataset.query('ICGC/2.0.0-21/Donor', paginate=TRUE,limit = 10000)

interval_size = 90
total_interval_to_follow = 1825
#filter by donor survival time for the control group
filter1 = '[["donor_survival_time__gt",0]]' 
#filter for the survival time and and project code to get the the query for PACA
filter2 = '[["project_code__prefix","PACA"],["donor_survival_time__gt",0]]'
#run the initial queries for both datasets
icgc_donor1 = Dataset.query('ICGC/2.0.0-21/Donor', paginate=TRUE,limit = 10000,filters = filter1) #control dataset
icgc_donor2 = Dataset.query('ICGC/2.0.0-21/Donor', paginate=TRUE,limit = 10000,filters = filter2) #PACA dataset
#get the total record count for both datasets
count1 = dim(icgc_donor1)[1]
count2 = dim(icgc_donor2)[1]

#create the x and y vectors to be plotted for both datasets
# x will be for time, which starts at 0 (as the initial filter)
# y will be for percent survival, which starts at 100%
f1_x = c(0)
f1_y = c(100)
f2_x = c(0)
f2_y = c(100)
#fill in the vectors with values 
for(day in seq(interval_size,total_interval_to_follow,interval_size)) {
  #control dataset
  f1 = list(list("donor_survival_time__gt",day)) #filter the query depending on the interval time given (day)
  q1 = Dataset.query('ICGC/2.0.0-21/Donor', paginate=TRUE,limit = 10000,filters = f1) #control dataset
  c1 = dim(q1)[1]
  #get the % total survival at current interval
  f1_percent_alive = (100*c1/count1)
  # populate the control x and y vectors with values for the current interval
  f1_x  <- c(f1_x, day)
  f1_y  <- c(f1_y, f1_percent_alive)
  # same PACA dataset
  f2 = list(list("project_code__prefix","PACA"),list("donor_survival_time__gt",day)) # add filter for PACA
  q2 = Dataset.query('ICGC/2.0.0-21/Donor', paginate=TRUE,limit = 10000,filters = f2)
  c2 = dim(q2)[1]
  f2_percent_alive = (100*c2/count2)
  f2_x  <- c(f2_x, day)
  f2_y  <- c(f2_y, f2_percent_alive)
}

plot(f1_x,f1_y,t="S",col="blue",main = "Kaplan-Meier Survival Curve",xlab="Time",ylab="Percent survival",xaxt='n')
axis(side=1, at=seq(0, total_interval_to_follow, by=interval_size))
lines(f2_x,f2_y,t="S",col="orange")
