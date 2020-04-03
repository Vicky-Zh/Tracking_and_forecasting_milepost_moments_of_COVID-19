#=======================================
# Prediction of turning points
# Author: Yanwen Zhang
# Date: Apr 2, 2020
# Description: In the paper <Tracking_and_forecasting_milepost_moments_of_the_epidemic_in_the_early_outbreak__framework_and_applications_to_the_COVID_19>, we proposed a method to predict "turning points", whose main idea is using the change velocity of infection rate (InfectionRateVelocity) and the change velocity of completion rate(RemovedRateVelocity) to forcast newly diagnoses cases and number of cases treated in the hospital in the future. Here, we proposed one of the algorithms to calculate the change rate and then make the prediction, which is the method we used in our paper mentioned above. At last, we offer a simple example to implement this method.
#=======================================



Iconicfun<-function(wd){
  #=====================================
  # Compute the iconic indicators
  #
  # Args:
  #   wd: dataframe with data and four variables, i.e. 
  #       the cumulative confirmed cases up to the given day t,
  #       the daily confirmed cases at day t, 
  #       the daily recovered ones and the daily deaths at day t，
  # Returns: dataframe with variables
  #   date: the exact day in the formate "%y-%m-%d" as a character
  #   confirmed: the daily confirmed cases at the given date
  #   recovered: the daily recovered cases at the given date
  #   deaths: the daily deaths at the given date
  #   cumconfirmed: the cumulative confirmed cases
  #   inhospitals: The number of infectious cases in hospital
  #   infectionrate: The daily infection rate
  #   removedrate: the daily removed rate
  n<-dim(wd)[1]
  date<-as.Date(wd[,1])
  cumconfirmed<-wd[,2]#cumulative confirmed cases
  confirmed<-wd[,3]
  recovered<-wd[,4]
  deaths<-wd[,5]
  inhospitals<-cumconfirmed-cumsum(recovered+deaths)#in-hospitals
  infectionrate<-confirmed[-1]/inhospitals[-n]#the daily infection rate
  removedrate<-(recovered+deaths)[-1]/inhospitals[-n]#the daily removed rate
  #return results
  result<-data.frame(date=date[-1],
       cumconfirmed=cumconfirmed[-1],
       confirmed=confirmed[-1],
       recovered=recovered[-1],
       deaths=deaths[-1],
       inhospitals=inhospitals[-1],
       infectionrate=infectionrate,
       removedrate=removedrate)
  return(result)
}



CalculateVelocity<-function (date, confirmed, inhospitals, infectionrate, removedrate, M, T){
  #=====================================
  # Compute the velocity of infection rate K change and completion rate I change.
  #
  # Args:
  #   date: the exact day in the formate "%y-%m-%d" as a character
  #   confirmed: the daily confirmed cases at the given date
  #   inhospitals: The number of infectious cases in hospital
  #   infectionrate: The daily infection rate
  #   removedrate: the daily removed rate
  #   M: the selection of time window.
  #   T: the selection of begining time, 
  #      which must be in the formate "%y-%m-%d" as a character.
  #
  # Returns:
  #   A list contains InfectionRateVelocity, RemovedRateVelocity, 
  #   and the final T.removed and M.removed used to calculate RemovedRateVelocity,
  #   which may be needed in later calculation.
  #=====================================
  f=data.frame(date=date,
               confirmed=confirmed,
               inhospitals=inhospitals,
               infectionrate=infectionrate,
               removedrate=removedrate)
  f$date=as.Date(f$date)
  
  # to initialize the t and m for real calculation
  T.infection=which(f==T)
  T.removed=which(f==T)
  M.infection=M
  M.removed=M
  infectionrate.0=f$infectionrate[T.infection]
  removedrate.0=f$removedrate[T.infection]
  confirmed.0=f$confirmed[T.infection]
  inhospitals.0=f$inhospitals[T.infection]
  
  # to calculate InfectionRateVelocity
  # This while loop is a correction process for "m" and "t" in special situations,
  # in other words, this loop will be skipped in most cases.
  while (f$infectionrate[T.infection-M.infection+1]<=f$infectionrate[T.infection] | f$infectionrate[T.infection-M.infection+1]==0){
    M.infection=M.infection-1
    if(M.infection>1) next
    else{
      T.infection=T.infection-1
      M.infection=M
      if(T.infection>=0) next
      else {
        stop("The infection rate K heaven't decrease yet.")
      }
    }
  }
  # The formula for velocity calculation.
  InfectionRateVelocity=(f$infectionrate[T.infection]/f$infectionrate[T.infection-M.infection+1])^(1/(M.infection-1))
  # to calculate RemovedRateVelocity
  # The meaning of this while loop is the same as above.
  while (f$removedrate[T.removed-M.removed+1]>=f$removedrate[T.removed] | f$removedrate[T.removed-M.removed+1]==0){
    M.removed=M.removed-1
    if(M.removed>1) next
    else{
      T.removed=T.removed-1
      M.removed=M
      if(T.removed>=0) next
      else {
        stop("The completion rate heaven't increase yet.")
      }
    }
  }
  # The formula for velocity calculation.
  RemovedRateVelocity=(f$removedrate[T.removed]/f$removedrate[T.removed-M.removed+1])^(1/(M.removed-1))
  velocity=list("InfectionRateVelocity"=InfectionRateVelocity,"RemovedRateVelocity"=RemovedRateVelocity,"T.removed"=T.removed,"M.removed"=M.removed,"T"=T)
  return(velocity)
}  



Prediction<-function(date, confirmed, inhospitals, infectionrate, removedrate, InfectionRateVelocity, RemovedRateVelocity,T){
  #=====================================
  # Predict future infectionrate, removedrate, E_t, inhospitals 
  # and get T.2, Z.1 and Z.2 at the same time.
  #
  # Args:
  #   date: the exact day in the formate "%y-%m-%d" as a character.
  #   confirmed: the daily confirmed cases at the given date.
  #   inhospitals: The number of infectious cases in hospital.
  #   infectionrate: The daily infection rate.
  #   removedrate: the daily removed rate.
  #   InfectionRateVelocity: the velocity of infection rate change.
  #   RemovedRateVelocity: the velocity of complerion rate change.
  #   T: the selection of begining time, 
  #      which must be in the formate "%y-%m-%d" as a character.
  #
  # Returns:
  #   A dataframe contains prediction result of removedrate, inhospitals,
  #   and T.2, Z.1, Z.2.
  #=====================================
  f=data.frame(date=date,
               confirmed=confirmed,
               inhospitals=inhospitals,
               infectionrate=infectionrate,
               removedrate=removedrate)
  f$date=as.Date(f$date)
  
  T.infection=which(f==T)
  infectionrate.0=f$infectionrate[T.infection]
  removedrate.0=f$removedrate[T.infection]
  confirmed.0=f$confirmed[T.infection]
  inhospitals.0=f$inhospitals[T.infection]
  
  infectionrate.pre=c(infectionrate.0)
  removedrate.pre=c(removedrate.0)
  confirmed.pre=c(confirmed.0)
  inhospitals.pre=c(inhospitals.0)
  t=1
  
  # to predict the first zero point Z.1.
  while (confirmed.pre[t]>1){
    t=t+1
    
    infectionrate=infectionrate.pre[t-1]*InfectionRateVelocity
    removedrate=removedrate.pre[t-1]*RemovedRateVelocity
    R_t=1+infectionrate-removedrate
    inhospitals=inhospitals.pre[t-1]*R_t
    E_t=inhospitals.pre[t-1]*infectionrate
    
    infectionrate.pre=c(infectionrate.pre,infectionrate)
    removedrate.pre=c(removedrate.pre,removedrate)
    confirmed.pre=c(confirmed.pre,E_t)
    inhospitals.pre=c(inhospitals.pre,inhospitals)
  }
  Z.1=as.Date(T)+t-1
  
  # to predict the second zero point Z.2.
  while (inhospitals.pre[t]>1 ){
    t=t+1
    
    infectionrate=infectionrate.pre[t-1]*InfectionRateVelocity
    removedrate=removedrate.pre[t-1]*RemovedRateVelocity
    R_t=1+infectionrate-removedrate
    inhospitals=inhospitals.pre[t-1]*R_t
    
    infectionrate.pre=c(infectionrate.pre,infectionrate)
    removedrate.pre=c(removedrate.pre,removedrate)
    inhospitals.pre=c(inhospitals.pre,inhospitals)
  }
  Z.2=as.Date(T)+t-1
  
  # After prediction process, we can get the second turing point.
  # If T.2 have already gone, we stop predicting it.
  if (which.max(inhospitals.pre)>1){
    T.2=as.Date(T)+which.max(inhospitals.pre)-1
  }else T.2=NA
  
  prediction<-data.frame("removedrate.pre"=removedrate.pre,"inhospitals.pre"=inhospitals.pre,"T.2"=T.2,"Z.1"=Z.1,"Z.2"=Z.2)
  
  return(prediction)
}



total_pre<-function(wd,M,T){
  #=====================================
  # Integrate functions above, and handle a special situation (removedrate>1).
  #
  # Args:
  #   wd: dataframe with data and four variables, i.e. 
  #       the cumulative confirmed cases up to the given day t,
  #       the daily confirmed cases at day t, 
  #       the daily recovered ones and the daily deaths at day t，
  #   M: the selection of time window.
  #   T: the selection of begining time, 
  #      which must be in the formate "%y-%m-%d" as a character.
  #
  # Returns:
  #   A list contains prediction result of removedrate, inhospitals, and T.2, Z.1, Z.2.
  #=====================================
  iconic=Iconicfun(wd)
  date=iconic$date
  confirmed=iconic$confirmed
  recovered=iconic$recovered
  deaths=iconic$deaths
  inhospitals=iconic$inhospitals
  infectionrate=iconic$infectionrate
  removedrate=iconic$removedrate
  
  velocity=CalculateVelocity(date, confirmed, inhospitals, infectionrate, removedrate, M, T)
  InfectionRateVelocity=as.numeric(velocity["InfectionRateVelocity"])
  RemovedRateVelocity=as.numeric(velocity["RemovedRateVelocity"])
  T.removed=as.numeric(velocity["T.removed"])
  M.removed=as.numeric(velocity["M.removed"])
  
  prediction=Prediction(date, confirmed, inhospitals, infectionrate, removedrate, InfectionRateVelocity, RemovedRateVelocity, T)
  removedrate.pre=prediction$removedrate.pre
  # This while loop is used to avoid RemovedRateVelocity>1, which is counterintuitive.
  while(removedrate.pre[length(removedrate.pre)]>1 & T.removed>=1){
    T.removed=T.removed-1
    while (f$removedrate[T.removed-M.removed+1]>=f$removedrate[T.removed] | f$removedrate[T.removed-M.removed+1]==0){
      M.removed=M.removed-1
      if(M.removed>1) next
      else{
        T.removed=T.removed-1
        M.removed=M
        if(T.removed>=0) next
        else {
          stop("The completion rate heaven't increase yet.")
        }
      }
    }
    RemovedRateVelocity=(f$removedrate[T.removed]/f$removedrate[T.removed-M.removed+1])^(1/(M.removed-1))
    prediction=Prediction(date, confirmed, inhospitals, infectionrate, removedrate, InfectionRateVelocity, RemovedRateVelocity, T)
    removedrate.pre=prediction$removedrate.pre
  }
  
  return(prediction)
}

#================================================
# A simply example with M=5.
# Users can change begining time T and Time window M.
filepath="Please input your filepath here"
filepath=edit(filepath)
result=matrix(0,ncol=3,nrow=32)
wd=read.csv(filepath)
for (i in 1:32){
  print(i)
  T=as.character(as.Date("2020-01-29")+i-1)
  print(T)
  x=total_pre(wd, M, T)
  result[i,1]=as.character(x[1,3])
  result[i,2]=as.character(x[1,4])
  result[i,3]=as.character(x[1,5])
}


