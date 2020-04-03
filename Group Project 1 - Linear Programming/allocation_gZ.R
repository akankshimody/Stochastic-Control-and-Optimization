rm(list=ls())
library('lpSolve')

#Question 1
#objective coefficients
c<-c(0.031,0.049,0.024,0.039,0.016,0.024,0.046,0.026,0.033,0.044)

#LHS of constraints
A<-matrix(0,13,10)
A[1,]<-c(1,1,0,0,-1,0,0,0,0,-1)
A[2,]<-c(0,0,2,2,-1,-1,-1,-1,-1,0)
A[3,]<-c(rep(1,10))
A[4:13,]<-diag(10)


#RHS of constraints
b<-c(0,0,10,rep(3,10))


#Direction for the constraints
dir<-c(rep("<=",13))

#solve the LP and assign the returned strcture to variable s
s=lp("max",c,A,dir,b,compute.sens = 1)
alc1=s$solution
alc1_val=s$objval
alc1_val
#Function which takes ROI vector, upper bound and budget as inputs, outputs the objective value and solution
ROI_vector=c(0.031,0.049,0.024,0.039,0.016,0.024,0.046,0.026,0.033,0.044)
Budget=10
Upper_bound=3

allocation <- function(ROI_vector, Budget, Upper_bound=NULL) {
  if (!is.null(Upper_bound)){
    A<-matrix(0,13,10)
    A[1,]<-c(1,1,0,0,-1,0,0,0,0,-1)
    A[2,]<-c(0,0,2,2,-1,-1,-1,-1,-1,0)
    A[3,]<-c(rep(1,10))
    A[4:13,]<-diag(10)
    b<-c(0,0,Budget,rep(Upper_bound,10))
    dir<-c(rep("<=",13))
    s=lp("max",ROI_vector,A,dir,b,compute.sens = 1)
  }
  else{
    A<-matrix(0,3,10)
    A[1,]<-c(1,1,0,0,-1,0,0,0,0,-1)
    A[2,]<-c(0,0,2,2,-1,-1,-1,-1,-1,0)
    A[3,]<-c(rep(1,10))
    b<-c(0,0,Budget)
    dir<-c(rep("<=",3))
    s=lp("max",ROI_vector,A,dir,b,compute.sens = 1)
  }}

result=allocation(ROI_vector,Budget)
alc2_val=result$objval
alc2=result$solution
alc2_val
#Question 2
newROI_vector=c(0.049,0.023,0.024,0.039,0.044,0.046,0.026,0.019,0.037,0.026)
Budget=10
Upper_bound=3

result_new=allocation(newROI_vector,Budget,Upper_bound)
alc3_val=result_new$objval
alc3=result_new$solution
#We can see that the objective value is the same for both allocations, but the solution differs
alc3_val
disappointment_1=alc1_val-alc3_val
disappointment_2=alc2_val-alc3_val
disappointment_1
disappointment_2
#Based on the results, it seems that the objective value is highest without the upper bound constraint. It might be better to not utilize this constraint to maximize objective value

#Calculating averages for alc1,alc2,alc3 on old ROI and new ROI vectors
mean_alc1=(sum(alc1*ROI_vector)+sum(alc1*newROI_vector))/2
mean_alc2=(sum(alc2*ROI_vector)+sum(alc2*newROI_vector))/2
mean_alc3=(sum(alc3*ROI_vector)+sum(alc3*newROI_vector))/2
max_=max(mean_alc1,mean_alc2,mean_alc3)

#Testing for different upper bound values
Upper_bound_list=c(1,2,3,5,10,20,50,100,500,1000)
for (i in Upper_bound_list){
  temp=(allocation(ROI_vector,Budget,i)$solution)
  mean_temp=(sum(temp*ROI_vector)+sum(temp*newROI_vector))/2
  if (mean_temp>max_){
    print(i)
    print(allocation(ROI_vector,Budget,i)$solution)
    print(allocation(ROI_vector,Budget,i)$objval)
  }
}
#We obtain a better average objective value 0.422 with allocation (0 2 0 2 0 0 2 0 2 2)


