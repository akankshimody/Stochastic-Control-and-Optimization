###############
##   PART 1  ##
###############


## QUESTION 1: 
## Formulate the budget allocation as a linear programming problem. List and describe the decision variables, the objective, and all the constraints.

## ANSWER 1: 

## Choose (a,b,c,d,e,f,g,h,i,j) 
##    where each of the above represents an amount (in millions) to invest in each advertising category

## To Maximize: 
## (3.1%)a + (4.9%)b + (2.4%)c + (3.9%)d + (1.6%)e + (2.4%)f + (4.6%)g + (2.6%)h + (3.3%)i + (4.4%)j

## Subject to the constraints: 
## a,b,c,d,e,f,g,h,i,j >= 0  (Non-negative constraint)
## a,b,c,d,e,f,g,h,i,j <= 3  (Upper-bound contstraint)
## a + b + c + d + e + f + g + h + i + j >= 0  (Budget constraint)
## a + b <= e + j  (Print & TV no more than Facebook & Email)
## e + f + g + h + i >= 2(c + d)  (Social Media at least 2x SEO & AdWords)




###############
## QUESTION 2: 
## Use the following (not shown) test case and solve the LP in R.

## ANSWER 2: 

library(lpSolve)

A = matrix(0,nrow=23,ncol=10)
A[1,1:10] = 1
A[2:11,1:10]=diag(10)
A[12:21,1:10]=diag(10)
A[22,1:10]=c(0,0,-2,-2,1,1,1,1,1,0)
A[23,1:10]=c(1,1,0,0,-1,0,0,0,0,-1)

b2 = rep(3,10)
b3 = rep(0,12)
B = c(10,b2,b3)

C = c(0.031,0.049,0.024,0.039,0.016,0.024,0.046,0.026,0.033,0.044)

dir1 = rep('<=',11)
dir2 = rep('>=',11)
dir3 = '<='
dir = c(dir1,dir2,dir3)

alc1=lp("max",C,A,dir,B, compute.sens = 1)

# Outputs the Objective Value for Q2
alc1$objval

# Outputs the Solution for Q2
alc1$solution




###############
## QUESTION 3: 
## Next, we will write a function in R that can construct an allocation for any ROI vector, upper bound, and budget.


## ANSWER 3: 

allocation_g29 <- function(ROI_vec,budget,upper_bound=budget) {
  library(lpSolve)
  
  A = matrix(0,nrow=23,ncol=10)
  A[1,1:10] = 1
  A[2:11,1:10]=diag(10)
  A[12:21,1:10]=diag(10)
  A[22,1:10]=c(0,0,-2,-2,1,1,1,1,1,0)
  A[23,1:10]=c(1,1,0,0,-1,0,0,0,0,-1)
  
  b2 = rep(upper_bound,10)
  b3 = rep(0,12)
  B = c(budget,b2,b3)
  
  C = ROI_vec
  
  dir1 = rep('<=',11)
  dir2 = rep('>=',11)
  dir3 = '<='
  dir = c(dir1,dir2,dir3)
  
  alc=lp("max",C,A,dir,B, compute.sens = 1)
  list(objval = alc$objval, sol = alc$solution)
  }





###############
## QUESTION 4: 
## Use allocation() function to calculate the optimal objective value without the 3rd constraint. 
## Report both the optimal objective value and optimal solution, which we call alc2, and compare them with the $3M counterparts.


## ANSWER 4: 

# ROI Vec same as in Question 1.
ROI = c(0.031,0.049,0.024,0.039,0.016,0.024,0.046,0.026,0.033,0.044)

# Use function defined above but without upper_bound constraint.
alc2 = allocation_g29(ROI,10)

# Objective Value
alc2$objval

# Solution
alc2$sol




###############
##   PART 2  ##
###############


## QUESTION 1: Get the optimal objective value (with $3M upper bound) and the corresponding solution, alc3, using the new ROI vector. 
## Are they the same as their counterparts using the previous ROI vector?


## ANSWER 1: 

ROI_new = c(0.049,0.023,0.024,0.039,0.044,0.046,0.026,0.019,0.037,.026)

alc3 = allocation_g29(ROI_new,10,3)

alc1$solution
alc3$sol
## No, the solution is not the same. 
## 1st ROI Vec Sol is (0 3 0 1 0 0 3 0 0 3)
## New ROI Vec Sol is (3 0 0 1 3 3 0 0 0 0)

alc1$objval
alc3$objval
## Objective Values: (0.447 vs. 0.456)




###############
## QUESTION 2:The disappointment of an allocation is defined as the difference between the objective values using the old and new ROI vector.Calculate the disappointment for alc1 and alc2 in the previous section. Do you think the 3rd constraint based on CMO’s experience is valuable?
disappoint_alc1 = sum(ROI*alc1$solution) - sum(ROI_new*alc1$solution)
disappoint_alc1
##0.192
disappoint_alc2 = sum(ROI*alc2$sol) - sum(ROI_new*alc2$sol)
disappoint_alc2
#0.22


## Although it may be tempting to use all of the advertising budget on the categories with the highest ROI, companies may experience diminishing returns. 
## This means that although a company might expect a 4.6% ROI on Instagram for the first $3MM spent, they may not be able to capture the same ROI at a different spending level. 
## The level at which these diminishing returns might affect the budget allocation decision is unclear, however. 

###############
## QUESTION 3:Try to find an allocation that dominates alc1, alc2, and alc3 regarding the average objective values using both the old and new ROI vector. To illustrate, the average objective values of alc1 is the average of $0.456M (ROI old) and $0.264M (ROI new), namely $0.360M. There are at least two possible ways to achieve this allocation.

#Find average of allocation using old vector and allocation using new vector
alc1_avg = (sum(ROI*alc1$solution)+sum(ROI_new*alc1$solution))/2
alc2_avg = (sum(ROI*alc2$sol)+sum(ROI_new*alc2$sol))/2
alc3_avg = (sum(ROI*alc3$sol)+sum(ROI_new*alc3$sol))/2
max_alc_avg = max(alc1_avg,alc2_avg,alc3_avg)

#Finding obj val using different upperbounds
upper_bound_values = c(0.5,1,1.5,2,2.5,3,3.5,4,5,8,10,15,20,40,60,80,100,150,200,250,500,1000)
max_i = 0
for (i in upper_bound_values){
  new_avg=(sum(allocation_g29(ROI,10,i)$sol*ROI)+sum(allocation_g29(ROI,10,i)$sol*ROI_new))/2
  if (new_avg>max_alc_avg){
    max_i = i
  }
}
print(max_i)
#print(allocation_g29(ROI,10,max_i)$sol)
#print(allocation_g29(ROI,10,max_i)$objval)

#Thus, for upper bound = 2.5, we see that the allocation dominates alc1,alc2,alc3
