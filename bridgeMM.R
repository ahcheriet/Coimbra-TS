# max.R
# Fetch command line arguments
myArgs <- commandArgs(trailingOnly = TRUE)
# Convert to numerics
#nums = as.numeric(myArgs)

nums = as.numeric(myArgs[1:(length(myArgs)-2)])
dist = myArgs[length(myArgs)]
action = myArgs[length(myArgs)-1]

  # cat will write the result to the stdout stream
#cat(max(nums))
library(PerMallows)
#print(nums)
#print(action)
#cat(max(nums))
options(width=10000)
if (action=='s'){
  numperms = nums[1]
  lenperms = nums[2]
  theta = nums[3]
  if (theta==0){
    res = runif.permutation (n=numperms , lenperms)
    }else{
      res = rmm(n=numperms,sigma0=1:lenperms,theta = theta, dist.name = dist)  
    }
  
}else{
  numperms = nums[1]
  lenperms = nums[2]
  nums = nums[3:length(nums)]
  nums = matrix(nums, nrow = numperms, byrow = TRUE)
  if (dist=='h'){
    res = lmm(data = nums, dist.name = 'h',estimation = 'exact')
  }else{
    res = lmm(data = nums, dist.name = dist)
  }
}
res