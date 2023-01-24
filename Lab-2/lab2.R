#2020BCS0094

# Create a R program to check whether the number is odd or even.

# Reading input and converting it to integer
inp = readline()
inp = as.integer(inp)

# condition to find weather even or odd
if( inp %% 2 == 0 ){
  print(paste(inp,"is even"))
}else{
  print(paste(inp,"is odd"))
}

# Write a R-program to find the factorial of a number 

# using a for loop

factorio<-1
for(x in 1:inp){
  factorio = factorio * x
}
print(factorio)

# using a while loop

factorio = 1
x = 1
while(x<=inp){
  factorio = factorio * x
  x=x+1
  }
print(factorio)

# using user defined function

fac<-function(inp){
  factorial(inp)
}

fac(inp)


# Write a R-program to find the sum of natural numbers

# using a for loop

sum1<-0
for(i in 1:inp){
  sum1= sum1+ i
}
print(sum1)

# using a while loop

sum2=0
i=0
while(i <= inp){
  sum2=sum2+i
  i=i+1
}
print(sum2)

# using a user defined function

sumfunc<-function(inp){
  inp = (inp * (inp + 1)) / 2;
}
print(sumfunc(inp))


# Create a R program to find a number is prime or not

primeOrNot<-function(inp){
  flag = TRUE
  for(z in 2:(inp-1) ){
    if((inp %% z) == 0){
      print(z)
      flag = FALSE
      
    }
    
  }
  if(flag){
    print(paste(inp,"is prime"))
  }else{
    print(paste(inp,"is not prime"))
  }
}

primeOrNot(inp)


# Write a R program to perform matrix multiplication 

A <- matrix(1:6, nrow=2)
A

B<- matrix(1:6,ncol=2)
B

# Matrix Multiplication
A%*%B


