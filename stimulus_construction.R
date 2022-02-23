#Matrix size (100x100) gives the number of individual colored cells in the stimulus
stim_q <- 100^2

#Colors proportions based on Voss, A., Rothermund, K., & Brandtstädter, J. (2008).
#The paper proposes 3 color proportions (45%, 47% and 49%), we add two more levels
p <- signif(seq(.45,.495, length.out =5),4)

shuffled_vectors_fun <- function(x, vector_length = stim_q){
  #The function creates a vector with length vector_length consisting of a repetition of 1s and 0s with the proportion of 1s given by the argument
  #x, then it shuffles the vector using the sample function
  #Takes two arguments
    ##x = a proportion (from 0 to 1) corresponding to the amount of 1s in the resulting vector
    ##vector_length = an integer with the length of the resulting vector, it defaults to the object stim_q
  
  sample( #Shuffle the repeated vector
    rep( #Repeat a vector of 1 and 0 over 100^2 times
      c(1,0), #vector of the integers used to populate the vectors
      round(#We round the numbers to avoid floating point precision errors
      c(#vector of the absolute number of 1s and 0s to populate the vectors
        vector_length*(1-x), 
        vector_length*x) 
        )
    ),
    vector_length
  )
}

##Training stimulus
#Create length(p) vectors of length 100^2 with shuffled 1s and 0s according to the proportions of object p
shuffled_vectors <- sapply(
  p, #Iterate over the three color proportions
  #Function to generate vectors of length 100^2 with 1s and 0s according to each proportion x (from p) and shuffle it
  shuffled_vectors_fun
)

#Transform the vectors in shuffled_vectors to a 3d array with dimensions 100x100xlength(p), the 100x100 are given by stim_q
training_stimuli <- array(
  shuffled_vectors,
  dim = c(100,100,length(p))
)
##End Training stimulus

##Experimental stimulus (Repeat procedure to generate training stimulus with slight variations)

#Create length(p) * 100 vectors of length 100^2 with shuffled 1s and 0s, with a different proportion p for each 100 elements (eg. the first 100 vectors have proportion 0.45, etc)
experimental_vectors <- sapply(
  rep( #Generate a vector of proportions containing 1000 repeated values for each proportion in p
    p,
    each = 100 
  ),
  shuffled_vectors_fun
)

#Transformed the shuffled vectors into a 3d array
experimental_array <- array(
  experimental_vectors,
  dim = c(100,100,length(p)*100)
)

#Save the results
saveRDS(training_stimuli,  file = 'training_stimulus.rds', version = 2)
saveRDS(experimental_array, file = 'experimental_stimulus.rds', version = 2)


