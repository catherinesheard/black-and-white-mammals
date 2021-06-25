# CODE CREATED ON: 16/07/2020
# LAST EDITED ON: 22/10/2020
# ANNOTATIONS LAST EDITED ON: 25/6/2021

# By Natasha Howell and Catherine Sheard

# This code is an example of one model described in this paper
# Specifically, this code tests the head pattern 'adjacent blocks of black-and-white pelage' against all seven aposematic defenses individually, as well as body mass
# Areas where the code can be adapted for other models have been specified

# Set working directory
setwd("C:/#working directory location name#")

# Read in the data
x<-read.csv("#dataset name#.csv")

# Recode the head and body patterns into binary variables
N<-dim(x)[1]
x$FHP0<-rep(0,N)
x$FHP0[which(x$Head.Pattern==0)]<-1 #a vector indicating whether the species has "head pattern 0" (no black and white patterning -- 1 = yes, 0 = no)
x$FHP1<-rep(0,N)
x$FHP1[which(x$Head.Pattern==1)]<-1 #a vector indicating whether the species has "head pattern 1" (solid blocks of adjacent black and white -- 1 = yes, 0 = no)
x$FHP2<-rep(0,N)
x$FHP2[which(x$Head.Pattern==2)]<-1 #a vector indicating whether the species has "head pattern 2" (black and white stripes -- 1 = yes, 0 = no)
x$FHP3<-rep(0,N)
x$FHP3[which(x$Head.Pattern==3)]<-1 #a vector indicating whether the species has "head pattern 3" (black and white complex patterns -- 1 = yes, 0 = no)

x$FHPALL<-rep(0,N)
x$FHPALL[which(x$Head.Pattern==1)]<-1
x$FHPALL[which(x$Head.Pattern==2)]<-1
x$FHPALL[which(x$Head.Pattern==3)]<-1

x$FB1<-rep(0,N) 
x$FB1[which(x$Body.Pattern==1)]<-1 #a vector indicating whether the species has "body pattern 1" (salt and pepper -- 1 = yes, 0 = no)
x$FB2<-rep(0,N)
x$FB2[which(x$Body.Pattern==2)]<-1 #a vector indicating whether the species has "body pattern 2" (vertical black and white stripes -- 1 = yes, 0 = no)
x$FB3<-rep(0,N)
x$FB3[which(x$Body.Pattern==3)]<-1 #a vector indicating whether the species has "body pattern 3" (longitudinal black and white stripes -- 1 = yes, 0 = no)
x$FB4<-rep(0,N)
x$FB4[which(x$Body.Pattern==4)]<-1 #a vector indicating whether the species has "body pattern 4" (dark dorsum, light ventrum -- 1 = yes, 0 = no)
x$FB5<-rep(0,N)
x$FB5[which(x$Body.Pattern==5)]<-1 #a vector indicating whether the species has "body pattern 5" (light dorsum, dark ventrum -- 1 = yes, 0 = no)
x$FB6<-rep(0,N)
x$FB6[which(x$Body.Pattern==6)]<-1 #a vector indicating whether the species has "body pattern 6" (irregular black and white blocks -- 1 = yes, 0 = no)
x$FB7<-rep(0,N)
x$FB7[which(x$Body.Pattern==7)]<-1 #a vector indicating whether the species has "body pattern 7" (disordered black and white stripes -- 1 = yes, 0 = no)
x$FB8<-rep(0,N)
x$FB8[which(x$Body.Pattern==8)]<-1 #a vector indicating whether the species has "body pattern 8" (large black-on-white or white-on-black spots -- 1 = yes, 0 = no)

x$FBALL<-rep(0,N) #a vector indicating whether the species has any sort of black-and-white body pattern (1 = yes, 0 = no)
x$FBALL[which(x$Body.Pattern==1)]<-1
x$FBALL[which(x$Body.Pattern==2)]<-1
x$FBALL[which(x$Body.Pattern==3)]<-1
x$FBALL[which(x$Body.Pattern==4)]<-1
x$FBALL[which(x$Body.Pattern==5)]<-1
x$FBALL[which(x$Body.Pattern==6)]<-1
x$FBALL[which(x$Body.Pattern==7)]<-1
x$FBALL[which(x$Body.Pattern==8)]<-1

# If running an Order-level model, the below line can be used to trim down the dataset to the specified Order
#x<-x[which(x$Order=="Carnivora"),]

# Install the necessary packages and open them
install.packages("MCMCglmm")
install.packages("phangorn")
library(MCMCglmm)
library(phangorn)

# Read in the Upham et al. 2019 phylogeny
# Read in a random 1,000 tree topologies
trees<-read.nexus("trees1k.nex")
# Take the first 100 tree topologies
t100<-trees[1:100]
# Select one tree to trim the data
tree<-t100[[1]]

# Match the tree tips to the data
x$NewName<-paste(gsub(" ","_",x$Species),toupper(x$Family),toupper(x$Order),sep="_")

#Make sure these two variables have been read in correctly
x$Adult.Body.Mass<-as.vector(x$Adult.Body.Mass)
x$Group.Size<-as.vector(x$Group.Size)

# Trim out species with missing data
  # NOTE: these variables only need trimming if they're being used in the model
#if(sum(x$Group.Size== "-999")){x<-x[-which(x$Group.Size== "-999"),]}
#if(sum(x$Group.Size== "999")){x<-x[-which(x$Group.Size== "999"),]}
#if(sum(x$Shade.Score== "999")){x<-x[-which(x$Shade.Score== "999"),]}
#if(sum(x$Shade.Score== "-")){x<-x[-which(x$Shade.Score== "-"),]}

# Create the aposematism variables
x$weap<-rep(0,dim(x)[1]) #weaponry: 1 = present, 0 = absent
x$weap[which(x$Weaponry==1)]<-1

x$pug<-rep(0,dim(x)[1]) #pugnacious behaviour: 1 = present, 0 = absent
x$pug[which(x$Pugnacious==1)]<-1

x$ande<-rep(0,dim(x)[1]) #anal defense: 1 = present, 0 = absent
x$ande[which(x$Anal.Defence==1)]<-1

x$tad<-rep(0,dim(x)[1]) #toxic compounds in the skin and/or predator-deterring physiological features: 1 = present, 0 = absent
x$tad[which(x$Toxic==1)]<-1
x$tad[which(x$Distasteful==1)]<-1

x$spine<-rep(0,dim(x)[1]) #spines: 1 = present, 0 = absent
x$spine[which(x$Spines==1)]<-1

x$lhth<-rep(0,dim(x)[1]) #long and/or thick hair: 1 = present, 0 = absent
x$lhth[which(x$Long.Hair==1)]<-1
x$lhth[which(x$Thick.Hair==1)]<-1

x$glide<-rep(0,dim(x)[1])
x$glide[which(x$Glide==1)]<-1

x$apo<-rep(0,dim(x)[1]) #a variable combining all aposematic variables: 1 = some form of aposematism present, 0 = all absent
x$apo[which(x$Weaponry==1)]<-1
x$apo[which(x$Pugnacious==1)]<-1
x$apo[which(x$Anal.Defence==1)]<-1
x$apo[which(x$Toxic==1)]<-1
x$apo[which(x$Distasteful==1)]<-1
x$apo[which(x$Spines==1)]<-1
x$apo[which(x$Long.Hair==1)]<-1
x$apo[which(x$Thick.Hair==1)]<-1
x$apo[which(x$Glide==1)]<-1

# Trim out anything in the dataset that does not appear on the tree
bad<-rep(0,dim(x)[1])
for(i in 1:dim(x)[1]){
  if(sum(x$NewName[i]==tree$tip.label)==0){bad[i]=1}
}

if(sum(bad)>0){x<-x[-which(bad==1),]} 

# Trim out anything on the tree that is absent from the dataset
t100<-lapply(t100,drop.tip,tip=setdiff(tree$tip.label,x$NewName))

# Scale the continuous variables to have mean 0 and variance 1
x$zMass<-scale(log(as.numeric(x$Adult.Body.Mass)))
x$zShade<-scale(as.numeric(x$Shade.Score))

# Conduct a dummy run to set up the structure of the model and get a starting point
# i=1 here is arbitrary
i=1
tree<-t100[[i]]

# Force the tree to be ultrametric
tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE)
animalA<-inverseA(tree)$Ainv

# Calculate the priors
  # NOTE: The '9' in 'mu=rep(0,9)' MUST be calculated according to the variables appearing in each model
  # Calculation as follows: 1 + (number of continuous variables) + (k-1 for each categorical variable), where 'k' is the number of categories in the variable
  # Variables that appear after 'V=gelman.prior(~' can be added/removed depending on what model is being run 
prior.Model<-list(B=list(mu=rep(0,9),V=gelman.prior(~zMass+weap+pug+ande+tad+spine+lhth+glide,data = x,  scale=1+pi^2/3)),R=list(V=1,fix=1),G=list(G1=list(V=1E-10,nu=-1)))

# Run the dummy run
  # NOTE: 'FHP1' denotes which head pattern is being tested, change to the code for another head or body pattern as necessary
  # NOTE: add/remove the independent variables that appear after the '~' in the first line of the model as necessary
Final.disp<-MCMCglmm(FHP1~zMass+weap+pug+ande+tad+spine+lhth+glide, 
              random=~NewName, 
              ginverse=list(NewName=animalA), 
              prior = prior.Model, 
              verbose=TRUE,
              family="categorical", 
              data = x,
              nitt=11000, # Number of iterations
              thin=10, # Sampling of iterations
              burnin=1000, # Initial iterations to discard
              pl=TRUE, # Stores posterior distribution of latent variables
              pr=TRUE, # Stores posterior distribution of random effects specifically
              slice=TRUE)

# Set starting point
nsamp.l<-nrow(Final.disp$VCV)
start1.l=list(R=Final.disp$VCV[nsamp.l,"units"], G=list(G1=Final.disp$VCV[nsamp.l,"NewName"]))

# Save the dummy run as an Rdata file
save(Final.disp,file="#model name - date of analysis#.Rdata")

# Run the model over 100 randomly-selected tree topologies
  # NOTE: 'FHP1' denotes which head pattern is being tested, change to the code for another head or body pattern as necessary
  # NOTE: add/remove the independent variables that appear after the '~' in the first line of the model as necessary
for(i in 1:100){
  tree<-t100[[i]]  
  tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE)
  
  animalA<-inverseA(tree)$Ainv
  
  mod<-MCMCglmm(FHP1~zMass+weap+pug+ande+tad+spine+lhth+glide,  
                random=~NewName, 
                ginverse=list(NewName=animalA), 
                prior = prior.Model, 
                verbose=TRUE,
                family="categorical", 
                start= start1.l,
                data = x,
                nitt=11000,  
                thin=1000, 
                burnin=1000, 
                pl=TRUE, 
                pr=TRUE, 
                slice=TRUE)
  print(i)
  
  #Save the results of the current tree iteratively in the final model
  Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,] 
  Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
  Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 
  
  # Update start point
  nsamp.l<-nrow(mod$VCV)
  start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"NewName"]))
  
  
  
  save(Final.disp,file="#model name - date of analysis#.Rdata")
  
}

# See a summary of the model once it has completed running
summary(Final.disp)

# See plots of the model distributions
par("mar")
par(mar=c(1,1,1,1))
plot(Final.disp)

# Save a copy of the model as an Rdata file
save(Final.disp,file="#model name - date of analysis#.Rdata")