# This code is for preparing data files for creating X-13ARIMA-SEATS data metafiles. 
# For log growth rate. (with same weight)

load("all.RData")
load("reg.data.RData")
rm(reg.data.ex)

# Exporting data for X13-ARIMA --------------------------------------------

# Creating data frames each containing one variable in one state.

state = levels(all$State)
var = c("Title.2","Title.16","Concurrent","Total","DI","SSI") 

# Generating a matrix specifying the saving path.
path.state = "E:/SkyDrive/Working/Third year/X13data.state/"
path.var = "E:/SkyDrive/Working/Third year/X13data.var/"

path.state.same = "E:/SkyDrive/Working/Third year/X13data.state.same/"
path.var.same = "E:/SkyDrive/Working/Third year/X13data.var.same/"

exp = ".txt"



# Exporting data for X13. Log growth rate series from reg.data ----------------------------

state = levels(reg.data$State) # 54, 50 states + DC + PR +NW + AG + A8 + A9
var = c("dlTitle.2","dlTitle.16","dlConcurrent","dlTotal") 

# Generating a matrix specifying the saving path.

# path.state.same = "E:/SkyDrive/Working/Third year/X13data.state.same/"
path.var.same.dl = "E:/SkyDrive/Working/Third year/X13data.var.same.dl/"

exp = ".txt"

# Input data: month weighted with the same factor(adjusted to the same length) 
# file.name.state.same = matrix(character(length(state)*length(var)),ncol=length(var))
file.name.var.same.dl = matrix(character(length(state)*length(var)),ncol=length(var))

# for (i in 1:length(state)){
#   file.name.state.same[i,]=paste(rep(path.state.same,length(var)),paste(rep(state[i],length(var)),var,sep="_"),rep(paste0(".same",exp),length(var)),sep="")
# }

for (i in 1:length(state)){
  file.name.var.same.dl[i,]=paste(rep(path.var.same.dl,length(var)),paste(var,rep(state[i],length(var)),sep="_"),rep(paste0(".same",exp),length(var)),sep="")
}

# # Saving the files
# for (i in 1:length(state)){
#   for (j in 1:length(var)){
#     write(t(as.matrix(subset(all.same,State==state[i],c("Cal.Year","Month",var[j])))),file=file.name.state.same[i,j],ncolumns=3)
#   }
# }

for (i in 1:length(state)){
  for (j in 1:length(var)){
    write(t(as.matrix(subset(reg.data, State==state[i], c("Cal.Year","Month",var[j])))[-1,]),file=file.name.var.same.dl[i,j],ncolumns=3)
  }
}


rm(exp,i,j, state, var, path.var.same, file.name.var.same.dl)
