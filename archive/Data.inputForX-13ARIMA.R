# This code is for preparing data files for creating X-13ARIMA-SEATS data metafiles.  

load("all.RData")
#rm(reg.data.ex)

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
  
# Input data: month weighted with different factors.   
  
  file.name.state = matrix(character(length(state)*length(var)),ncol=length(var))
  file.name.var = matrix(character(length(state)*length(var)),ncol=length(var))
  
  for (i in 1:length(state)){
    file.name.state[i,]=paste(rep(path.state,length(var)),paste(rep(state[i],length(var)),var,sep="_"),rep(exp,length(var)),sep="")
  }

  for (i in 1:length(state)){
    file.name.var[i,]=paste(rep(path.var,length(var)),paste(var,rep(state[i],length(var)),sep="_"),rep(exp,length(var)),sep="")
  }
  
  # Saving the files
  for (i in 1:length(state)){
    for (j in 1:length(var)){
      write(t(as.matrix(subset(all,State==state[i],c("Cal.Year","Month",var[j])))),file=file.name.state[i,j],ncolumns=3)
    }
  }

  for (i in 1:length(state)){
    for (j in 1:length(var)){
      write(t(as.matrix(subset(all,State==state[i],c("Cal.Year","Month",var[j])))),file=file.name.var[i,j],ncolumns=3)
    }
  }

# Input data: month weighted with the same factor(adjusted to the same length) 

  file.name.state.same = matrix(character(length(state)*length(var)),ncol=length(var))
  file.name.var.same = matrix(character(length(state)*length(var)),ncol=length(var))

  for (i in 1:length(state)){
    file.name.state.same[i,]=paste(rep(path.state.same,length(var)),paste(rep(state[i],length(var)),var,sep="_"),rep(paste0(".same",exp),length(var)),sep="")
  }

  for (i in 1:length(state)){
    file.name.var.same[i,]=paste(rep(path.var.same,length(var)),paste(var,rep(state[i],length(var)),sep="_"),rep(paste0(".same",exp),length(var)),sep="")
  }

  # Saving the files
  for (i in 1:length(state)){
    for (j in 1:length(var)){
      write(t(as.matrix(subset(all.same,State==state[i],c("Cal.Year","Month",var[j])))),file=file.name.state.same[i,j],ncolumns=3)
    }
  }

  for (i in 1:length(state)){
    for (j in 1:length(var)){
      write(t(as.matrix(subset(all.same,State==state[i],c("Cal.Year","Month",var[j])))),file=file.name.var.same[i,j],ncolumns=3)
    }
  }

file.name.var.same[1,1]
subset(all.same,State==state[1],c("Cal.Year","Month",var[4]))

write(t(as.matrix(subset(all.same,State==state[1],c("Cal.Year","Month",var[1])))),file=file.name.var.same[1,1],ncolumns=3)

#rm(exp,i,j, state, var, path.var.same)

