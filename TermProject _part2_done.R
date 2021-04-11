# Term Project

library(stats)

# If you do not have ggbiplot installed on your RStudio then run the 2 commented line below in order
#library(devtools)
#install_github("vqv/ggbiplot")  

require(ggbiplot)


projectData = read.table("TermProjectData.txt", header = TRUE, sep = ",") # Read the data


# Puts variables and data into new dataframe
newdf = data.frame(projectData$Global_active_power,projectData$Global_reactive_power, projectData$Voltage, projectData$Global_intensity, projectData$Sub_metering_1, projectData$Sub_metering_2, projectData$Sub_metering_3)

# Gets PCA for variables. Scales before calculating
PCvals = prcomp(na.omit(newdf), scale = TRUE)

loading_scores = PCvals$rotation[,1]
gene_scores = abs(loading_scores)
gene_scores_ranked = sort(gene_scores,decreasing = TRUE)
top_7_genes = names(gene_scores_ranked[1:7])

print(loading_scores)
print(top_7_genes)
print(gene_scores_ranked)

summary(PCvals)

print(PCvals)

PCvals$rotation[top_7_genes,1]

# ggbiplot(PCvals, choices = c(1,2))
# 
# ggbiplot(PCvals, choices = c(1,3))
# 
# ggbiplot(PCvals, choices = c(1,4))
# 
# ggbiplot(PCvals, choices = c(1,5))
# 
# ggbiplot(PCvals, choices = c(1,6))
# 
# ggbiplot(PCvals, choices = c(1,7))
# 
# 
# ggbiplot(PCvals, choices = c(2,3))
# 
# ggbiplot(PCvals, choices = c(2,4))
# 
# ggbiplot(PCvals, choices = c(2,5))
# 
# ggbiplot(PCvals, choices = c(2,6))
# 
# ggbiplot(PCvals, choices = c(2,7))
# 
# 
# ggbiplot(PCvals, choices = c(3,4))
# 
# ggbiplot(PCvals, choices = c(3,5))
# 
# ggbiplot(PCvals, choices = c(3,6))
# 
# ggbiplot(PCvals, choices = c(3,7))
# 
# 
# ggbiplot(PCvals, choices = c(4,5))
# 
# ggbiplot(PCvals, choices = c(4,6))
# 
# ggbiplot(PCvals, choices = c(4,7))
# 
# 
# ggbiplot(PCvals, choices = c(5,6))
# 
# ggbiplot(PCvals, choices = c(5,7))
# 
# 
# ggbiplot(PCvals, choices = c(6,7))


# Part 2

# Starts Mon Dec 18th 2006
# 9AM - 12PM
# Christmas day 9AM(Starts at 2.5) ->fluctuates between 2/1 -> around 12(spikes up to mid 4/low 5)

#Start at line 2378 (9AM on Dec 18th)
#Ends at 2558 (12PM on Dec 18th)
#180
#To get to next week is 10080 lines?

#Dec 16 2006 - Dec 1 2009
#154 weeks 3 days

#Dec 18 2006 - Nov 30 2009



dataGAP <- c()
dataGI <- c()
start = "09:00:00"
end =   "11:59:00"

for(i in 1:1556444){
  myDate <- as.Date(projectData$Date[i], "%d/%m/%Y")
  if(weekdays(myDate) == "Monday"){
    if((strptime(projectData$Time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(projectData$Time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      dataGAP <- c(dataGAP,projectData$Global_active_power[i])
      dataGI <- c(dataGI,projectData$Global_intensity[i])
      
    }
  }
}

sdataGAP = scale(dataGAP)
sdataGI = scale(dataGI)

newData = data.frame(sdataGAP,sdataGI)

# FIX DATA. ERROR CAUSED BY LEAP YEAR

#Dec 18 2006 -> 

library(depmixS4)
set.seed(1)

# dt = sort(sample(nrow(newData), nrow(newData)*.8))
# train = newData[dt,]
# test = newData[-dt,]

#22140
#5580

# 80/20 split

train = newData[1:22320,]
test = newData[22321:27900,]

# For 4 states
gc()

model4 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 4, family = list(gaussian(), gaussian()),ntimes = c(rep(180,124)))
model4test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 4, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31)))

fitModel4 <- fit(model4)

fbtest4 = forwardbackward(model4test)

scaledLogLikTrain4 = (logLik(fitModel4)*0.8)
scaledLogLikTest4 = ((fbtest4$logLike * 0.2) * (2/8))
print(scaledLogLikTrain4)
print(scaledLogLikTest4)


print(fbtest4$logLike)
print(fitModel4test)
print(fitModel4)


model5 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 5, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124)))
model5test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 5, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel5 <- fit(model5)

fbtest5 = forwardbackward(model5test)

scaledLogLikTrain5 = (logLik(fitModel5)*0.8)
scaledLogLikTest5 = ((fbtest5$logLike * 0.2) * (2/8))
print(scaledLogLikTrain5)
print(scaledLogLikTest5)


print(fbtest5$logLike)
print(fitModel4test)
print(fitModel5)


model6 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 6, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model6test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 6, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel6 <- fit(model6)

fbtest6 = forwardbackward(model6test)

scaledLogLikTrain6 = (logLik(fitModel6)*0.8)
scaledLogLikTest6 = ((fbtest6$logLike * 0.2) * (2/8))
print(scaledLogLikTest6)
print(scaledLogLikTrain6)

print(fbtest6$logLike)
print(fitModel6test)
print(fitModel6)



model7 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 7, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model7test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 7, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel7 <- fit(model7)

fbtest7 = forwardbackward(model7test)


scaledLogLikTrain7 = (logLik(fitModel7)*0.8)
scaledLogLikTest7 = ((fbtest7$logLike * 0.2) * (2/8))
print(scaledLogLikTest7)
print(scaledLogLikTrain7)

print(fbtest7$logLike)
print(fitModel7test)
print(fitModel7)


model8 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 8, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model8test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 8, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel8 <- fit(model8)

fbtest8 = forwardbackward(model8test)

scaledLogLikTrain8 = (logLik(fitModel8)*0.8)
scaledLogLikTest8 = ((fbtest8$logLike * 0.2) * (2/8))
print(scaledLogLikTest8)
print(scaledLogLikTrain8)

print(fbtest8$logLike)
print(fitModel8test)
print(fitModel8)


model9 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 9, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model9test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 9, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel9 <- fit(model9)

fbtest9 = forwardbackward(model9test)

scaledLogLikTrain9 = (logLik(fitModel9)*0.8)
scaledLogLikTest9 = ((fbtest9$logLike * 0.2) * (2/8))
print(scaledLogLikTest9)
print(scaledLogLikTrain9)

print(fbtest9$logLike)
print(fitModel9test)
print(fitModel9)



model10 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 10, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model10test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 10, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel10 <- fit(model10)
scaledLogLikTrain = logLik()

fbtest10 = forwardbackward(model10test)

scaledLogLikTrain10 = (logLik(fitModel10)*0.8)
scaledLogLikTest10 = ((fbtest10$logLike * 0.2) * (2/8))
print(scaledLogLikTest10)
print(scaledLogLikTrain10)

print(fbtest10$logLike)
print(fitModel10test)
print(fitModel10)


model11 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 11, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model11test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 11, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel11 <- fit(model11)

fbtest11 = forwardbackward(model11test)

scaledLogLikTrain11 = (logLik(fitModel11)*0.8)
scaledLogLikTest11 = ((fbtest11$logLike * 0.2) * (2/8))
print(scaledLogLikTest11)
print(scaledLogLikTrain11)

print(fbtest11$logLike)
print(fitModel11test)
print(fitModel11)


model12 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 12, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model12test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 12, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel12 <- fit(model12)

fbtest12 = forwardbackward(model12test)

scaledLogLikTrain12 = (logLik(fitModel12)*0.8)
scaledLogLikTest12 = ((fbtest12$logLike * 0.2) * (2/8))
print(scaledLogLikTest12)
print(scaledLogLikTrain12)

print(fbtest12$logLike)
print(fitModel12test)
print(fitModel12)


model13 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 13, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model13test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 13, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel13 <- fit(model13)

fbtest13 = forwardbackward(model13test)

scaledLogLikTrain13 = (logLik(fitModel13)*0.8)
scaledLogLikTest13 = ((fbtest13$logLike * 0.2) * (2/8))
print(scaledLogLikTest13)
print(scaledLogLikTrain13)

print(fbtest13$logLike)
print(fitModel13test)
print(fitModel13)


model14 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 14, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model14test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 14, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel14 <- fit(model14)

fbtest14 = forwardbackward(model14test)

scaledLogLikTrain14 = (logLik(fitModel14)*0.8)
scaledLogLikTest14 = ((fbtest14$logLike * 0.2) * (2/8))
print(scaledLogLikTest14)
print(scaledLogLikTrain14)

print(fbtest14$logLike)
print(fitModel14test)
print(fitModel14)


model15 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 15, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model15test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 15, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel15 <- fit(model15)

fbtest15 = forwardbackward(model15test)

scaledLogLikTrain15 = (logLik(fitModel15)*0.8)
scaledLogLikTest15 = ((fbtest15$logLike * 0.2) * (2/8))
print(scaledLogLikTest15)
print(scaledLogLikTrain15)

print(fbtest15$logLike)
print(fitModel15test)
print(fitModel15)


model16 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 16, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model16test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 16, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel16 <- fit(model16)

fbtest16 = forwardbackward(model16test)

scaledLogLikTrain16 = (logLik(fitModel16)*0.8)
scaledLogLikTest16 = ((fbtest16$logLike * 0.2) * (2/8))
print(scaledLogLikTest16)
print(scaledLogLikTrain16)

print(fbtest16$logLike)
print(fitModel16test)
print(fitModel16)


model17 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 17, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model17test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 17, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel17 <- fit(model17)

fbtest17 = forwardbackward(model17test)

scaledLogLikTrain17 = (logLik(fitModel17)*0.8)
scaledLogLikTest17 = ((fbtest17$logLike * 0.2) * (2/8))
print(scaledLogLikTest17)
print(scaledLogLikTrain17)

print(fbtest17$logLike)
print(fitModel17test)
print(fitModel17)


model18 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 18, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model18test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 18, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel18 <- fit(model18)

fbtest18 = forwardbackward(model18test)

scaledLogLikTrain18 = (logLik(fitModel18)*0.8)
scaledLogLikTest18 = ((fbtest18$logLike * 0.2) * (2/8))
print(scaledLogLikTest18)
print(scaledLogLikTrain18)

print(fbtest18$logLike)
print(fitModel18test)
print(fitModel18)



model19 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 19, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model19test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 19, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel19 <- fit(model19)

fbtest19 = forwardbackward(model19test)

scaledLogLikTrain19 = (logLik(fitModel19)*0.8)
scaledLogLikTest19 = ((fbtest19$logLike * 0.2) * (2/8))
print(scaledLogLikTest19)
print(scaledLogLikTrain19)

print(fbtest19$logLike)
print(fitModel19test)
print(fitModel19)


model20 <- depmix(response =list(sdataGAP~1,sdataGI~1), data = train, nstates = 20, family = list(gaussian(),gaussian()),ntimes = c(rep(180,124))) 
model20test <- depmix(response =list(sdataGAP~1,sdataGI~1), data = test, nstates = 20, family = list(gaussian(),gaussian()),ntimes = c(rep(180,31))) 

fitModel20 <- fit(model20)

fbtest20 = forwardbackward(model20test)

scaledLogLikTrain20 = (logLik(fitModel20)*0.8)
scaledLogLikTest20 = ((fbtest20$logLike * 0.2) * (2/8))
print(scaledLogLikTest20)
print(scaledLogLikTrain20)

print(fbtest20$logLike)
print(fitModel20test)
print(fitModel20)


# Update these to include up to 20 nstates
plot(4:20,c(BIC(fitModel4), BIC(fitModel5), BIC(fitModel6), BIC(fitModel7), BIC(fitModel8), BIC(fitModel9), BIC(fitModel10), BIC(fitModel11), BIC(fitModel12), BIC(fitModel13), BIC(fitModel14), BIC(fitModel15), BIC(fitModel16), BIC(fitModel17), BIC(fitModel18), BIC(fitModel19), BIC(fitModel20)), ty = "b")
plot(4:20,c(logLik(fitModel4), logLik(fitModel5), logLik(fitModel6), logLik(fitModel7), logLik(fitModel8), logLik(fitModel9), logLik(fitModel10), logLik(fitModel11), logLik(fitModel12), logLik(fitModel13), logLik(fitModel14), logLik(fitModel15), logLik(fitModel16), logLik(fitModel17), logLik(fitModel18), logLik(fitModel19), logLik(fitModel20)), ty = "b")



# Part 3
DataAnomalies1 = read.table("DataWithAnomalies1.txt", header = TRUE, sep = ",")


entryGAP <- c()
entryGI <- c()
start = "09:00:00"
end =   "11:59:00"

for(i in 1:518816){
  myDate <- as.Date(DataAnomalies1$Date[i], "%d/%m/%Y")
  if(weekdays(myDate) == "Monday"){
    if((strptime(DataAnomalies1$Time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(DataAnomalies1$Time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      entryGAP <- c(entryGAP,DataAnomalies1$Global_active_power[i])
      entryGI <- c(entryGI,DataAnomalies1$Global_intensity[i])
      
    }
  }
}

entry <- data.frame(entryGAP,entryGI)