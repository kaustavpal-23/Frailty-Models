#Libraries needed
library("parfm")
#Inputting the data
df<-read.csv("data.csv", sep = " ")
head(df)

df$Sex<-df$Sex-1
#Model Estimation (Gamma Frailty)
mod <- parfm(Surv(Time, Status) ~ Sex + Age + Disease, cluster="Patient", data=df, dist="exponential", frailty="gamma")
mod
#Confidence Interval
ci.parfm(mod, level=0.05)["Sex",]
#Frailty Prediction and Plotting
u<-predict(mod)
plot(u,sort = "i")
#Comparison for AIC and BIC
df.parfm <- select.parfm(Surv(Time, Status) ~ Sex + Age + Disease + Time + Status, cluster="Patient", data=df, dist=c("exponential", "weibull", "loglogistic", "lognormal", "logskewnormal"), frailty=c("gamma", "ingau", "possta", "lognormal"))
df.parfm
plot(df.parfm)

#Inverse Gaussian Frailty
parfm(Surv(Time, Status) ~ Sex + Age + Disease, cluster="Patient", data=df, dist="exponential", frailty="ingau")
#Positive Stable Frailty
parfm(Surv(Time, Status) ~ Sex + Age + Disease + Time, cluster="Patient", data=df, dist="exponential", frailty="possta", iniFpar=0.1)
#Semi-parametric Model
coxph(Surv(Time, Status) ~ Sex + Age + Disease + frailty(Patient, distribution="gamma", eps=1e-11), outer.max=15, data=df)