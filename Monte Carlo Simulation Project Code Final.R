#Universal Variables
#Risk-Free Interest Rate
#52-Week T-Bill Rates https://ycharts.com/indicators/1_year_treasury_rate
#Long-Term Average
r=.02856

#Dividend Yield
#From Appended Code from Vivian
div=.01363478261

#Volatility
#From Appended Code from Vivian
vol=0.239494036992927

#SPX Last 10 Years Low --> High
#https://www.barchart.com/stocks/quotes/$SPX/interactive-chart
#S0<- 1398:4818 /Range of SPX/
#S0 = c(1500,2000,2500,3000,3500,4000,4500,5000,5500) /Vector of SPX/
S0=c(1500,2000,2500,3000,3500,4000,4500,5000,5500,6000) #AoN Case
S0constant=4026 #11/25/2022 Close
#n1 = length(S0) #Size of Data

#Maturity Times Vector
#t= c(1/12,2/12,4/12,6/12,8/12,10/12,1,3,5,10)
t= c(1,2,3,4,5,6,7,8,9,10)
tconstant=1
n2 = length(t) #Size of Data

#Strike Prices
#Should the Strike Price Range Be Changed
#K = 2000:7000
K = c(1500,2000,2500,3000,3500,4000,4500,5000,5500,6000)
Kconstant=4026 #At The Money
n3 = length(K)



#--------------------------------------------------------------------------------------------------------------
#Theoretical Section (At The Money)
#We will have two calculations with varying K and T as constants
#--------------------------------------------------------------------------------------------------------------



#Constant T at T=1 --------------------------------------------------------------------------------------------

#D1 & D2 Values
d1cstt= ((log(S0/K)+ (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2cstt= (d1cstt - vol*sqrt(tconstant))

#Remove the Hash tags and run after component code to see full values
#d1cstt
#d2cstt

#Theoretical Call Price
BScallcstt= (S0*exp(-div*tconstant)*pnorm(d1cstt))-(K*exp(-r*tconstant)*pnorm(d2cstt))

#Theoretical Deltas
deltacstt=(exp(-div*tconstant)*pnorm(d1cstt))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcstt = (S0 * (exp(r-div) * tconstant))

#Constant K&S0 of 4026  At The Money Call------------------------------------------------------------------------

#D1 & D2 Values
d1cstk = ((log(S0constant/Kconstant)+ (r-div+(vol^2/2))*t)/(vol * sqrt(t)))
d2cstk = (d1cstk - vol*sqrt(t))

#Remove the Hash tags and run after component code to see full values
#d1cstk
#d2cstk

#Theoretical Call Price
BScallcstk = (S0constant*exp(-div*t)*pnorm(d1cstk))-(Kconstant*exp(-r*t)*pnorm(d2cstk))

#Theoretical Deltas
deltacstk=(exp(-div*t)*pnorm(d1cstk))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcstk = (S0constant * (exp((r - div) * t)))

#Constants for Both--------------------------------------------------------------------------------------------

#D1 & D2 Values
d1bothcst = ((log(S0constant/Kconstant)+ (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2bothcst = (d1bothcst - vol*sqrt(tconstant))

#Theoretical Call Price
BScallbothcst = (S0constant * exp(-div*tconstant) * pnorm(d1bothcst)) - (Kconstant * exp(-r*tconstant) * pnorm(d2bothcst))

#Theoretical Deltas
deltabothcst=(exp(-div*tconstant)*pnorm(d1bothcst))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStbothcst = (S0constant * exp(r - div) * tconstant)



#--------------------------------------------------------------------------------------------------------------
#Monte Carlo Simulation Section
#--------------------------------------------------------------------------------------------------------------



#Constant T at T=1 --------------------------------------------------------------------------------------------

#OCD CHECK THE FORMULA WHEN HOME

set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

#Stcsttframe = expand.grid(Zi,S0)
#Stcsttframe$Var3 <- (S0 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
#1500,2000,2500,3000,3500,4000,4500,5000,5500,6000

for(i in 1:5000)
{
  Stcstts01500 = (1500 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts02000 = (2000 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts02500 = (2500 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts03000 = (3000 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts03500 = (3500 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts04000 = (4000 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts04500 = (4500 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts05000 = (5000 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts05500 = (5500 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}
for(i in 1:5000)
{
  Stcstts06000 = (6000 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

Stcstt <- c(Stcstts01500,Stcstts02000,Stcstts02500,Stcstts03000,Stcstts03500,Stcstts04000,Stcstts04500,Stcstts05000,Stcstts05500,Stcstts06000)

#Calculating the Maximum of St-K and 0 in a Data Frame


#Generate Data Frames for each of the S0/K Values
Payoffmaxcalccstt1500 = expand.grid(Stcstts01500,1500)
Payoffmaxcalccstt2000 = expand.grid(Stcstts02000,2000)
Payoffmaxcalccstt2500 = expand.grid(Stcstts02500,2500)
Payoffmaxcalccstt3000 = expand.grid(Stcstts03000,3000)
Payoffmaxcalccstt3500 = expand.grid(Stcstts03500,3500)
Payoffmaxcalccstt4000 = expand.grid(Stcstts04000,4000)
Payoffmaxcalccstt4500 = expand.grid(Stcstts04500,4500)
Payoffmaxcalccstt5000 = expand.grid(Stcstts05000,5000)
Payoffmaxcalccstt5500 = expand.grid(Stcstts05500,5500)
Payoffmaxcalccstt6000 = expand.grid(Stcstts06000,6000)


#Perform Equation to caluate time 0 payoff at each of the S0/K Values


#1500
Payoffmaxcalccstt1500$Var3 <- Payoffmaxcalccstt1500$Var1 - Payoffmaxcalccstt1500$Var2
Payoffmaxcalccstt1500$Var4 <- 0
Payoffmaxcalccstt1500$Var5 <- pmax(Payoffmaxcalccstt1500$Var3, Payoffmaxcalccstt1500$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt1500$Payoff <- (Payoffmaxcalccstt1500$Var5 * exp(-r*tconstant))


#2000
Payoffmaxcalccstt2000$Var3 <- Payoffmaxcalccstt2000$Var1 - Payoffmaxcalccstt2000$Var2
Payoffmaxcalccstt2000$Var4 <- 0
Payoffmaxcalccstt2000$Var5 <- pmax(Payoffmaxcalccstt2000$Var3, Payoffmaxcalccstt2000$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt2000$Payoff <- (Payoffmaxcalccstt2000$Var5 * exp(-r*tconstant))


#2500
Payoffmaxcalccstt2500$Var3 <- Payoffmaxcalccstt2500$Var1 - Payoffmaxcalccstt2500$Var2
Payoffmaxcalccstt2500$Var4 <- 0
Payoffmaxcalccstt2500$Var5 <- pmax(Payoffmaxcalccstt2500$Var3, Payoffmaxcalccstt2500$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt2500$Payoff <- (Payoffmaxcalccstt2500$Var5 * exp(-r*tconstant))

#3000
Payoffmaxcalccstt3000$Var3 <- Payoffmaxcalccstt3000$Var1 - Payoffmaxcalccstt3000$Var2
Payoffmaxcalccstt3000$Var4 <- 0
Payoffmaxcalccstt3000$Var5 <- pmax(Payoffmaxcalccstt3000$Var3, Payoffmaxcalccstt3000$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt3000$Payoff <- (Payoffmaxcalccstt3000$Var5 * exp(-r*tconstant))

#3500
Payoffmaxcalccstt3500$Var3 <- Payoffmaxcalccstt3500$Var1 - Payoffmaxcalccstt3500$Var2
Payoffmaxcalccstt3500$Var4 <- 0
Payoffmaxcalccstt3500$Var5 <- pmax(Payoffmaxcalccstt3500$Var3, Payoffmaxcalccstt3500$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt3500$Payoff <- (Payoffmaxcalccstt3500$Var5 * exp(-r*tconstant))

#4000
Payoffmaxcalccstt4000$Var3 <- Payoffmaxcalccstt4000$Var1 - Payoffmaxcalccstt4000$Var2
Payoffmaxcalccstt4000$Var4 <- 0
Payoffmaxcalccstt4000$Var5 <- pmax(Payoffmaxcalccstt4000$Var3, Payoffmaxcalccstt4000$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt4000$Payoff <- (Payoffmaxcalccstt4000$Var5 * exp(-r*tconstant))

#4500
Payoffmaxcalccstt4500$Var3 <- Payoffmaxcalccstt4500$Var1 - Payoffmaxcalccstt4500$Var2
Payoffmaxcalccstt4500$Var4 <- 0
Payoffmaxcalccstt4500$Var5 <- pmax(Payoffmaxcalccstt4500$Var3, Payoffmaxcalccstt4500$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt4500$Payoff <- (Payoffmaxcalccstt4500$Var5 * exp(-r*tconstant))

#5000
Payoffmaxcalccstt5000$Var3 <- Payoffmaxcalccstt5000$Var1 - Payoffmaxcalccstt5000$Var2
Payoffmaxcalccstt5000$Var4 <- 0
Payoffmaxcalccstt5000$Var5 <- pmax(Payoffmaxcalccstt5000$Var3, Payoffmaxcalccstt5000$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt5000$Payoff <- (Payoffmaxcalccstt5000$Var5 * exp(-r*tconstant))

#5500
Payoffmaxcalccstt5500$Var3 <- Payoffmaxcalccstt5500$Var1 - Payoffmaxcalccstt5500$Var2
Payoffmaxcalccstt5500$Var4 <- 0
Payoffmaxcalccstt5500$Var5 <- pmax(Payoffmaxcalccstt5500$Var3, Payoffmaxcalccstt5500$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt5500$Payoff <- (Payoffmaxcalccstt5500$Var5 * exp(-r*tconstant))

#6000
Payoffmaxcalccstt6000$Var3 <- Payoffmaxcalccstt6000$Var1 - Payoffmaxcalccstt6000$Var2
Payoffmaxcalccstt6000$Var4 <- 0
Payoffmaxcalccstt6000$Var5 <- pmax(Payoffmaxcalccstt6000$Var3, Payoffmaxcalccstt6000$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt6000$Payoff <- (Payoffmaxcalccstt6000$Var5 * exp(-r*tconstant))




#All the above Collumns inside a single variable
Montypayoffcstt <- c(Payoffmaxcalccstt1500$Payoff,Payoffmaxcalccstt2000$Payoff,Payoffmaxcalccstt2500$Payoff,Payoffmaxcalccstt3000$Payoff,Payoffmaxcalccstt3500$Payoff,Payoffmaxcalccstt4000$Payoff,Payoffmaxcalccstt4500$Payoff,Payoffmaxcalccstt5000$Payoff,Payoffmaxcalccstt5500$Payoff,Payoffmaxcalccstt6000$Payoff)

#Mean of all Payoff Values Though do note that we are shifting about different S0/Ks here
Montypayoffcsttmean <- mean(Montypayoffcstt)

#Monte-Carlo Delta at Constant t=1
Montydeltacstt = ((BSStcstt * pnorm(d1cstt)) / (S0))

#Mean of Monte-Carlo Delta at Constant t=1
Montydeltacsttmean <- mean(Montydeltacstt)


#Constant K&S0 of 4026  At The Money Call----------------------------------------------------------------------

set.seed(57609)
#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

#t is 1:10


for(i in 1:5000)
{
  Stcstk1year = (S0constant * (exp((r-div-((vol^2)/2))*(1)) + (vol * sqrt(1) * Zi)))
}
for(i in 1:5000)
{
  Stcstk2year = (S0constant * (exp((r-div-((vol^2)/2))*(2)) + (vol * sqrt(2) * Zi)))
}
for(i in 1:5000)
{
  Stcstk3year = (S0constant * (exp((r-div-((vol^2)/2))*(3)) + (vol * sqrt(3) * Zi)))
}
for(i in 1:5000)
{
  Stcstk4year = (S0constant * (exp((r-div-((vol^2)/2))*(4)) + (vol * sqrt(4) * Zi)))
}
for(i in 1:5000)
{
  Stcstk5year = (S0constant * (exp((r-div-((vol^2)/2))*(5)) + (vol * sqrt(5) * Zi)))
}
for(i in 1:5000)
{
  Stcstk6year = (S0constant * (exp((r-div-((vol^2)/2))*(6)) + (vol * sqrt(6) * Zi)))
}
for(i in 1:5000)
{
  Stcstk7year = (S0constant * (exp((r-div-((vol^2)/2))*(7)) + (vol * sqrt(7) * Zi)))
}
for(i in 1:5000)
{
  Stcstk8year = (S0constant * (exp((r-div-((vol^2)/2))*(8)) + (vol * sqrt(8) * Zi)))
}
for(i in 1:5000)
{
  Stcstk9year = (S0constant * (exp((r-div-((vol^2)/2))*(9)) + (vol * sqrt(9) * Zi)))
}
for(i in 1:5000)
{
  Stcstk10year = (S0constant * (exp((r-div-((vol^2)/2))*(10)) + (vol * sqrt(10) * Zi)))
}


Stcstk <- c(Stcstk1year,Stcstk2year,Stcstk3year,Stcstk4year,Stcstk5year,Stcstk6year,Stcstk7year,Stcstk8year,Stcstk9year,Stcstk10year)


#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalccstk = expand.grid(Stcstk,Kconstant)
Payoffmaxcalccstk$Var3 <- Payoffmaxcalccstk$Var1 - Payoffmaxcalccstk$Var2
Payoffmaxcalccstk$Var4 <- 0
Payoffmaxcalccstk$Var5 <- pmax(Payoffmaxcalccstk$Var3, Payoffmaxcalccstk$Var4)


#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstk$Payoff <- (Payoffmaxcalccstk$Var5 * exp(-r*tconstant))

#Mean of all Payoff Values Though do note that we are shifting about different ts here
Montypayoffcstkmean <- mean(Payoffmaxcalccstk$Payoff)

#Monte Carlo Delta Values across different S0/K sets
Montydeltacstk = ((BSStcstk * pnorm(d1cstk)) / (S0constant))

#Monty Carlo Delta Average
Montydeltacstkmean <- mean(Montydeltacstk)

Montydeltacstkmean

#Constants for Both--------------------------------------------------------------------------------------------

set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

for(i in 1:5000)
{
  Stbothcst = (S0constant * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalcbothcst = expand.grid(Stbothcst,Kconstant)
Payoffmaxcalcbothcst$Var3 <- Payoffmaxcalcbothcst$Var1 - Payoffmaxcalcbothcst$Var2
Payoffmaxcalcbothcst$Var4 <- 0
Payoffmaxcalcbothcst$Var5 <- pmax(Payoffmaxcalcbothcst$Var3, Payoffmaxcalcbothcst$Var4)

#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalcbothcst$Payoff <- (Payoffmaxcalcbothcst$Var5 * exp(-r*tconstant))



#Mean of all 5000 payoffs
Payoffbothcstmean = mean(Payoffmaxcalcbothcst$Payoff)


#Little t = 0  in our case
#Monte Carlo Delta with both t & S0/K constants ATM
Montydeltaboth = ((BSStbothcst * pnorm(d1bothcst)) / (S0constant))




#______________________________________________________________________________________________________________
#--------------------------------------------------------------------------------------------------------------
#Theoretical Section (Out The Money)
#We will have two calculations with varying K and T as constants but a constant S0 of 4026 Throughout and Constant of K=5000 for OTM S0/K controlled
#--------------------------------------------------------------------------------------------------------------
#______________________________________________________________________________________________________________


Kout <- c(4027,4500,5000,5500,6000,6500,7000,7500,8000,8500)
Koutconstant = 5000

#Constant T at T=1 --------------------------------------------------------------------------------------------


#D1 & D2 Values
d1csttout= ((log(S0constant/Kout) + (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2csttout= (d1csttout - vol*sqrt(tconstant))


#Theoretical Call Price
BScallcsttout= (S0constant*exp(-div*tconstant)*pnorm(d1csttout))-(Kout*exp(-r*tconstant)*pnorm(d2csttout))

#Theoretical Deltas
deltacsttout=(exp(-div*tconstant)*pnorm(d1csttout))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcsttout = (S0constant * (exp(r-div) * tconstant))



#Constant S0 = 4026 K = 5000  At The Money Call------------------------------------------------------------------------

#D1 & D2 Values
d1cstkout = ((log(S0constant/Koutconstant)+ (r-div+(vol^2/2))*t)/(vol * sqrt(t)))
d2cstkout = (d1cstkout - vol*sqrt(t))

#Remove the Hash tags and run after component code to see full values
#d1cstk
#d2cstk

#Theoretical Call Price
BScallcstkout = (S0constant*exp(-div*t)*pnorm(d1cstkout))-(Koutconstant*exp(-r*t)*pnorm(d2cstkout))

#Theoretical Deltas
deltacstkout=(exp(-div*t)*pnorm(d1cstkout))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcstkout = (S0constant * (exp((r-div) * t)))

#Constants for Both--------------------------------------------------------------------------------------------

#D1 & D2 Values
d1bothcstout = ((log(S0constant/Koutconstant)+ (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2bothcstout = (d1bothcstout - (vol*sqrt(tconstant)))

#Theoretical Call Price
BScallbothcstout = (S0constant*exp(-div*tconstant)*pnorm(d1bothcstout))-(Koutconstant*exp(-r*tconstant)*pnorm(d2bothcstout))

#Theoretical Deltas
deltabothcstout=(exp(-div*tconstant)*pnorm(d1bothcstout))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStbothcstout = (S0constant * exp(r - div) * tconstant)



#--------------------------------------------------------------------------------------------------------------
#Monte Carlo Simulation Section OTM S0 = 4026
#--------------------------------------------------------------------------------------------------------------


#Constant T at T=1 OTM --------------------------------------------------------------------------------------------


set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

for(i in 1:5000)
{
  Stcstts4026out = (4026 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

Stcstts4026out


#Calculating the Maximum of St-K and 0 in a Data Frame

#Generate Data Frames for each of the S0/K Values
Payoffmaxcalccstt4026out = expand.grid(Stcstts4026out,Kout)


#Perform Equation to caluate time 0 payoff at each of the S0/K Values
Payoffmaxcalccstt4026out$Var3 <- Payoffmaxcalccstt4026out$Var1 - Payoffmaxcalccstt4026out$Var2
Payoffmaxcalccstt4026out$Var4 <- 0
Payoffmaxcalccstt4026out$Var5 <- pmax(Payoffmaxcalccstt4026out$Var3, Payoffmaxcalccstt4026out$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt4026out$Payoff <- (Payoffmaxcalccstt4026out$Var5 * exp(-r*tconstant))



#Mean of all Payoff Values Though do note that we are shifting about different S0/Ks here
Montypayoffcsttmeanout <- mean(Payoffmaxcalccstt4026out$Payoff)

#Monte-Carlo Delta at Constant t=1
Montydeltacsttout = ((BSStcsttout * pnorm(d1csttout)) / (S0constant))

#Mean of Monte-Carlo Delta at Constant t=1
Montydeltacsttmeanout <- mean(Montydeltacsttout)




#Constant S0 of 4026 and K=5000  At The Money Call----------------------------------------------------------------------

set.seed(57609)
#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

#t is 1:10


for(i in 1:5000)
{
  Stcstk1yearout = (S0constant * (exp((r-div-((vol^2)/2))*(1)) + (vol * sqrt(1) * Zi)))
}
for(i in 1:5000)
{
  Stcstk2yearout = (S0constant * (exp((r-div-((vol^2)/2))*(2)) + (vol * sqrt(2) * Zi)))
}
for(i in 1:5000)
{
  Stcstk3yearout = (S0constant * (exp((r-div-((vol^2)/2))*(3)) + (vol * sqrt(3) * Zi)))
}
for(i in 1:5000)
{
  Stcstk4yearout = (S0constant * (exp((r-div-((vol^2)/2))*(4)) + (vol * sqrt(4) * Zi)))
}
for(i in 1:5000)
{
  Stcstk5yearout = (S0constant * (exp((r-div-((vol^2)/2))*(5)) + (vol * sqrt(5) * Zi)))
}
for(i in 1:5000)
{
  Stcstk6yearout = (S0constant * (exp((r-div-((vol^2)/2))*(6)) + (vol * sqrt(6) * Zi)))
}
for(i in 1:5000)
{
  Stcstk7yearout = (S0constant * (exp((r-div-((vol^2)/2))*(7)) + (vol * sqrt(7) * Zi)))
}
for(i in 1:5000)
{
  Stcstk8yearout = (S0constant * (exp((r-div-((vol^2)/2))*(8)) + (vol * sqrt(8) * Zi)))
}
for(i in 1:5000)
{
  Stcstk9yearout = (S0constant * (exp((r-div-((vol^2)/2))*(9)) + (vol * sqrt(9) * Zi)))
}
for(i in 1:5000)
{
  Stcstk10yearout = (S0constant * (exp((r-div-((vol^2)/2))*(10)) + (vol * sqrt(10) * Zi)))
}


Stcstkout <- c(Stcstk1yearout,Stcstk2yearout,Stcstk3yearout,Stcstk4yearout,Stcstk5yearout,Stcstk6yearout,Stcstk7yearout,Stcstk8yearout,Stcstk9yearout,Stcstk10yearout)


#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalccstkout = expand.grid(Stcstkout,Koutconstant)
Payoffmaxcalccstkout$Var3 <- Payoffmaxcalccstkout$Var1 - Payoffmaxcalccstkout$Var2
Payoffmaxcalccstkout$Var4 <- 0
Payoffmaxcalccstkout$Var5 <- pmax(Payoffmaxcalccstkout$Var3, Payoffmaxcalccstkout$Var4)


#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstkout$Payoff <- (Payoffmaxcalccstkout$Var5 * exp(-r*tconstant))

#Mean of all Payoff Values Though do note that we are shifting about different ts here
Montypayoffcstkmeanout <- mean(Payoffmaxcalccstkout$Payoff)

#Monte Carlo Delta Values across different S0/K sets
Montydeltacstkout = ((BSStcstkout * pnorm(d1cstkout)) / (S0constant))

#Monty Carlo Delta Average
Montydeltacstkmeanout <- mean(Montydeltacstkout)


#Constants for Both S0=4026 K=5000 t=1 --------------------------------------------------------------------------------------------

set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

for(i in 1:5000)
{
  Stbothcstout = (S0constant * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalcbothcstout = expand.grid(Stbothcstout,Koutconstant)
Payoffmaxcalcbothcstout$Var3 <- Payoffmaxcalcbothcstout$Var1 - Payoffmaxcalcbothcstout$Var2
Payoffmaxcalcbothcstout$Var4 <- 0
Payoffmaxcalcbothcstout$Var5 <- pmax(Payoffmaxcalcbothcstout$Var3, Payoffmaxcalcbothcstout$Var4)

#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalcbothcstout$Payoff <- (Payoffmaxcalcbothcstout$Var5 * exp(-r*tconstant))



#Mean of all 5000 payoffs
Payoffbothcstmeanout = mean(Payoffmaxcalcbothcstout$Payoff)


#Little t = 0  in our case
#Monte Carlo Delta with both t & S0/K constants ATM
Montydeltabothout = ((BSStbothcstout * pnorm(d1bothcstout)) / (S0constant))




#______________________________________________________________________________________________________________
#--------------------------------------------------------------------------------------------------------------
#Theoretical Section (In The Money)
#We will have two calculations with varying K and T as constants but a constant S0 of 4026 Throughout
#--------------------------------------------------------------------------------------------------------------
#______________________________________________________________________________________________________________


Kin <- c(1, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000,4025)
Kinconstant = 3500

#Constant T at T=1 --------------------------------------------------------------------------------------------


#D1 & D2 Values
d1csttin= ((log(S0constant/Kin) + (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2csttin= (d1csttin - vol*sqrt(tconstant))


#Theoretical Call Price
BScallcsttin= (S0constant*exp(-div*tconstant)*pnorm(d1csttin))-(Kin*exp(-r*tconstant)*pnorm(d2csttin))

#Theoretical Deltas
deltacsttin=(exp(-div*tconstant)*pnorm(d1csttin))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcsttin = (S0constant * (exp(r-div) * tconstant))



#Constant S0 = 4026 K = 5000  At The Money Call------------------------------------------------------------------------

#D1 & D2 Values
d1cstkin = ((log(S0constant/Kinconstant)+ (r-div+(vol^2/2))*t)/(vol * sqrt(t)))
d2cstkin = (d1cstkin - vol*sqrt(t))


#Theoretical Call Price
BScallcstkin = (S0constant*exp(-div*t)*pnorm(d1cstkin))-(Kinconstant*exp(-r*t)*pnorm(d2cstkin))

#Theoretical Deltas
deltacstkin=(exp(-div*t)*pnorm(d1cstkin))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcstkin = (S0constant * (exp((r-div) * t)))

#Constants for Both--------------------------------------------------------------------------------------------

#D1 & D2 Values
d1bothcstin = ((log(S0constant/Kinconstant)+ (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2bothcstin = (d1bothcstin - (vol*sqrt(tconstant)))

#Theoretical Call Price
BScallbothcstin = (S0constant*exp(-div*tconstant)*pnorm(d1bothcstin))-(Kinconstant*exp(-r*tconstant)*pnorm(d2bothcstin))

#Theoretical Deltas
deltabothcstin=(exp(-div*tconstant)*pnorm(d1bothcstin))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStbothcstin = (S0constant * exp(r - div) * tconstant)



#--------------------------------------------------------------------------------------------------------------
#Monte Carlo Simulation Section ITM S0 = 4026 Kincostant =3500
#--------------------------------------------------------------------------------------------------------------


#Constant T at T=1 ITM --------------------------------------------------------------------------------------------


set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

for(i in 1:5000)
{
  Stcstts4026in = (4026 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

Stcstts4026in


#Calculating the Maximum of St-K and 0 in a Data Frame

#Generate Data Frames for each of the S0/K Values
Payoffmaxcalccstt4026in = expand.grid(Stcstts4026in,Kin)


#Perform Equation to caluate time 0 payoff at each of the S0/K Values
Payoffmaxcalccstt4026in$Var3 <- Payoffmaxcalccstt4026in$Var1 - Payoffmaxcalccstt4026in$Var2
Payoffmaxcalccstt4026in$Var4 <- 0
Payoffmaxcalccstt4026in$Var5 <- pmax(Payoffmaxcalccstt4026in$Var3, Payoffmaxcalccstt4026in$Var4)
#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt4026in$Payoff <- (Payoffmaxcalccstt4026in$Var5 * exp(-r*tconstant))



#Mean of all Payoff Values Though do note that we are shifting about different S0/Ks here
Montypayoffcsttmeanin <- mean(Payoffmaxcalccstt4026in$Payoff)

#Monte-Carlo Delta at Constant t=1
Montydeltacsttin = ((BSStcsttin * pnorm(d1csttin)) / (S0constant))

#Mean of Monte-Carlo Delta at Constant t=1
Montydeltacsttmeanin <- mean(Montydeltacsttin)




#Constant S0 of 4026 and K=3500  At The Money Call----------------------------------------------------------------------

set.seed(57609)
#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

#t is 1:10


for(i in 1:5000)
{
  Stcstk1yearin = (S0constant * (exp((r-div-((vol^2)/2))*(1)) + (vol * sqrt(1) * Zi)))
}
for(i in 1:5000)
{
  Stcstk2yearin = (S0constant * (exp((r-div-((vol^2)/2))*(2)) + (vol * sqrt(2) * Zi)))
}
for(i in 1:5000)
{
  Stcstk3yearin = (S0constant * (exp((r-div-((vol^2)/2))*(3)) + (vol * sqrt(3) * Zi)))
}
for(i in 1:5000)
{
  Stcstk4yearin = (S0constant * (exp((r-div-((vol^2)/2))*(4)) + (vol * sqrt(4) * Zi)))
}
for(i in 1:5000)
{
  Stcstk5yearin = (S0constant * (exp((r-div-((vol^2)/2))*(5)) + (vol * sqrt(5) * Zi)))
}
for(i in 1:5000)
{
  Stcstk6yearin = (S0constant * (exp((r-div-((vol^2)/2))*(6)) + (vol * sqrt(6) * Zi)))
}
for(i in 1:5000)
{
  Stcstk7yearin = (S0constant * (exp((r-div-((vol^2)/2))*(7)) + (vol * sqrt(7) * Zi)))
}
for(i in 1:5000)
{
  Stcstk8yearin = (S0constant * (exp((r-div-((vol^2)/2))*(8)) + (vol * sqrt(8) * Zi)))
}
for(i in 1:5000)
{
  Stcstk9yearin = (S0constant * (exp((r-div-((vol^2)/2))*(9)) + (vol * sqrt(9) * Zi)))
}
for(i in 1:5000)
{
  Stcstk10yearin = (S0constant * (exp((r-div-((vol^2)/2))*(10)) + (vol * sqrt(10) * Zi)))
}


Stcstkin <- c(Stcstk1yearin,Stcstk2yearin,Stcstk3yearin,Stcstk4yearin,Stcstk5yearin,Stcstk6yearin,Stcstk7yearin,Stcstk8yearin,Stcstk9yearin,Stcstk10yearin)


#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalccstkin = expand.grid(Stcstkin,Kinconstant)
Payoffmaxcalccstkin$Var3 <- Payoffmaxcalccstkin$Var1 - Payoffmaxcalccstkin$Var2
Payoffmaxcalccstkin$Var4 <- 0
Payoffmaxcalccstkin$Var5 <- pmax(Payoffmaxcalccstkin$Var3, Payoffmaxcalccstkin$Var4)


#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstkin$Payoff <- (Payoffmaxcalccstkin$Var5 * exp(-r*tconstant))

#Mean of all Payoff Values Though do note that we are shifting about different ts here
Montypayoffcstkmeanin <- mean(Payoffmaxcalccstkin$Payoff)

#Monte Carlo Delta Values across different S0/K sets
Montydeltacstkin = ((BSStcstkin * pnorm(d1cstkin)) / (S0constant))

#Monty Carlo Delta Average
Montydeltacstkmeanin <- mean(Montydeltacstkin)


#Constants for Both S0=4026 K=5000 t=1 --------------------------------------------------------------------------------------------

set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14

for(i in 1:5000)
{
  Stbothcstin = (S0constant * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalcbothcstin = expand.grid(Stbothcstin,Kinconstant)
Payoffmaxcalcbothcstin$Var3 <- Payoffmaxcalcbothcstin$Var1 - Payoffmaxcalcbothcstin$Var2
Payoffmaxcalcbothcstin$Var4 <- 0
Payoffmaxcalcbothcstin$Var5 <- pmax(Payoffmaxcalcbothcstin$Var3, Payoffmaxcalcbothcstin$Var4)

#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalcbothcstin$Payoff <- (Payoffmaxcalcbothcstin$Var5 * exp(-r*tconstant))



#Mean of all 5000 payoffs
Payoffbothcstmeanin = mean(Payoffmaxcalcbothcstin$Payoff)


#Little t = 0  in our case
#Monte Carlo Delta with both t & S0/K constants ATM
Montydeltabothin = ((BSStbothcstin * pnorm(d1bothcstin)) / (S0constant))




#-------------------------------------------------------------------------------------------------------------------------------------------

#Plots & Key Outputs

#-------------------------------------------------------------------------------------------------------------------------------------------

#10 Values
T_Range1 = c((1):10)

#5000 Values
lower_limr1 = (1)
upper_limr1 = 10
step_numr1 = 5000
step_sizer1 = (upper_limr1 - lower_limr1) / (step_numr1 - 1)

T_Range2 <- seq(from = lower_limr1, to = upper_limr1, by = step_sizer1)


#50000 Values for Monte-Carlo Delta Calculation
lower_limr3 = (1)
upper_limr3 = 10
step_numr3 = 50000
step_sizer3 = (upper_limr3 - lower_limr3) / (step_numr3 - 1)

T_Range3 <- seq(from = lower_limr3, to = upper_limr3, by = step_sizer3)



#50000 Range Values for the ATM S0/K range
lower_limr4 = (1500)
upper_limr4 = 6000
step_numr4 = 50000
step_sizer4 = (upper_limr4 - lower_limr4) / (step_numr4 - 1)

K_Range1 <- seq(from = lower_limr4, to = upper_limr4, by = step_sizer4)


#At The Money---------------------------------------------------------------------------------------------------------------------------------

#Graph Frame Additions
Payoffmaxcalccstk$T_Range <- T_Range3

#Montyprice vs T
plot(T_Range3,Payoffmaxcalccstk$Payoff)
abline(lm(Payoffmaxcalccstk$Payoff ~ T_Range3, data = Payoffmaxcalccstk), col = "blue")

meanPayoffmaxcalccstkpayoff <- mean(Payoffmaxcalccstk$Payoff)
meanPayoffmaxcalccstkpayoff

#Graph Frame
Montypayoffframe <- expand.grid(Montypayoffcstt,0)
Montypayoffframe$K_Range <- K_Range1

#Montyprice vs S0
plot(K_Range1,Montypayoffframe$Var1)
abline(lm(Montypayoffframe$Var1 ~ Montypayoffframe$K_Range, data = Payoffmaxcalccstk), col = "blue")

#BSprice vs T
plot(T_Range1,BScallcstk, type = "l")

#BSprice vs S0
plot(K,BScallcstt, type = "l")


#montydelta vs T
plot(T_Range1,Montydeltacstk, type = "l")


#montydleta vs S0
plot(K,Montydeltacstt, type = "l")


#BSdelta vs T
plot(T_Range1,deltacstk, type = "l")


#BSdelta vs S0
plot(K,deltacstt, type = "l")


#values for BS delta and price

#Delta ATM t=1 S0/K=4026 (4026 SPX Close 11-25-22)
deltabothcst


#Price ATM t=1 S0/K=4026 (4026 SPX Close 11-25-22)
BScallbothcst 


#Values for the Monty delta and price

#Monte Carlo Average Delta (Same Between Constant T and Constant S0/K)
Montydeltacsttmean

#Monte Carlo Price ATM t=1 S0/K=4026 (4026 SPX Close 11-25-22)
Payoffbothcstmean

#4. hopefully a list of st values (enough to fit into 1 slide)

#Monty vs T
Stcstk

#Monty vs K
Stcstt

#BS vs T
BSStcstk

#BS vs K
BSStcstt


#Monte Carlo Payoff vs its St values with Costant t=1 S0/K=4026
plot(Stbothcst,Payoffmaxcalcbothcst$Payoff)







#Out of The Money---------------------------------------------------------------------------------------------------------------------------------

#50000 Range Values for the ATM S0/K range
lower_limr5 = (4027)
upper_limr5 = 8500
step_numr5 = 50000
step_sizer5 = (upper_limr5 - lower_limr5) / (step_numr5 - 1)

K_Range1out <- seq(from = lower_limr5, to = upper_limr5, by = step_sizer5)


#Graph Frame Additions
Payoffmaxcalccstkout$T_Range <- T_Range3

#Monty price vs T
plot(T_Range3,Payoffmaxcalccstkout$Payoff)
abline(lm(Payoffmaxcalccstkout$Payoff ~ T_Range3, data = Payoffmaxcalccstkout), col = "blue")

meanPayoffmaxcalccstkpayoffout <- mean(Payoffmaxcalccstkout$Payoff)
meanPayoffmaxcalccstkpayoffout

#Graph Frame
Montypayoffframeout <- expand.grid(Payoffmaxcalccstt4026out$Payoff,0)
Montypayoffframeout$K_Range <- K_Range1out

#Montyprice vs S0
plot(K_Range1out,Montypayoffframeout$Var1)
abline(lm(Montypayoffframeout$Var1 ~ Montypayoffframeout$K_Range, data = Payoffmaxcalccstkout), col = "blue")


#BSprice vs T OTM vs ATM
plot(T_Range1,BScallcstkout, type = "l", col = "green")
#ATM Again
lines(T_Range1,BScallcstk, type = "l")


#BSprice vs K
plot(Kout,BScallcsttout, type = "l", col = "green")
#a bit scuffed framing wise because of K ranges just be better to have the two opposing graphs on this one

#montydelta vs T
plot(T_Range1,Montydeltacstkout, type = "l", col = "green")
lines(T_Range1,Montydeltacstk, type = "l")

#montydleta vs K
plot(Kout,Montydeltacsttout, type = "l")
#a bit scuffed framing wise because of K ranges just be better to have the two opposing graphs on this one

#BSdelta vs T
plot(T_Range1,deltacstkout, type = "l", col="green")
#lines(T_Range1,deltacstk, type = "l") ATM delta too large to fit without major changes

#BSdelta vs K
plot(Kout,deltacsttout, type = "l")


#values for BS delta and price

#Delta OTM t=1 S0=4026 K=5000 (4026 SPX Close 11-25-22)
deltabothcstout


#Price OTM t=1 S0=4026 K=5000 (4026 SPX Close 11-25-22)
BScallbothcstout


#Values for the Monty delta and price

#Monte Carlo Average Delta (Same Between Constant T and Constant S0/K)
Montydeltabothout

#Monte Carlo Price ATM t=1 S0=4026 K=5000(4026 SPX Close 11-25-22)
Payoffbothcstmeanout

#4. hopefully a list of st values (enough to fit into 1 slide)

#Monty vs T
Stcstkout

#Monty vs K
Stcstts4026out

#BS vs T
BSStcstkout

#BS vs K
BSStcsttout



#In the Money-------------------------------------------------------------------------------------------------------------------------------------


#50000 Range Values for the ITM S0/K range
lower_limr6 = (1)
upper_limr6 = 4025
step_numr6 = 50000
step_sizer6 = (upper_limr6 - lower_limr6) / (step_numr6 - 1)

K_Range1in <- seq(from = lower_limr6, to = upper_limr6, by = step_sizer6)


#Graph Frame Additions
Payoffmaxcalccstkin$T_Range <- T_Range3

#Monty price vs T
plot(T_Range3,Payoffmaxcalccstkin$Payoff)
abline(lm(Payoffmaxcalccstkin$Payoff ~ T_Range3, data = Payoffmaxcalccstkin), col = "blue")

meanPayoffmaxcalccstkpayoffin <- mean(Payoffmaxcalccstkin$Payoff)
meanPayoffmaxcalccstkpayoffin

#Graph Frame
Montypayoffframein <- expand.grid(Payoffmaxcalccstt4026in$Payoff,0)
Montypayoffframein$K_Range <- K_Range1in

#Montyprice vs K
plot(K_Range1in,Montypayoffframein$Var1)
abline(lm(Montypayoffframein$Var1 ~ Montypayoffframein$K_Range, data = Payoffmaxcalccstkin), col = "blue")


#BSprice vs T ITM vs ATM
plot(T_Range1,BScallcstkin, type = "l", col = "green")
#ATM Again
lines(T_Range1,BScallcstk, type = "l")


#BSprice vs K
plot(Kin,BScallcsttin, type = "l", col = "green")
#a bit scuffed framing wise because of K ranges just be better to have the two opposing graphs on this one

#montydelta vs T
plot(T_Range1,Montydeltacstkin, type = "l", col = "green")
lines(T_Range1,Montydeltacstk, type = "l")

#montydleta vs K
plot(Kin,Montydeltacsttin, type = "l")
#a bit scuffed framing wise because of K ranges just be better to have the two opposing graphs on this one

#BSdelta vs T
plot(T_Range1,deltacstkin, type = "l", col="green")
#lines(T_Range1,deltacstk, type = "l") ATM delta too large to fit without major changes

#BSdelta vs K
plot(Kin,deltacsttin, type = "l")


#values for BS delta and price

#Delta OTM t=1 S0=4026 K=5000 (4026 SPX Close 11-25-22)
deltabothcstin


#Price OTM t=1 S0=4026 K=5000 (4026 SPX Close 11-25-22)
BScallbothcstin


#Values for the Monty delta and price

#Monte Carlo Average Delta (Same Between Constant T and Constant S0/K)
Montydeltabothin

#Monte Carlo Price ATM t=1 S0=4026 K=5000(4026 SPX Close 11-25-22)
Payoffbothcstmeanin

#4. hopefully a list of st values (enough to fit into 1 slide)

#Monty vs T
Stcstkin

#Monty vs K
Stcstts4026in

#BS vs T
BSStcstkin

#BS vs K
BSStcsttin










