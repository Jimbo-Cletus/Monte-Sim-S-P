#Universal Variables

#Risk-Free Interest Rate
#52-Week T-Bill Rates https://ycharts.com/indicators/1_year_treasury_rate
#Long-Term Average
r=.02856

#Dividend Yield
#THIS IS NOW VIVIAN YEILD BRING IN THE EXPLICATION
div=.01363478261

#Volatility
#If able find time frame and get calculations based on different Ts
#https://www.barchart.com/stocks/quotes/$SPX/options?expiration=2022-11-18-w&view=stacked&moneyness=10
vol=.2666

#SPX Last 10 Years Low --> High
#https://www.barchart.com/stocks/quotes/$SPX/interactive-chart
#S0<- 1398:4818 /Range of SPX/
#S0 = c(1500,2000,2500,3000,3500,4000,4500,5000,5500) /Vector of SPX/
S0=c(1500,2000,2500,3000,3500,4000,4500,5000,5500,6000) #AoN Case
S0constant=4026 #11/25/2022 Close
#n1 = length(S0) #Size of Data

#Maturity Times Vector
t= c(1/12,2/12,4/12,6/12,8/12,10/12,1,3,5,10)
tconstant=1
n2 = length(t) #Size of Data

#Strike Prices
#Should the Strike Price Range Be Changed
#K = 2000:7000
K = c(1500,2000,2500,3000,3500,4000,4500,5000,5500,6000)
Kconstant=4026 #At The Money
n3 = length(K)



#--------------------------------------------------------------------------------------------------------------
#Theoretical Section
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
BScallcstk = (S0*exp(-div*t)*pnorm(d1cstk))-(Kconstant*exp(-r*t)*pnorm(d2cstk))

#Theoretical Deltas
deltacstk=(exp(-div*t)*pnorm(d1cstk))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStcstk = (S0 * (exp(r-div) * t))

#Constants for Both--------------------------------------------------------------------------------------------

#D1 & D2 Values
d1bothcst = ((log(S0constant/Kconstant)+ (r-div+(vol^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2bothcst = (d1bothcst - vol*sqrt(tconstant))

#Theoretical Call Price
BScallbothcst = (S0constant*exp(-div*tconstant)*pnorm(d1bothcst))-(Kconstant*exp(-r*tconstant)*pnorm(d2bothcst))

#Theoretical Deltas
deltabothcst=(exp(-div*tconstant)*pnorm(d1bothcst))

#M5-17 Module 5 Lesson 1 Formula List (Alpha = r since we are considering Risk Free)
BSStbothcst = (S0constant * exp(r - div) * tconstant)



#--------------------------------------------------------------------------------------------------------------
#Monte Carlo Simulation Section
#--------------------------------------------------------------------------------------------------------------



#Constant T at T=1 --------------------------------------------------------------------------------------------

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
Payoffmaxcalccstt = expand.grid(Stcstt,K)
Payoffmaxcalccstt$Var3 <- Payoffmaxcalccstt$Var1 - Payoffmaxcalccstt$Var2
Payoffmaxcalccstt$Var4 <- 0
Payoffmaxcalccstt$Var5 <- pmax(Payoffmaxcalccstt$Var3, Payoffmaxcalccstt$Var4)


#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstt$Payoff <- (Payoffmaxcalccstt$Var5 * exp(-r*tconstant))


#Monte Carlo Delta
#I MIGHT BE USING THE WRONG PAYOFF HERE

#E* (St1(ST>K))/S(0)
#= E*(st) N(d1)/S(0) 


Montydeltacstt = ((BSStcstt * pnorm(d1cstt)) / (S0))

Montydeltacsttmean <- mean(Montydeltacstt)


#Constant K&S0 of 4026  At The Money Call----------------------------------------------------------------------

set.seed(57609)
#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#Stcsttframe = expand.grid(Zi,S0)
#Stcsttframe$Var3 <- (S0 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
#1500,2000,2500,3000,3500,4000,4500,5000,5500,6000
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


#t= c(1/12,2/12,4/12,6/12,8/12,10/12,1,3,5,10)


#CHECK FORMULA ORDER OF OPERATIONS AND SHOULD I ELEMINATE THE ST VALUES BELOW 0
for(i in 1:5000)
{
  Stcstk1month = (S0constant * (exp((r-div-((vol^2)/2))*(1/12)) + (vol * sqrt(1/12) * Zi)))
}
for(i in 1:5000)
{
  Stcstk2month = (S0constant * (exp((r-div-((vol^2)/2))*(2/12)) + (vol * sqrt(2/12) * Zi)))
}
for(i in 1:5000)
{
  Stcstk4month = (S0constant * (exp((r-div-((vol^2)/2))*(4/12)) + (vol * sqrt(4/12) * Zi)))
}
for(i in 1:5000)
{
  Stcstk6month = (S0constant * (exp((r-div-((vol^2)/2))*(6/12)) + (vol * sqrt(6/12) * Zi)))
}
for(i in 1:5000)
{
  Stcstk8month = (S0constant * (exp((r-div-((vol^2)/2))*(8/12)) + (vol * sqrt(8/12) * Zi)))
}
for(i in 1:5000)
{
  Stcstk10month = (S0constant * (exp((r-div-((vol^2)/2))*(10/12)) + (vol * sqrt(10/12) * Zi)))
}
for(i in 1:5000)
{
  Stcstk1year = (S0constant * (exp((r-div-((vol^2)/2))*(1)) + (vol * sqrt(1) * Zi)))
}
for(i in 1:5000)
{
  Stcstk3year = (S0constant * (exp((r-div-((vol^2)/2))*(3)) + (vol * sqrt(3) * Zi)))
}
for(i in 1:5000)
{
  Stcstk5year = (S0constant * (exp((r-div-((vol^2)/2))*(5)) + (vol * sqrt(5) * Zi)))
}
for(i in 1:5000)
{
  Stcstk10year = (S0constant * (exp((r-div-((vol^2)/2))*(10)) + (vol * sqrt(10) * Zi)))
}


Stcstk <- c(Stcstk1month,Stcstk2month,Stcstk4month,Stcstk6month,Stcstk8month,Stcstk10month,Stcstk1year,Stcstk3year,Stcstk5year,Stcstk10year)


#Calculating the Maximum of St-K and 0 in a Data Frame
Payoffmaxcalccstk = expand.grid(Stcstk,Kconstant)
Payoffmaxcalccstk$Var3 <- Payoffmaxcalccstk$Var1 - Payoffmaxcalccstk$Var2
Payoffmaxcalccstk$Var4 <- 0
Payoffmaxcalccstk$Var5 <- pmax(Payoffmaxcalccstk$Var3, Payoffmaxcalccstk$Var4)


#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalccstk$Payoff <- (Payoffmaxcalccstk$Var5 * exp(-r*tconstant))


#Monte Carlo Delta
Montydeltacstk = ((BSStcstk * pnorm(d1cstk)) / (S0constant))

Montydeltacstkmean <- mean(Montydeltacstt)









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
Payoffmaxcalc = expand.grid(Stbothcst,Kconstant)
Payoffmaxcalc$Var3 <- Payoffmaxcalc$Var1 - Payoffmaxcalc$Var2
Payoffmaxcalc$Var4 <- 0
Payoffmaxcalc$Var5 <- pmax(Payoffmaxcalc$Var3, Payoffmaxcalc$Var4)

#c(S,K,T) = 𝐸∗ 𝑒−𝑟𝑇 𝑆𝑇 −𝐾 +
Payoffmaxcalc$Payoff <- (Payoffmaxcalc$Var5 * exp(-r*tconstant))


#Mean of all 5000 payoffs
Payoffmean = mean(Payoffmaxcalc$Payoff)


#Little t = 0  in our case
Montydeltaboth = ((BSStbothcst * pnorm(d1bothcst)) / (S0constant))

Montydeltameanboth <- mean(Montydeltaboth)




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#0s Removed NOT SURE IF THIS WAS RIGHT TO DO
#Montydeltatrimstep = ((exp(-r*tconstant) * Payoffmaxcalc$Payoff) / S0constant)

#Montydeltatrimstep[Montydeltatrimstep==0] <- NA

#Montydeltatrimmed <- Montydeltatrimstep[complete.cases(Montydeltatrimstep)]

#MONTE-CARLO DELTA AVERAGE IS SIGNIFICANTLY DIFFERENT THAN THE THEORETICAL ONE I MIGHT OF MESSED UP FORMULA ABOVE
#Montydeltamean = mean(Montydeltatrimmed)
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






#--------------------------------------------------------------------------------------------------------------
#Greek Approximations Using BS Theoretical Values
#--------------------------------------------------------------------------------------------------------------

#Eplison Range
eps = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1)

#H Range
#MAYBE CHANGE H RANGE TO BE ONE DECIMAL OVER
H = c(.25,.5,.75,1,2,3,4,5,6,7)



#Constants for Both---------------------------------------------------------------------------------------------

#Little t will be 0 in our case
#Formulas from Exam IFM Formulas and Tables for Paper/Pencil
Nd1primebothcst = exp(-d1bothcst^2/2)*(1/sqrt(2*pi))
Nd2primebothcst = exp(-d2bothcst^2/2)*(1/sqrt(2*pi))


#Formulas from Exam IFM Formulas and Tables for Paper/Pencil BUT BOOK SAYS SLIGHTLY DIFFERENT
Gammabothcst = ((exp(-div*tconstant) * Nd1primebothcst) / (BSStbothcst * vol * sqrt(tconstant)))
Thetabothcst = ((div * BSStbothcst * exp(-div *tconstant) * pnorm(d1bothcst)) - (r * Kconstant *exp(-r*tconstant) * pnorm(d2bothcst)) - ((Kconstant * exp(-r*tconstant) * Nd2primebothcst * vol) / (2 * sqrt(tconstant))))


#Delta Approximation --> V(S + epsilon,t) approx= V(S,t) + Delta(S,t)*epsilon
Deltaapproxbothcst <- (BScallbothcst + (deltabothcst * eps))

#Delta - Gamma Approximation --> V(S + epsilon,t) approx= V(S,t) + Delta(S,t)*epsilon + Gamma * epsilon^2
Deltagammaapproxbothcst <- (BScallbothcst + (deltabothcst * eps) + (Gammabothcst * (eps^2)))


#Delta - Gamma  -  Theta Approximation --> V(S + epsilon,t) approx= V(S,t) + Delta(S,t)*epsilon + .5 * Gamma * epsilon^2 + theta * h

DGTapproxbothcsthandeps = expand.grid(eps,H)
DGTapproxbothcsthandeps$Var3 <- (BScallbothcst + (deltabothcst * eps) + (.5 * (Gammabothcst * (eps^2))) + (Thetabothcst * H ))












#Constant T at T=1 --------------------------------------------------------------------------------------------

#Little t will be 0 in our case
#Formulas from Exam IFM Formulas and Tables for Paper/Pencil
Nd1primecstt = exp(-d1cstt^2/2)*(1/sqrt(2*pi))
Nd2primecstt = exp(-d2cstt^2/2)*(1/sqrt(2*pi))


#Formulas from Exam IFM Formulas and Tables for Paper/Pencil BUT BOOK SAYS SLIGHTLY DIFFERENT
Gammacstt = ((exp(-div*tconstant) * Nd1primecstt) / (BSStcstt * vol * sqrt(tconstant)))
Thetacstt = ((div * BSStcstt * exp(-div *tconstant) * pnorm(d1cstt)) - (r * K *exp(-r*tconstant) * pnorm(d2cstt)) - ((K * exp(-r*tconstant) * Nd2primecstt * vol) / (2 * sqrt(tconstant))))








#Constant K&S0 of 4026  At The Money Call----------------------------------------------------------------------------

#Little t will be 0 in our case
Nd1primecstk = exp(-d1cstk^2/2)*(1/sqrt(2*pi))
Nd2primecstk = exp(-d2cstk^2/2)*(1/sqrt(2*pi))

Gammacstk = ((exp(-div*t) * Nd1primecstk) / (BSStcstk * vol * sqrt(t)))
Thetacstk =  ((div * Stcstk * exp(-div *t) * pnorm(d1cstk)) - (r * Kconstant *exp(-r*t) * pnorm(d2cstk)) - ((Kconstant * exp(-r*t) * Nd2primecstk * vol) / (2 * sqrt(t))))






#-------------------------------------------------------------------------------------------------------------------------------------------

#PLOTS

#-------------------------------------------------------------------------------------------------------------------------------------------

#Needed 
#1. Monte Carlo price and delta using empirical mean (as written in the instructions), i'm assuming for payoff 
#2. graphs (delta and price changing with s and t)
#3. delta gamma theta results (and 1 graph)
#4. hopefully a list of st values (enough to fit into 1 slide) just to show them so we can lengthen ppt





#10 Values
T_Range1 = c((1/12):10)

#5000 Values
lower_limr1 = (1/12)
upper_limr1 = 10
step_numr1 = 5000
step_sizer1 = (upper_limr1 - lower_limr1) / (step_numr1 - 1)

T_Range2 <- seq(from = lower_limr1, to = upper_limr1, by = step_sizer1)

#2278 Values for Monte-Carlo Delta Calulation with 0 values removed
lower_limr2 = (1/12)
upper_limr2 = 10
step_numr2 = 2278
step_sizer2 = (upper_limr2 - lower_limr2) / (step_numr2 - 1)

T_Range3 <- seq(from = lower_limr2, to = upper_limr2, by = step_sizer2)


#Range of S0 values with 500,000 Steps

lower_limr3 = (1500)
upper_limr3 = 6000
step_numr3 = 500000
step_sizer3 = (upper_limr3 - lower_limr3) / (step_numr3 - 1)

S0_Range1 <- seq(from = lower_limr3, to = upper_limr3, by = step_sizer3)



#50000 Values for Monte-Carlo Delta Calulation with 0 values removed
lower_limr3 = (1/12)
upper_limr3 = 10
step_numr3 = 50000
step_sizer3 = (upper_limr3 - lower_limr3) / (step_numr3 - 1)

T_Range4 <- seq(from = lower_limr3, to = upper_limr3, by = step_sizer3)


#Constant T at T=1 --------------------------------------------------------------------------------------------

#Time range against V(Price)
plot(T_Range1,BScallcstt)

#Time range against Delta
plot(T_Range1,deltacstt)

#Graphs (delta and price changing with s and t) Theoretical

plot(S0,BScallcstt)
plot(S0,deltacstt)

#Graphs (delta and price changing with s and t) Monte-Carlo

plot(S0_Range1,Payoffmaxcalccstt$Payoff)
plot(S0,Montydeltacstt)

#Constant K&S0 of 4026  At The Money Call------------------------------------------------------------------------

#Time range against V(Price)
plot(T_Range1,BScallcstk)

#Time range against Delta
plot(T_Range1,deltacstk)

#Graphs (delta and price changing with s and t) Theoretical

plot(t,BScallcstk)
plot(t,deltacstk)

#Graphs (delta and price changing with s and t) Monte-Carlo

plot(T_Range4,Payoffmaxcalccstk$Payoff)
plot(t,Montydeltacstk)


#Constants for Both---------------------------------------------------------------------------------------------


plot(T_Range2,Payoffmaxcalc$Payoff)

#Risk neutral delta and price
plot(Payoffmaxcalc$Payoff,Montydeltaboth)

#BS payoff -> one value, our theoretical result
#Monte Carlo -> various values for payoff (S-K)
#y = call values
#x = St?
plot(Stbothcst,Payoffmaxcalc$Payoff)

#Blackscholes Delta and Price
plot(BScallbothcst,deltabothcst)
plot(T_Range2,Montydeltaboth)


















#Greek Approximations--------------------------------------------------------------------------------------------------

#NEED GGPLOT OR OTHER LIBRARY FOR GIGA GRAPHS (KNOWLEDGE FOR THAT AS WELL... SINCE THIS ISNT SAS)

