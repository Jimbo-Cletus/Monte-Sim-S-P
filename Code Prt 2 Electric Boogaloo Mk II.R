#Universal Variables

#Risk-Free Interest Rate
#52-Week T-Bill Rates https://ycharts.com/indicators/1_year_treasury_rate
#Long-Term Average
r=.02856

#Dividend Yield
#Change to Vivian Average
#Might Change to Mutual Fund Rates or something else
div=.0185

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
#We will have two calculations with varying K and T as constants
#--------------------------------------------------------------------------------------------------------------

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


#CHECK THIS
#Monte Carlo delta = 
Montydelta = (exp(-div*tconstant)*pnorm(d1bothcst))

#Constant T at T=1











#Constant K&S0 of 4026  At The Money Call





#--------------------------------------------------------------------------------------------------------------
#Greek Approximations Using BS Theoretical Values

#--------------------------------------------------------------------------------------------------------------

#Eplison Range
eps = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1)

#H Range
#MAYBE CHANGE H RANGE TO BE ONE DECIMAL OVER
H = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1)



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
Thetacstt = ((div * BSSt * exp(-div *tconstant) * pnorm(d1cstt)) - (r * K *exp(-r*tconstant) * pnorm(d2cstt)) - ((K * exp(-r*tconstant) * Nd2primecstt * vol) / (2 * sqrt(tconstant))))








#Constant K&S0 of 4026  At The Money Call----------------------------------------------------------------------------

#Little t will be 0 in our case
Nd1primecstk = exp(-d1cstk^2/2)*(1/sqrt(2*pi))
Nd2primecstk = exp(-d2cstk^2/2)*(1/sqrt(2*pi))

Gammacstk = ((exp(-div*t) * Nd1primecstk) / (BSStcstk * vol * sqrt(t)))
Thetacstk =  ((div * Stcstk * exp(-div *t) * pnorm(d1cstk)) - (r * Kconstant *exp(-r*t) * pnorm(d2cstk)) - ((Kconstant * exp(-r*t) * Nd2primecstk * vol) / (2 * sqrt(t))))






#-------------------------------------------------------------------------------------------------------------------------------------------

#PLOTS

#-------------------------------------------------------------------------------------------------------------------------------------------

T_Range = c((1/12):10)



#Constant T at T=1 --------------------------------------------------------------------------------------------

#Time range against V(Price)
plot(T_Range,BScallcstt)

#Time range against Delta
plot(T_Range,deltacstt)



#Constant K&S0 of 4026  At The Money Call------------------------------------------------------------------------

#Time range against V(Price)
plot(T_Range,BScallcstk)

#Time range against Delta
plot(T_Range,deltacstk)






