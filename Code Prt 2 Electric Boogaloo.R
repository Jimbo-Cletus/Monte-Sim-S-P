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
S0=c(1500,2000,2500,3000,3500,4000,4500,5000,5500,6000) 
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
d2cstt= (d1 - vol*sqrt(t))

#Remove the Hash tags and run after component code to see full values
#d1cstt
#d2cstt

#Theoretical Call Price
BScallcstt= (S0*exp(-div*tconstant)*pnorm(d1cstt))-(K*exp(-r*tconstant)*pnorm(d2cstt))

#Theoretical Deltas
deltacstt=(exp(-div*tconstant)*pnorm(d1cstt))



#Constant K at K=4026 At The Money Call------------------------------------------------------------------------

#D1 & D2 Values
d1cstk = ((log(S0/Kconstant)+ (r-div+(vol^2/2))*t)/(vol * sqrt(t)))
d2cstk = (d1cstk - vol*sqrt(t))

#Remove the Hash tags and run after component code to see full values
#d1cstk
#d2cstk

#Theoretical Call Price
BScallcstk = (S0*exp(-div*t)*pnorm(d1cstk))-(Kconstant*exp(-r*t)*pnorm(d2cstk))

#Theoretical Deltas
deltacstk=(exp(-div*t)*pnorm(d1cstk))



#--------------------------------------------------------------------------------------------------------------
#Monte Carlo Simulation Section
#We will have two calculations with varyings K and T as constants
#--------------------------------------------------------------------------------------------------------------


set.seed(57609)

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14
#Check if I am implementing the Z tilde right
for(i in 1:5000)
{
  Stcstk = (S0 * (exp((r-div-((vol^2)/2))*t) + (vol * sqrt(t) * Zi)))
}



















#--------------------------------------------------------------------------


#--------------------------------------------------------------------------








