#Risk-Free Interest Rate
#Might Change
#52-Week T-Bill Rates https://ycharts.com/indicators/1_year_treasury_rate
#Long-Term Average
r=.02856

#Dividend Yield
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
S0=4026 #Current SPX Price 
n1 = length(S0) #Size of Data

#Maturity Times Vector
t= c(1/12,2/12,3/12,4/12,5/12,6/12,7/12,8/12,9/12,10/12,11/12,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
tconstant=1
n2 = length(t) #Size of Data

#Strike Prices
#Should the Strike Price Range Be Changed
#K = 2000:7000
K = c(1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000)
Kconstant=4026 #At The Money
n3 = length(K)


#---------------------------------------------------------------------------------------------------------------------------
#Constant Time cstt = Constant Time
#---------------------------------------------------------------------------------------------------------------------------

d1cstt = ((log(S0/K)+ (r-div+(vl^2/2))*tconstant)/(vol * sqrt(tconstant)))
d2cstt = (d1cstt - vol*sqrt(tconstant))

#lapply(K,d1cstt)

#Theoretical Delta "Derivative of BScall in terms of S"
deltacstt=(exp(-div*tconstant)*pnorm(d1cstt))
#Theoretical Call 
BScallcstt= (S0*exp(-div*tconstant)*pnorm(d1cstt))-(K*exp(-r*tconstant)*pnorm(d2cstt))

#Monte-Carlo Estimation

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14
#Check if I am implementing the Z tilde right
for(i in 1:5000)
{
  Stcstt = (S0 * (exp((r-div-((vol^2)/2))*tconstant) + (vol * sqrt(tconstant) * Zi)))
}

#5000 Zs but then need to change K HOW?!?
for (i in 1:5000)
{
  calltermnialpayoffcstt = max((St[i]-K),0)
}

callpayoffcstt = exp(-r*tconstant) * calltermnialpayoffcstt
calltermnialpayoffcstt
#---------------------------------------------------------------------------------------------------------------

#Little t will be 0 in our case
Nd1primecstt = exp(-d1cstt^2/2)*(1/sqrt(2*pi))
Nd2primecstt = exp(-d2cstt^2/2)*(1/sqrt(2*pi))

#Gamma and Theta Formulas for option Greeks
Gammacstt = ((exp(-div*tconstant) * Nd1primecstt) / (Stcstt * vol * sqrt(tconstant)))

Thetacstt =  ((div * Stcstt * exp(-div *tconstant) * pnorm(d1cstt)) - (r * K *exp(-r*tconstant) * pnorm(d2cstt)) - ((K * exp(-r*tconstant) * Nd2primecstt * vol) / (2 * sqrt(tconstant))))

#---------------------------------------------------------------------------------------------------------------------------
#Constant Strike Price cstk = Constant Strike Price
#---------------------------------------------------------------------------------------------------------------------------

d1cstk = ((log(S0/Kconstant)+ (r-div+(vol^2/2))*t)/(vol * sqrt(t)))
d2cstk = (d1cstk - vol*sqrt(t))

#lapply(K,d1cstt)

#Theoretical Delta "Derivative of BScall in terms of S"
deltacstk = (exp(-div*t)*pnorm(d1cstk))
#Theoretical Call 
BScallcstk = (S0*exp(-div*t)*pnorm(d1cstk))-(K*exp(-r*t)*pnorm(d2cstk))

#Monte-Carlo Estimation

#Algorithm 1
#Generate a vector of 5000 normally distributed random numbers
Zi=rnorm(5000)
#5.1.16 5-14
#Check if I am implementing the Z tilde right
for(i in 1:5000)
{
  Stcstk = (S0 * (exp((r-div-((vol^2)/2))*t) + (vol * sqrt(t) * Zi)))
}

#5000 Zs but then need to change K HOW?!?
for (i in 1:5000)
{
  calltermnialpayoffcstk = max((St[i]-Kconstant),0)
}

callpayoffcstk = exp(-r*t) * calltermnialpayoffcstk
calltermnialpayoffcstk
#---------------------------------------------------------------------------------------------------------------

#Little t will be 0 in our case
Nd1primecstk = exp(-d1cstk^2/2)*(1/sqrt(2*pi))
Nd2primecstk = exp(-d2cstk^2/2)*(1/sqrt(2*pi))

#Gamma and Theta Formulas for option Greeks
Gammacstk = ((exp(-div*t) * Nd1primecstk) / (Stcstk * vol * sqrt(t)))

Thetacstk =  ((div * Stcstk * exp(-div *t) * pnorm(d1cstk)) - (r * Kconstant *exp(-r*t) * pnorm(d2cstk)) - ((Kconstant * exp(-r*t) * Nd2primecstk * vol) / (2 * sqrt(t))))



#Greeks Approximation


#-------------------------------------------------------------
#payoff=NULL
#for(i in 1: 5000)
#{
#  payoff=c(payoff, ifelse(St[i]>k,St[i]-K,0))
#  }
#clapply

#price=mean(payoff)
#-------------------------------------------------------------


















