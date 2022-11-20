#Generate 5000 Random Normal Variables
Zi=rnorm(5000)

#Risk-Free Interest Rate
#Might Change
#52-Week T-Bill Rates https://ycharts.com/indicators/1_year_treasury_rate
#Long-Term Average
r=.02856

#Dividend Yield
#Might Change to Mutual Fund Rates
div=.0185

#Volatility
#If able find time frame and get calculations based on different Ts
#https://www.barchart.com/stocks/quotes/$SPX/options?expiration=2022-11-18-w&view=stacked&moneyness=10
vol=.2666

S0=
  
#Maturity Times Vector
t= c(.25, .5,1,5,10)

#Strike Prices
K=

#D Valuies
d1= ((log(S0/K)+ (r-div+(vol^2/2))*t)/(vol * sqrt(t)))
d2= (d1 - vol*sqrt(t))

#Theoretical Delta
delta=(exp(-div*t)*pnorm(d1))

  
#5.1.16 5-14
St= (S0 * (exp((r-div-((vol^2)/2))*t) + (vol * sqrt(t) *Zi)))
#-------------------------------------------------------------
payoff=NULL
for(i in 1: 5000)
{
  payoff=c(payoff, ifelse(St[i]>k,St[i]-K,0))
  }
lapply

price=mean(payoff)
#-------------------------------------------------------------









