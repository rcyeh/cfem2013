Exchange_Trade_Summary
EX Overview: 
A detailed summary result for a volume break down across different exchange
First group by Listing Exchange. Ex: A Listed, N Listed
Then group by time. Ex: Early: First 10 mins of trading, Late: Last 10 mins; Midday is inbetween ealry and late
The total volume was broken down the trading venue.
EX_Detail: Based on Ex Overview, but breaks down even further by bid/ask venue
D Flows: Construct the volume fraction of the date chart
Individual Ticker Data: Testing of volume fraction on the Ticker Level

final_vector: Stat result for Market Maker Analysis
Tickers: The Ticker Symbol
mkt_order_frac: Fraction of Market Order
lim_order_frac: Fraction of Limit Order
other_lim_order_frac: Fraction of other Limit Order
price_imp_frac: Fraction of Price Improvement
vol_with_price_imp: The Exact Volume of Price Improvement
at_quote_frac: Fraction of trade at quote
os_quote_frac: Fraction of trade outside quote
eff_spread: The effective spread of the ticker
total_exe_vol: The total executed volume 
Price: Average Price of ticker over a month
D Vol: The amount of the ticker traded on D for the same month (By adding all trading day D Volume)
Spread: Effective Spread on the Exchange D for that Ticker (Data from KuangDi Subproject2)
Eff_Spread/Price: Effective Spread of Market Maker/Price	
Eff_Vol/D_Vol= Executed Volume for Market Maker/Total D Volume over a month
Spread/price: Effective Spread on the D/Pricce
D_vol_month	Exc/D total Vol	Rank(Exc Vol)=
tvol_month: Total Volume over the month for that ticker	
tick_size: Tick Size of the Ticker
1-Apr: Exchange D Volume on the day of April 1
tvol: Total Volume on the day of April 1
2-Apr: Exchange D Volume on the day of April 2
tvol: Total Volume on the day of April 2
...
30-Apr: Exchange D Volume on the day of April 30
tvol: Total Volume on the day of April 30

Market Maker D Exchange: This is cleaned up version of final_vector with some additional field for analysis
Tickers: Symbol of the ticker
ETF?: Whether the ticker is an ETF, if it is 1, then it is an ETF
Exc Vol April: Rank of Executed Market Maker Volume for the ticker in April
Exc Vol May: Rank of Executed Market Maker Volume for the ticker in May
tvol_month: Total Volume of Ticker over the same montj
April D Exchange: Rank of D Exchange Volume of the ticker in April
May D Exchange: Rank of D Exchange Volume of the ticker in May
April Rank Diff:  April D Exchange-Exc Vol April
May Rank Diff:	May D Exchange-Exc Vol May
Volume Bucket: The percentile of volume this ticker belongs to.  Bucket 1: Bottom 10%, Bucket 2: 10%-20%
Effective Spread: Effective Spread of the ticker
Price: Average Price of the ticker over the same month
LogP: Log10 of the Price;
SameSide: Whether the ticker switches side in the next month, 1: Stays same side
Middle: Whether the ticker is within +/- rank distance away from 45 degree line, 1: In the middle
PriceRank: Rank of Log Price
Distance: Distance to the 45 degree line
Weights: Weights computed by total volume, =total volume of this ticker/total volume of all tickers
Volatility: 1 if the ticker has top 10% volatility
Spread/Tick Size: Effective Spread of Market Maker/Tick Size

Ticker List: 
List of Ticker that is the intersect between Market Maker Ticker and D Exchange for all days
Take intersect of all D Exchange Tickers for each day
Then take intersect of the result with the Market Maker's list of tickers
