SingularityNET Ecosystem Analysis
================
Last updated: 2024-02-26

## Preliminary Work: Install/Load Packages

To try and ensure that this R Notebook will run successfully, we’ll use
the [renv
package](https://cran.r-project.org/web/packages/renv/index.html) to
create a project-specific library of packages. This will allow us to
install the packages that we need for this project without affecting any
other projects that we may be working on. Additionally, the project
library will track the specific versions of the dependency packages so
that any updates to those packages will not break this project.

The code chunk below will first install the renv package if it is not
already installed. Then we will load the package. Next, we’ll use the
`restore()` function to install any packages listed in the renv.lock
file. Once these packages are installed, we can load them into the R
session using the `library()` commands. Below the code chunk, we’ll list
out the packages that will be used in the project demo. And if you run
into any trouble using renv, then you can use the second code chunk
below and that should be an even more reliable approach to install the
required packages.

``` r
# Install renv package if not already installed
if(!"renv" %in% installed.packages()[,"Package"]) install.packages("renv")
# Load renv package
library(renv)
# Use restore() to install any packages listed in the renv.lock file
renv::restore(clean=TRUE, lockfile="../renv.lock")
# Load in the packages
library(geckor)
library(dplyr)
library(xts)
library(ggplot2)
```

- The [geckor package](https://github.com/next-game-solutions/geckor) is
  a wrapper for the [CoinGecko API](https://www.coingecko.com/en/api)
  and allows for easy import of bitcoin price data.
- The [dplyr package](https://cran.r-project.org/package=dplyr) package
  enables additional functionality for transforming data frames.
- The [xts package](https://cran.r-project.org/package=xts) is short for
  ‘eXtensible Time Series’, which contains tools for working with time
  series data.
- The [ggplot2 package](https://cran.r-project.org/package=ggplot2) for
  graphics and visuals.

Since the rmarkdown functionality is built into RStudio, this one is
automatically loaded when we open the RStudio. So no need to use the
`library()` function for this one. Another observation to make about the
code chunk above is that it is labeled as ‘setup’, which is a special
name, which the R Notebook will recognize and automatically run prior to
running any other code chunk. This is useful for loading in packages and
setting up other global options that will be used throughout the
notebook.

Then if you wish to try and update the versions of the various R
packages in the lock file, you can use the `renv::update()` function to
update the packages in the project library. However, it is possible that
these updates could break the code in this notebook. If so, you may need
to adapt the code to work with the updated packages.

My recommendation is to first run through the code using the versions of
the packages in the lock file. Then if you want to try and update the
packages, you can do so and then run through the code again to see if it
still works. If not, you can always revert back to the lock file
versions using the `renv::restore()` function.

If you update the packages and get everything working successfully, then
you can update the lock file using the `renv::snapshot()` function. This
will update the lock file with the versions of the packages that are
currently installed in the project library. Then you can commit the
updated lock file to the repository so that others can use the updated
versions of the packages.

### Alternative Package Installation Code

If you run into any trouble using renv in the code chunk above, then you
can use the code chunk below to install the required packages for this
analysis. This method will first check if you have already installed the
packages. If any are missing, it will then install them. Then it will
load the packages into the R session. A potential flaw in this approach
compared to using renv is that it will simply install the latest
versions of the packages, which could potentially break some of the code
in this notebook if any of the updates aren’t backwards compatible.

As long as you have downloaded the entire project repository, the renv
chunk above will likely be managing the packages. Thus, the `eval=FALSE`
option is used to prevent this chunk from running unless manually
executed. So if you only downloaded this one Rmd file, this code chunk
should take care of installing the packages for you.

``` r
# Create list of packages needed for this exercise, omit geckor since its not on CRAN
list.of.packages = c("devtools","dplyr","xts","ggplot2","rmarkdown")
# Check if any have not yet been installed
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# If any need to be installed, install them
if(length(new.packages)) install.packages(new.packages)
# Since geckor is no longer published to CRAN, install via GitHub
library(devtools)
devtools::install_github("next-game-solutions/geckor")
# Load in the remaining packages
library(geckor)
library(dplyr)
library(xts)
library(ggplot2)
```

## Price Data Import

Before we use the geckor package to import price data for the various
crypto tokens, we need to create a vector of character strings that
correspond to the CoinGecko ids for the coins/tokens that we’d like to
analyze. These can be found by manually going to an asset’s page on
[CoinGecko.com](https://www.coingecko.com/) and copying the “API id”
from the Info section. These ids also appear in the page url as well.

Once we have this list of coins, we can use the `coin_history_range()`
function to download the daily price data in USD. The API requires that
the start and end date parameters for the request are provided in a
[POSIXct date format](https://search.brave.com/search?q=POSIXct). This
is done with the `as.POSIXct()` function. For a starting point, we’ll
use the first date that CoinGecko has an observed market capitalization
for the original SingularityNET token (AGIX), which is February 1, 2018.
The `Sys.Date()` function returns the current date, which is used as the
end date for the request so that we pull as much data as possible.

Currently, the free CoinGecko API has restrictions on both the number of
assets per request and the overall usage (call credits/min). The limit
on the number of coins per request is 5. So for the 8 assets of this
analysis, we must split this into two requests to get all the price
data. The two responses are then combined into a single data frame using
`rbind()`.

``` r
coinids = c("ethereum","singularitynet","singularitydao","nunet","rejuve-ai","hypercycle","cogito-protocol","sophiaverse")
prices1 = coin_history_range(coin_id=coinids[1:4],
                            vs_currency = "usd",
                            from = as.POSIXct("2018-02-01"),
                            to = as.POSIXct(Sys.Date()))
# Pause for 120 seconds to avoid API rate limit
Sys.sleep(120)
prices2 = coin_history_range(coin_id=coinids[5:8],
                            vs_currency = "usd",
                            from = as.POSIXct("2023-03-16"), # RJV beginning
                            to = as.POSIXct(Sys.Date()))
prices = rbind(prices1,prices2)
```

## Data Cleaning

Reformat each of the variables to an appropriate data type. The
`timestamp` variable is converted to a date format, since the extra time
elements of the POSIXct data type are not needed. The `coin_id` variable
is reformatted to the factor data type since there are just a small
number of categories. Then the `vs_currency` variable is deleted since
all the prices are in USD units. Lastly, the `price`, `total_volume`,
and `market_cap` variables are already in numeric formats, so nothing
needs to be done for those.

``` r
prices_clean1 = prices
prices_clean1$timestamp = as.Date(prices_clean1$timestamp)
prices_clean1$coin_id = as.factor(prices_clean1$coin_id)
prices_clean1$vs_currency = NULL
```

The next data cleaning step is to resolve a missing observation in the
price series for the AGIX token on June 6, 2021. The next chunk will
impute this missing observation by taking the average of the previous
and next day’s market cap, volume, and price.

``` r
imputedobs = data.frame(timestamp=as.Date("2021-06-06"),
                        coin_id="singularitynet",
                        # Use the average of the previous and next day's price
                        price=mean(prices_clean1$price[prices$coin_id=="singularitynet" & prices$timestamp %in% as.Date(c("2021-06-05","2021-06-07"))]),
                        # Use the average of the previous and next day's trading volume
                        total_volume=mean(prices_clean1$total_volume[prices$coin_id=="singularitynet" & prices$timestamp %in% as.Date(c("2021-06-05","2021-06-07"))]),
                        # Use the average of the previous and next day's market cap
                        market_cap=mean(prices_clean1$market_cap[prices$coin_id=="singularitynet" & prices$timestamp %in% as.Date(c("2021-06-05","2021-06-07"))]))
# Insert imputed observation into data frame in appropriate position
prevobs = max(which(prices_clean1$coin_id=="singularitynet" & prices_clean1$timestamp==as.Date("2021-06-05")))
prices_clean2 = rbind(prices_clean1[1:prevobs,],
                      imputedobs,
                      prices_clean1[(prevobs+1):nrow(prices_clean1),])
```

Then before we get into the rest of the data cleaning process, let’s
create a new asset that represents the aggregate market capitalization
of all the SingularityNET ecosystem tokens. This will be useful for
comparing the growth of the ecosystem to the growth of the individual
projects. We can do this by summing the market caps of each of the
tokens.

``` r
snet = prices_clean2 |>
  filter(coin_id %in% c("singularitynet","singularitydao","nunet","rejuve-ai","hypercycle","cogito-protocol","sophiaverse")) |>
  group_by(timestamp) |>
  summarise(market_cap=sum(market_cap),
            total_volume=sum(total_volume))
snet$coin_id = "snet-ecosystem"
snet$price = snet$market_cap
prices_clean3 = rbind(prices_clean2,snet)
coinids_clean = c(coinids,"snet-ecosystem")
```

The following code chunk is a large one. This is because we loop the
cleaning for each coin/token. So be sure to read the comments within the
code chunk for more explanation of the steps in the process. After
isolating the observations for a coin, it creates a separate xts object
for three different frequencies of analysis: daily, weekly, and monthly.
The less frequent series have a cost in that there are fewer
observations to study. However, as time moves on, we will have more data
to analyze. Then for each of those frequencies, we calculate the
annualized returns, annualized growth rates in market cap, and daily
growth rates in trading volume.

``` r
dfs_daily = list()
dfs_weeks = list()
dfs_month = list()

#coin = coinids[1]
for (coin in coinids_clean) {
  # Start timer
  t = proc.time()
  # Extract price data for each coin
  subdf = filter(prices_clean3, coin_id==coin)
  # Create xts object for each frequency
  dailyxts = xts(subdf[,3:5], order.by=as.Date(subdf$timestamp))
  weeksxts = to.weekly(dailyxts)
  monthxts = to.monthly(dailyxts)
  # Calculate annualized return of the coin's price movements
  dailyxts$annret = (log(as.numeric(dailyxts$price)) - log(as.numeric(lag(dailyxts$price))))*365*100
  weeksxts$annret = (log(as.numeric(weeksxts$dailyxts.Close)) - log(as.numeric(lag(weeksxts$dailyxts.Close))))*(365/7)*100
  monthxts$annret = (log(as.numeric(monthxts$dailyxts.Close)) - log(as.numeric(lag(monthxts$dailyxts.Close))))*12*100
  # Calculate alternative annualized return measurement from the market cap of the coin
  dailyxts$annret2 = (log(as.numeric(dailyxts$market_cap)) - log(as.numeric(lag(dailyxts$market_cap))))*365*100
    # Since to.weekly and to.monthly only convert prices by default, we must run again for market caps.
  weeksxts2 = to.weekly(dailyxts$market_cap)
  weeksxts$annret2 = (log(as.numeric(weeksxts2$`dailyxts$market_cap.Close`)) - log(as.numeric(lag(weeksxts2$`dailyxts$market_cap.Close`))))*(365/7)*100
  monthxts2 = to.monthly(dailyxts$market_cap)
  monthxts$annret2 = (log(as.numeric(monthxts2$`dailyxts$market_cap.Close`)) - log(as.numeric(lag(monthxts2$`dailyxts$market_cap.Close`))))*12*100
  # Also calculate volume growth rates and reduce to daily values
  dailyxts$volgrowth = (log(as.numeric(dailyxts$total_volume)) - log(as.numeric(lag(dailyxts$total_volume))))*100
  weeksxts$volgrowth = (log(as.numeric(weeksxts$dailyxts.Volume)) - log(as.numeric(lag(weeksxts$dailyxts.Volume))))*100/7
  monthxts$volgrowth = (log(as.numeric(monthxts$dailyxts.Volume)) - log(as.numeric(lag(monthxts$dailyxts.Volume))))*100/(365/12)
  # Save data frames to output object
  dfs_daily[[coin]] = dailyxts
  dfs_weeks[[coin]] = weeksxts
  dfs_month[[coin]] = monthxts
  # Stop timer and output result
  T = proc.time() - t
  print(paste("Cleaning of", coin, "completed in", as.character(round(T[3],2)), "seconds."))
}
```

    ## [1] "Cleaning of ethereum completed in 0.01 seconds."
    ## [1] "Cleaning of singularitynet completed in 0.02 seconds."
    ## [1] "Cleaning of singularitydao completed in 0.01 seconds."
    ## [1] "Cleaning of nunet completed in 0.02 seconds."
    ## [1] "Cleaning of rejuve-ai completed in 0.01 seconds."
    ## [1] "Cleaning of hypercycle completed in 0.02 seconds."
    ## [1] "Cleaning of cogito-protocol completed in 0 seconds."
    ## [1] "Cleaning of sophiaverse completed in 0.02 seconds."
    ## [1] "Cleaning of snet-ecosystem completed in 0.01 seconds."

## Multivariate Return Distributions

Compute the average annual return and volatility of each of the
coins/tokens, as well as the correlation matrix, for the daily return
series.

``` r
# Compile data frame of annualized (price) returns
daily_annrets = merge(dfs_daily$ethereum$annret,
                      dfs_daily$singularitynet$annret,
                      dfs_daily$singularitydao$annret,
                      dfs_daily$nunet$annret,
                      dfs_daily$`rejuve-ai`$annret,
                      dfs_daily$hypercycle$annret,
                      dfs_daily$`cogito-protocol`$annret,
                      dfs_daily$sophiaverse$annret,
                      dfs_daily$`snet-ecosystem`$annret)
# Correct variable names
colnames(daily_annrets) = c("ETH", "AGIX", "SDAO", "NTX", "RJV", "HYPC", "CGV", "SOPH", "SNET")
# Compute average annual returns, volatility, and correlations
Er_daily = colMeans(daily_annrets,na.rm=TRUE)
Er_daily |> round(2)
```

    ##     ETH    AGIX    SDAO     NTX     RJV    HYPC     CGV    SOPH    SNET 
    ##   18.37    4.78   -5.08  -36.36  -61.77   88.36  -56.88 -124.55   23.94

``` r
sd_daily = apply(daily_annrets,2,sd,na.rm=TRUE)
sd_daily |> round(2)
```

    ##     ETH    AGIX    SDAO     NTX     RJV    HYPC     CGV    SOPH    SNET 
    ## 1705.00 3607.64 2738.30 2715.27 2321.02 1843.36 1568.28 2460.99 3386.29

``` r
Sharpe_daily = Er_daily/sd_daily
Sharpe_daily |> round(2)
```

    ##   ETH  AGIX  SDAO   NTX   RJV  HYPC   CGV  SOPH  SNET 
    ##  0.01  0.00  0.00 -0.01 -0.03  0.05 -0.04 -0.05  0.01

``` r
cor(daily_annrets, use="pairwise.complete.obs") |> round(2)
```

    ##       ETH AGIX SDAO  NTX  RJV HYPC  CGV SOPH SNET
    ## ETH  1.00 0.46 0.52 0.44 0.28 0.22 0.27 0.14 0.46
    ## AGIX 0.46 1.00 0.38 0.49 0.34 0.20 0.24 0.13 0.95
    ## SDAO 0.52 0.38 1.00 0.51 0.29 0.09 0.16 0.07 0.44
    ## NTX  0.44 0.49 0.51 1.00 0.31 0.15 0.21 0.08 0.54
    ## RJV  0.28 0.34 0.29 0.31 1.00 0.16 0.26 0.09 0.38
    ## HYPC 0.22 0.20 0.09 0.15 0.16 1.00 0.25 0.08 0.20
    ## CGV  0.27 0.24 0.16 0.21 0.26 0.25 1.00 0.06 0.25
    ## SOPH 0.14 0.13 0.07 0.08 0.09 0.08 0.06 1.00 0.15
    ## SNET 0.46 0.95 0.44 0.54 0.38 0.20 0.25 0.15 1.00

Compute the average annual return and volatility of each of the
coins/tokens, as well as the correlation matrix, for the weekly return
series.

``` r
# Compile data frame of annualized (price) returns
weeks_annrets = merge(dfs_weeks$ethereum$annret,
                      dfs_weeks$singularitynet$annret,
                      dfs_weeks$singularitydao$annret,
                      dfs_weeks$nunet$annret,
                      dfs_weeks$`rejuve-ai`$annret,
                      dfs_weeks$hypercycle$annret,
                      dfs_weeks$`cogito-protocol`$annret,
                      dfs_weeks$sophiaverse$annret,
                      dfs_weeks$`snet-ecosystem`$annret)
# Correct variable names
colnames(weeks_annrets) = c("ETH", "AGIX", "SDAO", "NTX", "RJV", "HYPC", "CGV", "SOPH", "SNET")
# Compute average annual returns, volatility, and correlations
Er_weeks = colMeans(weeks_annrets,na.rm=TRUE)
Er_weeks |> round(2)
```

    ##     ETH    AGIX    SDAO     NTX     RJV    HYPC     CGV    SOPH    SNET 
    ##   19.67    5.26  -11.80  -36.10  -65.84  125.01  -50.75 -210.83   23.97

``` r
sd_weeks = apply(weeks_annrets,2,sd,na.rm=TRUE)
sd_weeks |> round(2)
```

    ##     ETH    AGIX    SDAO     NTX     RJV    HYPC     CGV    SOPH    SNET 
    ##  666.70 1251.02 1051.12  982.57  924.23  715.17  577.85  626.94 1273.76

``` r
Sharpe_weeks = Er_weeks/sd_weeks
Sharpe_weeks |> round(2)
```

    ##   ETH  AGIX  SDAO   NTX   RJV  HYPC   CGV  SOPH  SNET 
    ##  0.03  0.00 -0.01 -0.04 -0.07  0.17 -0.09 -0.34  0.02

``` r
cor(weeks_annrets, use="pairwise.complete.obs") |> round(2)
```

    ##       ETH AGIX SDAO  NTX  RJV HYPC  CGV SOPH SNET
    ## ETH  1.00 0.50 0.48 0.51 0.50 0.05 0.24 0.24 0.46
    ## AGIX 0.50 1.00 0.50 0.67 0.69 0.11 0.22 0.38 0.97
    ## SDAO 0.48 0.50 1.00 0.71 0.50 0.02 0.32 0.14 0.42
    ## NTX  0.51 0.67 0.71 1.00 0.54 0.07 0.43 0.23 0.72
    ## RJV  0.50 0.69 0.50 0.54 1.00 0.06 0.40 0.05 0.70
    ## HYPC 0.05 0.11 0.02 0.07 0.06 1.00 0.39 0.08 0.10
    ## CGV  0.24 0.22 0.32 0.43 0.40 0.39 1.00 0.14 0.26
    ## SOPH 0.24 0.38 0.14 0.23 0.05 0.08 0.14 1.00 0.38
    ## SNET 0.46 0.97 0.42 0.72 0.70 0.10 0.26 0.38 1.00

Compute the average annual return and volatility of each of the
coins/tokens, as well as the correlation matrix, for the monthly return
series.

``` r
# Compile data frame of annualized (price) returns
month_annrets = merge(dfs_month$ethereum$annret,
                      dfs_month$singularitynet$annret,
                      dfs_month$singularitydao$annret,
                      dfs_month$nunet$annret,
                      dfs_month$`rejuve-ai`$annret,
                      dfs_month$hypercycle$annret,
                      dfs_month$`cogito-protocol`$annret,
                      dfs_month$sophiaverse$annret,
                      dfs_month$`snet-ecosystem`$annret)
# Correct variable names
colnames(month_annrets) = c("ETH", "AGIX", "SDAO", "NTX", "RJV", "HYPC", "CGV", "SOPH", "SNET")
# Compute average annual returns, volatility, and correlations
Er_month = colMeans(month_annrets,na.rm=TRUE)
Er_month |> round(2)
```

    ##     ETH    AGIX    SDAO     NTX     RJV    HYPC     CGV    SOPH    SNET 
    ##   21.26   12.22   -5.06  -45.32  -36.90  139.77  -37.69 -193.89   30.96

``` r
sd_month = apply(month_annrets,2,sd,na.rm=TRUE)
sd_month |> round(2)
```

    ##    ETH   AGIX   SDAO    NTX    RJV   HYPC    CGV   SOPH   SNET 
    ## 338.04 506.79 452.84 520.20 479.09 412.43 243.27 233.69 481.74

``` r
Sharpe_month = Er_month/sd_month
Sharpe_month |> round(2)
```

    ##   ETH  AGIX  SDAO   NTX   RJV  HYPC   CGV  SOPH  SNET 
    ##  0.06  0.02 -0.01 -0.09 -0.08  0.34 -0.15 -0.83  0.06

``` r
cor(month_annrets, use="pairwise.complete.obs") |> round(2)
```

    ##        ETH  AGIX  SDAO   NTX   RJV  HYPC  CGV  SOPH  SNET
    ## ETH   1.00  0.64  0.61  0.64  0.71 -0.23 0.70  0.91  0.60
    ## AGIX  0.64  1.00  0.78  0.88  0.85 -0.07 0.48  0.93  0.96
    ## SDAO  0.61  0.78  1.00  0.80  0.72 -0.23 0.26  0.72  0.69
    ## NTX   0.64  0.88  0.80  1.00  0.75 -0.13 0.50  0.62  0.90
    ## RJV   0.71  0.85  0.72  0.75  1.00 -0.20 0.48  0.75  0.87
    ## HYPC -0.23 -0.07 -0.23 -0.13 -0.20  1.00 0.03 -0.02 -0.08
    ## CGV   0.70  0.48  0.26  0.50  0.48  0.03 1.00  0.65  0.48
    ## SOPH  0.91  0.93  0.72  0.62  0.75 -0.02 0.65  1.00  0.91
    ## SNET  0.60  0.96  0.69  0.90  0.87 -0.08 0.48  0.91  1.00

## ETH-adjusted Returns

Now let’s examine the relationship of the returns on the SingularityNET
tokens to that of Ethereum, which is the blockchain that the tokens
primarily transact on.

First up is the SingularityNET token (AGIX):

``` r
AGIXfit_daily = lm(AGIX~ETH, data=daily_annrets)
summary(AGIXfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = AGIX ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -67470  -1177   -132    966  61172 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -13.08138   68.10377  -0.192    0.848    
    ## ETH           0.97216    0.03995  24.334   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3205 on 2213 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.2111, Adjusted R-squared:  0.2107 
    ## F-statistic: 592.2 on 1 and 2213 DF,  p-value: < 2.2e-16

``` r
ggplot(daily_annrets,aes(y=AGIX, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/agixregs-1.png)<!-- -->

``` r
daily_annrets$AGIXresid = xts(resid(AGIXfit_daily), order.by=as.Date(names(resid(AGIXfit_daily))))

AGIXfit_weeks = lm(AGIX~ETH, data=weeks_annrets)
summary(AGIXfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = AGIX ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8093.4  -467.3   -50.8   355.8  8820.2 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -13.22600   60.93912  -0.217    0.828    
    ## ETH           0.93982    0.09151  10.270   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1085 on 315 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.2509, Adjusted R-squared:  0.2485 
    ## F-statistic: 105.5 on 1 and 315 DF,  p-value: < 2.2e-16

``` r
ggplot(weeks_annrets,aes(y=AGIX, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/agixregs-2.png)<!-- -->

``` r
weeks_annrets$AGIXresids = xts(resid(AGIXfit_weeks), order.by=as.Date(names(resid(AGIXfit_weeks))))

AGIXfit_month = lm(AGIX~ETH, data=month_annrets)
summary(AGIXfit_month)
```

    ## 
    ## Call:
    ## lm(formula = AGIX ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -642.26 -265.27  -45.98  147.26 1257.36 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -8.0556    46.5048  -0.173    0.863    
    ## ETH           0.9537     0.1383   6.898 1.92e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 393.8 on 70 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.4046, Adjusted R-squared:  0.3961 
    ## F-statistic: 47.58 on 1 and 70 DF,  p-value: 1.919e-09

``` r
ggplot(month_annrets,aes(y=AGIX, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/agixregs-3.png)<!-- -->

``` r
month_annrets2 = data.frame(date=names(resid(AGIXfit_month)))
month_annrets2$AGIXresids = resid(AGIXfit_month)
```

Next is the SingularityDAO token (SDAO):

``` r
SDAOfit_daily = lm(SDAO~ETH, data=daily_annrets)
summary(SDAOfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = SDAO ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9910.0 -1211.5  -113.6   993.3 16280.8 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.69667   73.30625   0.037    0.971    
    ## ETH          0.95712    0.04929  19.417   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2340 on 1017 degrees of freedom
    ##   (1197 observations deleted due to missingness)
    ## Multiple R-squared:  0.2705, Adjusted R-squared:  0.2697 
    ## F-statistic:   377 on 1 and 1017 DF,  p-value: < 2.2e-16

``` r
ggplot(daily_annrets,aes(y=SDAO, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1197 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1197 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/sdaoregs-1.png)<!-- -->

``` r
daily_annrets$SDAOresid = xts(resid(SDAOfit_daily), order.by=as.Date(names(resid(SDAOfit_daily))))

SDAOfit_weeks = lm(SDAO~ETH, data=weeks_annrets)
summary(SDAOfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = SDAO ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2184.1  -481.2  -104.0   388.4  4426.8 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -6.6742    76.4137  -0.087    0.931    
    ## ETH           0.8870     0.1338   6.629 6.32e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 923.3 on 144 degrees of freedom
    ##   (172 observations deleted due to missingness)
    ## Multiple R-squared:  0.2338, Adjusted R-squared:  0.2285 
    ## F-statistic: 43.94 on 1 and 144 DF,  p-value: 6.318e-10

``` r
ggplot(weeks_annrets,aes(y=SDAO, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 172 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 172 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/sdaoregs-2.png)<!-- -->

``` r
weeks_annrets$SDAOresids = xts(resid(SDAOfit_weeks), order.by=as.Date(names(resid(SDAOfit_weeks))))

SDAOfit_month = lm(SDAO~ETH, data=month_annrets)
summary(SDAOfit_month)
```

    ## 
    ## Call:
    ## lm(formula = SDAO ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -659.33 -211.60  -17.84  151.72  995.56 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -15.0466    63.4661  -0.237 0.814154    
    ## ETH           1.0482     0.2441   4.293 0.000161 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 364.3 on 31 degrees of freedom
    ##   (40 observations deleted due to missingness)
    ## Multiple R-squared:  0.3729, Adjusted R-squared:  0.3527 
    ## F-statistic: 18.43 on 1 and 31 DF,  p-value: 0.0001606

``` r
ggplot(month_annrets,aes(y=SDAO, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 40 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 40 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/sdaoregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(SDAOfit_month)))
month_annrets3$SDAOresids = resid(SDAOfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

Then the NuNet token (NTX):

``` r
NTXfit_daily = lm(NTX~ETH, data=daily_annrets)
summary(NTXfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = NTX ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11087.8  -1177.1    -49.3    918.5  16764.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -25.4099    85.2090  -0.298    0.766    
    ## ETH           0.9065     0.0647  14.010   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2440 on 818 degrees of freedom
    ##   (1396 observations deleted due to missingness)
    ## Multiple R-squared:  0.1935, Adjusted R-squared:  0.1925 
    ## F-statistic: 196.3 on 1 and 818 DF,  p-value: < 2.2e-16

``` r
ggplot(daily_annrets,aes(y=NTX, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1396 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1396 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/ntxregs-1.png)<!-- -->

``` r
daily_annrets$NTXresid = xts(resid(NTXfit_daily), order.by=as.Date(names(resid(NTXfit_daily))))

NTXfit_weeks = lm(NTX~ETH, data=weeks_annrets)
summary(NTXfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = NTX ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2290.6  -471.3   -94.3   327.5  4159.5 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -24.7379    78.3801  -0.316    0.753    
    ## ETH           0.9471     0.1499   6.317 5.11e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 851.2 on 116 degrees of freedom
    ##   (200 observations deleted due to missingness)
    ## Multiple R-squared:  0.2559, Adjusted R-squared:  0.2495 
    ## F-statistic:  39.9 on 1 and 116 DF,  p-value: 5.106e-09

``` r
ggplot(weeks_annrets,aes(y=NTX, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 200 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/ntxregs-2.png)<!-- -->

``` r
weeks_annrets$NTXresids = xts(resid(NTXfit_weeks), order.by=as.Date(names(resid(NTXfit_weeks))))

NTXfit_month = lm(NTX~ETH, data=month_annrets)
summary(NTXfit_month)
```

    ## 
    ## Call:
    ## lm(formula = NTX ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -798.72 -223.87  -32.01  179.14 1099.76 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -25.4160    78.7912  -0.323 0.749701    
    ## ETH           1.2580     0.3039   4.139 0.000346 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 408.6 on 25 degrees of freedom
    ##   (46 observations deleted due to missingness)
    ## Multiple R-squared:  0.4066, Adjusted R-squared:  0.3829 
    ## F-statistic: 17.13 on 1 and 25 DF,  p-value: 0.0003463

``` r
ggplot(month_annrets,aes(y=NTX, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 46 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 46 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/ntxregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(NTXfit_month)))
month_annrets3$NTXresids = resid(NTXfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

Next is the Rejuve token (RJV):

``` r
RJVfit_daily = lm(RJV~ETH, data=daily_annrets)
summary(RJVfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = RJV ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8186.8 -1184.1  -116.3   888.4 11578.4 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -109.7857   120.3794  -0.912    0.362    
    ## ETH            0.7377     0.1376   5.361 1.52e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2233 on 344 degrees of freedom
    ##   (1870 observations deleted due to missingness)
    ## Multiple R-squared:  0.0771, Adjusted R-squared:  0.07442 
    ## F-statistic: 28.74 on 1 and 344 DF,  p-value: 1.521e-07

``` r
ggplot(daily_annrets,aes(y=RJV, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1870 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1870 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/rjvregs-1.png)<!-- -->

``` r
daily_annrets$RJVresid = xts(resid(RJVfit_daily), order.by=as.Date(names(resid(RJVfit_daily))))

RJVfit_weeks = lm(RJV~ETH, data=weeks_annrets)
summary(RJVfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = RJV ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2366.13  -442.15     7.69   434.13  1764.92 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -158.6927   116.6849  -1.360 0.180180    
    ## ETH            1.5758     0.3938   4.002 0.000217 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 808.6 on 48 degrees of freedom
    ##   (268 observations deleted due to missingness)
    ## Multiple R-squared:  0.2502, Adjusted R-squared:  0.2346 
    ## F-statistic: 16.02 on 1 and 48 DF,  p-value: 0.0002167

``` r
ggplot(weeks_annrets,aes(y=RJV, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 268 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 268 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/rjvregs-2.png)<!-- -->

``` r
weeks_annrets$RJVresids = xts(resid(RJVfit_weeks), order.by=as.Date(names(resid(RJVfit_weeks))))

RJVfit_month = lm(RJV~ETH, data=month_annrets)
summary(RJVfit_month)
```

    ## 
    ## Call:
    ## lm(formula = RJV ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -510.8 -208.4    7.1  233.9  586.5 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -206.7017   121.1862  -1.706   0.1223  
    ## ETH            2.8204     0.9344   3.018   0.0145 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 356 on 9 degrees of freedom
    ##   (62 observations deleted due to missingness)
    ## Multiple R-squared:  0.5031, Adjusted R-squared:  0.4478 
    ## F-statistic: 9.111 on 1 and 9 DF,  p-value: 0.01452

``` r
ggplot(month_annrets,aes(y=RJV, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 62 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 62 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/rjvregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(RJVfit_month)))
month_annrets3$RJVresids = resid(RJVfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

Then the HyperCycle token (HYPC):

``` r
HYPCfit_daily = lm(HYPC~ETH, data=daily_annrets)
summary(HYPCfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = HYPC ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6157.7  -917.6  -273.6   936.4 10366.5 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  57.1923   105.5480   0.542 0.588329    
    ## ETH           0.4803     0.1249   3.844 0.000149 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1801 on 291 degrees of freedom
    ##   (1923 observations deleted due to missingness)
    ## Multiple R-squared:  0.04833,    Adjusted R-squared:  0.04506 
    ## F-statistic: 14.78 on 1 and 291 DF,  p-value: 0.0001486

``` r
ggplot(daily_annrets,aes(y=HYPC, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1923 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1923 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/hypcregs-1.png)<!-- -->

``` r
daily_annrets$HYPCresid = xts(resid(HYPCfit_daily), order.by=as.Date(names(resid(HYPCfit_daily))))

HYPCfit_weeks = lm(HYPC~ETH, data=weeks_annrets)
summary(HYPCfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = HYPC ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1087.94  -395.13   -91.02   206.97  3120.32 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 115.9669   114.8025   1.010    0.318
    ## ETH           0.1325     0.3966   0.334    0.740
    ## 
    ## Residual standard error: 723 on 40 degrees of freedom
    ##   (276 observations deleted due to missingness)
    ## Multiple R-squared:  0.002782,   Adjusted R-squared:  -0.02215 
    ## F-statistic: 0.1116 on 1 and 40 DF,  p-value: 0.7401

``` r
ggplot(weeks_annrets,aes(y=HYPC, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 276 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 276 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/hypcregs-2.png)<!-- -->

``` r
weeks_annrets$HYPCresids = xts(resid(HYPCfit_weeks), order.by=as.Date(names(resid(HYPCfit_weeks))))

HYPCfit_month = lm(HYPC~ETH, data=month_annrets)
summary(HYPCfit_month)
```

    ## 
    ## Call:
    ## lm(formula = HYPC ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -456.5 -240.3 -174.9  289.1  706.6 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 187.6294   161.4434   1.162    0.283
    ## ETH          -0.7277     1.1425  -0.637    0.544
    ## 
    ## Residual standard error: 428.7 on 7 degrees of freedom
    ##   (64 observations deleted due to missingness)
    ## Multiple R-squared:  0.05478,    Adjusted R-squared:  -0.08025 
    ## F-statistic: 0.4057 on 1 and 7 DF,  p-value: 0.5444

``` r
ggplot(month_annrets,aes(y=HYPC, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 64 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 64 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/hypcregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(HYPCfit_month)))
month_annrets3$HYPCresids = resid(HYPCfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

Next is the Cogito token (CGV):

``` r
CGVfit_daily = lm(CGV~ETH, data=daily_annrets)
summary(CGVfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = CGV ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5938.6  -552.6   -97.7   415.4 10180.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -96.6816    94.7055  -1.021    0.308    
    ## ETH           0.4848     0.1094   4.431 1.39e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1514 on 256 degrees of freedom
    ##   (1958 observations deleted due to missingness)
    ## Multiple R-squared:  0.07123,    Adjusted R-squared:  0.06761 
    ## F-statistic: 19.63 on 1 and 256 DF,  p-value: 1.391e-05

``` r
ggplot(daily_annrets,aes(y=CGV, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1958 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1958 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/cgvregs-1.png)<!-- -->

``` r
daily_annrets$CGVresid = xts(resid(CGVfit_daily), order.by=as.Date(names(resid(CGVfit_daily))))

CGVfit_weeks = lm(CGV~ETH, data=weeks_annrets)
summary(CGVfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = CGV ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1047.69  -286.40   -11.43   206.21  2374.12 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -90.8446    97.2554  -0.934    0.357
    ## ETH           0.4825     0.3246   1.487    0.146
    ## 
    ## Residual standard error: 568.4 on 35 degrees of freedom
    ##   (281 observations deleted due to missingness)
    ## Multiple R-squared:  0.05939,    Adjusted R-squared:  0.03252 
    ## F-statistic:  2.21 on 1 and 35 DF,  p-value: 0.1461

``` r
ggplot(weeks_annrets,aes(y=CGV, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 281 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 281 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/cgvregs-2.png)<!-- -->

``` r
weeks_annrets$CGVresids = xts(resid(CGVfit_weeks), order.by=as.Date(names(resid(CGVfit_weeks))))

CGVfit_month = lm(CGV~ETH, data=month_annrets)
summary(CGVfit_month)
```

    ## 
    ## Call:
    ## lm(formula = CGV ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -294.46 -100.38   40.72  146.93  176.81 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -134.0870    78.2256  -1.714   0.1373  
    ## ETH            1.2390     0.5233   2.368   0.0557 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 188.9 on 6 degrees of freedom
    ##   (65 observations deleted due to missingness)
    ## Multiple R-squared:  0.483,  Adjusted R-squared:  0.3969 
    ## F-statistic: 5.606 on 1 and 6 DF,  p-value: 0.05569

``` r
ggplot(month_annrets,aes(y=CGV, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 65 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 65 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/cgvregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(CGVfit_month)))
month_annrets3$CGVresids = resid(CGVfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

Lastly is the Sophiaverse token (SOPH):

``` r
SOPHfit_daily = lm(SOPH~ETH, data=daily_annrets)
summary(SOPHfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = SOPH ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6270.0 -1010.1  -160.8   739.7 19717.2 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -159.4475   167.3609  -0.953   0.3418  
    ## ETH            0.3980     0.1902   2.093   0.0376 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2442 on 213 degrees of freedom
    ##   (2001 observations deleted due to missingness)
    ## Multiple R-squared:  0.02015,    Adjusted R-squared:  0.01554 
    ## F-statistic: 4.379 on 1 and 213 DF,  p-value: 0.03757

``` r
ggplot(daily_annrets,aes(y=SOPH, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 2001 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 2001 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/sophregs-1.png)<!-- -->

``` r
daily_annrets$SOPHresid = xts(resid(SOPHfit_daily), order.by=as.Date(names(resid(SOPHfit_daily))))

SOPHfit_weeks = lm(SOPH~ETH, data=weeks_annrets)
summary(SOPHfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = SOPH ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1858.70  -293.25   -94.01   183.84  1634.15 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -253.0526   115.4630  -2.192   0.0366 *
    ## ETH            0.4982     0.3703   1.345   0.1890  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 618.7 on 29 degrees of freedom
    ##   (287 observations deleted due to missingness)
    ## Multiple R-squared:  0.05873,    Adjusted R-squared:  0.02627 
    ## F-statistic: 1.809 on 1 and 29 DF,  p-value: 0.189

``` r
ggplot(weeks_annrets,aes(y=SOPH, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 287 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 287 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/sophregs-2.png)<!-- -->

``` r
weeks_annrets$SOPHresids = xts(resid(SOPHfit_weeks), order.by=as.Date(names(resid(SOPHfit_weeks))))

SOPHfit_month = lm(SOPH~ETH, data=month_annrets)
summary(SOPHfit_month)
```

    ## 
    ## Call:
    ## lm(formula = SOPH ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ## Aug 2023 Sep 2023 Oct 2023 Nov 2023 Dec 2023 Jan 2024 Feb 2024 
    ##    95.21    66.73   -77.04    14.75    22.61  -176.59    54.34 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -324.5629    47.2288  -6.872 0.000998 ***
    ## ETH            1.4826     0.2956   5.016 0.004048 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 104.2 on 5 degrees of freedom
    ##   (66 observations deleted due to missingness)
    ## Multiple R-squared:  0.8342, Adjusted R-squared:  0.8011 
    ## F-statistic: 25.16 on 1 and 5 DF,  p-value: 0.004048

``` r
ggplot(month_annrets,aes(y=SOPH, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 66 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 66 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/sophregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(SOPHfit_month)))
month_annrets3$SOPHresids = resid(SOPHfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

Lastly, let’s look at the ecosystem as a whole:

``` r
SNETfit_daily = lm(SNET~ETH, data=daily_annrets)
summary(SNETfit_daily)
```

    ## 
    ## Call:
    ## lm(formula = SNET ~ ETH, data = daily_annrets)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -67746  -1104   -115    925  64718 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.19786   63.94019   0.113     0.91    
    ## ETH          0.91172    0.03751  24.308   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3009 on 2213 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.2107, Adjusted R-squared:  0.2104 
    ## F-statistic: 590.9 on 1 and 2213 DF,  p-value: < 2.2e-16

``` r
ggplot(daily_annrets,aes(y=SNET, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/snetregs-1.png)<!-- -->

``` r
daily_annrets$SNETresid = xts(resid(SNETfit_daily), order.by=as.Date(names(resid(SNETfit_daily))))

SNETfit_weeks = lm(SNET~ETH, data=weeks_annrets)
summary(SNETfit_weeks)
```

    ## 
    ## Call:
    ## lm(formula = SNET ~ ETH, data = weeks_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8660.1  -457.2   -61.7   330.6 10077.6 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   6.6296    63.5997   0.104    0.917    
    ## ETH           0.8815     0.0955   9.230   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1132 on 315 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.2129, Adjusted R-squared:  0.2104 
    ## F-statistic: 85.19 on 1 and 315 DF,  p-value: < 2.2e-16

``` r
ggplot(weeks_annrets,aes(y=SNET, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/snetregs-2.png)<!-- -->

``` r
weeks_annrets$SNETresids = xts(resid(SNETfit_weeks), order.by=as.Date(names(resid(SNETfit_weeks))))

SNETfit_month = lm(SNET~ETH, data=month_annrets)
summary(SNETfit_month)
```

    ## 
    ## Call:
    ## lm(formula = SNET ~ ETH, data = month_annrets)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -621.62 -245.54  -41.85  128.22 1217.46 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.6315    45.6292   0.277    0.783    
    ## ETH           0.8618     0.1357   6.353 1.85e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 386.4 on 70 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.3657, Adjusted R-squared:  0.3566 
    ## F-statistic: 40.36 on 1 and 70 DF,  p-value: 1.847e-08

``` r
ggplot(month_annrets,aes(y=SNET, x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-gfm/snetregs-3.png)<!-- -->

``` r
month_annrets3 = data.frame(date=names(resid(SNETfit_month)))
month_annrets3$SNETresids = resid(SNETfit_month)
month_annrets2 = merge(month_annrets2, month_annrets3, all.x=TRUE)
```

### Residual Correlations

Now that we have the residual series for each of the coins/tokens, let’s
look at the correlations between them. These represent the correlations
of the asset returns after removing the effects that are explained by
their relationship with Ethereum.

``` r
cor(daily_annrets[,10:17], use="pairwise.complete.obs") |> round(2)
```

    ##           AGIXresid SDAOresid NTXresid RJVresid HYPCresid CGVresid SOPHresid
    ## AGIXresid      1.00      0.23     0.35     0.24      0.10     0.13      0.07
    ## SDAOresid      0.23      1.00     0.38     0.18     -0.03     0.03      0.01
    ## NTXresid       0.35      0.38     1.00     0.22      0.06     0.10      0.02
    ## RJVresid       0.24      0.18     0.22     1.00      0.11     0.19      0.05
    ## HYPCresid      0.10     -0.03     0.06     0.11      1.00     0.20      0.04
    ## CGVresid       0.13      0.03     0.10     0.19      0.20     1.00      0.00
    ## SOPHresid      0.07      0.01     0.02     0.05      0.04     0.00      1.00
    ## SNETresid      0.93      0.31     0.42     0.27      0.10     0.13      0.09
    ##           SNETresid
    ## AGIXresid      0.93
    ## SDAOresid      0.31
    ## NTXresid       0.42
    ## RJVresid       0.27
    ## HYPCresid      0.10
    ## CGVresid       0.13
    ## SOPHresid      0.09
    ## SNETresid      1.00

``` r
cor(weeks_annrets[,10:17], use="pairwise.complete.obs") |> round(2)
```

    ##            AGIXresids SDAOresids NTXresids RJVresids HYPCresids CGVresids
    ## AGIXresids       1.00       0.36      0.55      0.52       0.09      0.08
    ## SDAOresids       0.36       1.00      0.62      0.32      -0.01      0.22
    ## NTXresids        0.55       0.62      1.00      0.31       0.04      0.36
    ## RJVresids        0.52       0.32      0.31      1.00       0.04      0.33
    ## HYPCresids       0.09      -0.01      0.04      0.04       1.00      0.39
    ## CGVresids        0.08       0.22      0.36      0.33       0.39      1.00
    ## SOPHresids       0.29       0.01      0.11     -0.14       0.04      0.01
    ## SNETresids       0.97       0.30      0.61      0.53       0.09      0.12
    ##            SOPHresids SNETresids
    ## AGIXresids       0.29       0.97
    ## SDAOresids       0.01       0.30
    ## NTXresids        0.11       0.61
    ## RJVresids       -0.14       0.53
    ## HYPCresids       0.04       0.09
    ## CGVresids        0.01       0.12
    ## SOPHresids       1.00       0.28
    ## SNETresids       0.28       1.00

``` r
cor(month_annrets2[,-1], use="pairwise.complete.obs") |> round(2)
```

    ##            AGIXresids SDAOresids NTXresids RJVresids HYPCresids CGVresids
    ## AGIXresids       1.00       0.63      0.78      0.42       0.19     -0.28
    ## SDAOresids       0.63       1.00      0.66      0.61      -0.11     -0.31
    ## NTXresids        0.78       0.66      1.00      0.54       0.03      0.09
    ## RJVresids        0.42       0.61      0.54      1.00      -0.05     -0.05
    ## HYPCresids       0.19      -0.11      0.03     -0.05       1.00      0.27
    ## CGVresids       -0.28      -0.31      0.09     -0.05       0.27      1.00
    ## SOPHresids       0.32       0.51     -0.07      0.27       0.28     -0.08
    ## SNETresids       0.94       0.54      0.81      0.45       0.17     -0.26
    ##            SOPHresids SNETresids
    ## AGIXresids       0.32       0.94
    ## SDAOresids       0.51       0.54
    ## NTXresids       -0.07       0.81
    ## RJVresids        0.27       0.45
    ## HYPCresids       0.28       0.17
    ## CGVresids       -0.08      -0.26
    ## SOPHresids       1.00       0.32
    ## SNETresids       0.32       1.00
