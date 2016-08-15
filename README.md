

Master: [![Build Status master](https://travis-ci.org/gluc/datap.svg?branch=master)](https://travis-ci.org/gluc/datap) [![Windows Build status]( https://ci.appveyor.com/api/projects/status/github/gluc/datap?branch=master&svg=true)](https://ci.appveyor.com/project/gluc/datap) [![codecov.io](http://codecov.io/github/gluc/datap/coverage.svg?branch=master)](http://codecov.io/github/gluc/datap?branch=master)

Dev: [![Build Status dev](https://travis-ci.org/gluc/datap.svg?branch=dev)](https://travis-ci.org/gluc/datap) [![Windows Build status]( https://ci.appveyor.com/api/projects/status/github/gluc/datap?branch=dev&svg=true)](https://ci.appveyor.com/project/gluc/datap) [![codecov.io](http://codecov.io/github/gluc/datap/coverage.svg?branch=dev)](http://codecov.io/github/gluc/datap?branch=dev)

# What is datap?

datap is a lightweight DSL (Domain Specific Language) to define configurable, modular, and re-usable data processes for use in the R programming language. Use datap to acquire, pre-process, quality-assure, cache and merge data. Use the simple datap api to access all your datasets in the same way.

Your data can be anything, and it can come from any source (the internet, a third-party provider, screen-scraping, a file, from memory, by calling an R function, or data generated on the fly by your context, etc.).


# What is datap useful for?

datap is useful to many ends. For example

1. "Cool, today I start a new project. Hmm, I need data from various sources. Let me write a quick framework to unify data access." - "Such a framework already exists! It's called datap and is really easy to use!"
2. "Argh, I have so many data sets! And I keep forgetting their names!!!" - "Just use datap. You can structure your data sets and print them to the console with a single command."
3. "Where is this data coming from?" - "Hmmm, good question, let me debug that!" - "Why don't you just use datap? Your context will list the source and pre-processing explicitly, in a standard way!"
4. "So, how does your trading system know what timeseries flow into this signal?" - "That's simple. See, this function contains a hard-coded list of tickers. And if I need to add a timeseries, I just extend the list." - "Wouldn't it be nice if you could separate your code from your configuration? You could do that using datap. Then, in the attributes of each time series you can define for which signals it should be considered."
5. "We get our data from Bloomberg. Only problem is that not all our developers have access to Bloomberg. So for development, we'd like to use Yahoo finance. Just not sure how to do that." - "With datap that's straight forward. Just use two contexts: one for development, one for testing and production."

# How do I use datap?

In practice, each datap setup will consist of the following elements:

1. A datap context, i.e. a YAML file following a few specific rules
2. R functions (your own or from packages)
3. The datap interpreter, i.e. the R datap package
4. Client code, calling the datap API on the context to load data into your R session

## datap context

Each context is defined in in a yaml file and contains a series of hierarchically organised taps. Each tap represents a specific dataset, together with its source and pre-processing steps. Consider the following, very simple context that is provided with the datap package as context2.yaml:

```{YAML}
stocks:
  type: structure
  Apple:
    type: tap
    download:
      type: processor
      function: Quandl::Quandl(code = 'YAHOO/AAPL', type = 'xts')
  Tesla:
    type: tap
    download:
      type: processor
      function: Quandl::Quandl(code = 'YAHOO/TSLA', type = 'xts')
indices:
  type: structure
  S&P500:
    type: tap
    download:
      type: processor
      function: quantmod::getSymbols(Symbols = '^GSPC', auto.assign = FALSE)
```

It defines three taps (Apple, Tesla, and S&P500), and organises stocks and indices neatly in a hierarchical structure.

## R functions

The R functions do the actual units of work of the pre-processing steps defined in (1) above, like e.g. downloading data from the internet, data cleaning, merging, etc. These functions can come from your own R scripts, or from third-party packages. The packages are typically datap-agnostic. For example, you can use Quandl, rvest, etc. 

In the above example, we use the ´Quandl´ package and the ´quantmod´ package.

## datap interpreter

The interpreter parses the datap context, and maps pre-processing steps defined in (1) to actual library functions available in (2), so as to provide to the user - for each tap - a callable tap function 

If you have the datap package installed, you can load the context into memory using the datap::Load function:

```{r}
library(datap)
filePath <- system.file("extdata", "context2.yaml", package="datap")
context <- Load(filePath)
```

The context looks like this:

```{r}
context
```

```
##        levelName
## 1 context       
## 2  ¦--stocks    
## 3  ¦   ¦--Apple 
## 4  ¦   °--Tesla 
## 5  °--indices   
## 6      °--S&P500
```

And you can directly navigate to a tap to fetch the data:

```{r}
teslaBars <- context$stocks$Tesla$tap()
head(teslaBars)
```

```{yaml}
##             Open  High   Low Close   Volume Adjusted Close
## 2010-06-29 19.00 25.00 17.54 23.89 18766300          23.89
## 2010-06-30 25.79 30.42 23.30 23.83 17187100          23.83
## 2010-07-01 25.00 25.92 20.27 21.96  8218800          21.96
## 2010-07-02 23.00 23.10 18.71 19.20  5139800          19.20
## 2010-07-06 20.00 20.00 15.83 16.11  6866900          16.11
## 2010-07-07 16.40 16.63 14.98 15.80  6921700          15.80
```

For the user of the context, it is completely transparent where the data is coming from, and how it is pre-processed. For example, the S&P500 index is downloaded from Yahoo finance and not from Quandl. Yet, the user accesses the dataset in exactly the same way:

```{r}
spx <- context$indices$`S&P500`$tap()
head(spx$GSPC.Open)
```

```{yaml}
##            GSPC.Open
## 2007-01-03   1418.03
## 2007-01-04   1416.60
## 2007-01-05   1418.34
## 2007-01-08   1409.26
## 2007-01-09   1412.84
## 2007-01-10   1408.70
```

However, in a real world scenario, you may want to make sure data structure is uniform accross taps. You can do this by adding additional pre-processing steps. This, in a nutshell, is what constitutes the bells and whistles of datap.

