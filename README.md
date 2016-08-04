

Master: [![Build Status master](https://travis-ci.org/gluc/datap.svg?branch=master)](https://travis-ci.org/gluc/datap) [![Windows Build status]( https://ci.appveyor.com/api/projects/status/github/gluc/datap?branch=master&svg=true)](https://ci.appveyor.com/project/gluc/datap) [![codecov.io](http://codecov.io/github/gluc/datap/coverage.svg?branch=master)](http://codecov.io/github/gluc/datap?branch=master)

Dev: [![Build Status dev](https://travis-ci.org/gluc/datap.svg?branch=dev)](https://travis-ci.org/gluc/datap) [![Windows Build status]( https://ci.appveyor.com/api/projects/status/github/gluc/datap?branch=dev&svg=true)](https://ci.appveyor.com/project/gluc/datap) [![codecov.io](http://codecov.io/github/gluc/datap/coverage.svg?branch=dev)](http://codecov.io/github/gluc/datap?branch=dev)

# What is datap?

datap is a lightweight DSL (Domain Specific Language) to define configurable, modular, and re-usable data processes for use in the R programming language. datap contexts can be used to acquire, pre-process, quality-assure, and merge data in a way that is completely transparent to the user.

Your data can be anything, and it can come from any source (the internet, a third-party provider, screen-scraping, a file, from memory, by calling an R function, or data generated on the fly by your context, etc.).


# What is datap useful for?

datap is useful to many ends. For example

1. to quickly get you up and running with a new project, especially if you need data from multiple sources
2. to define readable and query-able attributes on your data sets and data series (e.g. "give me the timeseries of all stocks I am using for strategy X")
3. to organise your data sets hierarchically (e.g. "give me water quality of all OECD countries")
4. to separate configuration from code (configuration: your datap context; code: your code consuming the context. e.g. "Let's add another stock to that strategy!")
5. to start simply first, and become sophisticated later (e.g. start by downloading data from Quandl, for productive use you simply replace the datap context file with a Bloomberg configuration; or: start with Yahoo finance data, later use your own database. All with datap!)
6. to create an in-memory hierarchical database of data sets (datap can cache your data!)
7. to share data requirements, without actually sharing the data (on Stack Overflow: "Please use the context defined in this gist. It defines all the data you need to reproduce the problem.")
8. to define exactly how to update your data sets

# How do I use datap?

In practice, each datap setup will consist of the following elements:

1. A datap context, i.e. a YAML file
2. R functions (your own or from packages)
3. The datap interpreter, i.e. the R datap package

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

However, in a real world scenario additional pre-processing steps are necessary to make sure that the structure of the data is indeed uniform across datasets from different sources. This, in a nutshell, is what constitutes the bells and whistles of datap.

