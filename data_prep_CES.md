Get Child Care Estimates from the CES
========================================================
This document is used to calculate an integrated mean and standard deviation for child care expenditures as reported in the consumer expenditure survey. The script used here is a hack of the scripts written by [ajdamico](https://github.com/ajdamico/usgsd/tree/master/Consumer%20Expenditure%20Survey) and available for use the following [link](https://github.com/ajdamico/usgsd/tree/master/Consumer%20Expenditure%20Survey). The files were downloaded using scripts in that repo and the directories identified in the following scripts assume that the user has a directory structure that matches those resulting from the [ajdamico](https://github.com/ajdamico/usgsd/tree/master/Consumer%20Expenditure%20Survey) download scripts. The source scripts were designed to port the "Integrated Mean and SE.sas" code to R. The original SAS program is described by the BLS [here](www.bls.gov/cex/2010/csxdiary.pdf). The code here is mostly unchanged except to streamline for our focus on childcare expenditures.  

Preliminary Steps
-------------------------

The following code clears our memory, sets our working directory (as created by the [ajdamico](https://github.com/ajdamico/usgsd/tree/master/Consumer%20Expenditure%20Survey) scripts), and loads some libraries that are required for these initial steps. 


```r
# clear memory
rm(list = ls(all = TRUE))

# setwd('C:/Users/mienkoja/Dropbox/qualpaper/CES')
setwd("~/Dropbox/qualpaper/CES")

# turn off scientific notation in most output
options(scipen = 20)

require(stringr)  # load stringr package (manipulates character strings easily)
```

```
## Loading required package: stringr
```

```r
require(reshape2)  # load reshape2 package (transposes data frames quickly)
```

```
## Loading required package: reshape2
```

```r
require(sqldf)  # load the sqldf package (enables sql queries on data frames)
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required namespace: tcltk
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: RSQLite.extfuns
```

```r
require(RSQLite)  # load RSQLite package (creates database files in R)
require(plyr)  # load plyr package (allows for the merging of unequal dataframes via rbind.fill)
```

```
## Loading required package: plyr
```



Create Stub File
-------------------------
This chunk of code creates a "stub parameter file" and formats. Some of this code is probably overkill and can be streamlined at some point. The main change I have made to the original code here is to subset the file to only include expenditure data (i.e. `group == "EXPEND"`) and only include childcare expenditures (i.e. `ucc %in% c( "670310" , "340212" , "340211")`).


```r
# st <-
# readLines('C:/Users/mienkoja/Dropbox/qp_analysis/CES/2004/docs/Programs
# 2004/IntStub2004.txt')
st <- readLines("~/Dropbox/qualpaper/CES/2004/docs/Programs 2004/Intstub2004.txt")

# create a temporary file on the local disk..
tf <- tempfile()

# only keep rows starting with a one
st <- st[substr(st, 1, 1) == "1"]

# replace these two tabs with seven spaces instead
st <- gsub("\t\t", "       ", st)

# save to the temporary file created above
writeLines(st, tf)

# read that temporary file (the slightly modified IntStubYYYY.txt file) into
# memory as an R data frame
stubfile <- read.fwf(tf, width = c(1, -2, 1, -2, 60, -3, 6, -4, 1, -5, 7), col.names = c("type", 
    "level", "title", "ucc", "survey", "group"))

# eliminate all whitespace (on both sides) in the group column
stubfile$group <- str_trim(as.character(stubfile$group))

# subset the stubfile to only contain records a) in the four groups below b)
# where the survey column isn't 'T' c) where ucc indicates childcare
stubfile <- subset(stubfile, ucc %in% c("670310", "340212", "340211") & survey != 
    "T" & group == "EXPEND")

# remove the rownames from the stubfile (after subsetting, rows maintain
# their original numbering.  this action wipes it out.)
rownames(stubfile) <- NULL

# create a new count variable starting at 10,000
stubfile$count <- 10000 + (1:nrow(stubfile))

# create a new line variable by concatenating the count and level variables
stubfile$line <- paste0(stubfile$count, stubfile$level)

# start with a character vector with ten blank strings..
curlines <- rep("", 10)

# initiate a matrix containing the line numbers of each expenditure category
aggfmt1 <- matrix(nrow = nrow(stubfile), ncol = 10)

# loop through each record in the stubfile..
for (i in seq(nrow(stubfile))) {
    
    # if the 'ucc' variable is numeric (that is, as.numeric() does not return a
    # missing NA value)
    if (!is.na(as.numeric(as.character(stubfile[i, "ucc"])))) {
        
        # save the line number as the last element in the character vector
        curlines[10] <- stubfile[i, "line"]
        
        # otherwise blank it out
    } else curlines[10] <- ""
    
    # store the current line and level in separate atomic variables
    curlevel <- stubfile[i, "level"]
    curline <- stubfile[i, "line"]
    
    # write the current line inside the length-ten character vector
    curlines[curlevel] <- curline
    
    # if the current level is 1-8, blank out everything above it up to nine
    if (curlevel < 9) 
        curlines[(curlevel + 1):9] <- ""
    
    # remove actual value
    savelines <- curlines
    savelines[curlevel] <- ""
    
    # overwrite the entire row with the character vector of length ten
    aggfmt1[i, ] <- savelines
}

# convert the matrix to a data frame..
aggfmt1 <- data.frame(aggfmt1)

# ..and name its columns line1 - line10
names(aggfmt1) <- paste0("line", 1:10)

# tack on the ucc and line columns from the stubfile (which has the same
# number of records)
aggfmt1 <- cbind(aggfmt1, stubfile[, c("ucc", "line")])

# remove records where the ucc is numeric
aggfmt1 <- subset(aggfmt1, !is.na(as.numeric(as.character(ucc))))

# order the data frame by ucc
aggfmt1 <- aggfmt1[order(aggfmt1$ucc), ]

# rename line to compare
aggfmt1$compare <- aggfmt1$line
aggfmt1$line <- NULL

# reset the row names/numbers
rownames(aggfmt1) <- NULL

# transpose the data, holding ucc and compare
aggfmt2 <- melt(aggfmt1, id = c("ucc", "compare"))
names(aggfmt2)[4] <- "line"

# retain the ucc-to-line crosswalk wherever the 'line' variable is not blank
aggfmt <- subset(aggfmt2, line != "", select = c("ucc", "line"))

# re-order the data frame by ucc
aggfmt <- aggfmt[order(aggfmt$ucc), ]
```


Read in All Data
-------------------------
This chunk of code reads in all of the required data from the interview and diary files, creates an `mo_scope` variable, reads in the interview `mtab` and `itab` files along with the diary expenditure and `dtab` files. Finally, the data are merged to derive the appropriately weighted childcare expenditures. Again, some of this code is probably overkill and can be streamlined at some point. Note the strategic use of `gc()` throughout the code to clean up the RAM. This may be unecessary on some machines but as I was trying to 


```r
# read in the four 'fmld' files in the diary folder
# this contains all family diary records

load("2004/diary/fmld041.rda")
load("2004/diary/fmld042.rda")
load("2004/diary/fmld043.rda")
load("2004/diary/fmld044.rda")

d <- rbind(fmld041, fmld042, fmld043, fmld044)

# clear up RAM
gc()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# read in the five quarters of family data files (fmli)

# load all five R data files (.rda)
load("2004/intrvw/fmli041x.rda") 
load("2004/intrvw/fmli042.rda") 
load("2004/intrvw/fmli043.rda") 
load("2004/intrvw/fmli044.rda") 
load("2004/intrvw/fmli051.rda") 

# copy the fmliYY1x data frame to another data frame 'x'

fmli041x$qtr <- 1
fmli042$qtr <- 2
fmli043$qtr <- 3
fmli044$qtr <- 4
fmli051$qtr <- 5

# stack all five fmli# files together, into a large single data frame 'f'
f <- rbind.fill( fmli041x
                 ,fmli042
                 ,fmli043
                 ,fmli044
                 ,fmli051)

# only select families with young children 
f <- subset(f, f$as_comp5 > 0)

# delete all of the independent data frames from memory
rm( fmli041x
    ,fmli042
    ,fmli043
    ,fmli044
    ,fmli051)

# clear up RAM
gc()

# create a mo_scope variable in this large new family data frame
f <- 
  transform( 
    f ,
    mo_scope = 
      # the first quarter should be interview month minus one
      ifelse( qtr %in% 1 , as.numeric( qintrvmo ) - 1 ,
              # the final quarter should be four minus the interview month
              ifelse( qtr %in% 5 , ( 4 - as.numeric( qintrvmo )  ) ,
                      # all other quarters should have a 3
                      3 ) ) 
  )

# the source column for family records should be "I" (interview) throughout
f$source <- "I"

# the mo_scope variable for the 'd' (fmld) data frame should be 3 for all records
d$mo_scope <- 3
# ..and the source should be "D" throughout
d$source <- "D"

# create a character vector containing 45 variable names (wtrep01, wtrep02, ... wtrep44 and finlwt21)
wtrep <- c( paste0( "wtrep" , str_pad( 1:44 , 2 , pad = "0" ) ) , "finlwt21" )

# create a second character vector containing 45 variable names (repwt1, repwt2, .. repwt44, repwt45)
repwt <- paste0( "repwt" , 1:45 )

# create a third character vector that will be used to define which columns to keep
f.d.vars <- c( wtrep , "mo_scope" , "inclass" , "newid" , "source" )

# stack the family interview and diary records together,
# keeping only the 45 wtrep columns, plus the additional four written above
fmly <- rbind( f[ , f.d.vars ] , d[ , f.d.vars ] )

# remove data frames 'f' and 'd' from memory
rm( f , d )

# clear up RAM
gc()

# loop through the 45 wtrep variables in the newly-stacked fmly data frame..
for ( i in 1:45 ){
  
  # convert all columns to numeric
  fmly[ , wtrep[ i ] ] <- as.numeric( as.character( fmly[ , wtrep[ i ] ] ) )
  
  # replace all missings with zeroes
  fmly[ is.na( fmly[ , wtrep[ i ] ] ) , wtrep[ i ] ] <- 0
  
  # multiply by months in scope, then divide by 12 (months)
  fmly[ , repwt[ i ] ] <- ( fmly[ , wtrep[ i ] ] * fmly[ , "mo_scope" ] / 12 )
}

load("2004/diary/expd041.rda") 
load("2004/diary/expd042.rda") 
load("2004/diary/expd043.rda") 
load("2004/diary/expd044.rda") 

expd <- rbind( expd041
              ,expd042
              ,expd043
              ,expd044)

rm( expd041
    ,expd042
    ,expd043
    ,expd044)

gc()

load("2004/diary/dtbd041.rda") 
load("2004/diary/dtbd042.rda") 
load("2004/diary/dtbd043.rda") 
load("2004/diary/dtbd044.rda") 

dtbd <- rbind( dtbd041
              ,dtbd042
              ,dtbd043
              ,dtbd044)

rm( dtbd041
               ,dtbd042
               ,dtbd043
               ,dtbd044)

gc()

load("2004/intrvw/mtbi041x.rda") 
load("2004/intrvw/mtbi042.rda") 
load("2004/intrvw/mtbi043.rda") 
load("2004/intrvw/mtbi044.rda") 
load("2004/intrvw/mtbi051.rda") 

mtbi <- rbind( mtbi041x
               ,mtbi042
               ,mtbi043
               ,mtbi044
               ,mtbi051)

rm( mtbi041x
               ,mtbi042
               ,mtbi043
               ,mtbi044
               ,mtbi051)

gc()

load("2004/intrvw/itbi041x.rda") 
load("2004/intrvw/itbi042.rda") 
load("2004/intrvw/itbi043.rda") 
load("2004/intrvw/itbi044.rda") 
load("2004/intrvw/itbi051.rda") 

itbi <- rbind( itbi041x
                ,itbi042
                ,itbi043
                ,itbi044
                ,itbi051)

rm( itbi041x
               ,itbi042
               ,itbi043
               ,itbi044
               ,itbi051)


# clear up RAM
gc()

# copy (effectively rename) the 'amount' and 'value' columns to 'cost'
dtbd$cost <- dtbd$amount
itbi$cost <- itbi$value

# limit the itbi and mtbi (interview) data frames to records from the current year with pubflags of two
expend.itbi <- subset( itbi , pubflag == 2 & refyr == 2004 )
expend.mtbi <- subset( mtbi , pubflag == 2 & ref_yr == 2004 )

# choose which columns to keep when stacking these data frames
edmi.vars <- c( "newid" , "ucc" , "cost" )

# stack the itbi and mtbi files
expend.im <- 
  rbind( 
    expend.itbi[ , edmi.vars ] , 
    expend.mtbi[ , edmi.vars ] 
  )

# create a new 'source' column, with "I" (interview) throughout
expend.im$source <- "I"

# limit the expenditure diary to the same short list of variables, and only with a pubflag of two
expend.expd <- subset( expd , pub_flag == 2 , select = edmi.vars )

# create a new 'source' column, with "D" (diary) throughout
expend.expd$source <- "D"

# multiply the diary records' cost column by 13
expend.expd$cost <- expend.expd$cost * 13

# stack the interview and diary expenditure records together
expend <- rbind( expend.im , expend.expd )

# remove all of these smaller R data frames from memory
rm( itbi , mtbi , expend.itbi , expend.mtbi , expend.im , expend.expd )

# clear up RAM
gc()

# order the expenditure data frame by the unique consumer unit id (newid)
expend <- expend[ order( expend$newid ) , ]

#only grab the uccs we are interested in
expend <- subset(expend, expend$ucc %in% c( "670310" , "340212" , "340211"))

# create a character vector rcost1 - rcost45
rcost <- paste0( "rcost" , 1:45 )

# partially build the sql string, multiply each 'wtrep##' variable by 'cost' and rename it 'rcost##'
wtrep.cost <- paste0( "( b.cost * a." , wtrep , " ) as " , rcost , collapse = ", " )

# build the entire sql string..
sql.line <- 
  paste( 
    # creating a new 'pubfile' table, saving a few columns from each table
    "select a.newid , a.inclass , b.source , b.ucc ," ,
    wtrep.cost ,
    # joining the family and expenditure tables on two fields
    "from fmly a inner join expend b on a.newid = b.newid AND a.source = b.source" 
  )

pubfile <- sqldf(sql.line)
save.image("~/Dropbox/qualpaper/cc.RData")
```


Note the `save.image()` at the end of the previous chunk. I am not currently evaluating this chunk when I compile my markdown to HTML. The code runs fine outside of knitr, but knitr has a yet undiagnosed problem in trying to load the compressed data files. Once this problem is fixed, the following chunk will be unecessary.  


```r
load("~/Dropbox/qualpaper/cc.RData")
```


Calculate Aggregate Expenditures 
-------------------------
This next chunk sums all 45 weight variables to derive replicate populations. These weights are then used to calculate aggregate expenditures for each ucc. 


```r

# create a character vector containing 45 variable names (rpop1, rpop2, ... rpop44, rpop45)
rpop <- paste0( "rpop" , 1:45 )

# partially build the sql string, sum each 'repwt##' variable into 'rpop##'
rpop.sums <- paste( "sum( " , repwt , ") as " , rpop , collapse = ", " )

# partially build the sql string, sum each 'rcost##' variable into the same column name, 'rcost##'
rcost.sums <- paste( "sum( " , rcost , ") as " , rcost , collapse = ", " )

# create a total population sum (not grouping by 'inclass' -- instead assigning everyone to '10')
pop.all <- sqldf(paste( "select 10 as inclass, source, " , rpop.sums , "from fmly group by source"))
```

```
## Loading required package: tcltk
```

```r

# create a population sum, grouped by inclass (the income class variable)
pop.by <- sqldf(paste( "select inclass, source," , rpop.sums , "from fmly group by inclass, source"))

# stack the overall and grouped-by population tables
pop <- rbind( pop.all , pop.by )


# create the right hand side of the aggregate expenditures table
aggright <-
  # use a sql query from the temporary database (.db) file
  sqldf(  
    paste( 
      # group by inclass (income class) and a few other variables
      "select inclass, source, ucc," , 
      rcost.sums , 
      "from pubfile group by source , inclass , ucc" ,
      # the 'union' command stacks the grouped data (above) with the overall data (below)
      "union" ,
      # do not group by inclass, instead assign everyone as an inclass of ten
      "select '10' as inclass, source , ucc," , 
      rcost.sums , 
      "from pubfile group by source , ucc" 
    )
  )

# create three character vectors containing every combination of..

# the expenditure table's source variable
so <- names( table( expend$source ) )
# the expenditure table's ucc variable
uc <- names( table( expend$ucc ) )
# the family table's inclass (income class) variable
cl <- names( table( fmly[ , 'inclass' ] ) )
# add a '10' - overall category to the inclass variable
cl <- c( cl , "10" )

# now create a data frame containing every combination of every variable in the above three vectors
# (this matches the 'COMPLETETYPES' option in a sas proc summary call
aggleft <- expand.grid( so , uc , cl )

# name the columns in this new data frame appropriately
names( aggleft ) <- c( 'source' , 'ucc' , 'inclass' )

# perform a left-join, keeping all records in the left hand side, even ones without a match
agg <- merge( aggleft , aggright , all.x = TRUE )
```


Calculate Mean and Standard Errors 
-------------------------
This chunk combines all of the data formatted above and calculates means by dividing the aggregates by the relevant source population (i.e. diary vs. interview). Expenditure means are then summed per ucc for each income category. Standard errors are calculated using the BLS replicate formula identified in the original SAS code referenced above. 


```r

# create a character vector containing mean1, mean2, ... , mean45
means <- paste0("mean", 1:45)

# merge the population and weighted aggregate data tables together
avgs1 <- merge(pop, agg)

# loop through all 45 weights..
for (i in 1:45) {
    # calculate the new 'mean##' variable by dividing the expenditure (rcost##)
    # by the population (rpop##) variables
    avgs1[, means[i]] <- (avgs1[, rcost[i]]/avgs1[, rpop[i]])
    
    # convert all missing (NA) mean values to zeroes
    avgs1[is.na(avgs1[, means[i]]), means[i]] <- 0
}

# keep only a few columns, plus the 45 'mean##' columns
avgs1 <- avgs1[, c("source", "inclass", "ucc", means)]

# partially build the sql string, sum each 'mean##' variable into the same
# column name, 'mean##'
avgs.sums <- paste("sum( ", means, ") as ", means, collapse = ", ")

# merge on the 'line' column from the 'aggfmt' data frame
avgs3 <- merge(avgs1, aggfmt)

# remove duplicate records from the data frame
avgs3 <- sqldf("select distinct * from avgs3")

# construct the full sql string, grouping each sum by inclass (income class)
# and line (expenditure category)
sql.avgs <- paste("select inclass, line,", avgs.sums, "from avgs3 group by inclass, line")

# execute the sql string
avgs2 <- sqldf(sql.avgs)

# copy the avgs2 table over to a new data frame named 'se'
se <- avgs2

# create a character vector containing 44 strings, diff1, diff2, .. diff44
diffs <- paste0("diff", 1:44)

# loop through the numbers 1-44, and calculate the diff column as the square
# of the difference between the current mean and the 45th mean
for (i in 1:44) se[, diffs[i]] <- (se[, means[i]] - se[, "mean45"])^2
# for example, when i is 30, diff30 = ( mean30 - mean45 )^2

# save the 45th mean as the overall mean
se$mean <- se$mean45

# sum the differences, divide by 44 to calculate the variance, then take the
# square root to calculate the standard error
se$se <- sqrt(rowSums(se[, diffs])/44)

# retain only a few important columns in the se data frame
se <- se[, c("inclass", "line", "mean", "se")]

# transpose the se data frame by line and inclass, storing the value of the
# mean column save this result into a new data frame 'tab1m'
tab1m <- dcast(se, line ~ inclass, mean, value.var = "mean")

# transpose the se data frame by line and inclass, storing the value of the
# se column save this result into a new data frame 'tab1s'
tab1s <- dcast(se, line ~ inclass, mean, value.var = "se")

# create new columns in each data table, designating 'mean' and 'se'
tab1m$estimate <- "MEAN"
tab1s$estimate <- "SE"

# stack the mean and se tables together, into a new data frame called tab1
tab1 <- rbind(tab1m, tab1s)
```


Save Data for Later Analysis 
-------------------------

```r
cc_by_inc <- tab1
# extract means for own home, other home, and center child care
cc_own <- tab1[1, 11] * 0.91
cc_oth <- tab1[2, 11] * 0.91
cc_ctr <- tab1[3, 11] * 0.91

cc_own_se <- tab1[4, 11] * 0.91
cc_oth_se <- tab1[5, 11] * 0.91
cc_ctr_se <- tab1[6, 11] * 0.91

cc <- c("cc_own", "cc_oth", "cc_ctr", "cc_own_se", "cc_oth_se", "cc_ctr_se", 
    "cc_by_inc")

rm(list = setdiff(ls(), cc))
save.image("~/Dropbox/qualpaper/cc_out.RData")
```

