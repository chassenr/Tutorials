# information on melt and cast
# seananderson.ca/2013/10/19/reshape.html

library(data.table)  

DF = data.frame(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9, stringsAsFactors = F)
DT = data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)

DF
DT

identical(dim(DT),dim(DF)) # TRUE
identical(DF$x, DT$x)      # TRUE
is.list(DF)                # TRUE
is.list(DT)                # TRUE

is.data.frame(DT)          # TRUE

# how many data.tables?
tables()

DT[2]                      # 2nd row
DT[,v]                     # v column (as vector)
DT[,list(v)]               # v column (as data.table)
DT[2:3,sum(v)]             # sum(v) over rows 2 and 3
DT[2:5,cat(v,"\n")]        # just for j's side effect
DT[c(FALSE,TRUE)]          # even rows (usual recycling)

DT[,2,with=FALSE]          # 2nd column
colNum = 2
DT[,colNum,with=FALSE]     # same

setkey(DT,x)               # set a 1-column key. No quotes, for convenience.
setkeyv(DT,"x")            # same (v in setkeyv stands for vector)
v="x"
setkeyv(DT,v)              # same
# key(DT)<-"x"             # copies whole table, please use set* functions instead

DT["a"]                    # binary search (fast)
DT[x=="a"]                 # same; i.e. binary search (fast)

DT[,sum(v),by=x]           # keyed by
DT[,sum(v),by=key(DT)]     # same
DT[,sum(v),by=y]           # ad hoc by

DT["a",sum(v)]                    # j for one group
DT[c("a","b"),sum(v),by=.EACHI]   # j for two groups, for each i specified before the first comma

# The world of databasing! Joins etc..

X = data.table(c("b","c"),foo=c(4,2))
X
setkey(X, V1)
DT

DT[X]                      # join
DT[X,sum(v),by=.EACHI]     # join and eval j for each row in i
DT[X,mult="first"]         # first row of each group
DT[X,mult="last"]          # last row of each group
DT[X,sum(v)*foo,by=.EACHI] # join inherited scope

setkey(DT,x,y)             # 2-column key
setkeyv(DT,c("x","y"))     # same
# similar to having 2 element in cast formula

# Now think of selects like joins!

DT["a"]                    # join to 1st column of key
DT[.("a")]                 # same, .() is an alias for list()
DT[list("a")]              # same

DT[.("a",3)]               # join to 2 columns - x = a and y = 3
DT[.("a",3:6)]             # join 4 rows (2 missing)
DT[.("a",3:6),nomatch=0]   # remove missing
DT[.("a",3:6),roll=TRUE]   # rolling join (locf)

DT[,sum(v),by=.(y%%2)]     # by expression, here modulo (remainder)
DT[,.SD[2],by=x]           # 2nd row of each group, .SD is a sort 
                           # sub-data.table

DT[,tail(.SD,2),by=x]      # last 2 rows of each group
DT[,lapply(.SD,sum),by=x]  # apply through columns by group

# You can really get specific...

DT[,list(MySum=sum(v),
         MyMin=min(v),
         MyMax=max(v)),
   by=.(x)] 

DT[,list(MySum=sum(v),
         MyMin=min(v),
         MyMax=max(v)),
   by=.(x,y%%2)]        # by 2 expressions

DT[,sum(v),x]
DT[,sum(v),x][V1<20]       # compound query (chain commands)
DT[,sum(v),x][order(-V1)]  # ordering results


# Adding new columns...
print(DT[,z:=42L])         # add new column by reference
DT
print(DT[,z:=NULL])        # remove column by reference
DT
print(DT["a",v:=42L])      # subassign to existing v column by reference
DT
print(DT["b",v2:=84L])     # subassign to new column by reference (NA padded)
DT

DT[,m:=mean(v),by=x][]     # add new column by reference by group
# NB: postfix [] is shortcut to print()

# Find out WHERE the mins are with compact code...
DT[,.SD[which.min(v)],by=x][]  # nested query by group

# inverse joins
DT[!.("a")]                # not join
DT[!"a"]                   # same
DT[!2:4]                   # all rows other than 2:4

DT[x!="b" | y!=3]          # not yet optimized, currently vector scans
DT[!.("b",3)]              # same result but much faster


# new feature: 'on' argument, from v1.9.6+
DT1 = data.table(x=c("c", "a", "b", "a", "b"), a=1:5)
DT2 = data.table(x=c("d", "c", "b"), mul=6:8)

DT1[DT2, on=c(x="x")] # join on columns 'x'
DT1[DT2, on="x"] # same as above
DT1[DT2, .(sum(a) * mul), by=.EACHI, on="x"] # using by=.EACHI
DT1[DT2, .(sum(a) * mul), by=.EACHI, on="x", nomatch=0L] # using by=.EACHI


# Reset and review...

DF = data.frame(x=c("b","b","b","a","a"),v=rnorm(5))
DF

DT = data.table(x=c("b","b","b","a","a"),v=rnorm(5))
DT

CARS = data.table(cars)
head(CARS)

tables()


sapply(DT,class)


tables()
DT


DT[2,]         # select row 2
DT[x=="b",]    # select rows where column x == "b"

cat(try(DT["b",],silent=TRUE))


setkey(DT,x)
DT


tables()


DT["b",]


DT["b",mult="first"]
DT["b",mult="last"]


DT["b"]

grpsize = ceiling(1e7/26^2)   # 10 million rows, 676 groups
tt=system.time( DF <- data.frame(
  x=rep(LETTERS,each=26*grpsize),
  y=rep(letters,each=grpsize),
  v=runif(grpsize*26^2),
  stringsAsFactors=FALSE)
)

tt

head(DF,3)
tail(DF,3)
dim(DF)

tt <- system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])   # 'vector scan'

tt

head(ans1,3)
dim(ans1)


DT = as.data.table(DF)       # but normally use fread() or data.table() directly, originally 
system.time(setkey(DT,x,y))  # one-off cost, usually


# use our keys to do the search...
ss <- system.time(ans2 <- DT[list("R","h")])   # binary search
ss
head(ans2,3)
dim(ans2)
identical(ans1$v, ans2$v)



system.time(ans1 <- DT[x=="R" & y=="h",])   # works but is using data.table badly
system.time(ans2 <- DT[DT$x=="R" & DT$y=="h",])   # the data.frame way
mapply(identical,ans1,ans2)



# compact syntax...
identical( DT[list("R","h"),],
           DT[.("R","h"),])

# some functions on the fly...
DT[,sum(v)]

# handy and compact grouping functions...
DT[,sum(v),by=x]


# compare performance...
ttt=system.time(tt <- tapply(DT$v,DT$x,sum)); ttt
tsss=system.time(ss <- DT[,sum(v),by=x]); tsss

head(tt)
head(ss)
identical(as.vector(tt), ss$V1)




ttt=system.time(tt <- tapply(DT$v,list(DT$x,DT$y),sum)); ttt
sss=system.time(ss <- DT[,sum(v),by="x,y"]); sss
tt[1:5,1:5]
head(ss)
identical(as.vector(t(tt)), ss$V1)

sst <- data.table::dcast(
  ss,
  x ~ y,
  fun.aggregate = sum,
  value.var = "V1"
)

tt[1:5,1:5]
sst[1:5,1:5, with = F]

# A bit more about DT's dcast

# multiple value.var and multiple fun.aggregate
dt <- data.table(
  x = sample(5,20,TRUE),
  y = sample(2,20,TRUE), 
  z = sample(letters[1:2], 20,TRUE),
  d1 = runif(20),
  d2 = 1L
  )

# multiple value.var
dcast(dt, x + y ~ z, fun=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=list("d1", "d2"))


# what can be cast, must also be meltable...

set.seed(45)
require(data.table)
DT <- data.table(
  i_1 = c(1:5, NA), 
  i_2 = c(NA,6,7,8,9,10), 
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)), 
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE), 
  c_1 = sample(c(letters[1:3], NA), 6, TRUE), 
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"), 
  d_2 = as.Date(6:1, origin="2012-01-01"))
# add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]

# id, measure as character/integer/numeric vectors
melt(DT, id=1:2, measure="f_1")
melt(DT, id=c("i_1", "i_2"), measure=3) # same as above
melt(DT, id=1:2, measure=3L, value.factor=TRUE) # same, but 'value' is factor
melt(DT, id=1:2, measure=3:4, value.factor=TRUE) # 'value' is *ordered* factor

# preserves attribute when types are identical, ex: Date
melt(DT, id=3:4, measure=c("d_1", "d_2"))
melt(DT, id=3:4, measure=c("i_1", "d_1")) # attribute not preserved

# on list
melt(DT, id=1, measure=c("l_1", "l_2")) # value is a list
melt(DT, id=1, measure=c("c_1", "l_1")) # c1 coerced to list

# on character
melt(DT, id=1, measure=c("c_1", "f_1")) # value is char
melt(DT, id=1, measure=c("c_1", "i_2")) # i2 coerced to char

# on na.rm=TRUE. NAs are removed efficiently, from within C
melt(DT, id=1, measure=c("c_1", "i_2"), na.rm=TRUE) # remove NA

# NEW FEATURE: measure.vars can be a list
# melt "f_1,f_2" and "d_1,d_2" simultaneously, retain 'factor' attribute
# convenient way using internal function patterns()
melt(DT, id=1:2, measure=patterns("^f_", "^d_"), value.factor=TRUE)

# same as above, but provide list of columns directly by column names or indices
melt(DT, id=1:2, measure=list(3:4, c("d_1", "d_2")), value.factor=TRUE)

# na.rm=TRUE removes rows with NAs in any 'value' columns
melt(DT, id=1:2, measure=patterns("f_", "d_"), value.factor=TRUE, na.rm=TRUE)

# return 'NA' for missing columns, 'na.rm=TRUE' ignored due to list column
melt(DT, id=1:2, measure=patterns("l_", "c_"), na.rm=TRUE)