##################################################
##################################################
## Author: Luke Fostvedt
## Purpose: for simple creation of summary tables with option for LaTeX formatting
## dependencies: This script works purely with base R.
## last updated: November 7, 2019
##
## This script was originally developed to simplify my life. 
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##################################################
##################################################


#############################################
# Categorical data summary table
# This function is included for backwards compatibility
# An updated function has been created and is below this function
##################################################
# x1: the variable for the columns
# x2: The variable to 
# x1names: The names of the column variable for tabulation 
# x2names: The names of the row variable to be tabulate against
# Total: determines if a column tabulation should be included. 
#        default is false for backwards compatibility
tabcovsum<- function(x1,x2,x1names,x1vname,Total=F){
  # creating the table for two categorical variables
  tc <- table(x1,x2)
  if(Total) tc <- cbind(tc,"Total"=table(x1)) # table of just the levels ignoring levels
  
  # calculating the number of missing values
  tmiss<- table(factor(1*(is.na(x1)),levels=c(1,0)),x2)
  if(Total) tmiss <- cbind(tmiss, "Total" =table(factor(1*(is.na(x1)),levels=c(1,0))))# table of just the missingness ignoring cross levels
  # if there are missing values this will show up in the table tmiss
  if(sum(tmiss[1,]>0)){
    tc <- rbind(tc,tmiss[1,])     # adding another row for the variable will missing counts (%)
    x1names <- c(x1names,"Missing") # adding missing for the last row
  } 
  lx1 <- length(x1names)   # figuring out how many rows are needed for the \hline to be placed right
  x1names[lx1] <- paste("\\hline",x1names[lx1]) # adding \hline to the last row of the table (ie to the last entry of levels)
  tots <- apply(tc,2,sum) # counting the total N for each group of x1
  tperc <- t(round(100*t(tc)/tots))  # transposing the table tc so that the division is across group totals then transposing again
  tperc1 <- paste0("(",tperc,"\\%)") # adding in the latex % code
  tab1 <- data.frame(matrix(paste(tc,tperc1),dim(tc))) # creating a dataframe from the matrix that contains the counts and percentages
  colnames(tab1) <- colnames(tc) # naming the columns
  
  x1vname <- x1vname  # providing the names for the columns
  vnames <- c(x1vname,rep("",length(x1names)-1)) # adding in blank entries for rows so that the variable name isnt repeated over and over
  
  tab1 <- cbind(Variable=vnames,Category=x1names,tab1) # creating the output table
  row.names(tab1) <- NULL   # removing the row names
  return(tab1)
}


#############################################
# Categorical data summary table
# This function is included for backwards compatibility
# An updated function has been created and is below this function
#
#  **Similar to above but with the top row being blank**
#
##################################################
# x1: the variable for the columns
# x2: The variable to 
# x1names: The names of the column variable for tabulation 
# x2names: The names of the row variable to be tabulate against
# Total: determines if a column tabulation should be included. 
#        default is false for backwards compatibility
# LaTeX: if LaTeX formatting is desired
# PERCENT: if the percent symbol is desired (%)
# Per.Digits: How many decimal points for percentages. Default is 0
#             which will produce integer values
##################################################


pmx.sum.cat.table<- function(x1,x2,x1.names,x2.names,Total=FALSE,LaTeX=TRUE,PERCENT=FALSE,Perc.Digits=0,Cat.Perc.Across=FALSE){
  # creating the table for two categorical variables
  tc <- table(x1,x2)
  if(Total) tc <- cbind(tc,"Total"=table(x1))
  
  # calculating the number of missing values
  tmiss<- table(factor(1*(is.na(x1)),levels=c(1,0)),x2)
  if(Total) tmiss <- cbind(tmiss, "Total" =table(factor(1*(is.na(x1)),levels=c(1,0))))
  # if there are missing values this will show up in the table tmiss
  if(sum(tmiss[1,]>0)){
    tc <- rbind(tc,tmiss[1,])     # adding another row for the variable will missing counts (%)
    x1.names <- c(x1.names,"Missing") # adding missing for the last row
  } 
  if(LaTeX) x1.names <- paste("\\;",x1.names)
  lx1 <- length(x1.names)   # figuring out how many rows are needed for the \hline to be placed right
  if(LaTeX) x1.names[lx1] <- paste("\\hline",x1.names[lx1]) # adding \hline to the last row of the table (ie to the last entry of levels)
  
  # if the percentages should be across rows or within columns  
  if(Cat.Perc.Across){
    tots <- apply(tc,1,sum) # counting the total N for each group of x1
    if(Total) tots <- tots/2  # the total column doubles the sum since it adds the total to the group numbers
    tperc <- sodig(100*as.numeric(tc)/as.numeric(tots),Perc.Digits)  # division is across factor level then transposing 
  }else{    
    tots <- apply(tc,2,sum) # counting the total N for each group of x1
    tperc <- sodig(t(100*t(tc)/tots),Perc.Digits)  # transposing the table tc so that the division is across group totals then transposing again
  }
  
  if(PERCENT){  # if percent symbols are desired
    if(LaTeX){ 
      tperc1 <- paste0("(",tperc,"\\%)") # adding in the latex % code
    }else{
      tperc1 <- paste0("(",tperc,"%)")
    }
  }else{ # if percent symbols are not requested then no latex option needed
    tperc1 <- paste0("(",tperc,")")
  }
  tab1 <- data.frame(matrix(paste(tc,tperc1),dim(tc))) # creating a dataframe from the matrix that contains the counts and percentages
  colnames(tab1) <- colnames(tc) # naming the columns
  
  #  x1vnames <- x1.names  # providing the names for the columns
  tab1 <- cbind(Group=x1.names,tab1) # creating the output table
  vname <- matrix(c(x2.names,rep("",ncol(tab1)-1)),nrow=1) # adding in blank entries for rows so that the variable name isnt repeated over and over
  colnames(vname) <- colnames(tab1)
  tab2 <- rbind(vname,tab1)
  row.names(tab2) <- NULL   # removing the row names
  return(tab2)
}


##################################################
# Other useful functions

# converting digits to characters with the correct number of digits
# characters are needed for 0 final decimal places to not be dropped
# this also makes combining rows in tables easier
sodig <- function(x,digits=1){
  ddd <- paste0("%.",digits,"f")
  out.vec <- sprintf(ddd,round(x,digits))
  out.vec[out.vec=="NA"] <- NA
  out.vec
}

gm_mean_pmx = function(x, warn.note=T){# warning can be turned off if no geometric calculations are included
  if(warn.note & sum(x<=0,na.rm=T)>0) cat("There were",sum(x<=0,na.rm=T),"values that were negative and/or 0 and were removed to calculate geo_mean\n")
  if(warn.note & sum(is.na(x))>0) cat("There were ",sum(is.na(x)),"NA values removed to calculate geo_mean\n")
  mean_gm = mean(log(x[x > 0]), na.rm=T)
  exp(mean_gm)
}

geo_sd_pmx <- function(x,warn.note=T) {# warning can be turned off if no geometric calculations are included
  if(warn.note & sum(x<=0,na.rm=T)>0) cat("There were",sum(x<=0,na.rm=T),"values that were negative and/or 0 and were removed to calculate geo_sd\n")
  if(warn.note & sum(is.na(x))>0) cat("There were ",sum(is.na(x)),"NA values removed to calculate geo_sd\n")
  sdlog <- sd(log(x[x > 0]), na.rm = T)
  exp(sdlog)
}

geo_cv_pmx<-function(x, warn.note=T){# warning can be turned off if no geometric calculations are included
  if(warn.note & sum(x<=0,na.rm=T)>0) cat("There were",sum(x<=0,na.rm=T),"values that were negative and/or 0 and were removed to calculate geo_cv\n")
  if(warn.note & sum(is.na(x))>0) cat("There were ",sum(is.na(x)),"NA values removed to calculate geo_cv\n")
  sqrt(exp(log(geo_sd_pmx(x,warn.note))^2)-1)
}

# since min and max will return -Inf or Inf and a warning for null vectors (ie min(NULL)= -Inf)
# I am making a slightly adjusted function to eliminate the warning
min_pmx <- function(x,na.rm){
  if(sum(is.na(x))==length(x)) return(NA)
  min(x,na.rm=na.rm)
}
max_pmx <- function(x,na.rm){
  if(sum(is.na(x))==length(x)) return(NA)
  max(x,na.rm=na.rm)
}

##################################################
# Numerical covariate Variable summary table
##################################################

######################################################
# sum.cont.table is a function that will be called into later function
# it calls the the sodig() and geo_mean_pmx and geo_sd_pmx and geo_cv_pmx functions
######################################################

sum.cont.table <- function(data,
                           variables, # this is the main column variable as well as any subgroup variables
                           xvar="DV", # variable to be summarized
                           digits=1, # number of digits for the variable (default is 1 for sake of a default)
                           warn.note=T){ # will be TRUE if geometric functions included
  
  # in the event that there are no other variables to summarise by, then a constant variable is 
  # needed for tapply to give the output I want in table form
  if(is.null(variables)) {
    data$OnlyVar = "V1"
    a<- tapply(data[,xvar],data[,"V1"],median,na.rm=TRUE)
  }else{
    a<- tapply(data[,xvar],data[,variables],median,na.rm=TRUE)
  }
  
  #a<- tapply(data[,xvar],data[,variables],median,na.rm=TRUE)
  # expand.grid with the dimnames gives me a list of the unique levels for the variables being compared
  dat.out <- expand.grid(dimnames(a))
  if(length(variables)==1) colnames(dat.out) <- variables
  #start.names <- colnames(dat.out)
  
  # converting the columns to character so that rows can be added without causing an error
  for(i in 1:ncol(dat.out)) dat.out[,i] <- as.character(dat.out[,i])
  
  # adding a column for each function. as.vector() for tapply() fills out across rows first
  # as long as the tapply data arguments above match the tapply arguments below, as.vector
  # will fill out consistently. if the variables were rearranged then it would not match properly
  
  dat.out$Mean   <- as.vector(tapply(data[,xvar],data[,variables],mean,na.rm=T)) #keeping mean as numeric for SE calculation below
  
  dat.out$Median <- sodig(as.vector(tapply(data[,xvar],data[,variables],median,na.rm=T)),digits)
  dat.out$Min    <- sodig(as.vector(tapply(data[,xvar],data[,variables],min_pmx,na.rm=T)),digits)
  dat.out$Max    <- sodig(as.vector(tapply(data[,xvar],data[,variables],max_pmx,na.rm=T)),digits)
  dat.out$SD     <- sodig(as.vector(tapply(data[,xvar],data[,variables],sd,na.rm=T)),digits+1)
  dat.out$GeoMean<- sodig(as.vector(tapply(data[,xvar],data[,variables],gm_mean_pmx,warn.note)),digits+1)
  dat.out$GeoSD  <- sodig(as.vector(tapply(data[,xvar],data[,variables],geo_sd_pmx,warn.note)),digits+1)
  dat.out$GeoCV  <- sodig(as.vector(tapply(data[,xvar],data[,variables],geo_cv_pmx,warn.note)),digits+1)
  dat.out$Miss   <- as.vector(tapply(data[,xvar],data[,variables],function(x)sum(is.na(x))))
  dat.out$Length <- as.vector(tapply(data[,xvar],data[,variables],function(x)length(x)))
  
  dat.out$`Std. Error` <- sodig(dat.out$Mean/sqrt(dat.out$Length-dat.out$Miss),digits+1)
  dat.out$Mean   <- sodig(dat.out$Mean,digits+1) # now converting Mean to character
  
  # combining some of the functions to standard output by creating new columns
  dat.out$`Mean (Std. Dev.)` <- paste0(dat.out$Mean," (",dat.out$SD,")")
  dat.out$`Range (Min; Max)` <- paste0("(",dat.out$Min,"; ",dat.out$Max,")")
  dat.out$`GeoMean (GeoSD)` <- paste0(dat.out$GeoMean," (",dat.out$GeoSD,")")
  dat.out[is.na(dat.out$SD),c("Mean (Std. Dev.)","Range (Min; Max)","GeoMean (GeoSD)")] <- NA
  
  return(dat.out)
}


######################################################
# sum.by.tab() is the most complicated of the functions
# it calls the sum.cont.table() functions within it
######################################################
sum.by.tab <- function(data,
                       xvar, # variable to be summarized
                       sum.by=NULL, # the main column variable for summarizing.
                       more.variables=NULL, # subgrouping for summarizing.
                       digits=0, # number of significant digits for the variable being summarized (eg Age is an integer and would be 0)
                       LaTeX=TRUE,
                       PERCENT=FALSE,  # add the percentage sign (%), default is no sign
                       Perc.Digits=0,
                       warn.note=T){ # if geometric functions are included this will include the warning for missing, negative, and/or 0 values
  # creating a new variable to keep track of the levels and the summation across all levels
  data$NAME <- "Total"
  variables <- c("NAME",sum.by)  
  
  # creating the table for each of the levels
  a1 <- sum.cont.table(data,variables,xvar,digits=digits,warn.note)
  # creating the summary table for all of the levels combined
  a2 <- sum.cont.table(data,"NAME",xvar,digits=digits,warn.note)
  
  # creating the percent for sample sizes across levels. (a2 is the total)
  a1$N.PERCENT <- sodig(100*a1$Length/a2$Length,Perc.Digits)
  a2$N.PERCENT <- sodig(100,Perc.Digits)
  
  # calculating the percent missing. This is within a group though.
  a1$MISS.PERCENT <- sodig(100*a1$Miss/a1$Length,Perc.Digits) # missing within each group
  a2$MISS.PERCENT <- sodig(100*a2$Miss/a2$Length,Perc.Digits) # total missing
  
  
  # need to have the same columns for both to use rbind()
  # for combining a1 and a2. 
  
  
  a3 <- a2[,-which(colnames(a2) %in% variables)]  # keep common columns for overall average.
  a4 <- a1[1,variables] # keeping the names of the columns
  a4[1,sum.by] <- "Total"  # naming the levels for these to be ALL since they are the aggregates from a1 
  a5 <- cbind(a4,a3)    # combining the a4 variables to the left side of the dataframe for merging
  aout <- rbind(a1,a5)  # rbinding the totals with the per group levels
  aot <- cbind(Var1="Total",aout) # creating a new variable to help differentiate when additional variables (eg subgroups) are included
  
  ##################
  # Adding additional variables
  # The loop below will only be run if additional variables (eg subgroups) are provided
  # The calculations will be going variable by variables and each subgroup may have different
  # numbers of levels
  
  ind.vars.df <- data.frame() # creating a dummy dataframe for the new summaries
  if(length(more.variables)>0){  # only perform the additional functions if subgroup variables are provided
    for(k in 1:length(more.variables)){
      f1 <- sum.cont.table(data,c(more.variables[k]),xvar,digits=digits,warn.note) #same as above in a1
      f1 <- cbind(NAME="Total",f1,N.PERCENT="100")  # adding in "Total" since this is the total
      colnames(f1)[1:length(sum.by)] <- sum.by # making sure the names of the columns match all iteratiosn
      f2 <- sum.cont.table(data,c(sum.by,more.variables[k]),xvar,digits=digits,warn.note)  # calculating the summaries by level
      ## calculating the missing percent
      f1$MISS.PERCENT <- sodig(100*f1$Miss/f1$Length,Perc.Digits)
      f2$MISS.PERCENT <- sodig(100*f2$Miss/f2$Length,Perc.Digits)
      ## calculating the N (%)
      ### creating f.m to merge total N with subgroups
      f.m <- f1[,c(more.variables[k],"Length")]
      colnames(f2)[colnames(f.m)=="f2"] <- "Total.N"   # need to calculate total sample size by group
      colnames(f.m)[colnames(f.m)=="Length"] <- "Total.N" # this is the total N
      f3 <- merge(f2,f.m,by=more.variables[k])  # merging by the levels of the main variable and calculating for each subgroup
      f3$N.PERCENT <- sodig(100*f3$Length/f3$Total.N)
      f4 <- f3[,which(colnames(f3) != "Total.N")]  # don't need Total.N anymore
      ## merging with main data frame
      tmp.df <- rbind(f1,f4[,colnames(f4)])  # combining together. using colnames() to make sure the columns match
      colnames(tmp.df)[which(colnames(tmp.df)==more.variables[k])]<- "NAME" # defining a variable "NAME" so all dataframe columns match
      ind.vars.df <- rbind(ind.vars.df,cbind(Var1=more.variables[k],tmp.df)) # combining to the ind.vars.df dataframe at each iteration
    }  
  }
  
  # need a line in case there are no additional subgroups
  if(nrow(ind.vars.df)>0){
    out.df <- rbind(aot,ind.vars.df[,colnames(aot)])
  }else{
    out.df <- aot # if no subgroups then the previous dataframe is the output
  }
  
  # defining new variables for sample size and missing with combined percentage info 
  if(PERCENT==FALSE){
    out.df$`N (PERC)` <- paste0(out.df$Length," (",out.df$N.PERCENT,")")
    out.df$`Missing (PERC)` <- paste0(out.df$Miss," (",out.df$MISS.PERCENT,")")
  }else{
    if(LaTeX){
      out.df$`N (PERC)` <- paste0(out.df$Length," (",out.df$N.PERCENT,"\\%)")
      out.df$`Missing (PERC)` <- paste0(out.df$Miss," (",out.df$MISS.PERCENT,"\\%)")
    }else{
      out.df$`N (PERC)` <- paste0(out.df$Length," (",out.df$N.PERCENT,"%)")
      out.df$`Missing (PERC)` <- paste0(out.df$Miss," (",out.df$MISS.PERCENT,"%)")
    }}
  out.df[is.na(out.df$Length),c("N (PERC)")] <- NA
  out.df[is.na(out.df$Miss),c("Missing (PERC)")] <- NA
  # removing the old variables
  out.df1 <- out.df[,-which(colnames(out.df) %in% c("N.PERCENT","Length","Miss","MISS.PERCENT"))]
  return(out.df1)
}


######################################################
# arrange.columns() to pick and arrange columns
# The columns are still the summary calculations returned by sum.by.tab()
# and will be converted to rows in a later function
######################################################

##################################
# Function to pick and arrange columns
arrange.columns <- function(ds2,  # processed dataset as returned from sub.by.tab()
                            funs.summary=c("n","median","mean","sd","range","geomean","geosd","geocv","miss","se")){
  ############################
  # This function will provide the combined info if both mean and sd are requested or geomean and geosd are requested
  # picking variables to include in summary
  include.vars <- NULL
  
  # creating a dataframe with the names of the function as well as the order provided
  # for each function included, I plan to match with one of the calculations
  # then I can just ignore any rows that are missing and the output will be in the
  # order of the vector with the functions.
  df.rf <- data.frame(
    fun.order=c(1:length(funs.summary)),
    funs.sel = funs.summary,
    fun.include = NA  # all are NA to start and will be changed if the function is included
  )  
  
  if("n" %in% df.rf$funs.sel) df.rf$fun.include[which(df.rf$funs.sel=="n")] <- "N (PERC)"
  
  # adding mean and sd as one line if both included
  # otherwise if only one or none included then its own line
  if("mean" %in% df.rf$funs.sel & "sd" %in% df.rf$funs.sel){
    df.rf$fun.include[which(df.rf$funs.sel=="mean")] <- "Mean (Std. Dev.)"
    df.rf$fun.include[which(df.rf$funs.sel=="sd")] <- "Mean (Std. Dev.)"
  }else{
    if("mean" %in% df.rf$funs.sel) df.rf$fun.include[which(df.rf$funs.sel=="mean")] <- "Mean"
    if("sd" %in% df.rf$funs.sel)  df.rf$fun.include[which(df.rf$funs.sel=="sd")] <- "SD"
  } 
  
  if("median" %in% df.rf$funs.sel) df.rf$fun.include[which(df.rf$funs.sel=="median")] <- "Median"
  if("range" %in% df.rf$funs.sel) df.rf$fun.include[which(df.rf$funs.sel=="range")] <- "Range (Min; Max)"
  
  # adding geo mean and sd as one line if both included
  if("geomean" %in% df.rf$funs.sel & "geosd" %in% df.rf$funs.sel){
    df.rf$fun.include[which(df.rf$funs.sel=="geomean")] <- "GeoMean (GeoSD)"
    df.rf$fun.include[which(df.rf$funs.sel=="geosd")] <- "GeoMean (GeoSD)"
  }else{
    if("geomean" %in% df.rf$funs.sel)     df.rf$fun.include[which(df.rf$funs.sel=="geomean")] <- "GeoMean"
    if("geosd" %in% df.rf$funs.sel)     df.rf$fun.include[which(df.rf$funs.sel=="geosd")] <- "GeomSD"
  }
  
  if("geocv" %in% df.rf$funs.sel)  df.rf$fun.include[which(df.rf$funs.sel=="geocv")] <- "GeoCV"
  if("miss" %in% df.rf$funs.sel) df.rf$fun.include[which(df.rf$funs.sel=="miss")] <- "Missing (PERC)"
  
  if("se" %in% df.rf$funs.sel) df.rf$fun.include[which(df.rf$funs.sel=="se")] <- "Std. Error"  
  
  df.rf1 <- subset(df.rf,!duplicated(fun.include))  
  include.vars <- df.rf1$fun.include[!is.na(df.rf1$fun.include)]
  
  # this is hard coded to 1:3 since the fist 3 columns will not be summaries
  include.vars.ordered <- c(colnames(ds2)[1:3],include.vars)
  
  return(ds2[,include.vars.ordered]) # includign the first 3 columns and the selected summary stats
  
}


#############################################
## Function to format the output into a table for either Word or LaTeX
## The ds1 is the summarized table already from the previous function
## This function transposes the results so that summary statistics are rows
## and the groups are the columns
## format.cont.tab() should alse be able to read in a sum.by.tab() object but all functions will be included
#############################################

format.cont.tab <- function(ds1, # dataset returned from arrange.columns()
                            col.sum.by, # the main grouping variable (columns)
                            more.variables,  # subgroups to be included
                            col.sum.by.levels=NULL, # the user can input specific names if desired
                            LaTeX=T){ # for LATeX formatting
  
  # cleaning up the table for output
  if("N (PERC)" %in% colnames(ds1) & LaTeX)  colnames(ds1)[which(colnames(ds1)=="N (PERC)")] <- "N (\\%)"  
  if("Missing (PERC)" %in% colnames(ds1)& LaTeX)  colnames(ds1)[which(colnames(ds1)=="Missing (PERC)")] <- "Missing (\\%)"  
  if("N (PERC)" %in% colnames(ds1) & LaTeX==F)  colnames(ds1)[which(colnames(ds1)=="N (PERC)")] <- "N (%)"  
  if("Missing (PERC)" %in% colnames(ds1)& LaTeX==F)  colnames(ds1)[which(colnames(ds1)=="Missing (PERC)")] <- "Missing (%)"    
  
  # the function allows the user to specify the levels for the columns.
  # in general i do not expect this to be used and the function will default to the 
  # unique levels.
  if(length(col.sum.by.levels)>0){
    ds1[,col.sum.by] <- factor(ds1[,col.sum.by],levels=col.sum.by.levels)
  }else{
    ds1[,col.sum.by] <- factor(ds1[,col.sum.by],levels=unique(ds1[,col.sum.by])) 
  }
  
  # creating the first part with the total across all col.sum.by groups
  # unique gives me the unique variables and subgroups to merge 
  tab.lev <- unique(ds1[,c("Var1","NAME")])
  m.df1 <- merge(tab.lev[1,],ds1,by=colnames(tab.lev))
  m.df2 <- m.df1[order(m.df1[,col.sum.by]),-c(1,2)]   # I dont want Var1 and NAME because I will use t() to transpose and these get in the way
  out <- t(m.df2)[-1,] # transposing the dataframe.
  colnames(out) <- levels(ds1[,col.sum.by]) 
  # This will be the number of columns, plue the name "Total" since
  # the functions will be added later in the same column as Total
  top.row.name <- c("All",rep("",ncol(out)))  # a blank row for the overall summaries
  
  rn.main <- row.names(out)
  if(LaTeX==T){ 
    rn.main <- paste("\\;",row.names(out))  # adding a little indent to make it look nice
    rn.main[length(rn.main)] <- paste("\\hline",rn.main[length(rn.main)])  # adding a line at the end of the subgroup
  }
  final.out <- rbind(top.row.name,cbind(Group=rn.main,out))  # I expect this to get renamed in the LaTeX code so group is an arbitrary name
  
  # All the later levels to the first table
  # coded this way to allow for consistent column ordering
  # tab.lev will only have more than 1 row if there are subgroups to calculate
  # the first row is the Total Total row with overall totals
  if(nrow(tab.lev)>1){
    for(i in 2:nrow(tab.lev)){
      m.df1 <- merge(tab.lev[i,],ds1,by=colnames(tab.lev))  # getting a dataframe with summary stats for the specific subgroup
      m.df2 <- m.df1[order(m.df1[,col.sum.by]),-c(1,2)]  # removing Var1 and NAME for transposing
      out.new <- t(m.df2)[-1,]
      colnames(out.new) <- levels(ds1[,col.sum.by])  # same levels as above table
      
      # if latex output then pretty up with some indentation and \hline
      if(LaTeX==TRUE){
        rn1 <- paste("\\;", row.names(out.new))  
        out.new1 <- cbind(Group=rn1,out.new)
        out.new1[nrow(out.new1),1] <- paste("\\hline",out.new1[nrow(out.new1),1])  # adding \hline to the last level for a bottom horizontal line
      }else{
        rn1 <- row.names(out.new)
        out.new1 <- cbind(Group=rn1,out.new)
      }
      group.label <- tab.lev[i,2]  # adding the group label for the subgroup chunk in this iteration
      group.label.row <- c(group.label,rep("",ncol(out.new1)-1)) # adding a new line with blank entries to make it look cleaner
      out.new2 <- rbind(group.label.row,out.new1) # binding the blank row with group info about summary stats
      final.out <- rbind(final.out,out.new2)
    }
  }
  rownames(final.out) <- NULL
  
  return(final.out)
}



##############################################################
# Combining all the functions to a single function 
# that will return a table with numeric summaries
##############################################################


pmx.sum.table <- function(data,
                          xvar="DV",  # variable to be summarized
                          sum.by=NULL, # column groups to summarize across
                          more.variables = NULL, # subgroups to be included
                          digits=1, # number of significant figures
                          LaTeX=T, # if LaTeX formating is desired
                          PERCENT=FALSE,  # should percent signs be included (eg 100%)
                          Perc.Digits=0,  # number of digits for percent calculations
                          # list of available functions
                          funs.summary=c("n","median","mean","sd","range","geomean","geosd","geocv","miss","se")
){
  # # # # # # # # # # # #
  # Warnings for user 
  # # # # # # # # # # # #  
  
  if(!(class(data[,xvar]) %in% c("numeric","integer"))){
    cat("The variable: ",xvar," --- is not numeric. Please Fix.")
    break
  }
  
  if(length(sum.by) != 1) {
    cat("WARNING: currently this function can only summarize by a single variable",
        "\nYou can merge the variables into a unique variable if you want to compare across multiple.")
    break
  }  
  int.var <-intersect(more.variables,sum.by)
  if(length(int.var)>0){
    warning("WARNING:",int.var,"is in both the sum.by and more.variables arguments",
            "\n",int.var,"is being removed from more.variables")
    more.variables <- setdiff(more.variables,sum.by)
  }
  
  available.functions <- c("n","median","mean","sd","range","geomean","geosd","geocv","miss","se")
  delfun <- setdiff(funs.summary,available.functions)
  if(length(delfun)>0){
    funs.summary = intersect(funs.summary,available.functions)
    cat("The function(s):",delfun,"\nare not available. Please check you have them named correctly",
        "\nThe available functions are:\n",available.functions)
    break
  }
  
  # only print the geometric calculations warnings if a geometric calculation is requested
  if(length(intersect(funs.summary,c("geomean","geosd","geocv")))>0){
    warn.note=TRUE
  }else{
    warn.note=FALSE
  }
  
  
  tab1 <-  sum.by.tab(data=data,
                      xvar=xvar,
                      sum.by=sum.by,
                      more.variables=more.variables,
                      digits=digits,
                      LaTeX=LaTeX,
                      PERCENT=PERCENT,
                      Perc.Digits=Perc.Digits,
                      warn.note=warn.note)
  tab2 <- arrange.columns(ds2=tab1,
                          funs.summary=funs.summary)
  tab3 <- format.cont.tab(ds1 = tab2,
                          col.sum.by=sum.by,
                          more.variables = more.variables,
                          col.sum.by.levels=NULL, # this is null because I dont want users to use it within this function
                          LaTeX=LaTeX)
  return(tab3)
}


##############################################################
# Should both continuous and categorical summaries be desired in one table 
# this function will wrap the functions pmx.sum.table() and pmx.sum.cat.table()
# to make a summary table
# it is advised not to use more.variables since it will only perform the additional
# calculations for the continuous variables in pmx.sum.table()
#
# data:             data set containing the summary variables
# cont.xvars:       vector of continious variable(s) to be summarized, default is null
# cat.xvars:        vector of continious variable(s) to be summarized, default is null
# sum.by:           what to summarize across, must be a single variable. default is null
# more.variables:   if subgroups are to be summarized within each variable, **recommend not using**
# digits:           number of significant digit(s); need one defined for each sum.by variable, default is 0
# LaTeX:            TRUE/FALSE if the output should be printed with LaTeX formatting, default is TRUE
# Cont.Var.Name:    name of the continuous summary variable(s) to print; 
#                   need one for each sum.by variable, default is NULL
# Cat.Var.Name:     name of the categoric summary variable(s) to print; 
#                   need one for each sum.by variable, default is NULL
# PERCENT:          TRUE/FALSE for adding percent sign within parentheses (%), default is FALSE
# Perc.Digits:      Number of digits for percent
# funs.summary:  vector of summary functions
##############################################################


pmx.sum.table.comb <- function(data,
                               cont.xvars=NULL,    # variable(s) to be summarized
                               cat.xvars=NULL,     # categorical variables to be summarized
                               sum.by=NULL, # what to summarize across
                               more.variables = NULL, # if subgroups are to be summarized within each variable
                               digits=0, # number of significant digit(s); need one defined for each sum.by variabel
                               LaTeX=T,# if the output should be printed with LaTeX formatting
                               Cont.Var.Name=NULL, #name of the summary variable(s); need one for each sum.by variable
                               Cat.Var.Name=NULL, #
                               PERCENT=F,  # add percent sign within parentheses (%)
                               Perc.Digits=0,
                               Cat.Perc.Across=FALSE,
                               funs.summary=c("n","median","mean","sd","range","geomean","geosd","geocv","miss","se")
) { # vector with the functions to summarize
  
  # keeping track of warnings
  warn.track <- 0
  ww <- NULL
  
  ##########
  # Checking that all requested variables are in the dataset
  if(length(setdiff(c(cont.xvars,cat.xvars),colnames(data)))>0){
    cat("WARNING: There are variables requested that are not in the dataset\n")
    cat("\n",setdiff(c(cont.xvars,cat.xvars),colnames(data)),"are not in the dataset")
    warn.track <- c(warn.track,1)
  }
  
  
  ###### Checks for if continuous variables are provided
  if(length(cont.xvars)>0){
    # Checking that the continuous covariates are class numeric or integer
    for(vv in 1:length(cont.xvars)){
      ww <- c(ww,class(data[,cont.xvars[vv]]))
      wwn <- which(!(ww %in% c("numeric","integer"))) # this will be null is all are integer or numeric
    }
    if(length(wwn)>0){
      warn.track <- c(warn.track,1)
      cat("The variables:",cont.xvars[wwn]," are not of class numeric. Please Fix\n")
      warning("You ask for a non-numeric variable to be summarized. please change to class numeric or integer")
    }
    
    if(length(digits)!=length(cont.xvars)) {
      cat("WARNING: Digits are not specified for each variable to be summarized.")
      cat("\nVariables:",cont.xvars,  "\nSig.Digits:",digits,"\n")
      warn.track <- c(warn.track,1)
    }
    
    if(length(Cont.Var.Name)!=length(cont.xvars)) {
      cat("WARNING: Names for the variables are not specificed for each variable to be summarized.\n")
      cat("Variables:",cont.xvars,  "\nNames:",Cont.Var.Name,"\n")
      warn.track <- c(warn.track,1)
    }
  }# end of checks for continuous variables
  
  
  
  ###### Checks for if continuous variables are provided
  if(length(cat.xvars)>0){
    if(length(Cat.Var.Name)!=length(cat.xvars)) {
      cat("WARNING: Names for the variables are not specificed for each variable to be summarized.\n")
      cat("\nVariables:",cont.xvars,  "\nNames:",Cont.Var.Name,"\n")
      warn.track <- c(warn.track,1)
    }
  }
  
  if(length(funs.summary)==0) {
    cat("WARNING: funs.summary cannot be null. please include at least one of the available summary functions\n")  
    warn.track <- c(warn.track,1)
  }
  
  if(sum(warn.track)>0) stop("\nWARNING: You have issues!")
  
  if(length(cat.xvars)>0){
    if(LaTeX){
      Cat.Var.Name <- paste0(Cat.Var.Name,"; N(\\%)")
    }else{
      Cat.Var.Name <- paste0(Cat.Var.Name,"; N(%)")
    }
  }
  
  # for the unique levels of each of the more.variables to paste with summary variable
  uvm <- NULL
  for(dd in more.variables){
    if(is.factor(data[,dd])){
      uvm <- c(uvm,levels(data[,dd])) #
    }else{
      uvm <- c(uvm,unique(data[,dd]))
    }    
  }
  
  ## Making the contvariable component
  outdf.cont <- NULL
  if(length(cont.xvars)>0){ # If no continuous covariates are specified then will not make this part of the table
    for(jj in 1:length(cont.xvars)){
      # calling the function to make the table for each variabel    
      a <- pmx.sum.table(data,
                         xvar=cont.xvars[jj],
                         sum.by=sum.by,
                         more.variables=more.variables, 
                         digits=digits[jj],
                         LaTeX=LaTeX,
                         PERCENT=PERCENT,
                         Perc.Digits=Perc.Digits, 
                         funs.summary=funs.summary)
      
      a[1,1] <- Cont.Var.Name[jj]  # renaming the name of the variable in the blank row
      if(length(more.variables)>0){
        lfn <- sum(!is.na(uvm)) # counting non-NA value (same as the length after removing NAs)
        lfn1 <- nrow(a)/(lfn+1)  # adding 1 since also have the first row with totals
        row.paste <- which(1:nrow(a) %% lfn1 == 1) # the mod function will determine which rows are multiples of lfn1 and need the name pasted with a level
        for(ii in row.paste[-1]) a[ii,1] <- paste(Cont.Var.Name[jj],a[ii,1],sep=": ") # -1 since I dont want a doubling on the total row
      }
      outdf.cont <- rbind(outdf.cont,a)
    }
  }
  
  ## Making the Cat variable component  
  outdf.cat <- NULL
  if(length(cat.xvars)>0){
    for(kk in 1:length(cat.xvars)){
      if(length(levels(data[,cat.xvars[kk]]))>0){  # if the variable is not a factor, then i use unique values
        rnl <- levels(data[,cat.xvars[kk]])
        x1.names <- rnl[!is.na(rnl)]   # removing NA from the levels since the function will tabulate and rename them
      }else{
        rnm <- unique(data[,cat.xvars[kk]])
        x1.names <- rnm[!is.na(rnm)]  # removing NA from the levels since the function will tabulate and rename them
      } 
      # calling the function to make the table for each variabel  
      a <- pmx.sum.cat.table(x1=data[,cat.xvars[kk]],
                             x2=data[,sum.by],
                             x1.names=x1.names,
                             x2.names=Cat.Var.Name[kk],
                             Total=TRUE,  # since pmx.cont.table includes total this is set to true
                             LaTeX=LaTeX,
                             PERCENT=PERCENT,
                             Perc.Digits=Perc.Digits,
                             Cat.Perc.Across=Cat.Perc.Across)
      outdf.cat <- rbind(outdf.cat,a)
    }
  }
  #  # I need the final columns to have the same name. cont default is ALL and cat default is Total. Picking Total for both
  #  colnames(outdf.cont)[ncol(outdf.cont)] <- colnames(outdf.cat)[ncol(outdf.cat)]
  out.df <- rbind(outdf.cont,outdf.cat)
  return(out.df)
}




