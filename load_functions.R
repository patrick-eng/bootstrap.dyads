#' Dyad-Ratios Functions for assignment into Global Environment
#'
#' Storage for Stimson functions, with significant changes and bug fixes within the extract and aggregate functions.
#'
#' No arguments or functions to call, assigned to environment using the call_dr_code command.

  display <- function(out,filename=NULL) {
    if (is.null(filename)) filename=""
    d<-out$dimensions
    p<-out$period
    m<-out$latent1
    if (d==2) m2<-out$latent2
    T<-out$T
    mo=100*(p-as.integer(p))
    for (t in 1:T) {
      yr<-format(as.integer(p[t]),nsmall=0)
      month<-format(mo[t],digits=2)
      lat1<-format(m[t],nsmall=3)
      if (d==1) {
        cat(c(yr,month,lat1),fill=TRUE,file=filename,append=TRUE)
      } else {
        lat2<-format(m2[t],nsmall=3)
        cat(c(yr,month,lat1,lat2),fill=TRUE,file=filename,append=TRUE)
      }
    }
}



  ##########################################################################################
  plot.Zextract<-function(outobject) {
    dim<- outobject$dimensions
    T<- outobject$T
    vect1<-outobject$latent1
    t<-seq(1:T)
    if (dim>1) {
      vect2<-outobject$latent2
      miny<-min(vect1)
      if (miny>min(vect2)) miny<-min(vect2)
      maxy<-max(vect1)
      if (maxy<max(vect2)) maxy<-max(vect2)
      dummy<-rep(miny,T-1) #dummy is a fake variable used to reset axes to handle min/max of both series
      dummy[T]<-maxy
      leg.text<-c("","Dimension 1","Dimension 2")
      plot(t,dummy,type="l",lty=0,main="Final Estimation Results: Two Dimensions",xlab="Time Point",ylab="Latent Variables")
      lines(t,vect1,col=1)
      lines(t,vect2,col=2)
      legend(1,maxy,leg.text,col=c(0,1,2),lty=c(0,1,1))
    } else {
      plot(t,vect1,type="l",main="Final Estimation Results",xlab="Time Point",ylab="Latent Variable")
      if (dim == 2) lines(t,vect2,col=2)
    }
  }
  ##########################################################################################
  summary.Zextract<- function(outobject) {
    T=outobject$T
    nvar=outobject$nvar
    dim<- outobject$dimensions
    vn<- c(outobject$varname,"Variable Name")
    vn<- format(vn,justify="right")
    nc<- format(outobject$N,justify="right")
    ld<- format(outobject$loadings1,digits=3,justify="right")
    mean<- format(outobject$means,digits=6,justify="right")
    sd<- format(outobject$std.deviations,digits=6,justify="right")
    cat("Variable Loadings and Descriptive Information: Dimension 1\n")
    cat(paste(vn[nvar+1],"Cases","Loading","   Mean ","Std Dev","\n"))
    for (v in 1:nvar) {
      cat(paste(vn[v],"  ",nc[v]," ",ld[v],mean[v],sd[v],"\n"))
    }
    if (dim == 2) {
      ld<- format(outobject$loadings2,digits=3,justify="right")
      cat("\nVariable Loadings and Descriptive Information: Dimension 2\n")
      cat(paste(vn[nvar+1],"Cases","Loading","   Mean ","Std Dev","\n"))
      for (v in 1:nvar) {
        cat(paste(vn[v],"  ",nc[v]," ",ld[v],mean[v],sd[v],"\n"))
      }
    }
  }
  ##########################################################################################
  findper<-function(unit,curdate,mind,miny,minper,aggratio) { #returns intFindPer
    datcurdate<-curdate
    class(datcurdate)<-"Date"
    mo <- findmonth(datcurdate)
    qu <- 1 + as.integer((mo - 1)/3)
    dy <- findday(datcurdate)
    yr <- findyear(datcurdate)
    arinv<- 1/aggratio
    if (unit == "D") intFindPer <- curdate - mind +1 #curdate - mindate + 1
    if (unit == "A" || unit == "O") intFindPer <- as.integer((yr - miny) / aggratio) + 1
    if (unit == "Q") part <- qu
    if (unit == "M") part <- mo
    if (unit == "Q" || unit == "M") intFindPer <- (yr - miny - 1) * arinv + part + (arinv - (minper - 1))
    return(intFindPer)
  } #findper
  ##########################################################################################
  findday<-function(DateVar) {
    z<-as.POSIXlt(DateVar)
    v<-unlist(z)
    findday<-as.integer(v[4])
  } #end findday
  ##########################################################################################
  findmonth<-function(DateVar) {
    z<-as.POSIXlt(DateVar)
    v<-unlist(z)
    findmonth<-as.integer(v[5])+1
  } #end findmonth
  ##########################################################################################
  findyear<<-function(DateVar) {
    z<-as.POSIXlt(DateVar)
    v<-unlist(z)
    findyear<-as.integer(v[6])+1900
  } #end findyear
  ##########################################################################################
  aggregate <- function(varname,date,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) {   #
    #READ A NEW RECORD, CALCULATE PERIOD, AND SET UP AGGREGATION INTO MAT.ISSUE[NPERIODS,NVAR]
    vl<- character(nvar)
    mind<- as.integer(mindate)/86400
    maxd<- as.integer(maxdate)/86400
    vfac<- factor(varname) #make a factor vector
    vlev<- levels(vfac)    #find unique categories
    Mat.Issue<- array(dim=c(nperiods,nvar))

    nrec<-length(varname) #added for R compatibility
    lp<- 0
    per<- 0
    x<- 0
    c<- 0
    nkeep<- 0
    lv<- "0"

    ncases[ncases == 0] <- 1000
    ncases[is.na(ncases)] <- 1000
    # P. English moved ncases overwrite to outside of loop


    for (record in 1:nrec) { # MASTER LOOP THROUGH INPUT DATA, 1 TO NREC
      mo <- findmonth(date[record])
      qu <- 1 + as.integer((mo - 1)/3)
      dy <- findday(date[record])
      yr <- findyear(date[record])

      # P. English added code to force curdate to quartlerly formats for unit==Q
      if(unit=="Q"){

        if(dy > 28){
          dy = 28
        }

        quarter_date <- ISOdate(yr,qu,dy,0,0,0,tz="GMT")
        curdate <- as.integer(quarter_date)/86400

      } else {

        curdate<- as.integer(date[record])

      }


      if (curdate >= mind &  curdate <= maxd) {  #is date within range?
        nkeep <- nkeep + 1
        if (nkeep==1) { #startup routine for first good case
          firstcase<- TRUE
          lp <- findper(unit,curdate=as.integer(date[record]),mind,miny,minper,aggratio) # P. English added code here and in other findper to search for period in original record date
          lv <- varname[record]
          x <- index[record] * ncases[record] #start new sums for case 1
          c <- ncases[record]
          for (i in 1:nvar) {
            if (lv==vlev[i]) v=i #determine v by matching to position of labels vector
          } #end for
        } else {
          firstcase<- FALSE
        } #end if
        if (firstcase == FALSE) { #skip over the rest for first good case
          per<- findper(unit,curdate=as.integer(date[record]),mind,miny,minper,aggratio) #here we translate date into agg category # P. English added code here and in other findper to search for period in original record date
          if ((varname[record] !=  lv) || (per !=lp)) { #found a new period or variable name
            if (lp > 0 &&  lp <= nperiods) {
              Mat.Issue[lp, v] <- x / c #recompute for either period or var change
              x<- 0
              c<- 0
            }

            if (varname[record] !=  lv) { #new var only
              for (i in 1:nvar) {
                if (varname[record]==vlev[i]) v=i #determine v by matching to position of labels vector
              } #end for
              vl[v]<- as.character(varname[record]) #this will only catch names that have good cases - P. English added as.character() fix
              lv<-vl[v]  #reassign new varname to lastvar
            } # new var
            lp <- findper(unit,curdate=as.integer(date[record]),mind,miny,minper,aggratio) # P. English added code here and in other findper to search for period in original record date
            x <- index[record] * ncases[record] #start new sums for current case
            c <- ncases[record]

          } else {
            x<- x + index[record] * ncases[record] #a continuing case, increment sums
            c<- c + ncases[record]
          }

          ## P. English added code to make sure that final record is recorded into the issue matrix
          if(record==nrec){

            Mat.Issue[lp, v] <- x / c #recompute for either period or var change

          }

        } # end of first case special loop
      } #end of date test loop
    } #newrec: next record
    vl<- vlev #overwrite previous assignment which had good names only
    agglist<- list(lab=vl,iss=Mat.Issue)
    return(agglist) #list includes labels and issue matrix
  }

  ##########################################################################################

  esmooth<- function(mood, fb, alpha, increase){
    ##########################################################################################
    smooth<- function(alpha) { #for time series "series" and alpha "alpha[1]" compute sum of squared forecast error
      ferror<- numeric(1)
      T<- length(series)
      xvect<- numeric(T)
      xvect[1] <-  series[1]
      for (t in 2:T) {
        xvect[t] <-  alpha[1] * series[t] + (1 - alpha[1]) * xvect[t - 1]
      }
      sumsq <-  0
      for (t in 3:T) {
        ferror <-  series[t] - xvect[t - 1]
        sumsq <-  sumsq + ferror ^ 2
      }
      return(sumsq) #this is the value of the function for a particular parameter alpha[1]
    } # END OF FUNCTION SMOOTH
    ##########################################################################################

    ## P. English added code to enable heavier smoother
    # also added increase arguement to dominate, esmooth, and extract functions

    if(increase==TRUE){
      smoothf=0.4
    }

    if(increase==FALSE){
      smoothf=1
    }

    series<- mood[fb,] #create series to be smoothed
    sm.out<- optim(c(.75),smooth,method="L-BFGS-B",lower=0.3,upper=smoothf)  #call smoother, with added functionality (PE)
    alpha<- sm.out$par                          #assign result to alpha
    #NOW SMOOTH USING ALPHA
    T<- length(series)
    for (t in 2:T) {
      mood[fb,t] <-  alpha * series[t] + (1 - alpha) * mood[fb,t - 1]
    }
    return(alpha)
  } #END OF FUNCTION ESMOOTH
  ##########################################################################################

  residmi<<- function(issue,v,mood) { #function regresses issue(v) on mood and then residualizes it
    o<- lm(issue[,v] ~ mood[3,]) #regress issue on mood to get a,b
    issue[,v]<- 100 + issue[,v] - (o$coef[1]+o$coef[2]*mood[3,]) #100 + Y - (a+bx)
    return(issue[,v])
  }
  ##########################################################################################
  iscorr<- function(issue,mood) { #compute issue-scale correlations
    Nv<- length(issue[1,])
    Np<- length(issue[,1])
    Rvector<- numeric(Nv)
    for (v in 1:Nv) {
      N<- Np - sum(is.na(issue[,v]))
      if (N > 1) Rvector[v]<- cor(issue[,v],mood[3,],use="complete.obs",method="pearson")
    }
    return(Rvector)
  } #end function iscorr
  ##########################################################################################
  dominate<- function(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha,increase) {
    nitems<- numeric(nperiods)
    if (fb==2) alpha1<-alpha
    if (fb==1) {
      unexp<-numeric(1)
      everlap<- integer(1)
      alpha<- 1
      alpha1<- 1
    }

    if (fb == 1) {
      startper <- 1
      mood[fb, startper] <- 100
      firstj <- 2
      lastj <- nperiods
      stepj <- 1
      jprev <- 1
    } else {
      startper <- nperiods
      mood[fb, startper] <- mood[1, nperiods] #reuse forward metric
      firstj <- nperiods - 1
      lastj <- 1
      stepj <- -1
      jprev <- nperiods
    } #    end if
    for (j in seq(firstj,lastj,by=stepj)) {
      mood[fb, j] <- 0
      everlap <- 0 ## of years which have contributed sums to mood
      if (fb == 1) {
        firstj2 <- 1
        lastj2 <- j - 1
      } else  {
        firstj2 <- j + 1
        lastj2 <- nperiods
      } # end if

      for (j2 in firstj2:lastj2) {
        sum <- 0     #has already been estimated
        consum <- 0  #sum of communalities across issues
        overlap <- 0
        for (v in 1:nvar) {
          xj <- issue[j, v]                      #xj is base year value
          sngx2 <- issue[j2, v]                  #sngx2 is comparison year value
          if (!is.na(xj) && !is.na(sngx2)) {
            overlap <- overlap + 1               #numb of issues contributing to sum
            ratio <- xj / sngx2
            if (csign[v] < 0)  ratio <- 1 / ratio
            sum <- sum + valid[v] * ratio * mood[fb, j2]
            consum <- consum + valid[v]
          } #              end if
        } #next v
        if (overlap > 0) {
          everlap <- everlap + 1
          mood[fb, j] <- mood[fb, j] + sum / consum
        } # end if
      } #next j2
      nitems[j] <- everlap
      if (everlap > 0) mood[fb, j] <- mood[fb, j] / everlap else mood[fb, j] <- mood[fb, jprev] #if undefined, set to lag(mood)
      jprev <- j #last value of j, whether lead or lag
    } #next j
    if (smoothing == TRUE) {
      alpha<- esmooth(mood, fb, alpha, increase)     #NOW SMOOTH USING ALPHA
      mood.sm<- mood[fb,] #set up alternate vector mood.sm
      for (t in 2:nperiods) {
        mood.sm[t]<- alpha*mood[fb,t]+(1-alpha)*mood.sm[t-1]
      } #end for
      mood[fb,]<- mood.sm #now assign back smoothed version
    } else {
      alpha1 <- 1
      alpha <- 1
    }
    if (smoothing == TRUE && fb == 1) alpha1 <- alpha
    dominate.out<- list(alpha1=alpha1,alpha=alpha,latent=mood[fb,]) #output object
    return(dominate.out)
    #  return(mood[fb,])
  } #end dominate algorithm
  ##########################################################################################





  #begindt<-NA #ISOdate(2004,6,1)
  #enddt<-NA #ISOdate(2004,10,31)

  ##########################################################################################
  ## MAIN EXTRACT CODE BEGINS HERE #########################################################
  extract<- function(data,varname,date,index,ncases=NULL,unit="A",mult=1,begindt=NA,enddt=NA,
                     npass=1,smoothing=TRUE,endmonth=12,print=TRUE,log=TRUE,filename="dyad-ratios-log.txt",increase=FALSE) {

    # P.English edits to add data and print calls for bootstrap model

    # P.English edits - add code to sink loadings, dimensionality, and counts into a text file (log and filename)

    ## Added code by P.English - if print is TRUE, then code is as is before

    if(print==TRUE){

      ## Added code P.English - construct single-reference objects from parsed data and variable names as above

      varname <- data[,varname]
      date <- data[,date]
      index <- data[,index]

      formula<-match.call(extract)
      nrecords<- length(varname)
      if (is.null(ncases)) ncases<- rep(0,nrecords) else ncases <- data[,ncases] ### P.English added else condition here
      moddate<- date #create temporary date vector, leaving original unmodified
      if ((unit=="A" || unit=="O") && endmonth<12) {
        for (i in 1:nrecords) { #first loop through raw data file
          month<- findmonth(moddate[i])
          year<- findyear(moddate[i])
          if (month>endmonth) moddate[i]<- ISOdate(year+1,1,1) #modified date become 1/1 of next year
        } #end loop through data
      } # end if

      if (is.na(begindt)) minper<-findmonth(min(moddate)) else minper<-findmonth(begindt)
      if (is.na(begindt)) miny<-findyear(min(moddate)) else miny<-findyear(begindt)
      if (is.na(begindt)) minday<-findday(min(moddate)) else minday<-findday(begindt)
      if (is.na(enddt)) maxper<-findmonth(max(moddate)) else maxper<-findmonth(enddt)
      if (is.na(enddt)) maxy<-findyear(max(moddate)) else maxy<-findyear(enddt)
      if (is.na(enddt)) maxday<-findday(max(moddate)) else maxday<-findday(enddt)
      if (unit=="Q") {
        minper<- as.integer((minper-1)/3)+1
        maxper<- as.integer((maxper-1)/3)+1

        ## P. English added code to force maxday to 28 if higher than 28, or 2nd and 3rd quarters will not generate a curdate
        ## P. English added code to set minday to first in the quarter, or else observations in earlier days but later months of same quarter are omitted

        minday = 1

        if(maxday > 28){
          maxday = 28
        }

      }

      mindate<- ISOdate(miny, minper,minday,0,0,0,tz="GMT")
      maxdate<- ISOdate(maxy, maxper, maxday,0,0,0,tz="GMT") #86400=24*60*60

      #SETCONS:
      latent<- numeric(1)
      aggratio<- 0
      fb<- 1 #initialize
      auto<- "start"  #meaningless value
      alpha<- 1
      alpha1<- 1
      pass<- 1
      holdtola<- 0.001
      tola<- holdtola
      iter<- 0
      lastconv<- 99999
      wtmean<- 0 #for it=1
      wtstd<- 1
      fract<- 1

      if (unit=="A") {
        nperiods<- maxy-miny+1
        aggratio<- 1
        months<- 12
      }
      if (unit=="O") {
        years<- mult
        months<- years*12
        aggratio<- 2
        odd<- (maxy-miny+1) %% mult  #mod
        nperiods=as.integer((maxy-miny)/mult) + odd
      }
      if (unit=="M") {
        fract<- 100
        nperiods<- (maxy-miny)*12
        nperiods<- nperiods-12 + (12-minper+1) + maxper
        aggratio<- 1/12
        months<- 1
      }
      if (unit=="Q") {
        aggratio<- 1/4
        months<- 3
        nperiods<- as.integer((maxy-miny)/aggratio)
        nperiods<- nperiods-4 + (4-minper+1) + maxper
        fract<- 10
      }
      if (unit=="D") {
        months=1
        nperiods<- (as.integer(maxdate)-as.integer(mindate))/86400 + 1 #86400=24*60*60
      }

      arinv<- 1/aggratio
      aggratio<- months/12
      nrecords<- length(index)

      #HERE WE SET UP FUNDAMENTAL DIMENSIONS AND DECLARE VECTORS
      if (fb != 2)  mood<- array(dim=c(3,nperiods))
      vfac<- factor(varname) #make a factor vector
      vlev<- levels(vfac)    #find unique categories
      nvar<- length(vlev)    #how many are there?, includes unusable series
      valid<- numeric(nvar)
      csign<<- numeric(nvar)
      vl<- character(nvar)
      r<- numeric(nvar)
      oldr<- rep(1,nvar) # r=1 for all v initially

      issue<- array(dim=c(nperiods,nvar))
      count<- numeric(nperiods)
      vl<- numeric(nvar)
      period<- numeric(nperiods)
      converge<- 0
      evalue<- 0

      # create numeric variable period, eg, yyyy.0m
      if (unit=="D") {
        period<-seq(1:nperiods)
      } else {
        if (months >= 12) {
          for (l in 1:nperiods) {
            p <- (l - 1) * aggratio
            period[l] <- miny + p
          } #next l
        } else {
          y <- 0
          i <- 0
          my <- miny
          if (minper == 1)  my <- my - 1
          for (l in 1:nperiods) {
            i<- 1 + ((l-1) %% arinv)
            mq <- minper + i - 1
            mq<- 1 + ((mq-1) %% arinv)
            if (mq == 1)  y <- y + 1 #first month or quarter, increment year
            period[l] <- my + y + mq / fract
          } # end for
        } #end else
      } # end if


      agglist<- aggregate(varname,moddate,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) # call aggregate to produce issue matrix
      vl<- agglist$lab #extract two elements of the list from aggregate call
      issue<- agglist$iss
      rm(agglist) #don't need this anymore

      #NOW REDUCE ISSUE MATRIX TO ELIMINATE UNUSABLE SERIES (WN<2)
      ndrop<- 0
      nissue<- numeric(nperiods)
      std<- numeric(nperiods)
      for (v in 1:nvar) {
        std[v]<- 0 #default
        nissue[v]<- sum(!is.na(issue[,v])) #criterion is 2 cases for npass=1 or 3 for npass=2
        if (nissue[v]>npass) std[v]<- sqrt(var(issue[,v],na.rm=TRUE)) #this is just a test for variance >0
        if (std[v]<.001) {  #case dropped if std uncomputable (NA) or actually zero (constant)
          ndrop<- ndrop+1
          print(paste("Series",vl[v],"discarded.  After aggregation cases =",nissue[v]))
        }
      }
      nvarold<- nvar
      nvar<- nvar-ndrop
      pointer<- 1
      found<- FALSE


      for (v in 1:nvar) { #now reduced nvar
        while (found==FALSE && pointer<=nvarold) { #find first valid column and push down
          if (std[pointer]>.001) { #good case, transfer
            issue[,v]<- issue[,pointer]
            vl[v]<- vl[pointer]
            pointer<- pointer+1
            found<- TRUE
          } else {
            pointer<- pointer+1 #bad case, increment pointer
          } #end if
        } #end while
        found<- FALSE
      } #for
      length(vl)<- nvar #reduce
      length(issue)<- nperiods*nvar  #chop off unused columns
      attr(issue,"dim")<- c(nperiods,nvar)
      N<- numeric(nvar)


      ### P. English added code to extract number of observations per time period
      freqs <- as.data.frame(period)

      for(i in 1:nrow(issue)){
        p <- table(is.na(issue[i,]))
        k <- as.numeric(p[1])
        if(k==nvar & is.na(issue[i,1])){
          k <- 0
        }
        freqs$count[i] <- k
      }


      #export<<-list(nperiods,nvar,issue)


      for (pass in 1:npass) { #newpass: RESTART FOR SECOND DIMENSION CASE
        if (pass == 2) { #reset iteration control parameters
          iter <- 0
          tola = holdtola
          lastconv <- 99999
          converge<- lastconv
          conv<- converge
        } else {
          av<- numeric(nvar)
          std<- numeric(nvar)
          #      ngood<- 0
          for (v in 1:nvar) { #compute av and std by issue nvar now reduced to good cases
            wn<- as.integer(nperiods-sum(is.na(issue[,v])))
            av[v] <- mean(issue[,v],na.rm=TRUE)
            std[v]<- sqrt(var(issue[,v],na.rm=TRUE) * ((wn - 1)/wn)) #convert to population standard deviation
            issue[,v]<- 100 + 10 * (issue[,v] - av[v])/std[v]  #standardize
            #        ngood<- ngood+1
          }#end for
        }
        #READY FOR ESTIMATION, SET UP AND PRINT OPTIONS INFO
        out<- as.character(10) #initial length only
        out[1]<- print(paste("Estimation report:"))
        if (pass == 1) {
          if (months >= 12) {
            out[2]<- print(paste("Period:", miny, " to", maxy,"     ", nperiods, " time points"))
          } else {
            out[2]<- print(paste("Period:", miny,  minper, " to", maxy, maxper, nperiods, " time points"))
          }
          out[3]<- print(paste("Number of series: ", nvar+ndrop))
          out[4]<- print(paste("Number of usable series: ", nvar))
          out[5]<- print(paste("Exponential smoothing: ",smoothing))
        }
        out[6]<- print(paste("Iteration history: Dimension ",pass))
        print("")
        out[7]<- ("Iter Convergence Criterion Reliability Alphaf Alphab") # P. English removed print
        outcount<- 7

        for (p in 1:nperiods) {
          count[p]<- sum(!is.na(issue[p,]))
        }
        valid<- rep(1,times=nvar)
        csign<<- rep(1,times=nvar)
        auto <- "y"              #iterative estimation on by default
        quit <- 0                #false implies go ahead and estimate


        ## P.English added code to set up convergence data log object

        converge_log <- matrix(ncol=6)
        converge_log <- as.data.frame(converge_log)
        names(converge_log) <- c("Iteration", "Converge", "Crit", "Reliability", "Forward Smoothing", "Backward Smoothing")

        while (iter == 0 || converge > tola) {   #MASTER CONTROL LOOP WHICH ITERATES UNTIL SOLUTION REACHED

          for (fb in 1:2) { #    MASTER fb LOOP       fb=1 is forward, 2 backward
            dominate.out<- dominate(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha,increase)  #master estimation routine
            alpha1<- dominate.out$alpha1
            alpha<- dominate.out$alpha
            mood[fb,]<- dominate.out$latent
          } #next fb

          fb <- 3 #average mood from here on
          for (p in 1:nperiods) { #    AVERAGE
            mood[fb, p] <- (mood[1, p] + mood[2, p]) / 2
          } #next p
          moodmean<-mean(mood[3,])
          sdmood<-sd(mood[3,])
          for (p in 1:nperiods) {  #PLACEMENT OF THIS LOOP MAY NOT BE RIGHT
            mood[fb,p] <- ((mood[fb,p] - moodmean) * wtstd / sdmood) + wtmean
          } #end for

          #plot commands
          t<- seq(1:nperiods) #time counter used for plot below
          lo<- 50 #force scale of iterative plot to large range
          hi<- 150
          if (min(mood[3,]) < lo) lo=min(mood[3,]) #whichever larger, use
          if (max(mood[3,]) > hi) hi=max(mood[3,])
          dummy<- rep(lo,nperiods) #dummy is fake variable used to set plot y axis to 50,150
          dummy[nperiods]<- hi
          if (iter==0) {
            plot(t,dummy,type="l",lty=0,xlab="Time Period",ylab="Estimate by iteration",main="Estimated Latent Dimension") #create box, no visible lines
          } else {
            lines(t,mood[3,],col=iter)
          }

          iter <- iter + 1

          if (auto == "y") r<- iscorr(issue,mood) else auto <- "y"   #recompute correlations

          wtmean<- 0
          wtstd<- 0
          vsum<- 0
          goodvar<- 0
          converge<- 0 #start off default
          evalue<- 0
          totalvar<- 0

          for (v in 1:nvar) {
            wn<- nperiods-sum(is.na(issue[,v]))
            if (!is.na(sign(r[v]))) csign[v]<<- sign(r[v])
            wn<- nperiods-sum(is.na(issue[,v]))
            if (wn>1) { #sum over variables actually used
              vratio <- wn / nperiods
              evalue <- evalue + vratio * r[v]^2
              totalvar <- totalvar + vratio
            } #end if

            #convergence tests
            if (wn > 3) {
              conv <- abs(r[v] - oldr[v])      #conv is convergence test for item=v
              conv <- conv * (wn / nperiods)                #weight criterion by number of available periods
              if (conv > converge)  converge <- conv        #converge is the global max of conv
            } #end if
            if (!is.na(r[v])) oldr[v] <- r[v]
            if (!is.na(r[v])) valid[v] <- r[v]^2
            if (!is.na(av[v])) wtmean <- wtmean + av[v] * valid[v]
            if (!is.na(std[v])) wtstd <- wtstd + std[v] * valid[v]
            if (!is.na(r[v])) vsum <- vsum + valid[v]
          } #end v loop

          if (vsum > 0)  wtmean <- wtmean / vsum
          if (vsum > 0)  wtstd <- wtstd / vsum
          if (pass == 1) {
            mean1 <- wtmean
            std1 <- wtstd
            e1=evalue
          } else {
            wtmean <- mean1
            wtstd <- std1 #*unexp
          } #end if
          fbcorr <- cor(mood[1,],mood[2,]) #fnfrontback

          if (quit != 1) {
            outcount<- outcount+1
            cv<- format(round(converge,4),nsmall=4)
            itfmt<-format(round(iter),justify="right",length=4)
            out[outcount]<- paste(itfmt,"       ",cv,"   ",round(tola,4),"    ",round(fbcorr,3),round(alpha1,4),round(alpha,4))
          }
          if (converge > lastconv)  tola <- tola * 2
          lastconv <- converge

          auto = "y"  #skip corr on iter=1, set auto on

          # P.English added code to iteratively collect convergence statistics for log
          converge_log[iter,1] <- itfmt
          converge_log[iter,2] <- cv
          converge_log[iter,3] <- round(tola,4)
          converge_log[iter,4] <- round(fbcorr,3)
          converge_log[iter,5] <- round(alpha1,4)
          converge_log[iter,6] <- round(alpha,4)


          if (iter >= 50) break #get out of while loop
        } #END MASTER WHILE ITERATION CONTROL LOOP

        print(converge_log) # P. English added code to print out convergence results as a table

        if (auto == "y" && converge<tola) { #IF WE REACH THIS CODE WE HAVE A FINAL SOLUTION TO BE REPORTED
          if (pass == 1) out1<- out #hold output for 2 dimensional solution
          auto <- "Q"
          quit <- 1                #flag solution reached, last time through
          r<- iscorr(issue,mood)   #final iteration correlations
          if (pass == 1) r1<- r #hold correlations for 2 dimensional solution

          if (pass > 1) {
            unexp <- totalvar
            totalvar <- unexp * totalvar
            evalue <- evalue * unexp
          } #    end if

          if (pass == 1) {
            expprop <- evalue / totalvar
            tot1 <- totalvar
          } else {
            erel <- evalue / totalvar          #% exp relative
            totalvar <- (1 - expprop) * tot1   #true var=original var discounted by %exp
            evalue <- erel * totalvar          #rescale to retain %exp relationship
            expprop <- evalue / tot1           #now reduce eral to expprop
          } #    end if

          for (v in 1:nvar) {
            N[v]<- sum(!is.na(issue[,v]))
          }
          var.out<- list(varname=vl,loadings=r,means=av,std.deviations=std)

          print("")
          outcount<- outcount+1
          out[outcount]<- print(paste("Eigen Estimate ", round(evalue,2), " of possible ",round(tot1,2)))
          outcount<- outcount+1
          out[outcount]<- print(paste("  Percent Variance Explained: ",round(100 * expprop,2)))


          if (pass !=  2 && npass>1) {
            for (v in 1:nvar) {
              valid[v] <- 0               #reset all, regmoodissue will set good=1
              if (csign[v] != 0)  issue[,v]<- residmi(issue,v,mood)   #regmoodissue()
            } #v loop
          }  # if
          #begin prn output routine # mood[fb,] is now our estimate,    WHAT ABOUT A SECOND DIMENSION
          latent<- mood[fb,] #vector holds values for output
          if (pass == 1) latent1<- latent #hold first dimension
          print("")
          out[outcount+1]<- print(paste("Final Weighted Average Metric:  Mean: ",round(wtmean,2)," St. Dev: ",round(wtstd,2)))
          #for Zelig output
          if (npass==1) {
            extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1)
            extract.out[["frequencies"]] <- freqs
            # P. English added frequency table
          } else {
            for (i in 6:outcount) {
              out[i-5]=out[i]
            }
            length(out)<- outcount-5
            extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1,setup2=out,loadings2=r,latent2=latent)
            extract.out[["frequencies"]] <- freqs
            # P. English added frequency table
          }
        } #end if auto="y"
      } #end of for pass=1,2 loop

      par(col=1) #reset on termination
      class(extract.out)<- "Zextract"

      ## P.English added code to sink outputs into log

      loadings <- as.data.frame(extract.out[["varname"]])
      names(loadings) <- "Variable Name"
      loadings$N <- N
      loadings$Mean <- round(av, digits=2)
      loadings$StdDev <- round(std, digits=2)
      loadings$Loading <- round(r1, digits=2)



      extract.out[["Loadings Table"]] <- loadings
      # P. English added loadings object to list
      extract.out[["Eigenvalue estimate"]] <- round(100 * expprop,2)
      # P. English added eigenvalue estimate object to list

      if(log==TRUE){

        sink(filename, append=FALSE)

        print(paste("Estimation report:"))
        if (pass == 1) {
          if (months >= 12) {
            print(paste("Period:", miny, " to", maxy,"     ", nperiods, " time points"))
          } else {
            print(paste("Period:", miny,  minper, " to", maxy, maxper, nperiods, " time points"))
          }
          print(paste("Number of series: ", nvar+ndrop))
          print(paste("Number of usable series: ", nvar))
          print(paste("Exponential smoothing: ",smoothing))
        }
        out[6]<- print(paste("Iteration history: Dimension ",pass))
        print("")

        print(converge_log)

        print("")
        print(paste("Eigen Estimate ", round(evalue,2), " of possible ",round(tot1,2)))
        print(paste("  Percent Variance Explained: ",round(100 * expprop,2)))
        print("")

        print("Variable information:")
        print(loadings)
        print("")

        print("Number of cases per period:")
        print(freqs)

        sink()

      }

      return(extract.out)
    } #end of extract

    ## P. English added code - but if print is false then console and plot returns must be overridden

    if(print==FALSE){


      ## Added code P.English - construct single-reference objects from parsed data and variable names as above

      varname <- data[,varname]
      date <- data[,date]
      index <- data[,index]

      formula<-match.call(extract)
      nrecords<- length(varname)
      if (is.null(ncases)) ncases<- rep(0,nrecords) else ncases <- data[,ncases] ### P.English added else condition here
      moddate<- date #create temporary date vector, leaving original unmodified
      if ((unit=="A" || unit=="O") && endmonth<12) {
        for (i in 1:nrecords) { #first loop through raw data file
          month<- findmonth(moddate[i])
          year<- findyear(moddate[i])
          if (month>endmonth) moddate[i]<- ISOdate(year+1,1,1) #modified date become 1/1 of next year
        } #end loop through data
      } # end if

      if (is.na(begindt)) minper<-findmonth(min(moddate)) else minper<-findmonth(begindt)
      if (is.na(begindt)) miny<-findyear(min(moddate)) else miny<-findyear(begindt)
      if (is.na(begindt)) minday<-findday(min(moddate)) else minday<-findday(begindt)
      if (is.na(enddt)) maxper<-findmonth(max(moddate)) else maxper<-findmonth(enddt)
      if (is.na(enddt)) maxy<-findyear(max(moddate)) else maxy<-findyear(enddt)
      if (is.na(enddt)) maxday<-findday(max(moddate)) else maxday<-findday(enddt)
      if (unit=="Q") {
        minper<- as.integer((minper-1)/3)+1
        maxper<- as.integer((maxper-1)/3)+1

        ## P. English added code to force maxday to 28 if higher than 28, or 2nd and 3rd quarters will not generate a curdate
        ## P. English added code to set minday to first in the quarter, or else observations in earlier days but later months of same quarter are omitted


        minday = 1

        if(maxday > 28){
          maxday = 28
        }

      }
      mindate<- ISOdate(miny,minper,minday,0,0,0,tz="GMT")
      maxdate<- ISOdate(maxy, maxper, maxday,0,0,0,tz="GMT") #86400=24*60*60

      #SETCONS:
      latent<- numeric(1)
      aggratio<- 0
      fb<- 1 #initialize
      auto<- "start"  #meaningless value
      alpha<- 1
      alpha1<- 1
      pass<- 1
      holdtola<- 0.001
      tola<- holdtola
      iter<- 0
      lastconv<- 99999
      wtmean<- 0 #for it=1
      wtstd<- 1
      fract<- 1

      if (unit=="A") {
        nperiods<- maxy-miny+1
        aggratio<- 1
        months<- 12
      }
      if (unit=="O") {
        years<- mult
        months<- years*12
        aggratio<- 2
        odd<- (maxy-miny+1) %% mult  #mod
        nperiods=as.integer((maxy-miny)/mult) + odd
      }
      if (unit=="M") {
        fract<- 100
        nperiods<- (maxy-miny)*12
        nperiods<- nperiods-12 + (12-minper+1) + maxper
        aggratio<- 1/12
        months<- 1
      }
      if (unit=="Q") {
        aggratio<- 1/4
        months<- 3
        nperiods<- as.integer((maxy-miny)/aggratio)
        nperiods<- nperiods-4 + (4-minper+1) + maxper
        fract<- 10
      }
      if (unit=="D") {
        months=1
        nperiods<- (as.integer(maxdate)-as.integer(mindate))/86400 + 1 #86400=24*60*60
      }

      arinv<- 1/aggratio
      aggratio<- months/12
      nrecords<- length(index)

      #HERE WE SET UP FUNDAMENTAL DIMENSIONS AND DECLARE VECTORS
      if (fb != 2)  mood<- array(dim=c(3,nperiods))
      vfac<- factor(varname) #make a factor vector
      vlev<- levels(vfac)    #find unique categories
      nvar<- length(vlev)    #how many are there?, includes unusable series
      valid<- numeric(nvar)
      csign<<- numeric(nvar)
      vl<- character(nvar)
      r<- numeric(nvar)
      oldr<- rep(1,nvar) # r=1 for all v initially

      issue<- array(dim=c(nperiods,nvar))
      count<- numeric(nperiods)
      vl<- numeric(nvar)
      period<- numeric(nperiods)
      converge<- 0
      evalue<- 0

      # create numeric variable period, eg, yyyy.0m
      if (unit=="D") {
        period<-seq(1:nperiods)
      } else {
        if (months >= 12) {
          for (l in 1:nperiods) {
            p <- (l - 1) * aggratio
            period[l] <- miny + p
          } #next l
        } else {
          y <- 0
          i <- 0
          my <- miny
          if (minper == 1)  my <- my - 1
          for (l in 1:nperiods) {
            i<- 1 + ((l-1) %% arinv)
            mq <- minper + i - 1
            mq<- 1 + ((mq-1) %% arinv)
            if (mq == 1)  y <- y + 1 #first month or quarter, increment year
            period[l] <- my + y + mq / fract
          } # end for
        } #end else
      } # end if


      agglist<- aggregate(varname,moddate,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) # call aggregate to produce issue matrix
      vl<- agglist$lab #extract two elements of the list from aggregate call
      issue<- agglist$iss
      rm(agglist) #don't need this anymore

      #NOW REDUCE ISSUE MATRIX TO ELIMINATE UNUSABLE SERIES (WN<2)
      ndrop<- 0
      nissue<- numeric(nperiods)
      std<- numeric(nperiods)
      for (v in 1:nvar) {
        std[v]<- 0 #default
        nissue[v]<- sum(!is.na(issue[,v])) #criterion is 2 cases for npass=1 or 3 for npass=2
        if (nissue[v]>npass) std[v]<- sqrt(var(issue[,v],na.rm=TRUE)) #this is just a test for variance >0
        if (std[v]<.001) {  #case dropped if std uncomputable (NA) or actually zero (constant)
          ndrop<- ndrop+1
          paste("Series",vl[v],"discarded.  After aggregation cases =",nissue[v])
        }
      }
      nvarold<- nvar
      nvar<- nvar-ndrop
      pointer<- 1
      found<- FALSE

      for (v in 1:nvar) { #now reduced nvar
        while (found==FALSE && pointer<=nvarold) { #find first valid column and push down
          if (std[pointer]>.001) { #good case, transfer
            issue[,v]<- issue[,pointer]
            vl[v]<- vl[pointer]
            pointer<- pointer+1
            found<- TRUE
          } else {
            pointer<- pointer+1 #bad case, increment pointer
          } #end if
        } #end while
        found<- FALSE
      } #for
      length(vl)<- nvar #reduce
      length(issue)<- nperiods*nvar  #chop off unused columns
      attr(issue,"dim")<- c(nperiods,nvar)
      N<- numeric(nvar)




      ### P. English added code to extract number of observations per time period
      freqs <- as.data.frame(period)

      for(i in 1:nrow(issue)){
        p <- table(is.na(issue[i,]))
        k <- as.numeric(p[1])
        if(k==nvar & is.na(issue[i,1])){
          k <- 0
        }
        freqs$count[i] <- k
      }

      #export<<-list(nperiods,nvar,issue)


      for (pass in 1:npass) { #newpass: RESTART FOR SECOND DIMENSION CASE
        if (pass == 2) { #reset iteration control parameters
          iter <- 0
          tola = holdtola
          lastconv <- 99999
          converge<- lastconv
          conv<- converge
        } else {
          av<- numeric(nvar)
          std<- numeric(nvar)
          #      ngood<- 0
          for (v in 1:nvar) { #compute av and std by issue nvar now reduced to good cases
            wn<- as.integer(nperiods-sum(is.na(issue[,v])))
            av[v] <- mean(issue[,v],na.rm=TRUE)
            std[v]<- sqrt(var(issue[,v],na.rm=TRUE) * ((wn - 1)/wn)) #convert to population standard deviation
            issue[,v]<- 100 + 10 * (issue[,v] - av[v])/std[v]  #standardize
            #        ngood<- ngood+1
          }#end for
        }
        #READY FOR ESTIMATION, SET UP AND PRINT OPTIONS INFO
        out<- as.character(10) #initial length only
        out[1]<- paste("Estimation report:")
        if (pass == 1) {
          if (months >= 12) {
            out[2]<- paste("Period:", miny, " to", maxy,"     ", nperiods, " time points")
          } else {
            out[2]<- paste("Period:", miny,  minper, " to", maxy, maxper, nperiods, " time points")
          }
          out[3]<- paste("Number of series: ", nvar+ndrop)
          out[4]<- paste("Number of usable series: ", nvar)
          out[5]<- paste("Exponential smoothing: ",smoothing)
        }
        out[6]<- paste("Iteration history: Dimension ",pass)
        out[7]<- "Iter Convergence Criterion Reliability Alphaf Alphab"
        outcount<- 7

        for (p in 1:nperiods) {
          count[p]<- sum(!is.na(issue[p,]))
        }
        valid<- rep(1,times=nvar)
        csign<<- rep(1,times=nvar)
        auto <- "y"              #iterative estimation on by default
        quit <- 0                #false implies go ahead and estimate

        # P. English added code for convergence log

        converge_log <- matrix(ncol=6)
        converge_log <- as.data.frame(converge_log)
        names(converge_log) <- c("Iteration", "Converge", "Crit", "Reliability", "Forward Smoothing", "Backward Smoothing")


        while (iter == 0 || converge > tola) {   #MASTER CONTROL LOOP WHICH ITERATES UNTIL SOLUTION REACHED

          for (fb in 1:2) { #    MASTER fb LOOP       fb=1 is forward, 2 backward
            dominate.out<- dominate(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha,increase)  #master estimation routine
            alpha1<- dominate.out$alpha1
            alpha<- dominate.out$alpha
            mood[fb,]<- dominate.out$latent
          } #next fb

          fb <- 3 #average mood from here on
          for (p in 1:nperiods) { #    AVERAGE
            mood[fb, p] <- (mood[1, p] + mood[2, p]) / 2
          } #next p
          moodmean<-mean(mood[3,])
          sdmood<-sd(mood[3,])
          for (p in 1:nperiods) {  #PLACEMENT OF THIS LOOP MAY NOT BE RIGHT
            mood[fb,p] <- ((mood[fb,p] - moodmean) * wtstd / sdmood) + wtmean
          } #end for

          ## P. English - Plot commands removed

          iter <- iter + 1
          if (auto == "y") r<- iscorr(issue,mood) else auto <- "y"   #recompute correlations

          wtmean<- 0
          wtstd<- 0
          vsum<- 0
          goodvar<- 0
          converge<- 0 #start off default
          evalue<- 0
          totalvar<- 0

          for (v in 1:nvar) {
            wn<- nperiods-sum(is.na(issue[,v]))
            if (!is.na(sign(r[v]))) csign[v]<<- sign(r[v])
            wn<- nperiods-sum(is.na(issue[,v]))
            if (wn>1) { #sum over variables actually used
              vratio <- wn / nperiods
              evalue <- evalue + vratio * r[v]^2
              totalvar <- totalvar + vratio
            } #end if

            #convergence tests
            if (wn > 3) {
              conv <- abs(r[v] - oldr[v])      #conv is convergence test for item=v
              conv <- conv * (wn / nperiods)                #weight criterion by number of available periods
              if (conv > converge)  converge <- conv        #converge is the global max of conv
            } #end if
            if (!is.na(r[v])) oldr[v] <- r[v]
            if (!is.na(r[v])) valid[v] <- r[v]^2
            if (!is.na(av[v])) wtmean <- wtmean + av[v] * valid[v]
            if (!is.na(std[v])) wtstd <- wtstd + std[v] * valid[v]
            if (!is.na(r[v])) vsum <- vsum + valid[v]
          } #end v loop

          if (vsum > 0)  wtmean <- wtmean / vsum
          if (vsum > 0)  wtstd <- wtstd / vsum
          if (pass == 1) {
            mean1 <- wtmean
            std1 <- wtstd
            e1=evalue
          } else {
            wtmean <- mean1
            wtstd <- std1 #*unexp
          } #end if
          fbcorr <- cor(mood[1,],mood[2,]) #fnfrontback

          if (quit != 1) {
            outcount<- outcount+1
            cv<- format(round(converge,4),nsmall=4)
            itfmt<-format(round(iter),justify="right",length=4)
            out[outcount]<- paste(itfmt,"       ",cv,"   ",round(tola,4),"    ",round(fbcorr,3),round(alpha1,4),round(alpha,4))
          }
          if (converge > lastconv)  tola <- tola * 2
          lastconv <- converge

          auto = "y"  #skip corr on iter=1, set auto on

          # P.English added code to iteratively collect convergence statistics for log
          converge_log[iter,1] <- itfmt
          converge_log[iter,2] <- cv
          converge_log[iter,3] <- round(tola,4)
          converge_log[iter,4] <- round(fbcorr,3)
          converge_log[iter,5] <- round(alpha1,4)
          converge_log[iter,6] <- round(alpha,4)


          if (iter >= 50) break #get out of while loop
        } #END MASTER WHILE ITERATION CONTROL LOOP

        if (auto == "y" && converge<tola) { #IF WE REACH THIS CODE WE HAVE A FINAL SOLUTION TO BE REPORTED
          if (pass == 1) out1<- out #hold output for 2 dimensional solution
          auto <- "Q"
          quit <- 1                #flag solution reached, last time through
          r<- iscorr(issue,mood)   #final iteration correlations
          if (pass == 1) r1<- r #hold correlations for 2 dimensional solution

          if (pass > 1) {
            unexp <- totalvar
            totalvar <- unexp * totalvar
            evalue <- evalue * unexp
          } #    end if

          if (pass == 1) {
            expprop <- evalue / totalvar
            tot1 <- totalvar
          } else {
            erel <- evalue / totalvar          #% exp relative
            totalvar <- (1 - expprop) * tot1   #true var=original var discounted by %exp
            evalue <- erel * totalvar          #rescale to retain %exp relationship
            expprop <- evalue / tot1           #now reduce eral to expprop
          } #    end if

          for (v in 1:nvar) {
            N[v]<- sum(!is.na(issue[,v]))
          }
          var.out<- list(varname=vl,loadings=r,means=av,std.deviations=std)

          outcount<- outcount+1
          out[outcount]<- paste("Eigen Estimate ", round(evalue,2), " of possible ",round(tot1,2))
          outcount<- outcount+1
          out[outcount]<- paste("  Percent Variance Explained: ",round(100 * expprop,2))


          if (pass !=  2 && npass>1) {
            for (v in 1:nvar) {
              valid[v] <- 0               #reset all, regmoodissue will set good=1
              if (csign[v] != 0)  issue[,v]<- residmi(issue,v,mood)   #regmoodissue()
            } #v loop
          }  # if
          #begin prn output routine # mood[fb,] is now our estimate,    WHAT ABOUT A SECOND DIMENSION
          latent<- mood[fb,] #vector holds values for output
          if (pass == 1) latent1<- latent #hold first dimension
          out[outcount+1]<- paste("Final Weighted Average Metric:  Mean: ",round(wtmean,2)," St. Dev: ",round(wtstd,2))
          #for Zelig output
          if (npass==1) {
            extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1)
            extract.out[["frequencies"]] <- freqs
            # P. English added frequency table
          } else {
            for (i in 6:outcount) {
              out[i-5]=out[i]
            }
            length(out)<- outcount-5
            extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1,setup2=out,loadings2=r,latent2=latent)
            extract.out[["frequencies"]] <- freqs
            # P. English added frequency table
          }
        } #end if auto="y"
      } #end of for pass=1,2 loop

      par(col=1) #reset on termination
      class(extract.out)<- "Zextract"


      ## P.English added code to collect loadings information into a data frame for log

      loadings <- as.data.frame(extract.out[["varname"]])
      names(loadings) <- "Variable Name"
      loadings$N <- N
      loadings$Mean <- round(av, digits=2)
      loadings$StdDev <- round(std, digits=2)
      loadings$Loading <- round(r1, digits=2)



      extract.out[["Loadings Table"]] <- loadings
      # P. English added loadings object to list
      extract.out[["Eigenvalue estimate"]] <- round(100 * expprop,2)
      # P. English added eigenvalue estimate object to list

      if(log==TRUE){

        sink(filename, append=FALSE)

        print(paste("Estimation report:"))
        if (pass == 1) {
          if (months >= 12) {
            print(paste("Period:", miny, " to", maxy,"     ", nperiods, " time points"))
          } else {
            print(paste("Period:", miny,  minper, " to", maxy, maxper, nperiods, " time points"))
          }
          print(paste("Number of series: ", nvar+ndrop))
          print(paste("Number of usable series: ", nvar))
          print(paste("Exponential smoothing: ",smoothing))
        }
        out[6]<- print(paste("Iteration history: Dimension ",pass))
        print("")

        print(converge_log)

        print("")
        print(paste("Eigen Estimate ", round(evalue,2), " of possible ",round(tot1,2)))
        print(paste("  Percent Variance Explained: ",round(100 * expprop,2)))
        print("")

        print("Variable information:")
        print(loadings)
        print("")

        print("Number of cases per period:")
        print(freqs)

        sink()

      }

      return(extract.out)
    } #end of extract

  }



