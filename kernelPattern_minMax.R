rm(list = ls())

library("np")
library("quantmod")
library("TTR")
library("parallel")

current.date <- Sys.Date()

generateContenders <- function(allBackDated = FALSE, daysBack = 300, numDaysPast = 10){
    ## allBackDated is a boolean to check if all patterns are required
    ## daysBack takes of no. of days back in history to start from
    ## noDaysPast valid if allBackDated = FALSE and denotes no. of
    ## days prior to current date when pattern was finised

    start.date <- current.date - daysBack
                                        # getting price volume information
    symbols.list <- read.csv("DATA/bseSymbols.csv")[ ,2]
    symbols.list <- paste0(symbols.list, ".BO")

    ## system.time(price.data <- mclapply(symbols.list, function(x){
    ##                            tryCatch(
    ##                                getSymbols(x, env = NULL,
    ##                                           from = start.date),
    ##                                error = function(e) NULL)
    ##                        }, mc.cores = 3))

    ## names(price.data) <- symbols.list
    ## price.data <- price.data[!sapply(price.data, is.null)]
    ## save(price.data, file = "prices.Rda")

    load("prices.Rda")
                                        # searching for patterns in each stock
    contenders <- mclapply(price.data, function(price){
                               tryCatch({
                                   data <- price
                                   company.name <- strsplit(colnames(data)[1], "\\.")[[1]][1]
                                   print(company.name)
                                   colnames(data) <- c("open", "high", "low", "close",
                                                       "volume", "adjusted")
                                   
                                   data <- data[paste0(start.date, "/", current.date)]
                                   
                                   data <- as.data.frame(data)
                                   data$date.no <- order(rownames(data))
                                   data$date <- as.Date(rownames(data))

                                   data$xopen <- data$open
                                   data$xhigh <- data$high
                                   data$xlow <- data$low
                                   data$xclose <- data$close
                                   
                                   for (i in 2:nrow(data)){
                                       data$xopen[i] <- (data$xopen[i-1] + data$close[i-1])/2
                                       data$xclose[i] <- (data$open[i] + data$high[i] + data$low[i] + data$close[i])/4
                                       data$xhigh[i] <- max(data$high[i], data$xopen[i], data$xclose[i])
                                       data$xlow[i] <- min(data$low[i], data$xopen[i], data$xclose[i])
                                   }

                                   
                                   model.np <- npreg(xclose ~ date.no, regtype = "ll", bws = 3,
                                                     gradients = TRUE, data = data)
                                   
                                   ## summary(model.np)
                                   ## npsigtest(model.np)
                                   
                                   data$fitted <- fitted(model.np)
                                   data$rpv <- NA
                                   data$rpv[-1] <- diff(fitted(model.np)) * data$volume[-1]
                                   
                                   data$ma.volume <- SMA(data$volume, 50)
                                   data$ma.rpv <- SMA(data$rpv, 50)
                                   
                                        # setting the min and max locations
                                   data$max.loc <- NA
                                   data$min.loc <- NA
                                   data$max.loc[which(diff(sign(diff(data$fitted))) == -2) + 1] <- 1
                                   data$min.loc[which(diff(sign(diff(data$fitted))) == 2) + 1] <- 1
                                   
                                   max.loc <- which(data$max.loc == 1) 
                                   min.loc <- which(data$min.loc == 1)

                                   max.loc <- max.loc[max.loc > 50]
                                   min.loc <- min.loc[min.loc > 50]

                                   good.points <- list()
                                   i <- 1

                                   for(a in max.loc){
                                       
                                       k <- 20
                                       while(k > 5){
                                           dat <- data[data$date >= data$date[a-k], ]

                                           d.k <- data$date[a-k]
                                           p.k <- dat[dat$date == d.k, "fitted"]

                                           d.a <- data$date[a]
                                           p.a <- dat[dat$date == d.a, "fitted"]
                                           m.price <- max(data[data$date < d.a, "fitted"])
                                           

                                           uprv1 <- abs(mean(dat[dat$date %in% d.k:d.a & dat$rpv > 0,
                                                                 "rpv"],
                                                             na.rm = TRUE))
                                           dprv1 <- abs(mean(dat[dat$date %in% d.k:d.a & dat$rpv <= 0,
                                                                 "rpv"],
                                                             na.rm = TRUE))

                                           if(is.na(dprv1) | dprv1 == 0)
                                               dprv1 <- abs(mean(data[data$date %in% data$date[a - 50]:d.a, "rpv"],
                                                                 na.rm = TRUE))

                                           alpha1 <- uprv1/dprv1
                                           delta <- p.a/p.k
                                           
                                           if(delta >= 1.15 & alpha1 > 1 & p.a > 30 &
                                              p.a > m.price){
                                               min.loc1 <- min.loc[which(min.loc > a)]
                                               
                                               for(b in min.loc1){
                                                   d.b <- data$date[b]
                                                   p.b <- data[data$date == d.b, "fitted"]
                                                   
                                                   dat <- data[data$date %in% d.a:d.b, ]
                                                   avg.vol <- mean(dat$volume, na.rm = TRUE)
                                                   avg.ma.vol <- mean(dat$ma.volume, na.rm = TRUE)

                                                   dur.cond1 <- ((d.b - d.a) < 60) & ((d.b - d.a) > 20)
                                                   
                                                   if(p.b < 0.85*p.a & p.b >= 0.65*p.a &
                                                      avg.vol < avg.ma.vol &
                                                      dur.cond1){

                                                       max.loc1 <- max.loc[which(max.loc > b)]
                                                       
                                                       for(c in max.loc1){
                                                           d.c <- data$date[c]
                                                           p.c <- data[data$date == d.c, "fitted"]
                                                           
                                                           dat <- data[data$date %in% d.b:d.c, ]
                                                           uprv2 <- abs(mean(dat[dat$rpv > 0, "rpv"], na.rm = TRUE))
                                                           dprv2 <- abs(mean(dat[dat$rpv <= 0, "rpv"], na.rm = TRUE))
                                                           
                                                           if(is.na(dprv2) | dprv2 == 0)
                                                               dprv2 <- abs(mean(data[data$date %in% data$date[c - 50]:d.c,
                                                                                      "rpv"], na.rm = TRUE))
                                                           
                                                           alpha2 <- uprv2/dprv2
                                                           dur.cond2 <- ((d.c - d.b) > 3) & ((d.c - d.b) < 30) 
                                                           
                                                           if(p.c > 0.6*p.a + 0.4*p.b &
                                                              alpha2 > 1 &
                                                              dur.cond2 & p.c <= p.a){

                                                               min.loc2 <- min.loc[which(min.loc > c)]
                                                               for(d in min.loc2){
                                                                   d.d <- data$date[d]
                                                                   p.d <- data[data$date == d.d, "fitted"]

                                                                   dat <- data[data$date %in% d.c:d.d, ]
                                                                   uprv3 <- abs(mean(dat[dat$rpv > 0, "rpv"], na.rm = TRUE))
                                                                   dprv3 <- abs(mean(dat[dat$rpv <= 0, "rpv"], na.rm = TRUE))

                                                                   if(is.na(dprv3) | dprv3 == 0)
                                                                       dprv3 <- abs(mean(data[data$date %in%
                                                                                                  data$date[d - 50]:d.d, "rpv"],
                                                                                         na.rm = TRUE))

                                                                   beta <- uprv2/dprv3
                                                                   dur.cond3 <- ((d.d - d.c) < 20) &
                                                                       ((d.d - d.c) > 3)

                                                                   price.cond <- p.d <= p.c 
                                                                   
                                                                   if(price.cond & beta > 1 & dur.cond3 & (current.date - d.d < numDaysPast)){
                                                                       gamma <- log(alpha2) + log(beta) + delta
                                                                       
                                                                       good.points[[i]] <- data.frame(d.k, d.a,
                                                                                                      d.b, d.c, d.d,
                                                                                                      gamma)
                                                                       i <- i + 1
                                                                   }
                                                                   else
                                                                       next
                                                               }
                                                           }
                                                           else
                                                               next
                                                       }
                                                   }
                                                   else
                                                       next
                                               }
                                           }
                                           k <- k - 1
                                       }
                                   }

                                   good.points <- do.call(rbind, good.points)
                                   
                                   if(!is.null(good.points)){
                                        # find the best points with highest gamma

                                       best.point <- c(good.points[which(good.points$gamma == max(good.points$gamma)), ])
                                       if(best.point$gamma > 5){
                                           pdf(paste0("PLOTS/", company.name,
                                                      ".pdf"))
                                           par(mfrow = c(2, 1))
                                           plot(data$date, data$xclose, cex = .1,
                                                xlab = "date", ylab = "price",
                                                type = "p", xaxt = "n",
                                                main = company.name)
                                           axis.Date(side = 1, data$date,
                                                     format = "%d/%m/%Y")
                                           lines(data$date, fitted(model.np),
                                                 type = "l", col = "blue")
                                           abline(v = best.point[1:5])
                                        #                                       abline(v = data$date[max.loc], col = "green")
                                        #                                       abline(v = data$date[min.loc], col = "pink")
                                           barplot(data$volume, ylab = "volume",
                                                   xlab = "date")
                                           dev.off()
                                           return(good.points)
                                       }
                                       else
                                           return(NULL)
                                   }
                                   else
                                       return(NULL)
                               }, error = function(e) NULL)
                           }, mc.cores = 3)

    save(contenders, file = "contenders.Rda")
}
