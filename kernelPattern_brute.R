rm(list = ls())

library("np")
library("quantmod")
library("TTR")
library("parallel")

price <- getSymbols("SHILGRAVQ.BO", env = NULL)
save(price, file = "data.rda")

load("data.rda")
data <- price
colnames(data) <- c("open", "high", "low", "close", "volume", "adjusted")

data <- data["2015-05-10/2015-12-31"]

data <- as.data.frame(data)
data$date <- order(rownames(data))
 
model.np <- npreg(close ~ date, regtype = "ll", bws = 2,
                  gradients = TRUE, data = data)

summary(model.np)
npsigtest(model.np)

data$fitted <- fitted(model.np)
data$rpv <- NA
data$rpv[-1] <- diff(fitted(model.np)) * data$volume

data$ma.volume <- SMA(data$volume, 50)
data$ma.rpv <- SMA(data$rpv, 50)

T = 0 # this parameter is used for the rolling window
good.stock = FALSE

i <- 1
t <- 51

good.points <- list()

while(t < (nrow(data)-T)){
    cat("t = ", t, "\n")
    dat <- data[data$date >= t, ]
    
    
    d.k <- dat[dat$date == t, "date"]
    p.k <- dat[dat$date == t, "fitted"]
    
    ## search for region K to A
    k <- 25
    while(k > 15){
        cat("Searching SETUP with width =", k, "\n")
        
        datA <- dat[dat$date < d.k + k, ]
        
        ## first find absolute maxima point A
        d.a <- datA[datA$fitted == max(datA$fitted), "date"]
        p.a <- datA[datA$fitted == max(datA$fitted), "fitted"]
        
        uprv1 <- abs(mean(datA[datA$date %in% d.k:d.a & datA$rpv > 0,
                               "rpv"],
                          na.rm = TRUE))
        dprv1 <- abs(mean(datA[datA$date %in% d.k:d.a & datA$rpv <= 0,
                               "rpv"],
                          na.rm = TRUE))

        if(is.na(dprv1))
            dprv1 <- mean(datA$ma.rpv, na.rm = TRUE)
        
        alpha1 <- uprv1/dprv1        
        delta <- p.a/p.k
        
        if(delta > 1 & alpha1 > 1){
            
            cat("Okay good setup! Let's move on now... \n")
            
            ## search for region A to B
            a <- 40
            while(a > 10){
                
                cat("Let's search for LEFT SIDE CUP with width =", a, "\n")
                
                datB <- dat[dat$date < d.a + a & dat$date > d.a, ]                            
                p.b <- datB[datB$fitted == min(datB$fitted), "fitted"]
                d.b <- datB[datB$fitted == min(datB$fitted), "date"]
                avg.vol <- mean(datB$volume, na.rm = TRUE)
                avg.ma.vol <- mean(datB$ma.volume, na.rm = TRUE)

                if(p.b < p.a & avg.vol < avg.ma.vol){
                    
                    cat("Voila! You found the bottom, it's all uphill from here...\n")
                    
                    ## search for region B to C
                    b <- a
                    while(b > round(a/3)){
                        
                        cat("Let's search for RIGHT SIDE CUP with width =", b, "\n")
                        datC <- dat[dat$date > d.b & dat$date <= d.b + b, ]
                        p.c <- datC[datC$fitted == max(datC$fitted), "fitted"]
                        d.c <- datC[datC$fitted == max(datC$fitted), "date"]
                        
                        uprv2 <- abs(mean(datC[datC$rpv > 0, "rpv"], na.rm = TRUE))
                        dprv2 <- abs(mean(datC[datC$rpv <= 0, "rpv"], na.rm = TRUE))

                        if(is.na(dprv2))
                            dprv2 <- mean(datC$ma.rpv, na.rm = TRUE)
                        
                        alpha2 <- uprv2/dprv2
                        
                        if(p.c > p.b & alpha2 > 1){
                            
                            cat("Almost there... be patient now! :D \n")
                            
                            ## search for region C to D
                            c <- b/2
                            while(c > round(b/4)){
                                cat("Let's search for the handle now with width =", c, "\n")
                                cat(t, " ", k, " ", a, " ", b, " ", c, "\n")
                                
                                datD <- dat[dat$date > d.c & dat$date <= d.c + c, ]
                                p.d <- datD[datD$fitted == min(datD$fitted), "fitted"]
                                d.d <- datD[datD$fitted == min(datD$fitted), "date"]
                                
                                uprv3 <- abs(mean(datD[datD$rpv > 0, "rpv"], na.rm = TRUE))
                                dprv3 <- abs(mean(datD[datD$rpv <= 0 , "rpv"], na.rm = TRUE))

                                if(is.na(dprv3))
                                    dprv3 <- mean(datD$ma.rpv, na.rm = TRUE)
                                
                                beta <- uprv2/dprv3
                                
                                if(p.d <= p.c & (p.d > 0.8*p.c + 0.2*p.b) &
                                   beta > 1){
                                    if(p.c <= p.a & p.d > p.b){
                                        good.stock <- TRUE
                                        gamma <- log(alpha2) + log(beta) + delta

                                        good.points[[i]] <- data.frame(d.k, d.a,
                                                                       d.b, d.c, d.d,
                                                                       gamma)
                                        cat("Hurrah! Got ", i, " hits! \n")

                                        k <- 15
                                        a <- 10
                                        b <- round(a/3)
                                        c <- round(b/4)
                                        i <- i + 1
                                        t <- t + 15
                                    }
                                }
                                c <- c - 1
                            }
                        }
                        b <- b - 1
                    }
                }
                a <- a - 1
            }
        }
        k <- k - 1
    }
    t <- t + 1
}

good.points <- do.call(rbind, good.points)

                                        # find the best points with highest gamma
best.point <- c(good.points[which(good.points$gamma == max(good.points$gamma)), ])

                                        # plot price-volume along with algorithm search points
par(mfrow = c(2, 1))
plot(data$date, data$close, cex = .1, xlab = "date", ylab = "price")
lines(data$date, fitted(model.np), lty = 1, col = "blue")
abline(v = best.point[1:5])
barplot(data$volume, ylab = "volume", xlab = "date")

