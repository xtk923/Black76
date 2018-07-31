#' A function that updates call price according to black 76 formula
#' @param Vol Volatility, in the context of this package, it is a trial value
#' @param F Future price of the option
#' @param K Strike price of the option
#' @param T Time to expiry in year
#' @param r Risk free interst rate
#' @return c: The call price
#' @export
callPrice = function(Vol, F, K, T, r){
    d1 = (log(x = F/K, base = exp(1)) + (Vol^2/2)* T)/(Vol * sqrt(T)) # Calculate d1 and d2 according to Black formula
    d2 = d1 - Vol*sqrt(T)
    c = exp(-r * T) * (F * pnorm(d1) - K * pnorm(d2)) # Calculate call price
    return(c)
}

#' A function that updates put price accoridng to black 76 formula
#' @param Vol Volatility, in the context of this package, it is a trial value
#' @param F Future price of the option
#' @param K Strike price of the option
#' @param T Time to expiry in year
#' @param r Risk free interst rate
#' @return p: The put price
#' @export
putPrice = function(Vol, F, K, T, r){
    d1 = (log(x = F/K, base = exp(1)) + (Vol^2/2)* T)/(Vol * sqrt(T)) # Calculate d1 and d2 according to Black formula
    d2 = d1 - Vol*sqrt(T)
    p = exp(-r * T) * (K * pnorm(-d2) - F * pnorm(-d1)) # Calculate put price
    return(p)
}


#' A function that calculate the implied volatility from a data frame given
#' @param df The data frame containing information of your options. It must follow the format
#' df  = data.frame(
#'                  strike = c(50, 20),
#'                  type = c("C", "P"),
#'                  optionPrice = c(1.62, 0.01),
#'                  futurePrice = c(48.03, 48.03),
#'                  time_to_expiry = c(0.1423, 0.1423)
#'                  )
#' @param r = 0.03 The default risk free interest rate is set to 3 percents. You may alter it if necessary
#' @return vols: A vector containing implied votalities, in the order of input data frame, df
#' @export
calImpliedVol = function(df, r = 0.03){
    ## Since option price increases monotonically with volatility, we
    ## solve volatility by iterative method

    ## Check input data frame format
    if(is.null(df$futurePrice) || is.null(df$strike) || is.null(df$time_to_expiry) || is.null(df$type) || is.null(df$optionPrice)){
        print("Headers of data frame read unsuccessfully. Please check your input format.")
        return(NULL)
        break
    }

    vols = c()                          # Create a vector to store the volatility values
    for(i in 1:dim(df)[1]){             # Calculate row by row
        ## Extract parameters from dataframe
        F = df[i,]$futurePrice          # Future price
        K = df[i,]$strike               # Strike
        T = df[i,]$time_to_expiry       # Time to expiry in year
        type = df[i,]$type              # Type of option
        price = df[i,]$optionPrice      # Option price

        ePrice = 0   # estimated price
        j = 1        # counter
        VolL = 0.001 # arbitrary very small left boundary for volatility
        VolR = 100   # arbitrary vary large right boundary for valatility

        while(abs(price - ePrice) > 0.001 && j <= 100){
            ## Iterate untill the prices are essentially equal.
            ## Upper limit set to 100 iterations, to avoid
            ## possible crash. This part follows standard
            ## algorithm for searching in monotonic function
            Vol = mean(c(VolL, VolR))
            if(type == "C"){
                if(callPrice(Vol, F, K, T, r) > price){
                    VolR = Vol
                }else if(callPrice(Vol, F, K, T, r) < price){
                    VolL = Vol
                }
                ePrice = callPrice(Vol, F, K, T, r)
            }
            else if(type == "P"){
                if(putPrice(Vol, F, K, T, r) > price){
                    VolR = Vol
                }else if(putPrice(Vol, F, K, T, r) < price){
                    VolL = Vol
                }
                ePrice = putPrice(Vol, F, K, T, r)
            }
            else{
                print("WARNING! Check your input data frame!\n")
            }
            j = j + 1
            if(j == 100){
                cat("WARNING! Volatility for ", i,
                    "th stock with strike", K, "type", type,
                    "option price", price, "future price", F, "time to expiry", T,
                    "is not evaluated precisely!\n")
            }
        }
        vols = c(vols, Vol) # Store the value of implied Vol in the vector
    }

    return(vols)
}

#' A function that plots the implied volatility from a data frame
#' given
#' @param df The data frame containing information of your options. It
#'     must follow the format df = data.frame( strike = c(50, 20),
#'     type = c("C", "P"), optionPrice = c(1.62, 0.01), futurePrice =
#'     c(48.03, 48.03), time_to_expiry = c(0.1423, 0.1423) )
#' @param r = 0.03 The default risk free interest rate is set to 3
#'     percents. You may alter it if necessary
#' @param labelPoints = FALSE By default, point labels are not
#'     included. If set to TRUE, a string in the order of
#'     "strike, type, optionPrice, futurePrice, time_to_expiry" will
#'     be attached to each point.
#' @return vols: A vector containing implied votalities, in the order
#'     of input data frame, df
#' @export
plotImpliedVol = function(df, r = 0.03, labelPoints = FALSE){
    ## calculate the implied volatility
    vols = calImpliedVol(df, r)


    if(length(vols) == 0){
        print("No data to plot.")
        return(NULL)
    }


    ## This part is devoted to plotting results
    colors = c()                        # Create a color map based on option type
    shapes = c()
    for(type in df$type){
        if(type == "C"){
            colors = c(colors, "green")
            shapes = c(shapes, 1)
        }
        else if(type == "P"){
            colors = c(colors, "red")
            shapes = c(shapes, 2)
        }
    }

    if(labelPoints){
        df$pointLabels = with(df,c(paste(strike, type, optionPrice, futurePrice, time_to_expiry)))
    }
    ## Bind volatility data to original data frame
    ## Create two data frames for call options and put options
    df$Vols = vols
    df$strikeOverFuturePrice = df$strike/df$futurePrice
    dfCall = df[df$type == "C",]
    dfPut = df[df$type == "P",]

    m = matrix(c(1,1,1,2,3,3,4,4,5,5,6,6), 3, 4, byrow = T)
    layout(m) # Set the layout of plot

    ## Plot the implied volatility against ratio between strike and future price
    with(df, plot(strike/futurePrice, Vols, col=colors, pch = shapes,
                  main="Implied Votality vs Strike-Future Price ratio",
                  xlab = "Strike / Future Price",
                  ylab = "Implied Votality",
                  xlim = c(min(strike/futurePrice) * 0.8, max(strike/futurePrice) * 1.1),
                  ylim = c(min(Vols) * 0.8, max(Vols) * 1.1)))
    if(dim(dfCall)[1] >= 2){try(abline(lm(Vols ~ strikeOverFuturePrice, data = dfCall), col="green"))}
    if(dim(dfPut)[1] >= 2){try(abline(lm(Vols ~ strikeOverFuturePrice, data = dfPut), col = "red"))}
    if(labelPoints){with(df, text(strike/futurePrice, Vols, labels = pointLabels, pos = 3, cex = 0.7))}
    ## a legend
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend("center", legend = c("Call", "Put"), pch=c(1, 2), col = c("green", "red"))

    ## Plot the implied votality against the strike
    with(df, plot(strike, Vols, col= colors, pch = shapes, main="Implied Votality vs Strike", xlab = "Strike",
                  ylab = "Implied Votality",
                  xlim = c(min(strike) * 0.8, max(strike) * 1.1),
                  ylim = c(min(Vols) * 0.8, max(Vols) * 1.1)))
    if(dim(dfCall)[1] >= 2){try(abline(lm(Vols ~ strike, data = dfCall), col="green"))}
    if(dim(dfPut)[1] >= 2){try(abline(lm(Vols ~ strike, data = dfPut), col = "red"))}
    if(labelPoints){with(df, text(strike, Vols, labels = pointLabels, pos = 3, cex = 0.7))}

    ## Plot the implied votality against the future price
    with(df, plot(futurePrice, Vols, col= colors, pch = shapes, main="Implied Votality vs Future Price", xlab = "Future Price",
                  ylab = "Implied Votality",
                  xlim = c(min(futurePrice) * 0.8, max(futurePrice) * 1.1),
                  ylim = c(min(Vols) * 0.8, max(Vols) * 1.1)))
    if(dim(dfCall)[1] >= 2){try(abline(lm(Vols ~ futurePrice, data = dfCall), col="green"))}
    if(dim(dfPut)[1] >= 2){try(abline(lm(Vols ~ futurePrice, data = dfPut), col = "red"))}
    if(labelPoints){with(df, text(futurePrice, Vols, labels = pointLabels, pos = 3, cex = 0.7))}

    ## Plot the implied votality against the option price
    with(df, plot(optionPrice, Vols, col= colors, pch = shapes, main="Implied Votality vs Option Price", xlab = "Option Price",
                  ylab = "Implied Votality",
                  xlim = c(min(optionPrice) * 0.8, max(optionPrice) * 1.1),
                  ylim = c(min(Vols) * 0.8, max(Vols) * 1.1)))
    if(dim(dfCall)[1] >= 2){try(abline(lm(Vols ~ optionPrice, data = dfCall), col="green"))}
    if(dim(dfPut)[1] >= 2){try(abline(lm(Vols ~ optionPrice, data = dfPut), col = "red"))}
    if(labelPoints){with(df, text(optionPrice, Vols, labels = pointLabels, pos = 3, cex = 0.7))}

    ## Plot the implied votality against the time to expiry in year
    with(df, plot(time_to_expiry, Vols, col= colors, pch = shapes,
                  main="Implied Votality vs Time to Expiry (Year)", xlab = "Time to Expiry (Year)",
                  ylab = "Implied Votality",
                  xlim = c(min(time_to_expiry) * 0.8, max(time_to_expiry) * 1.1),
                  ylim = c(min(Vols) * 0.8, max(Vols) * 1.1)))
    if(dim(dfCall)[1] >= 2){try(abline(lm(Vols ~ time_to_expiry, data = dfCall), col="green"))}
    if(dim(dfPut)[1] >= 2){try(abline(lm(Vols ~ time_to_expiry, data = dfPut), col = "red"))}
    if(labelPoints){with(df, text(time_to_expiry, Vols, labels = pointLabels, pos = 3, cex = 0.7))}

    ## Return the vector of Vols as required
    return(vols)
}
