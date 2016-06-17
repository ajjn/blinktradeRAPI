library(httr)
library(jsonlite)
library(digest)
options(stringsAsFactors=FALSE)
options(scipen=999)
source("helpers.R")
## Define APIKEY and SECRET,generated at blinktrade UI
## Also define BROKERID and TRADING_PAIR
## For example BROKERID <- 4; TRADING_PAIR <- "BTCBRL"
source(file.path(path.expand("~"), "blinktradeAPI_keys.R"))

##    *--------------*----*
##    | BrokerID     | ID |
##    *--------------*----*
##    | SurBitcoin   |  1 |
##    | VBTC         |  3 |
##    | FoxBit       |  4 |
##    | Testnet      |  5 |
##    | UrduBit      |  8 |
##    | ChileBit     |  9 |
##    *--------------*----*



sendMessage <- function(msg, env= 'prod') {
    ## This functions sends a msg via POST method
    ## to the blinktrade api server.
    
    if(env == 'prod')
        BLINKTRADE_API_URL <- 'https://api.blinktrade.com'
    else
        BLINKTRADE_API_URL <- 'https://api.testnet.blinktrade.com'
    BLINKTRADE_API_VERSION = 'v1'
    TIMEOUT_IN_SECONDS = 10

    nonce <- as.character(as.numeric(Sys.time())*1000000)
    signature <- hmac(SECRET, nonce, algo="sha256")
    
    headers <- c('user-agent' = 'blinktrade_tools/0.1',
                 'Content-Type' = 'application/json', # You must POST a JSON message
                 'APIKey' = APIKEY, # Your APIKey
                 'Nonce' = nonce, # The nonce must be an integer, always greater than the previous one.
                 'Signature' =  signature) # Use the API Secret to sign the nonce using HMAC_SHA256 algo
    
    url <- paste(BLINKTRADE_API_URL,'/tapi/', BLINKTRADE_API_VERSION,
                 '/message',sep="")
    r <- tryCatch(POST(url, body=msg, encode='json',
                       add_headers(headers)),
                  error = function(e){processError(e,
                      "Error while sending POST request in blinktrade API")})
    r <- processResponse(r)
}
processResponse <- function(r) {
    ## This function processes the response r of the server
    ## and returns either NULL or a data frame

    if (is.null(r))
        return(NULL)
    if (status_code(r) == 200) {
        raw <- tryCatch(fromJSON(content(r, "text")),
                        error = function(e){processError(e,
                            "Error: converting raw fromJSON")})        
        if (is.null(raw))
            return(NULL)
        if (length( raw$Responses) > 0) {
            data <- tryCatch(flatten(as.data.frame(raw)),
                             error = function(e){processError(e,
                                 "Error: function call flatten")})        
            if (is.null(data))
                return(NULL)
            if (nrow(data) == 0)
                return(NULL)
        }
        else 
            data <- data.frame()
        return(data)
    }
    return(NULL)
}


getBalance <- function() {
    ## This function generates the balance request
    ## message and sends the query.

    msg <- '{"MsgType": "U2", "BalanceReqID": 1}'
    data <- sendMessage(msg)
    if (is.null(data))
        return(NULL)
    if (nrow(data) == 0)
        return(data)
    return(data$Responses.4.BRL[[1]] / 1E8 )
}

getOpenOrders <- function() {
    ## This function requests for open orders
    ## and returns data frame or NULL

    msg <- '{"MsgType": "U4",
    "OrdersReqID": 1,
    "Page": 0,
    "PageSize": 100,
    "Filter":["has_leaves_qty eq 1"]}'
    data <- sendMessage(msg)
    if (is.null(data))
        return(NULL)
    if (nrow(data) == 0)
        return(data)
    ## Extract the orders
    orders <- as.data.frame(data$Responses.OrdListGrp)
    if (nrow(orders) > 0) {
        names(orders) <- data$Responses.Columns[[1]]
        ## Change quantities  from satoshis
        orders$Price <- as.numeric(orders$Price) / 1E8
        orders$LeavesQty <- as.numeric(orders$LeavesQty) / 1E8
        orders$OrderQty <- as.numeric(orders$OrderQty) / 1E8
        
    }
    return(orders)
}


getExecutedOrders <- function() {
    ## This function requests for executed orders
    ## and returns data frame or NULL

    msg <- '{"MsgType": "U4",
    "OrdersReqID": 1,
    "Page": 0,
    "PageSize": 100,
    "Filter":["has_cum_qty eq 1"]}'
    data <- sendMessage(msg)
    if (is.null(data))
        return(NULL)
    if (nrow(data) == 0)
        return(data)
    orders <- as.data.frame(data$Responses.OrdListGrp)
    if (nrow(orders) > 0){
        names(orders) <- data$Responses.Columns[[1]]
        ## Change quantities  from satoshis
        orders$Price <- as.numeric(orders$Price) / 1E8
        orders$LeavesQty <- as.numeric(orders$LeavesQty) / 1E8
        orders$OrderQty <- as.numeric(orders$OrderQty) / 1E8
    }
    return(orders)
}


sendNewOrder <- function(side, price, quantity) {
    ## This function requests for a new order
    ## Parameters
    ## side: 1 for buy, 2 for sell
    ## price: in real currency
    ## quantity: in bitcoins
    ## and returns data frame or NULL

    if (side != 1 && side != 2) {
        print(paste("Side has to be 1== BUY or 2== SELL. Got:", side))
        return(NULL)
    }

    ## Change price and quantity to satoshis
    priceS <- round(price * 1E8)
    quantityS <- round(quantity * 1E8)
    client_order_id <- round(unclass(Sys.time())*100)
    msg <- '{'
    msg <- paste(msg, '"MsgType":"D"', sep="") ## New order
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"ClOrdID":', client_order_id, sep="") ## New order
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"Symbol":"', TRADING_PAIR, '"', sep="") ## New order
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"Side":"', side, '"', sep="") ## 1 == BUY, 2 == SELL
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"OrdType":"2"', sep="") ## 2 == limited order
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"Price":', priceS, sep="") ## in satoshis
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"OrderQty":', quantityS, sep="") ## in satoshis
    msg <- paste(msg, ',', sep="")
    msg <- paste(msg, '"BrokerID":', BROKERID, sep="") ## in satoshis
    msg <- paste(msg, '}', sep="")

    data <- sendMessage(msg)
    if (is.null(data))
        return(NULL)
    if (nrow(data) == 0)
        return(data)
    data$Responses.LeavesQty <- as.numeric(data$Responses.LeavesQty ) / 1E8
    data$Responses.OrderQty <- as.numeric(data$Responses.OrderQty ) / 1E8
    data$Responses.Price <- as.numeric(data$Responses.Price ) / 1E8
    return(data)
}

cancelOrder <- function(id) {
    ## This function sends a request for cancelling of an
    ## order with id given as a parameter

    msg <- paste('{"MsgType":"F", "ClOrdID":', id, '}', sep="")
    data <- sendMessage(msg)
    if (is.null(data))
        return(NULL)
    if (nrow(data) == 0)
        return(data)
    data$Responses.OrderQty <- as.numeric(data$Responses.OrderQty ) / 1E8
    data$Responses.CumQty <- as.numeric(data$Responses.CumQty ) / 1E8
    return(data)
}

getTicker <- function(address) {
    ## This function sends a request of
    ## public ticker data to a server at a given address
    
    r <- tryCatch(GET(address), error = function(e){processError(e,
                                    paste("Error while processing GET function"))})
    if(is.null(r)) 
        return (r)
    if (status_code(r) == 200) {
        ticker <- as.data.frame(fromJSON(content(r, "text")))
        ticker$date <- Sys.time()
        return(ticker)
    }
    else return(NULL)
}

getOrderbook <- function(address) {
    ## This function sends a request of
    ## public orderbook data to a server at a given address

    r <- tryCatch(GET(address), error = function(e){processError(e,
                                    paste("Error while processing GET function"))})
    if(is.null(r)) 
        return (r)
    if (status_code(r) == 200) {
        ticker <- content(r, "text")
        orders <- fromJSON(content(r, "text"))
        combined <- combine.df(as.data.frame(orders$bids),
                               as.data.frame(orders$asks))
        names(combines) <- c("bids", "bids.vol", "bids.traider", "asks",
                             "asks.vol", "traider.asks")
        return(combined)
    }
    else return(NULL)
}

getTrades <- function(address) {
    ## This function sends a request of
    ## public trade data to a server at a given address

    r <- GET(address)
    if(is.null(r)) 
        return (r)
    if (status_code(r) == 200) {
        trades <- as.data.frame(fromJSON(content(r, "text")))
        return(trades)
    }
    else return(NULL)
}
