#!/usr/local/bin/racket
#lang rackjure

; <bitbar.title>Coinbase buy prices</bitbar.title>
; <bitbar.version>v1.0</bitbar.version>
; <bitbar.author>Daniel Kvasnicka</bitbar.author>
; <bitbar.author.github>dkvasnicka</bitbar.author.github>
; <bitbar.desc>Fetches BUY proces for cryptocurrencies traded at Coinbase</bitbar.desc>
; <bitbar.dependencies>racket, rackjure package</bitbar.dependencies>
 
(require net/url
         json)

(define currencies '(BTC ETH LTC)) ; change to set watched coins
(define url-template "https://api.coinbase.com/v2/prices/~s-USD/buy") 

(define get-json 
  (compose bytes->jsexpr 
           port->bytes 
           get-pure-port 
           string->url))

(displayln
  (string-join
    (map
      (compose
        (λ~> 'data 'amount)
        get-json
        (curry format url-template))
      currencies)
    " • "))
