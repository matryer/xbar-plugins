#!/usr/local/bin/racket
#lang rackjure

; <xbar.title>Coinbase buy prices</xbar.title>
; <xbar.version>v1.0</xbar.version>
; <xbar.author>Daniel Kvasnicka</xbar.author>
; <xbar.author.github>dkvasnicka</xbar.author.github>
; <xbar.desc>Fetches BUY proces for cryptocurrencies traded at Coinbase</xbar.desc>
; <xbar.dependencies>racket, rackjure package</xbar.dependencies>
 
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
