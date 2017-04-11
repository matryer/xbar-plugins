#!/bin/bash

# <bitbar.title>Decred BTC price on Poloniex</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>James Hooker</bitbar.author>
# <bitbar.author.github>g0blinResearch</bitbar.author.github>
# <bitbar.desc>Shows the last Decred price (in BTC) on the Poloniex exchange</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/g0blinResearch/bitbar-decred-btc-poloniex/master/screenshot.png</bitbar.image>
#
# Credit goes to https://www.monero.how for the base of this plugin (see https://raw.githubusercontent.com/matryer/bitbar-plugins/master/Monero/monero-btc-poloniex.30s.sh)

decredIconBase64='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABmJLR0QA/wD/AP+gvaeTAAAFvUlEQVRYw+2Xf3BU1RXHP+e+t9lAUmFQWixTrTACRkIIAskGlKAYfiSB6dTaglDUdhSmLZNMx5YWgxYbBuUP0OlMpsqAlrZQpbUQIhDB0EiyCcEACT9SOm2pCIitTCok2d337rv+sVmzGxO7mbH+lTPzZt6ePXvf537POfedhUEbtEH7gi3nR51Jx36vue5zf7705cz9YcfQkOuE2lMNuWmgO4TXNg/3APLXhTm81v9J7KPvvJ3S5aV4N10f5jlyFhO+jVbTQP28Fd7nBjStJJRrC/vEolPZIAqUjU9sKTm81v/72c+EqSmLQj12ovF2F12lxUtzcY0rWhw8XNHioi+5eH/Rxj1waNI3qgFmtO2mbsKi5IGml4Q4ujmVQGkkR9nefrEYLgqUBcoGlKypKfOvB3g0eJytgWweORb8mufTQRc92hUXBw+NxhWNi4cjGhf9tmuZxee6Wi7+Z8q6foFUb8fRzanklkQIbkppNMJ9wLl4dMGU37c+9NKidcbeGshmyYHTbJsauIBItsG8GVvHxF0Sve4Wz2sb67/j2xgIXHg1+ZQl1FNJ5AZfmtmnlMmLpU8ssGyOZp09m7txW7Z54I/vsuubt7A02OCPpHVs0Xjzu1M2Ik6hbsV0u/iY0Dhu8ZUBAeX9NEz9s36+8+crXGoadqtY1CmL0bH0iQ1Kcd6y1aw3SlLeLay4Tvrcw/xhTFHcKob7Wysf9MR73kWPckXjoHHFq27OeGhuUimLWf2zfqau1r73GoatMYZ3MIzuI+zrnvEOL3ghNKNqZTqR3XMEYwAoOHkAEN7MXPgqlppp4N89CpiCrLbfZCUNFCgNMesH15RfO+XAL4Eb47U0ieG3ATVFFaHlr5emmsUbwgBUZ/UIcOiOhX9HTFnCr7SZkxTQtNIQwU2pRFJ8xcbwRK+vrwL/ENiPUAVUiaEKQ7UxfKu4Ijxtx89S+1TcOCl7EjYjck9fcXZvR9OmTxasIFGUP4E8+VaZv+2zmqC4IkzlSv+nHzTUa484Ca4vJ52y3FVdDwM3S0+a2oz2L655KgozZ4PpF6g3zMyWvdEdhd3eirQmX9QieUCsPsFQWlsuzszVUcfB1f/ztGDOseiRdGRSEfe0HPJ5Ipvi29qD+gF0mclMlNtfC3BkQyLIY8eD/QIdnHp/tNta9kyw1bUdwISeg1Iut2Z89+Wkaqib84P4XnI69HCgq3fUi9kBHj8enOYKT0XQeGgc5aIxuOJaDnqcizdcYETCVgwrATLP/Y7WcQ8lpVBtNHPdGbTchX1FPdFQw6+zA01GzGtKKEAoBAoNptAY5gmMAUbEd5eCjWmOWznl9PZPwfQLJJiDiXw8M3ttaGzsY8HT1wDYmJPPyuZa2ZqV9wqG2Qb+Kb1aU3pgPhThyU7MmtrJy73mO5eRdA0Fnx9yEqg2PVkb6cGR/F+EHzRA9dNfihU/zdXZZnlTI69MnlmnPPINnJe46u2GumjBXWPGp5e33LnEyTm9c2DzEEDejyNfQbw2ZSeOH933V7ExSmRfmt/+/uurrPADe//KrqLx3PpWh0y9qb7BRU9PGD+Mrg9L5/zGics++qzu7OfVYThVl3IFZVYA13ujm2iR3ihilnZpp7L4hfDIXUXjWbTzEv+6N818lJo+w8CWhPFDyLPF3zT9zM4pAHed2T7w8SNz/QVuaB81Wvn0DqW4O06h2Ns+em9x0ef3ArsfH3qheNt/qXxkGAALWqt+rvHKHdFodGwMafdEzzuWsbRx8pntnMhYljzQjJ8Y6p6LhuSXhQqUj7kIs5TFV8XCdMMYZSGi6LBsU7hnxZC/dcPwRmYhc09VLnHRm120Ewc2VGPmn8xY1jDgAa0/e7i0XV3NMXRehs7LYE20VXq7rfatGhKJxcw7tZf9E6Pz0aKrNaoldILxMorzzkh8H76f2pq9tJP/ty34VVfSsZNO/nbwf+egDdoXbh8D8XNf059XWtcAAAAASUVORK5CYII='

xmrLast=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_DCR | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
printf "%.*f | image=%s\n" 5 "$xmrLast" "$decredIconBase64"
echo "---"
echo "Decred homepage | href=\"https://www.decred.org\""
