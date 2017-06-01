#!/bin/bash
# <bitbar.title>Cryptocurrency Prices</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>viiraj</bitbar.author>
# <bitbar.author.github>viiraj</bitbar.author.github>
# <bitbar.desc>Display the spot USD prices of Bitcoin (from Coinbase) and the spot prices of 9 other cryptoassets (from Poloniex) including Monero, Ethereum, Decred, Digibyte, Ripple, Dash, Litecoin, Siacoin and Gridcoin Research.</bitbar.desc>
# <bitbar.image>https://media.giphy.com/media/xUPGcf2tFHHoAPssy4/giphy.gif</bitbar.image>
#
# Based on the Monero BTC price at Poloniex Bitbar plugin by Monero.how (github.com/monerohow) and the Coinbase.com Bitcoin Prices Bitbar plugin by Charles Lenhert (github.com/CLL80). Thank you to the original coders who made this job very easy.
#

bitcoin_icon='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'
monero_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACtElEQVR4nE2Tz2udRRSGn3Nm7v2aLzeWFouBtCm6cFFwIXRTRVOjNbfEH0TIP9CKdtMWWlxYqNeqkC4KCt1o/dLiOiC6kbTYhWYT8C9Qd4VWBKNok/vd3Myc4+K7LZnVC3Oed87Me0YAHETAAfqneTEG3lNhRpwpABceGPycMjfKirXdjDwSv3Upnp7mmipnNBDJgNEsBQJYJpvz9R9/c2F6hdpBxBcJv08Sn9nmhzDGLH0cwUaYjCwcMBylRHLN2oN/mDsEQ5UV8uEtPg8Fs9SA4M15j2FGOiA4NYSClyaf4LqskGXrFEfLkl/ygJ+Ss1qMs8SAtNvEG5HYQ6z7XI7wcqvkRL3JC9oSzhJxhPaeZa4O+3xCQQTyiG3ggpj6LJUVn7nTJuBROaciHCc3LXoPLSp6g1o+3WWSKIiDWq62Ki55D1UhkBCBGVVh8tFryxXMFwljlX80rKXppIGXxir/0BcJcmVUbRCFA1EFw0nZBXDe2Hz91Dtzdq+ofuxtnZaOCLGs/NLCyVdm39xsPwu3v0wuRPeEYJqMLcaJLuxvMg9TXk58+1b31efHl/1iWfn5t0++9pwXe78X9PAolX2ME7MziMn5wGqO1EnugwvGwxBCmVrl7YX5E7M56Y7F9p0ixs5wOPgXkNrlK6192oxf49gyt5q0mqtld0kpuYgcGNK+S8SCyGRKya0p8f037XrDOOKLhIXNufPSLs5s76S/ROSQqh40M1NVFRFyzqaqmnO+D9wrWvFJS4Pl78o71/TjIz1/OEzf7GT7b2Ji4lgI4aCZZRERd3czMxERM8shhKlOp3NsJ1t/O+tNWcFlNG3e7XaLEMIXIYR3VTXmnHH3Jl4RQgjknLOZ3drY2Di3vr5e03zEx7PuAPPz88dF5H1gBnhqtP8nsJZSurG6unp3N/M/llI6HEcL354AAAAASUVORK5CYII='
eth_icon='iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAACXBIWXMAAAsTAAALEwEAmpwYAAABWWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgpMwidZAAAA4UlEQVQoFcWQvw4BQRCHz5+gkahERCNI6HQSEr3aS+i8iWcQr6AinkEhoqJSikahEnyztxN3l80mCjHJd7sz85vf7l4Q+CPtb/+wW/7WW69aYnAGbWuQSRqpMFp/2qTH2oGJzaWesnuzJIc1b9JtwRlGMIQXxE5XMXXjKu45EHEdGlCAKRThAdEZ0jDUdUC6gS2c4A4XGIOE6j6bsG6+ej05sQZHWMMKxETeLRrn8M3Wr6x7yMMBlhCL5P31b+5QiXsXKjAHCdGbU03m+Khhld4C+laTdWi9JXnzf0Kf4Dz9DdLXHM44uKqBAAAAAElFTkSuQmCC'
dash_icon='iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAg9JREFUKBWNUzFrFFEQnnnv7bKed7dgIIVZxGAVRQsjFqaw8bSyVEs7Y2MRktNKuFKJcFgmP0FLMQQSFERs5EBRSCOCeGwSMHgXcC93uzvjvL1lz4MoDvse882bmW9m3luEMWEEFgOOGcdBcY5WK+RfIYVToTRYmQIA8OydFaddPek6aYXo196hydKqi5V+mnxpYF8cpFTALHDHP/VSwIyUHYlN/ZF4qLL4opTL7MneNLMrLdOah3jXn66Z8kSNKc0daRTL0h7KONIYIJHluMCD6HFRWrCwfgyNOQMEB4xKCYEvFBk7M2uNukPAt1Dre4IBKP1gggcbZ5nUbWb6KaX0WLNGTnmy+/Vpa3VeaEYS1DfvgnJAMkMaRy8wWNpYd6dOX6Peftam8soQb2/tclya9pIORROTOmxciKburwXI7mdUxmdKADVcNKDU63649UYh9YEUJwhlRFprNy/1RpwAit05cEs+pANr/vT90ZX3Wc/B0uY56eIoKpRpxTIbx8swIDGBVpTuk4KHQndVeRWgXudJe7lWNyfqr+agVH2LdpriKRyySMJEFxvmk8ZkAGhcCezuyDCblt7IBM9D1P0h971ncX6PxS1keJhIcXzwUbOz+G35cgg3nmkMFt4dId9FCLdtssPluJhD+Vavy+MRkUB4fjN/EJnlPzZ5z2BXLrY826zd/y5yPDwf/5N+A+ZZ3MPujFBfAAAAAElFTkSuQmCC'
dcr_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAADT0lEQVR4nHWTb2iVZRjGr/t5nvd9z/ues9Y2JjZh4dwGW4HMjJCgOcIVYiyKpRjTQdGKapJCISyOs+ifrRkG4kgqKLNpNEtGEWyOQMh0BQsP6tqBmZoetzo7f94/532fuw+xgkm/TzfXh/vDdfEDltLJ8pZsCa3jSbV4q+c/LKzNaw4rHSOytOu+tY2mARbMwAuzp9ZoSwZOPtKUkCImY/5r1+9N090UgEEgsMoWefWljDXkaSUURLjxzcLL3+6mQYDJDb0HLhmlgaL0QNkSSMrSROJyuiN94uBJ6tgfJZOCAGDT28XW37Pmp3lXrrAd0B1xb3D8VXtnKQI2Xzj5WNryPoksmYi0RgkaMSuGinnv0Fhj57PUnGTzfD8FTx/MPfHTrP2Z68qSYcNe5vhHx/rGuok2+n0zP6z+q9JsvHrzj/gcghfnRLjGNE2sCsxuAoDH9xW3z2St9wtFUS5NAAyoOJAIS6mVK/Kbj/RUTi2Wlspkyp7Kjp9dcER9ZS76RXUdKKydumoezpeEdIzIS5h61Pd5VhcQ+oYom8+odczD53vO1YkLVVOyqbo6t/7i8BEh5Z6QwlVqOmP0uVrKhAoXGqqDDcd3xc/wktnW3z+ugG94InaXBgBJYjmYWQO+CkO5jgS4PKY/P7YrfgadbLY2QwPARD+FADDR1hYuPnvm8nf3TXp/buFQkgNxVoHYIgYx1A0AaG2GyNec43cb7+H9XV/13DC5RefdnJBCWSRrf3bnNxVjwox7UVSvKvYqQ/BvOkSLG+gtqVRqX1MT5QCgDcDW1PenlVjoLdZVN5cyWRSJEBZ8TpCTrYHaMVT30I/06Hv+9umM+XFQAqpsf3JZWXigZnm84GA+NbCt6lceZWtD08jhOYuf9LM5N+44RktQvnWoof3YwxdHLTIk8ODe4kdXPLvb9wFDAlICMgrztbe7XSdeuW1EgdA+MzJw3dY7i7kCJwzzSm1od33Z8MgpQpKF8Trp9je8HTcL6jkvkHWs2YAk2BZwZ0W+94vexAeSiDvSX790Df47oSGU7WuuI6eH/umWADCY2dh91F9Z9CmGMNAFZQrpueaha4OTtAcA9evk7Ol6D4E9F/nS0cL4T9Ek/6vo/zLceYvqfwNpQYLrKeD+7AAAAABJRU5ErkJggg=='
xrp_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAADHklEQVR4nH2TzW9UZRTGn/PeO3dm2o7ttJRgERKgaAuC2OoQqUZACSGwIBpYuKAYiSEWFqIxMS7udIPEhYoxauISCAlFNI0oanRIiAUphGinY0OUL4kd6Pd0Zjr3vu95jwvQuur5A35Pnjy/Q5jjRISISEQm6rZ+O5oeGAme8WBkTT2dOrlt9SFKp0FzAeD7StLrVeqLZO+QadwSTk4AViMWjSMVL3z800up/bMAEdrRAwUAJ3bA7uyB6tlJ/MLXg89nClU/lMYmtQvtiGGpsNA8pWV/W6LVvZckCkS2B2AAIADNh79xAfBwIVhhTMw6oslaVsKMCDOKocbRi3/Nd+/3tCJ98Vf7F60fK1n9Xov+rfnB5ru+L2rggSu534eLKmAGsbGwYkMtlFRGNjc35AkA/P7h1KkxOjI8Yx/mMMR8h0e2JsM97z/X2htzgKWfXRgaKqpHlNFgK6iprsLjXuHTvr0bXqNxGa/t+HL8wp+SbFGFEQ1mChFxl0SCyhsroxt2tURuPHYs33+zRA/No8ofiQjdWVKjvvqu8+nDRDC0uTf31KVSrK9cCS0xK2ELYm2ZrapTRoN1cZRjybZE8MvFV2Ibo+qJciizQ6lpAxhtIMaQMANsAKNJmBGGJsJGEmIsooQAaK9omZ0YACh761b99nPTV64F8cXRmSkN1hSIche4RncuV5vaaqtHX//5zvcjZW5a6JkBMHJLEzid2bflCMNXBAB7fxzamLmLE7endQPYotbRU882cNfxF9cecwEsOnT68o0Zpw06BKBQ5bpI1ZqD597c9g79q2smm13Q1V9aAybvQKvz656O9pu+L6qvKfPk+XzlfLkciDIGsMxGi1MfMXr3qprl9J9I3WT/b/EKP+vluh8N2z88sy9bcD7SkxNMYl1iBhsDlw0We5WOeyZ2kxURovsq+4Npya3McQ5AY7W9psaZxFpxLIs1LGIsPLK8tqkuP9czESAQueyuOnj7bG4S62xxCmCGF69BS1X5k8F3d3e5cwDE99OKqFtfLfy9/eXPL31wPe9ucoTCZY3qZOatzrfJu67+AYvGsl/e3vzPAAAAAElFTkSuQmCC'
dgb_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAADG0lEQVR4nF2TS2hcZRiGn/8/Z85cMrmREpIht4qFtjRNiTLJINEki6Ybqy4CihUFqcVaEEFc6EKQKpqNXZSCETfajWShDeJlI7GoIUFKm9ZAk9gmaZzEXjSXmTkz5/zn/1zEavBdv8+7eh/FdhQgAHQ+24t2j6t41WPKTbQAiCmvSKXwA2JHmTk/tZNR/8LDb3vMLY5Qk3mZpi6PqmYgvl2VMpTWYPVyQGHtHNncG4yeMNsrw8MO7HdYuDVOa27IbcsKTso2pET3tieUscL1O6Es3rXWWl/bW1OK5cnv2NN2FGYjzdhYxNyNEVpzQ+weCExJlPlj3TFhoHo64jzUGiftRsrc3nAsWqndgwFtuSHmb37A2FikOPD0w9S1T6muYyKh6Bf76tSBTJL6Kofu1iQVI/yaL/PNzBbj1zbwA0S5ysqVzxQbK1kX7Z5SmW4tKh411lTUC7kG6lMO94qGO4WIurjmyUO1PN9bz/RiicMfLqhNSaKau7Vsrp5y8dL9km4CjL7nC33vz4OjePepJvxQOP3lGjVph4uvP0i2I0X//houTBe0U5sh8lIDmlgig5sEsQpAWRjcl+bNoUZ6O1I4WrG5abj5ZwDAum9BicJJQCzZrNkRpUAi4ejBWgT4ZPIvoqKhsTHOkX3VzK5V+Pl6AZ3QWNm+jSYs5zE+KC0mEnTK4fHOau4WDF/8sk51XYwLJ9qpRJZnPl4iDC04WiQsQ1jOa4LCBFurOK5nKRsG9qZ5YJdHMbC8daSRq6f34oeW7HsLzCz56ITCErMUViHYmtBYc5bVS1aMD64rgbG88/Vtvrq6xeHOaiZ/K/LK53nmlnzcpMaKIxgf8pcsImcVAF3PnaG971V2Dwb4JY9ChYZMkpOP7sJY4acbRX68tgnJGNZJBix+77F48Qwz51/778rzy+O05YZ0S49or8rGlNG1CZQIbAVIKXQspqT5fUqxPPkte9qegNnovkzw0kcu05MjpJtO0nzII90M+h+ZojIU78uUP0f2kR0y/V/ng8d6UPo4XrpfufEWAEx5RYLiBDDKlU+ndzJ/A2YXZ5AuWABsAAAAAElFTkSuQmCC'
sia_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACZklEQVR4nI2Ty2sTURTGf/feyUxtrKkvRARRCq1YFRFUEJSmolux4iwtpXs3gtuSjehKdCEVxajgZrpRFEV8TNEuTCwUiyKC9h8o+EgTJ4+Ze1ykaYwieDb3Lr77+75zOFdRzAdk0sOUonEeLTxkcFDh+wn/WQ6KETasNZTKp8jlHvD4igc0ATKhmUbDUFO9uCic9i0KaSPe3jrBp6kbFPOvmL3dB0A44SCi/mkbBKZ1bYsKd/rR9iIpc4m9ZwoAzN/bgchhRLYilBA9x+f7If5UQhAYfD9RBIFhJ4Zdfp1Cfi+OMwO2QCIpFAfJrHZxHRCgEkFi56hUz3No/DlBYNoJwgmHbC6mmH/BpnXDfFsC14FKlABfEAzG9JFJQ1RNqMan2D/6QK8AprHLTTmUygkKqDWeoZwDrErvpsIuRB3hR7mI5xmwd3lzd7tambbKWYo3B9DOPF2eS63+ltLCIbK5uGOAYb6XHlWgd00/X0uTzQTTQ81TO8Oku10EaNgLZHMx7wN35fHjKx7Zse8kchmtBOSY7qBbtmE0RFGC6E+IKAY/tBOUN8eIKFL6Iz+rCtSWToBiCSuCk9Iom2FqSsNg5z4oJdTqChEQ8RwAhhZlGVCk0VB4KWgkY/j+G8LQIQybuigyhKHCLEgLpla8ReDJVZf1a97R7Q0QxwkiZ9k3eo0/a/Z2llVdL6lESTve8mZRyB+l23uOtYCCOH6NtTOIqjWtRAEncVN7qDeSzv5ENEpZirdG6PImMWYjRoPWINLKCtU6VBvgOvz9YVpJZq730ZM+h5XjJHYTVrzfnDRuSlFrPP0FFWsBHSFePtgAAAAASUVORK5CYII='
ltc_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAADAFBMVEUAAACAgIC1tbW4uLi2tra3t7e2tra3t7e3t7e4uLi3t7e4uLi3t7e3t7e3t7e4uLi4uLi5ubm+vr7BwcHDw8PFxcXJycnOzs7Q0NDU1NTV1dXW1tbZ2dnb29vd3d3m5ubn5+f5+fn8/Pz9/f3+/v7///8mJiYnJycoKCgpKSkqKiorKyssLCwtLS0uLi4vLy8wMDAxMTEyMjIzMzM0NDQ1NTU2NjY3Nzc4ODg5OTk6Ojo7Ozs8PDw9PT0+Pj4/Pz9AQEBBQUFCQkJDQ0NERERFRUVGRkZHR0dISEhJSUlKSkpLS0tMTExNTU1OTk5PT09QUFBRUVFSUlJTU1NUVFRVVVVWVlZXV1dYWFhZWVlaWlpbW1tcXFxdXV1eXl5fX19gYGBhYWFiYmJjY2NkZGRlZWVmZmZnZ2doaGhpaWlqampra2tsbGxtbW1ubm5vb29wcHBxcXFycnJzc3N0dHR1dXV2dnZ3d3d4eHh5eXl6enp7e3t8fHx9fX1+fn5/f3+AgICBgYGCgoKDg4OEhISFhYWGhoaHh4eIiIiJiYmKioqLi4uMjIyNjY2Ojo6Pj4+QkJCRkZGSkpKTk5OUlJSVlZWWlpaXl5eYmJiZmZmampqbm5ucnJydnZ2enp6fn5+goKChoaGioqKjo6OkpKSlpaWmpqanp6eoqKipqamqqqqrq6usrKytra2urq6vr6+wsLCxsbGysrKzs7O0tLS1tbW2tra3t7e4uLi5ubm6urq7u7u8vLy9vb2+vr6/v7/AwMDBwcHCwsLDw8PExMTFxcXGxsbHx8fIyMjJycnKysrLy8vMzMzNzc3Ozs7Pz8/Q0NDR0dHS0tLT09PU1NTV1dXW1tbX19fY2NjZ2dna2trb29vc3Nzd3d3e3t7f39/g4ODh4eHi4uLj4+Pk5OTl5eXm5ubn5+fo6Ojp6enq6urr6+vs7Ozt7e3u7u7v7+/w8PDx8fHy8vLz8/P09PT19fX29vb39/f4+Pj5+fn6+vr7+/v8/Pz9/f3+/v7////Ybd+0AAAAEHRSTlMAAiZvcHF+f7Kztbbm5/b3GL4FdwAAAItJREFUeJxtj0cWwjAMROWCC9iJTU3obe5/RSLFsOJvRp6nkSWiCe1jztFrmlGLvgi9VfJelR9LdixXI3BjNVNe+u/AKClNXnofwEYKR5FlB7yOYgTKLFfgeRAjiXEGMJRmcGR4A+taq0R46B4zJx7K316asS2dbot9Mf9WJ2XbcZ1R7V7tQkrByfkfCTkPOQ2ZcvUAAAAASUVORK5CYII='
grc_icon='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAADlElEQVR4nE2TW2hbdQCHf/9zS3KSnJNLk6ZJM2vVjrFp7eqoIPZlDDf3avooFlTYg8MbQ3yw2FbBUqpIHeoUHT7ZiLIXwYFUN/aw6VZ3cWVpbZOtS5ukzTk55ySn5/r3aej3/n1vH8F/MAD8X86de/ji5csvmWbniKo0+8RQiKS60xvxeHJxcOjgt6Ojo7cBEEopCCGUACATExNkcnLS/2By8k1Fab6raWqy/9G9WFu/B1mWUd2oIBGLQhRF3aP0k5nZuQlCCCilIIVCgS0Wi97s7MwXqWTy1XK5jGZT80TeJkZzlQi8gKZGaXffk9S2TLalbEMMR3769LPThbGxMRAA+HB6+vV8vvdjOd5lr/+zwjerSyQY3kU8HkVjexPUtuB2XNxXklSIZBzf7ghyLPHRzNzcO2RqaipPqP/34NBwWG1skM3yeeLG8ujJ70M2248r1y7CcxxwTg1+YxV367308YNP+6ulZTpyaOQQZ7b1l0H96MrKmteu/Mh4qV7IUi8CLCAwLtKxENKpx1De7IIjJtDn/0GWrl4FqMvpnc5rXEtVj1qOTw2lTrbrZQzvex65rh5sKnfQMH9ALishxMloNdcgBDMgIgdHW2cUK0Z1XTvMtdRWnudcops78PkIdF3HcrsEj/8ZBw44iIkjsO029j4yAKPNQLEG0JW+T5yGg1ptq4exHAvbjRoMQ4PACTCMNtqaBttWEQqXoGkuWqoHlohIxNLwIABMGCzjw9ANwgUDgequFc/4lKdeZ4MYuo56rYSeXD+uXGIR4ivwHQscayDZ1YfWzgYII1EhkiOEQY1LpzK/UlodduyQb6mr4NR1EC4KOGlU14IIiRSuW0dUyqB87zwS7TJE+Tnf9htsMpn6nTly7NgZSYqZcjwKVhqkZvUOBF+D6frguRwsW0IwmMZ65Qbcyp/ofugojSWSEMUw3f/E4DwBgO/Onn1v+fat9+vbO7ZSr/JsZ4kEEjH4ARnUc9BpKQhRiqeefREDg8/Yjepdoamon7996tQJsrCwwBYKBfrN1199/9fStRcUVUNzZ8cTWZMJsBbheRZycg9N9e735USckSIiaSrqYjaXO14sFm1CKSUPxjg9Pz9988b1N0yzHbJdHxwnQAgEIUUj4FmA43g3KslfZrLZt8bHx3cppYQAwIMIAFoqlYYu/Lb4yq2b1w/XtrbynueTqCRtplPdF/bk82dOnDx56X8O/RdZlMHVyU64dQAAAABJRU5ErkJggg=='

#spot_price=$(curl -s "https://api.coinbase.com/v2/prices/BTC-USD/spot" -H "CB-VERSION: 2017-04-16" | egrep -o '{"data":{"amount":"[0-9]+(\.)?' | sed 's/{"data":{"amount"://'  | sed 's:^.\(.*\).$:\1:')
buy_price=$(curl -s "https://api.coinbase.com/v2/prices/BTC-USD/buy" -H "CB-VERSION: 2017-04-16" | egrep -o '{"data":{"amount":"[0-9]+(\.)?' | sed 's/{"data":{"amount"://'  | sed 's:^.\(.*\).$:\1:')
#sell_price=$(curl -s "https://api.coinbase.com/v2/prices/BTC-USD/sell" -H "CB-VERSION: 2017-04-16" | egrep -o '{"data":{"amount":"[0-9]+(\.)?' | sed 's/{"data":{"amount"://'  | sed 's:^.\(.*\).$:\1:')

xmrPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_XMR | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
ethPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_ETH | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
dcrPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_DCR | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
dgbPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_DGB | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
xrpPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_XRP | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
dashPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_DASH | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
ltcPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_LTC | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
siaPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_SC | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
grcPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_GRC | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)


echo "$buy_price | templateImage=$bitcoin_icon"
printf "%.*f | image=%s\n" 5 "$ethPoloniex" "$eth_icon"
printf "%.*f | image=%s\n" 5 "$xmrPoloniex" "$monero_icon"
printf "%.*f | image=%s\n" 5 "$dcrPoloniex" "$dcr_icon"
printf "%.*f | image=%s\n" 5 "$dashPoloniex" "$dash_icon"
printf "%.*f | image=%s\n" 5 "$ltcPoloniex" "$ltc_icon"
printf "%.*f | image=%s\n" 8 "$xrpPoloniex" "$xrp_icon"
printf "%.*f | image=%s\n" 8 "$dgbPoloniex" "$dgb_icon"
printf "%.*f | image=%s\n" 8 "$siaPoloniex" "$sia_icon"
printf "%.*f | image=%s\n" 8 "$grcPoloniex" "$grc_icon"

