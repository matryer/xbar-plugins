#!/Users/f/Documents/BitBar/gdax/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>GDAX Trade Prices</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Florin Langer</bitbar.author>
# <bitbar.author.github>CSFlorin</bitbar.author.github>
# <bitbar.desc>Get latest trade prices (in USD) for BTC, LTC, and ETH on the GDAX exchange. Just change the first line in gdax.5s.py to match your BitBar plugins folder (e.g., #!/Users/<your username>/Documents/BitBar/gdax/bin/python).</bitbar.desc>
# <bitbar.image>http://www.hosted-somewhere/pluginimage</bitbar.image>
# <bitbar.dependencies>Python 3 (gdax)</bitbar.dependencies>
# <bitbar.abouturl>http://csflorin.github.io/</bitbar.abouturl>

try:
    import gdax
except ImportError:
    import pip
    if __name__ == "__main__":
        pip.main(["install", "gdax"])

import json, os, sys

if __name__ == "__main__":
    btc_img = 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAAKT2lDQ1BQaG90b3Nob3AgSUNDIHByb2ZpbGUAAHjanVNnVFPpFj333vRCS4iAlEtvUhUIIFJCi4AUkSYqIQkQSoghodkVUcERRUUEG8igiAOOjoCMFVEsDIoK2AfkIaKOg6OIisr74Xuja9a89+bN/rXXPues852zzwfACAyWSDNRNYAMqUIeEeCDx8TG4eQuQIEKJHAAEAizZCFz/SMBAPh+PDwrIsAHvgABeNMLCADATZvAMByH/w/qQplcAYCEAcB0kThLCIAUAEB6jkKmAEBGAYCdmCZTAKAEAGDLY2LjAFAtAGAnf+bTAICd+Jl7AQBblCEVAaCRACATZYhEAGg7AKzPVopFAFgwABRmS8Q5ANgtADBJV2ZIALC3AMDOEAuyAAgMADBRiIUpAAR7AGDIIyN4AISZABRG8lc88SuuEOcqAAB4mbI8uSQ5RYFbCC1xB1dXLh4ozkkXKxQ2YQJhmkAuwnmZGTKBNA/g88wAAKCRFRHgg/P9eM4Ors7ONo62Dl8t6r8G/yJiYuP+5c+rcEAAAOF0ftH+LC+zGoA7BoBt/qIl7gRoXgugdfeLZrIPQLUAoOnaV/Nw+H48PEWhkLnZ2eXk5NhKxEJbYcpXff5nwl/AV/1s+X48/Pf14L7iJIEyXYFHBPjgwsz0TKUcz5IJhGLc5o9H/LcL//wd0yLESWK5WCoU41EScY5EmozzMqUiiUKSKcUl0v9k4t8s+wM+3zUAsGo+AXuRLahdYwP2SycQWHTA4vcAAPK7b8HUKAgDgGiD4c93/+8//UegJQCAZkmScQAAXkQkLlTKsz/HCAAARKCBKrBBG/TBGCzABhzBBdzBC/xgNoRCJMTCQhBCCmSAHHJgKayCQiiGzbAdKmAv1EAdNMBRaIaTcA4uwlW4Dj1wD/phCJ7BKLyBCQRByAgTYSHaiAFiilgjjggXmYX4IcFIBBKLJCDJiBRRIkuRNUgxUopUIFVIHfI9cgI5h1xGupE7yAAygvyGvEcxlIGyUT3UDLVDuag3GoRGogvQZHQxmo8WoJvQcrQaPYw2oefQq2gP2o8+Q8cwwOgYBzPEbDAuxsNCsTgsCZNjy7EirAyrxhqwVqwDu4n1Y8+xdwQSgUXACTYEd0IgYR5BSFhMWE7YSKggHCQ0EdoJNwkDhFHCJyKTqEu0JroR+cQYYjIxh1hILCPWEo8TLxB7iEPENyQSiUMyJ7mQAkmxpFTSEtJG0m5SI+ksqZs0SBojk8naZGuyBzmULCAryIXkneTD5DPkG+Qh8lsKnWJAcaT4U+IoUspqShnlEOU05QZlmDJBVaOaUt2ooVQRNY9aQq2htlKvUYeoEzR1mjnNgxZJS6WtopXTGmgXaPdpr+h0uhHdlR5Ol9BX0svpR+iX6AP0dwwNhhWDx4hnKBmbGAcYZxl3GK+YTKYZ04sZx1QwNzHrmOeZD5lvVVgqtip8FZHKCpVKlSaVGyovVKmqpqreqgtV81XLVI+pXlN9rkZVM1PjqQnUlqtVqp1Q61MbU2epO6iHqmeob1Q/pH5Z/YkGWcNMw09DpFGgsV/jvMYgC2MZs3gsIWsNq4Z1gTXEJrHN2Xx2KruY/R27iz2qqaE5QzNKM1ezUvOUZj8H45hx+Jx0TgnnKKeX836K3hTvKeIpG6Y0TLkxZVxrqpaXllirSKtRq0frvTau7aedpr1Fu1n7gQ5Bx0onXCdHZ4/OBZ3nU9lT3acKpxZNPTr1ri6qa6UbobtEd79up+6Ynr5egJ5Mb6feeb3n+hx9L/1U/W36p/VHDFgGswwkBtsMzhg8xTVxbzwdL8fb8VFDXcNAQ6VhlWGX4YSRudE8o9VGjUYPjGnGXOMk423GbcajJgYmISZLTepN7ppSTbmmKaY7TDtMx83MzaLN1pk1mz0x1zLnm+eb15vft2BaeFostqi2uGVJsuRaplnutrxuhVo5WaVYVVpds0atna0l1rutu6cRp7lOk06rntZnw7Dxtsm2qbcZsOXYBtuutm22fWFnYhdnt8Wuw+6TvZN9un2N/T0HDYfZDqsdWh1+c7RyFDpWOt6azpzuP33F9JbpL2dYzxDP2DPjthPLKcRpnVOb00dnF2e5c4PziIuJS4LLLpc+Lpsbxt3IveRKdPVxXeF60vWdm7Obwu2o26/uNu5p7ofcn8w0nymeWTNz0MPIQ+BR5dE/C5+VMGvfrH5PQ0+BZ7XnIy9jL5FXrdewt6V3qvdh7xc+9j5yn+M+4zw33jLeWV/MN8C3yLfLT8Nvnl+F30N/I/9k/3r/0QCngCUBZwOJgUGBWwL7+Hp8Ib+OPzrbZfay2e1BjKC5QRVBj4KtguXBrSFoyOyQrSH355jOkc5pDoVQfujW0Adh5mGLw34MJ4WHhVeGP45wiFga0TGXNXfR3ENz30T6RJZE3ptnMU85ry1KNSo+qi5qPNo3ujS6P8YuZlnM1VidWElsSxw5LiquNm5svt/87fOH4p3iC+N7F5gvyF1weaHOwvSFpxapLhIsOpZATIhOOJTwQRAqqBaMJfITdyWOCnnCHcJnIi/RNtGI2ENcKh5O8kgqTXqS7JG8NXkkxTOlLOW5hCepkLxMDUzdmzqeFpp2IG0yPTq9MYOSkZBxQqohTZO2Z+pn5mZ2y6xlhbL+xW6Lty8elQfJa7OQrAVZLQq2QqboVFoo1yoHsmdlV2a/zYnKOZarnivN7cyzytuQN5zvn//tEsIS4ZK2pYZLVy0dWOa9rGo5sjxxedsK4xUFK4ZWBqw8uIq2Km3VT6vtV5eufr0mek1rgV7ByoLBtQFr6wtVCuWFfevc1+1dT1gvWd+1YfqGnRs+FYmKrhTbF5cVf9go3HjlG4dvyr+Z3JS0qavEuWTPZtJm6ebeLZ5bDpaql+aXDm4N2dq0Dd9WtO319kXbL5fNKNu7g7ZDuaO/PLi8ZafJzs07P1SkVPRU+lQ27tLdtWHX+G7R7ht7vPY07NXbW7z3/T7JvttVAVVN1WbVZftJ+7P3P66Jqun4lvttXa1ObXHtxwPSA/0HIw6217nU1R3SPVRSj9Yr60cOxx++/p3vdy0NNg1VjZzG4iNwRHnk6fcJ3/ceDTradox7rOEH0x92HWcdL2pCmvKaRptTmvtbYlu6T8w+0dbq3nr8R9sfD5w0PFl5SvNUyWna6YLTk2fyz4ydlZ19fi753GDborZ752PO32oPb++6EHTh0kX/i+c7vDvOXPK4dPKy2+UTV7hXmq86X23qdOo8/pPTT8e7nLuarrlca7nuer21e2b36RueN87d9L158Rb/1tWeOT3dvfN6b/fF9/XfFt1+cif9zsu72Xcn7q28T7xf9EDtQdlD3YfVP1v+3Njv3H9qwHeg89HcR/cGhYPP/pH1jw9DBY+Zj8uGDYbrnjg+OTniP3L96fynQ89kzyaeF/6i/suuFxYvfvjV69fO0ZjRoZfyl5O/bXyl/erA6xmv28bCxh6+yXgzMV70VvvtwXfcdx3vo98PT+R8IH8o/2j5sfVT0Kf7kxmTk/8EA5jz/GMzLdsAAAAgY0hSTQAAeiUAAICDAAD5/wAAgOkAAHUwAADqYAAAOpgAABdvkl/FRgAAAfhJREFUeNrslzFIW2EQx3+nJVOh4FQoFISUgBAoCIVMrkKgkKklHQWnODrVpbPgVCiFrg5OLhkKXSyFDiWCpOCiIAiCUzBUEIpyLpdwvL4vJO997YvSP9zwLi/5/rnv7n93oqpME2aYMuQmJCIDa4uImtXN13S+bffu0O5/hO49oQcZcqYMPLLHE+ASqDofQBlYBOadrzTWAao6kQFtQHNYB/gE1NLOL4KQt+9A2f9+0TlUA/ZEZC7zlblIPQZagX/+1nLoBbBqz50RkXqX+cpSiB2lHNAMRGQ9QOhbzCu7muDdTeA4xf+kSB06TfFdFkXoqeVWEp//NaFZoAHsJQQU4BzYGgpv3nlIRLqm1B4nQM8pdDXw9QPgjaoe5i57V2XdHMJ4AXwEKjHLvhtBsX8BS6o6eXMdExs+UYEFa7j1QFI/BNoi8uxvRag5guwr4DoQqVYROrTjqyqBSlHNdT/gLxVFKCQDZzORRG8SvLQmmxq5LCOsF7rnVkFJzCeqqWLvLQeqDOAM+JKlqmoRJ0ZvjazjRzVyPp0Dr1V1N9PWEZHQVxPPD6raz9xcRWTZxogkWo7se+CnjbAr5vsBrAE39tnvQS/NtQal5NQAfhupm6/pfNvjrGH/V+k7RyjGxOj3+cF4egz0gTm33/dskvwjh6ISio3bAQAjDpZC/AXC2gAAAABJRU5ErkJggg=='
    ltc_img = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABS2lUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxMzggNzkuMTU5ODI0LCAyMDE2LzA5LzE0LTAxOjA5OjAxICAgICAgICAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIi8+CiA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgo8P3hwYWNrZXQgZW5kPSJyIj8+IEmuOgAAAQ1JREFUOI2N0z0vREEUxvEfsWIThCi0REIlKCUk9EqVSiEiSgofQqNVaH0BnUIlkf0AQkFIZKMgWYWXeF3FnZuMmx17TzK5z5k5/2cyZ+7QOsbwiGc0sZqo05WYn8RQ0F+4Shl0JubnI32L8yivxFzKYCbSN2gEvYjtdgYDmIjyWviu4QRV2bGSMYs3WfO+sYLdkL9j+j8YNkJxbvAadA1T7WDYiwxyk50yIHTgLILvMVcWrmC/sPtWWXgERwW4iaV2YH6Ny7K/rx6tveC6rEEd6ziI1i5xF3S1MPrRR9a0PCo4xkLIG7Im9hTqutGLQ2zGj2k8gmEwjFbxiQf+vsZRfOAJPwmwiWFc4BR+ARk4P0QEdiHgAAAAAElFTkSuQmCC'
    eth_img = 'iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAABS2lUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxMzggNzkuMTU5ODI0LCAyMDE2LzA5LzE0LTAxOjA5OjAxICAgICAgICAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIi8+CiA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgo8P3hwYWNrZXQgZW5kPSJyIj8+IEmuOgAAA2tJREFUOI2tlE9IY1cUxr/38swLkj7/zItJFAImMsE/4EKfoBK0BQUXDbgqtq4FW51FE5gZdHBR2qrjJgil2oAbySJkUxdZ6CKCQZIGLAl9eWCjiMQgMTHUPJTEvNwupobRqMyiZ3Xuge/Hd+659wD/c1APC3t7e5WcEAKj0Qi32/11IpHgJiYmfs1ms6Co+7KpqalKzjwExmIxAEC5XAbHcUgmk2qPx/NeluUXVqvVpyhKJp/Pg6bpRx1WAU0mEwCAZVlQFIX5+XmnSqVq1ul08Hg8DofD8ValUuH6+rrK6aPAlpYWAIDBYEA4HLaGw+G3ZrMZAHBycuIslUq/C4IQury8/DSHkiSBoiikUiksLi7+DEDLsuxdi8zS0tJPfr//i97eXuRyuSqXVRfR19cHm80Gr9f7VTgcHgeAYrGIQqEAADg/P//c5/O9YhgGuVwO6XT6eYc1NTU4Ozv7bHt7+8e7WltbG2iaRiKRAACsrq6+M5lMvtbW1lS5XH609Uq4XC7YbLb3AEhnZyexWq3EZrORwcFBYrFYSFdXFwFABEH4JZ1OQ1GUe3rVQ2Amk/n28PDwB7PZTN3c3CCRSICiKMiyjOPjY2g0GjQ3N0MURcFgMERomk663e7Sky13dHSkLy4u/hZF0XpX02g0lXd3enoKAGhqagLHcayiKGoAN08CLRbLrl6v/0un01nn5uZwe3sLhmGgUn1ohqZpOBwODAwMlHiel7Ra7T8f66umLElSvdfrbWpoaIAoirDb7RBFEbFYDKOjo4hGo+jp6cH4+HgoEAicyrL8xDT+i0gkgrW1NQOApCAIJBgMks3NTbKxsUFCoRAZGhoiAK5mZmZebm1tIRAIPA8sFAoghGB6evpLAAQAWV5eJi6Xi9ydJycnvzk6OsLu7i78fv/zwGAwiP39fayvr6O/v38RAKmvryeNjY0EALHb7WupVAqSJCEejyMej9/TV91hNBrFwcEBjEYjnE7nm9ra2iDP89Dr9QDw59jY2Hd1dXVQq9VgWRYsy97TV025vb0dAEBRFDiOw8rKyvcLCwt/XF1dYXZ29jXP86WdnR089UOqgFqtFsCH5cqyLIaHhyPd3d2v8/l8nd1u3wGATCbz6fuwVKo8ehSLRWSzWZjN5t9GRkYYo9EIRVHA8/yjMAD4FxPvXKowwHqHAAAAAElFTkSuQmCC'
    public_client = gdax.PublicClient()

    # Print price with 2 decimal places
    path = os.path.dirname(sys.argv[0])
    product = json.load(open(path + '/gdax/gdax_settings.json'))['product']
    price = float(public_client.get_product_ticker(product)["price"])
    print("$" + str(int(price)) + str(round(price - int(price), 2))[1:].ljust(3, "0") + " | image=" + eval(product[0:3].lower() + '_img'))

    print("---")

    # Print 24-hour percent increase/decrease
    open_price = float(public_client.get_product_24hr_stats(product)["open"])
    percent_dec = round(((open_price-price)/open_price)*100, 2)
    if (percent_dec > 0):
        print("-" + str(percent_dec) + "% | color=red")
    else:
        print("+" + str(-percent_dec) + "% | color=green")

    print("Open GDAX | href='https://gdax.com'")

    print("Change Coin | bash=\"" + path + "/gdax/Change Coin.sh\" terminal=false refresh=true")
