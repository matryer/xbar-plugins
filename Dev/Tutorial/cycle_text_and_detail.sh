#!/bin/bash

#
# Include BitBar metadata like this at the top of the file
# (commented out, of course):
#
# <xbar.title>Cycle text and detail text</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.desc>Example of how to include items that cycle in the top, and items that only appear in the dropdown.</xbar.desc>
# <xbar.image>/9j/4AAQSkZJRgABAQAAkACQAAD/4QCeRXhpZgAATU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAACQAAAAAQAAAJAAAAABAAOShgAHAAAAEgAAAISgAgAEAAAAAQAAAZCgAwAEAAAAAQAAAZAAAAAAQVNDSUkAAABTY3JlZW5zaG90/+EJIWh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8APD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4gPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPiA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPiA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIi8+IDwvcmRmOlJERj4gPC94OnhtcG1ldGE+ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgPD94cGFja2V0IGVuZD0idyI/PgD/7QA4UGhvdG9zaG9wIDMuMAA4QklNBAQAAAAAAAA4QklNBCUAAAAAABDUHYzZjwCyBOmACZjs+EJ+/+INJElDQ19QUk9GSUxFAAEBAAANFGFwcGwCEAAAbW50clJHQiBYWVogB+YACgAEAAkAHwA1YWNzcEFQUEwAAAAAQVBQTAAAAAAAAAAAAAAAAAAAAAAAAPbWAAEAAAAA0y1hcHBsAAAAAAAAAAAAAAAAAAAAAAAAAAAA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D2IPBFfLvij9jH4CeJ7xr8abLp0j8sLKUxoT/uHco/ACgD8Ma6Hw94S8UeLbsWHhfTrnUJiQNlvE0hyfXaDj8a/bbwt+xz8A/C0/2n+yDqMg6G+kMwH/AAHhPzBr6R0zSdK0W1Fjo9rFaQr0jhQRqPwUAUAfjB4K/Yb+NfigJca3Hb6HA/JN1Jukx/uJuP4EivrnwX+wB8M9GCz+M7+61mUAZRP9Hiz34XL/APjwr72ooA8/8IfCn4b+AkC+D9EtLBhzvjjBk/Fzlj+Jr0CiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACrlrqN9ZHNpK0fsDx+XSqdFAHRHxXrxTZ5/47Vz/KsS4ubi6k825dpG9WOagooAKKKKACiiigAooooAKKKKACiiigAooooA/9P+3CiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/1P7cKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/V/twooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA/9b+3CiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/1/7cKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/Z</xbar.image>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Tutorial/cycle_text_and_detail.sh</xbar.abouturl>
#
# Text above --- will be cycled through in the menu bar,
# whereas text underneath will be visible only when you
# open the menu.
#

echo one
echo two
echo three
echo ---
echo These lines are only visible
echo when you open the menu.
