#!/usr/bin/env bash
#
# Unsplash Background
# -------------------
# Periodically change all desktops/spaces
# background to a random unsplash photo
#
# <bitbar.title>Unsplash Background</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Marko Radak</bitbar.author>
# <bitbar.author.github>iammarkoradak</bitbar.author.github>
# <bitbar.desc>Periodically change all desktops/spaces background to a random unsplash photo.</bitbar.desc>

echo '|templateImage=iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAABG2lUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS41LjAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIi8+CiA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgo8P3hwYWNrZXQgZW5kPSJyIj8+Gkqr6gAAAYVpQ0NQc1JHQiBJRUM2MTk2Ni0yLjEAACiRdZG5SwNBFIe/HBLxwMIgFhYpgoWoaJSgnQfigUiIEYzabNYcQhKX3RURSwvbFBYe2BjExlo78R8QBEGtRLAVCwUbCeubRIiIecPMfPOb997MvAH3YlbPWd4+yOVtMzo5FliMLwV8z3hpA+rwa7pljEYis9S0z3tcar7rUblq+/1rjatJSwdXvfCIbpi28JTw3KZtKN4X9usZbVX4XLjblAsKPyo9UeFXxekyu1VOvxmLjgv7hQPpX5z4xXrGzAkPCgdz2Q395z7qJU3J/MK80qV3YBFlkjECTDPBOGH6GZYxTA8hemVFjfhQOX6OdYnVZTTYwmSNNBlsukXdkOxJmVOiJ6VlxUNM/cHf2lqpgVDlhKYZ+ZgXx/noAt8RlHYd5+vEcUpF8Ehdrveq8et7MPQmeqGqBY+hZQcurqpa4hQuC9D+ZGimVpY80t2pFLyfQXMcWm+hYblSt599ig8Q24bZGzg4hE7xb1n5BqImZw3P0BJqAAAACXBIWXMAAAsTAAALEwEAmpwYAAABAUlEQVQ4je3Sr0pEQRzF8c/6B5vBYNiiWXwGg4r4GgYRBJvZF7BZLLJZg4Jlg6KCybL6KiaFVXcNd4TLMDN7ZQXLHvjB5cxvvnO4HCb6b63iBa/RXGFxHPADhpm5/w2ojWP08IxBATwIO71wp10CXxRAo+a8DmpF4D5mEw/e4ibsb2EzsdPHXC5xKsleOFvGUvjez+xmFS9eBr+DrzCd4F2PA95RVS72V7BbAk+VXsE7FhL+fDhrrDjBafCfat5j8M5KiUeBP7GmasO2qhEtrKv+dxYc122Q8D5woqrcT90OMJO4O51L3E2kaDrdOih+9RBv2Egkz2mIOxw13J/oj/QNH2aDYhPXbpMAAAAASUVORK5CYII='
echo '---'
echo 'Next Image | refresh=true'

curl -L --silent -o /tmp/.background.png "https://unsplash.it/2560/1600/?random"

sqlite3 ~/Library/Application\ Support/Dock/desktoppicture.db "update data set value = '/tmp/.background.png'"

killall Dock
