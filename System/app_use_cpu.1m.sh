#!/usr/bin/env bash
#  <xbar.title>CPU Google Chrome</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Alexandre Espinosa Menor</xbar.author>
#  <xbar.author.github>alexandregz</xbar.author.github>
#  <xbar.image>https://imgur.com/a/ShPdCbi.png</xbar.image>
#  <xbar.desc>App CPU usage, by default Google Chrome</xbar.desc>
#  <xbar.dependencies>bash, bc</xbar.dependencies>
#  <xbar.var>string(VAR_APP="Google Chrome.app"): App name, test with pgrep to check if is correct</xbar.var>
#  <xbar.var>number(VAR_RAM=24): Your amount of RAM in GBs</xbar.var>

#  Another App example, for example if you use UTM: string(VAR_APP="QEMU")

# Google Chrome icon
ICON="iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAEDmlDQ1BrQ0dDb2xvclNwYWNlR2VuZXJpY1JHQgAAOI2NVV1oHFUUPpu5syskzoPUpqaSDv41lLRsUtGE2uj+ZbNt3CyTbLRBkMns3Z1pJjPj/KRpKT4UQRDBqOCT4P9bwSchaqvtiy2itFCiBIMo+ND6R6HSFwnruTOzu5O4a73L3PnmnO9+595z7t4LkLgsW5beJQIsGq4t5dPis8fmxMQ6dMF90A190C0rjpUqlSYBG+PCv9rt7yDG3tf2t/f/Z+uuUEcBiN2F2Kw4yiLiZQD+FcWyXYAEQfvICddi+AnEO2ycIOISw7UAVxieD/Cyz5mRMohfRSwoqoz+xNuIB+cj9loEB3Pw2448NaitKSLLRck2q5pOI9O9g/t/tkXda8Tbg0+PszB9FN8DuPaXKnKW4YcQn1Xk3HSIry5ps8UQ/2W5aQnxIwBdu7yFcgrxPsRjVXu8HOh0qao30cArp9SZZxDfg3h1wTzKxu5E/LUxX5wKdX5SnAzmDx4A4OIqLbB69yMesE1pKojLjVdoNsfyiPi45hZmAn3uLWdpOtfQOaVmikEs7ovj8hFWpz7EV6mel0L9Xy23FMYlPYZenAx0yDB1/PX6dledmQjikjkXCxqMJS9WtfFCyH9XtSekEF+2dH+P4tzITduTygGfv58a5VCTH5PtXD7EFZiNyUDBhHnsFTBgE0SQIA9pfFtgo6cKGuhooeilaKH41eDs38Ip+f4At1Rq/sjr6NEwQqb/I/DQqsLvaFUjvAx+eWirddAJZnAj1DFJL0mSg/gcIpPkMBkhoyCSJ8lTZIxk0TpKDjXHliJzZPO50dR5ASNSnzeLvIvod0HG/mdkmOC0z8VKnzcQ2M/Yz2vKldduXjp9bleLu0ZWn7vWc+l0JGcaai10yNrUnXLP/8Jf59ewX+c3Wgz+B34Df+vbVrc16zTMVgp9um9bxEfzPU5kPqUtVWxhs6OiWTVW+gIfywB9uXi7CGcGW/zk98k/kmvJ95IfJn/j3uQ+4c5zn3Kfcd+AyF3gLnJfcl9xH3OfR2rUee80a+6vo7EK5mmXUdyfQlrYLTwoZIU9wsPCZEtP6BWGhAlhL3p2N6sTjRdduwbHsG9kq32sgBepc+xurLPW4T9URpYGJ3ym4+8zA05u44QjST8ZIoVtu3qE7fWmdn5LPdqvgcZz8Ww8BWJ8X3w0PhQ/wnCDGd+LvlHs8dRy6bLLDuKMaZ20tZrqisPJ5ONiCq8yKhYM5cCgKOu66Lsc0aYOtZdo5QCwezI4wm9J/v0X23mlZXOfBjj8Jzv3WrY5D+CsA9D7aMs2gGfjve8ArD6mePZSeCfEYt8CONWDw8FXTxrPqx/r9Vt4biXeANh8vV7/+/16ffMD1N8AuKD/A/8leAvFY9bLAAAAOGVYSWZNTQAqAAAACAABh2kABAAAAAEAAAAaAAAAAAACoAIABAAAAAEAAAAQoAMABAAAAAEAAAAQAAAAABedU8gAAACUSURBVDgRpZABDoAwCAM34/9f6h90kJXUDpJFTcywaw+kt/y5c7l11VWogmWOAbthwDx74OvDuTS8BsREvFtMGwOkqgaI76H5VgEw0f9rnKyFed6/7v7swMEKeNG5dVUroPKVegawKbALDpZLNFM2ukJSz8ktpOaAwsLKFxwIwyjS0afeeAcMY0BWhzcKcWEa7b74H8fWGQBe8+8HAAAAAElFTkSuQmCC"

if [ ${VAR_APP} != "Google Chrome.app" ]
then
    # CPU icon
    ICON="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAAAsTAAALEwEAmpwYAAAAXElEQVR4nO2WsQoAIAhE7/9/+rU0RUtmlHQPHByUE+RUMkWgRzS3gLqQFHUEMBTtTjDrxdMCRjIFXGmAd0DeAdkHZB8gYhwcMKLat4Bv/oFnYfENT5+c7wUYnaIBHMrfIYwepnEAAAAASUVORK5CYII="
fi

SUM_TOTAL=$(ps -xm -o %mem,rss,comm -p $(pgrep ${VAR_APP})|awk '{print $1}' |awk '{SUM += $1} END { print SUM }')

# sum total default value to 0.0
COMMAND_GB=$(echo "${SUM_TOTAL:-0.0}*${VAR_RAM}/100" | bc -l | xargs printf "%.2f GB")

echo "${VAR_APP} $COMMAND_GB | templateImage=$ICON"
