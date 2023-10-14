#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" bash 
#^added homebrew install locations (both pre-M1 and M1) to PATH

#  <xbar.title>Night Shift Toggler</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Anderson</xbar.author>
#  <xbar.author.github>andersonaddo</xbar.author.github>
#  <xbar.desc>A quick xbar tool to toggle MacOS Night Shift with less clicks. Check out https://github.com/andersonaddo/xbar-macos-nightshift-toggler for important installation instructions!</xbar.desc>
#  <xbar.dependencies>nightlight (Homebrew)</xbar.dependencies>
#  <xbar.image>https://raw.githubusercontent.com/andersonaddo/xbar-macos-nightshift-toggler/main/xbarScreenshot.png</xbar.image>
#  <xbar.abouturl>https://github.com/andersonaddo/xbar-macos-nightshift-toggler</xbar.abouturl>

#icons (144dpi, each about 34 x 34ppx)
nightshift_off_image="iVBORw0KGgoAAAANSUhEUgAAACIAAAAiCAYAAAA6RwvCAAABcGlDQ1BpY2MAACiRdZG/S8NAFMe/bdWKVjooRcQhQxWHFkRBHbUOXYqUWsGqS3JNWiFJwyVFiqvg4lBwEF38Nfgf6Cq4KgiCIoi4uftrkRLfNYUWaS+8vA/fu+/j3TvAn9KZYXfNA4bp8EwyIa3m1qTgO3oQoZhFRGa2tZBOp9Bx/TzCJ/JDXNTqfK7t6s+rNgN8vcQzzOIOMXWD1JZjCd4jHmJFOU98Qhzj1CDxrdAVj98EFzz+EsyzmUXAL2pKhRZWWpgVuUE8QRw19DJr9CNuElLNlWXKIxSjsJFBEglIUFDGJnQ4iFM2aWbtfZN13xJK5GH0t1ABJ0cBRfLGSC1TVZWyRrpKn46KmPv/edra9JRXPZQAul9d93MMCO4Dtarr/p66bu0MCLwA12bTX6I5zX2TXm1q0WMgvANc3jQ15QC42gWGny2Zy3UpQOHXNODjAhjIAYP3QN+6N6vGPs6fgOw2PdEdcHgEjNP58MYfJEJoGstSxXwAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAFuSURBVFhH7VbLDcIwDG0pLADDAetwYQ46Agd2QOKGGIMJoASnapAbkviTCipEJUSrPj+/+FcXxa9cxpiV/X39PCCivXKFTHIJhrL/C/EjGY2Iyz3873LDDxwTqpbKmBOvAGdlWd41gqwIsGucLfAEfaZqpEaObxoRnc1LBDxjzh5lNCIWhU8TOwklEEW2Ao5HDJ8UknISmx1aweL2BQGX1ACjipKKIOs9ODmgbiJvWaQakOd543PA+xnGaHyQNp6IY6LtVWLYxYrrgipICdYdqC3W7lP+lnMyTEJApKiWlkbcNULfbPi4UsOU/Vp+iDmyZfLpYX6OA+170rbvVChrAfirsyFWxLOQWwYH53NypBqzl7EWBbtYA2mwX9I3e2rGqL6+n1wDUqtib7OShjqAb7qDBalSAw1vVtKixs4q9IA5e4I4k7WGvEcJqP2j28qwmAGCG6CghHC9ciLC5crC/YX44cvpBse1zsrJ2Iyf0Qh4EHoZebEAAAAASUVORK5CYII="
nightshift_on_image="iVBORw0KGgoAAAANSUhEUgAAACIAAAAiCAYAAAA6RwvCAAABcGlDQ1BpY2MAACiRdZG/S8NAFMe/bdWKVjooIuKQoRaHFkRRHLUOXYqUWsGqS3JNWiFJwyVFiqvg4lBwEF38Nfgf6Cq4KgiCIoi4uftrkRLfNYUWaS+8vA/fu+/j3TvAn9KZYXfNA4bp8EwyIa3m1qTgO3owTDGDqMxsayGdTqHj+nmET+SHuKjV+Vzb1Z9XbQb4eolnmcUdYuoGqS3HErxHPMSKcp74hDjGqUHiW6ErHr8JLnj8JZhnM4uAX9SUCi2stDArcoN4gjhi6GXW6EfcJKSaK8uURynGYCODJBKQoKCMTehwEKds0sza+ybrviWUyMPob6ECTo4CiuSNkVqmqipljXSVPh0VMff/87S16SmveigBdL+67uc4ENwHalXX/T113doZEHgBrs2mv0RzmvsmvdrUIsdAeAe4vGlqygFwtQuMPFsyl+tSgMKvacDHBTCQAwbvgb51b1aNfZw/AdlteqI74PAIiNL58MYfQnxoKBbnYuAAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAFdSURBVFhH7Va7DQIxDOWAIZgACdGA2IMCMQKMAfPcbYTEEDSUCEh0CXKCHb/kmityDSjxe35+zm8yqV91IO1Aoxn0MR+NacynYex8Li5JGpN5AZqYEtwUqS6KOQIYJAag6a3d2sqk6jCWPsrzmN+NhBNbQwVordBEIVwlrdHyFs0jQt5FzCFooXGwrUGs1IjjeY0TcSQ3Z1H8aIT8tUazsKhcB0pxj8aRcQox1l2J9achbRCwZz9ucl1EfnIUI39bZou2CFC9OnJIXGxHKuxy8bSQYNeUXHD+HhqCtYLmyDqILz2aFBFA8VI8tGuQZIHN0SsOwYtCUi4MdZHDJx3hxJjqVoToYGNc3O9VZmKWNianGHGxIn1NJcrFB47EYN/bkhea28ozzhlujGvNjfZQOnw4cczYS8DfkXVmH7v71OGkkSgHW/4L3xCuCelTE8Ac+Q+C3+Xia3x1gDrwBafnzv3RUEgdAAAAAElFTkSuQmCC"

#Nightshift modes
OFF=0
ON=1
NOT_SUPPORTED=2

nightshift_mode=$NOT_SUPPORTED

get_nighshift_state() {
  local nightshift_mode_raw=$(nightlight status)

  if [[ $nightshift_mode_raw == *"on"* ]]; then
    nightshift_mode=$ON
  elif [[ $nightshift_mode_raw == *"off"* ]]; then
    nightshift_mode=$OFF
  else
    nightshift_mode=$NOT_SUPPORTED
  fi
}

toggle_nightshift_state() {
  nightlight toggle
}

main() {
  get_nighshift_state

  if [[ $nightshift_mode == $NOT_SUPPORTED ]]; then
    echo "⚠️"
    echo "---"
    echo "Please install nightlight: brew install smudge/smudge/nightlight"
    exit 0
  fi

  if [[ $nightshift_mode == $ON ]]; then
    echo "| templateImage=$nightshift_on_image"
    echo "---"
    echo "Night Shift is on."
  else
    echo "| templateImage=$nightshift_off_image"
    echo "---"
    echo "Night Shift is off."
  fi

  echo "Toggle Night Shift | bash='$0' param1=toggle_mode terminal=false refresh=true"
  echo "Refresh | refresh=true"
}

if [ "$1" == "toggle_mode" ]; then
  toggle_nightshift_state
else
  main
fi
