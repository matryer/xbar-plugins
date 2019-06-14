#!/bin/bash

# Shows infos on artist, album, track and more in cmus or pianobar.
#
# Special thanks to Google for providing the open-source icons: https://github.com/google/material-design-icons
# and to mcchrish and alekseysotnikov for their helpful existing BitBar scripts
#
# metadata
# <bitbar.title>Music Controls - Next Track Button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastián Barschkis</bitbar.author>
# <bitbar.author.github>sebbas</bitbar.author.github>
# <bitbar.desc>Plays the next track in cmus, iTunes, Spotify or pianobar.</bitbar.desc>
# <bitbar.dependencies>perl</bitbar.dependencies>
# <bitbar.image>https://raw.githubusercontent.com/sebbas/music-controls-bitbar/master/music-controls-screenshot.png</bitbar.image>
# <bitbar.abouturl>http://github.com/sebbas/music-controls-bitbar</bitbar.abouturl>

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
export LC_CTYPE="UTF-8"

pianobar_ctlfile="$HOME/.config/pianobar/ctl"
pianobar_stationsfile="$HOME/.config/pianobar/stations"
pianobar_playingfile="$HOME/.config/pianobar/playing"
cmus_cachefile="$HOME/.config/cmus/cache"

display_length=40

# Enables features that are still under development
EXPERIMENTAL_MODE=0

NONE="none"
CMUS="cmus"
ITUNES="iTunes"
SPOTIFY="Spotify"
PIANOBAR="pianobar"

list_icon_light="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAONJREFUaAXtlkEKAjEMRUdBvEZv48o7eSddeBrnGC41XRQkpFBsMzT6CqE07fxJXsNkloUBAQhAAAIQgAAE/pfAriH1V8OZniMtMVT199WdIBskEOSiCBMCP0ug5RtMH/C8fvqAJ120IQCBMQRyH+gxHcVNHEk7Pdc9wVtNMPueYhexo5j78EigaK4S/ck7g/Kyb2cdn6VzlUNJH5x1bSWQfZuWVQ+cWgLFv4p4c1nN+i906CHk/WwhrefQJXQXasmb3Cj9T/IPET2PEt5KJycQplwsKKHKxUoAHwQgAAEIzEPgDVrLdHaFYNcHAAAAAElFTkSuQmCC"
list_icon_dark="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAO5JREFUaAXtlsENwjAMRVskxBrZhhM7sRMcmIaOwbH8SOnlKyWA4ypRv6UotRM79qvVdBgkIiACIiACIiACIrBfAmOp9BlS2mNZHyEW/4PFuQVfFdDCW1AOIrBnAsVvsO4B5/bQPeAMWOFFQASsBOI9YBE+H7HuGIHtbrol+ejLiaV4L8xXjBOvV9fTgX9PnBAFmqCfeU9VnQ78WeVkVgLcYA+8t0l9pYBo3q6tLGQ+FLAsTXj4uq1a/Rc6WiC5+i6YM3PXLfRAQcGVXK3gRP4J/VIr9iZxUgF9tEuOCArop11yBcgmAiIgAiLQFIE3waT5BVt6JEkAAAAASUVORK5CYII="

note_icon_light="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAJNJREFUSA1jYBgFBEKAkYA8Nun/2ASBYljNYsKhmGrCQ98CFgrCAmuYo5s39INo1AfocYrBJxREEkAdm4H4DRCDcvBxIKYaEAWa9AyIQQZjwxRbtACHwTDLKLbgNTUswBcHv/A4ERR0RAF8FuzGY8IePHJES4kBVWKLZJAYKAFQBUgCTdkKxG+heBOQBomNAuqFAACH8ifJWZDB1wAAAABJRU5ErkJggg=="
note_icon_dark="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAALFJREFUSA1jYBgFBEKAkYA8hvR/IMAQBAowAgE2cSZsgtQUG/oWsJAbHLjCHN28oR9Eoz5Aj1MMPt4gAmZaCSDeDMRvgBgEjmOYQK4A0DBRIH4GMhUbINdcuD6goQuwGQwTgyskwMBaQIH0AA16DaREcOmnRkb7hctwoPgzPHIoUvgieTeKSlTOHlQuGTxgEIkBMbZIBomJkmEkphagQZJAvBWI30LxJpAYpspREQpCAACt5Y+AqibnJQAAAABJRU5ErkJggg=="

person_icon_light="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAANFJREFUSA1jYBgFQyUEJIAO3QzEb6AYxAaJUQWIAk15BsT/0fBzIF8QiCkGC4AmoBsO4y+k2HSgAa/xWACSwwuY8MpCJH/hUYNPDqyNGAt247FgDx45oqXEgCqxRTJIDJQAqAIkgaZsBeK3ULwJSIPERgBgJOBHQ6B8KBCbATGILQTEIPAOiM8D8SkgXg1lAynigT1QKcgAWIYiRIPUOgAxUSAcqOoPEBMyFF0epAeklyB4ClSBrplYPkgvCsAWByDDKAEoZhKTkymxbFTvIAgBAOvwVavvg1ziAAAAAElFTkSuQmCC"
person_icon_dark="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAOBJREFUSA3tk0EOwUAUhjtuQFIpTmFjyyWk12NtR6w4gU1PoRbFAUQyvpEuxmhmXqMLpC/5ktd5//x/+0QUtfUTG9BaJ7CBc4npk0ZeHqMYcnDrxEH34xBMFq6z9bxsIqCwDN22CAV0QgLmN4/GN3tekwTsPAF7z0w2Yid9qPqRzVkscwmoMBrAFi4la3MWuPYnY+X7DtYwZj6HCZi+B6aukMEBVkop08sL4ylkIC2jnYkSEKZwlzpbOnMndUPeVoToiGjoCoXPOesa2dqqAG0L6vYEvHhK/sl1M1r9l23gARRQLiEZXQo0AAAAAElFTkSuQmCC"

album_icon_light="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAW1JREFUSA3Vlc0uBUEQhYdIxDu4icWNBQsPwRWxsmLjSRCPI5ZWVvcF7PxGsBmJJWuRCL5Dd+vbXWN6sOAkJ1VTdaqqZ7pnpqr+O8ZabmCS/BpchQuwD5/hLTyGh/AAPsHOWKGihq8tlEbaTthG3dY4zr+gV00RujaPB7UOGbAErSYu6uKrVj0C4k2eInoNp0N21BlyeeFC89il0XS4usObhY8h4pxNrLXaB+KLqdjF7htq1CvDPhFrwHqm/AxsNNSoV4YrIumA00yVB6RJ69TrHePewVrP/ijKyz2Hfh98KtUoHnpNeBVWJyCFVhajRCO9pausR3QWd2/wpWl8RHHNniFU4VebrFzaXNfqleE7x1RH2BoQjmn8ounLeQN72eiPwBBz6XJzWOvdUFovWh+aX9hf/1RoYootAtZtl8RUW4QdVCUNvUbHcreocyRaxq+hb9JkpZHWRLzJliD9Zc44UY09gT/6Zbpef9y8Aedy0zAzcRrpAAAAAElFTkSuQmCC"
album_icon_dark="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAYRJREFUSA3VlbFKA0EQhu9EEN/BgEWw0IAPoQnBykqbPEkUnyelVaq8gIUQE0WizQmWWosgOb/B3ctmd25zOSx04WdmZ/5/Zvf2bi9J/vtIYxvI83yL/Ck4AYegCb7ACxiDIbhO0/QTu96geBdkYNUQTnet6gguVlX18nPRVGpSo7jbK94EZhvIauoO0bbdnRSHTGKbxBPYcQmOP8J/MPMD7LGTc91XJnsc/IcbTGjQK1n2O/GjJTITiYG3Ek3P54tgUEI+C8gmAP+8RDMINBBnCnkSEL0Amomim1nahnWw2rO/cfKyy3tgz8GmljgmWNTatCzs3PGtm1vH2CocoYY8VqY9oqnXIJiimwJ/qI/oNlAnSQtl7JAl11J0YS0K1XlN5RXWRvGauh+a3JzPoKGsSEIj8Ghy+9jg2zA5+dCa6g3LUn79qjBNF4YmfW3PFWP9RaWIR7HLigUtTS65q0jJMIWgAzJbIWKF0wkr/ESKQ9YICP1f5q7hZdg7UP+XaQr9ffMNJgYZyQXHjjUAAAAASUVORK5CYII="

station_icon_light="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAARxJREFUSA1jYBikgA/orlwgvgrEItR0oy7QsBlA/AWI/0NxDJCmCLABdUcC8WEghhn6FsjuB2J1ICYbyAJ1tgDxayAGGfwPiPcCMcgykKVkAUagLlcg3gDEf4EYZPBzIG4HYnkgpgjYA3XfAmKQoZTga0D9/kCMAR4CRSgxGFnvYwzTkQzHJkeKGMwisB4mUnSSo5YaFqC4GN0R1LAA3UwUPihZwgDIJdQEYLNp7gNcFtQDvSIJxU3U8hYssiqxGFgBFIPJE0tjGAPTKIEhw8AAEoPJE0uDjcEWRMgRD7MLmxhMDi+NzYIkLDoSsYiRLITs9WagbmkoBrGR5YhlYzjgCZkGYbPwEYbpQAFQEXudCpaAzMBaXAPFhyAAABRSlmFQeETmAAAAAElFTkSuQmCC"
station_icon_dark="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAATBJREFUSA3NlL9qAkEQh28tLFNZKGJSWvkMVtapAzbaBvukCiQBy7R5jLyAVZ7BIHaiYBHSpU0u38rNsQ4reDsGHPjY+XPzm91b7rLsHC3P8wuYwBwaJ9sjYj14hW8QG5oGoFKHG3gXRdYveIFusjjNHXiGT/D2CzPww+pJwjQ6GMAb/IC3LUzhKklUmhDowxKs9oHAteg6cUiu8C8lNq4b51xnT0O2vZdMCLROLUGjUot5gN6xnm4eoAV1HF5yrouWmEveaf/7CQ4NeGD3rYJHy0nKXrks1vsyWTjk7oL6Ua7WyIKupi5Sawb1o1zRiL2i8uLlIdZYLigfdmMDxpHHR5FctZQ69xNxu8D7lU2ml0dHYUOyLQXjuuYz2P04w1d0i+jCKOzbvcbkBDpnIvEHdSy7CinTWbUAAAAASUVORK5CYII="

star_icon_light="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAATJJREFUSA3tlMFKQkEUhq0WkUIhbaSNhLsWbsKde6GXEAIfwbVPkDsTV7XMB+kJIiFcFQiStYxyIfb9cEcmY65nJFd24GOOc/45Z2bOeDOZbbMKBxYbs1sy32wq+zGJvxIOrUV2rUJ0l7Cf0IhYZ5LuoHqBecIzo+b+zC7I5JK7sWbJvhcQaXdHcAIlaMEp+FbgxwAOQEWnkGpNok8wgRm4nVpHrXmDISjXL1PDr8CaMKRrkyO1P3oh657A/LrUwM+I03ygNTUd3cLO8dSP0FW4+TGa8mJVpNM3FLiLzPlDPjIUkGYt07t317BqLIYqpH2LqkuL9LL0jIV835a1fizo94i4nd/jn3lK+Zpz8a4XM7uPKF+hHlihP5Ni7/AQ0ASns0Q6YPnu59FdQw7+Lf4GvgGCKIb8c6+V/AAAAABJRU5ErkJggg=="
star_icon_dark="iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAT9JREFUSA3tlLFqQjEUho0UCgqFtg8gXR1cpFt3oS8hCD6B+hi6WXHSUR/EJygdxKkFQVqtOIhd5PqlTeRqc825Fqd64CPJyf+f5CaXJBL/KoIguNec7KMp3oXOSRag8C18Ga6kiySlQnQluDSUY/j8Unat4A1svNJRfqdQQbFHWznUFiT2C5fI7E6f842h4tBV0U3Jz+ETFkqpwKH7SSGuwRA+YA1xQ3umMILar4VIJqEOf40GBaLvh8kyHPsFsr+LBQqwAmksEYoufXt0GPKg78MXEwS5rTFOB2PfV535XpyaO1rMY8EC4x2TdEDhO0FxK8lE1T30Fj3smdaM6wbdD8e+Njzn7rO1tt0e7QCyVqn7JkfzHS07J26xvcA7FF0m8voBLMIMnl2ayByGFDTB++6juYYnSEcWPE8cOoENzGJkOFk6QRwAAAAASUVORK5CYII="

# Trigger next action in library section (e.g. play track in cmus)
if [[ "$1" = 'cmus_song' ]]; then
  f=$(echo -e "player-play $2")
  cmus-remote -C "$f"
  exit
fi
if [[ "$1" = 'itunes_next' ]]; then
  osascript -e 'tell application "iTunes" to next track'
  exit
fi
if [[ "$1" = 'spotify_next' ]]; then
  osascript -e 'tell application "Spotify" to next track'
  exit
fi
if [[ "$1" = 'pianobar_station' ]]; then
  echo -ne "\ns$2\n" > "$pianobar_ctlfile"
  exit
fi

# Ensure that pianobar fifo config file exists
if [ ! -e "$pianobar_ctlfile" ]; then
  mkfifo "$pianobar_ctlfile"
fi

BitBarDarkMode=${BitBarDarkMode}
current_source="$NONE"

# Get pid of music apps to see if they are currently running
cmus_pid=$(pgrep -x "$CMUS")
itunes_pid=$(pgrep -x "$ITUNES")
spotify_pid=$(pgrep -x "$SPOTIFY")
pianobar_pid=$(pgrep -x "$PIANOBAR")

# Keep track of music source
# Reorder items in for -loop to your liking to change order of precendece
# (i.e. if available, left-most audio source will be used first)
for s in "$CMUS" "$ITUNES" "$SPOTIFY" "$PIANOBAR"; do
  if [[ $s = "$CMUS" && $cmus_pid ]]; then
    current_source="$CMUS"
    break
  elif [[ $s = "$ITUNES" && $itunes_pid ]]; then
    current_source="$ITUNES"
    break
  elif [[ $s = "$SPOTIFY" && $spotify_pid ]]; then
    current_source="$SPOTIFY"
    break
  elif [[ $s = "$PIANOBAR" && $pianobar_pid ]]; then
    current_source="$PIANOBAR"
    break
  fi
done

# Do not display menu icon if no audio source is active
if [[ $current_source = "$NONE" ]]; then
  exit
fi

function playing_info
{
  if [[ $current_source = "$CMUS" ]]; then
    track=$(cmus-remote -C "format_print %{title}")
    artist=$(cmus-remote -C "format_print %{artist}")
    album=$(cmus-remote -C "format_print %{album}")
  elif [[ $current_source = "$ITUNES" ]]; then
    track=$(osascript -e 'try' -e 'tell application "iTunes" to name of current track as string' -e 'on error errText' -e '""' -e 'end try');
    artist=$(osascript -e 'try' -e 'tell application "iTunes" to artist of current track as string' -e 'on error errText' -e '""' -e 'end try');
    album=$(osascript -e 'try' -e 'tell application "iTunes" to album of current track as string' -e 'on error errText' -e '""' -e 'end try');
  elif [[ $current_source = "$PIANOBAR" ]]; then
    # First check if 'playing' file exists
    if [ -f "$pianobar_playingfile" ]; then
      IFS=$'\n' read -d '' -r -a lines < "$pianobar_playingfile"
      artist="${lines[0]}"
      track="${lines[1]}"
      album="${lines[2]}"
      station="${lines[3]}"
      rating="${lines[4]}"
    fi
  fi

  if [[ $track = "" || $artist = "" || $album = "" ]]; then
    echo "Nothing playing in $current_source"
    return 1
  else
    echo "Now playing in $current_source"
  fi

  # Get icon for current track, artist, album based on dark / light mode
  if [[ "$BitBarDarkMode" ]]; then
    track_icon=$note_icon_dark
    artist_icon=$person_icon_dark
    album_icon=$album_icon_dark
    station_icon=$station_icon_dark
    star_icon=$star_icon_dark
  else
    track_icon=$note_icon_light
    artist_icon=$person_icon_light
    album_icon=$album_icon_light
    station_icon=$station_icon_light
    star_icon=$star_icon_light
  fi

  echo "$track | image=$track_icon length=$display_length"
  echo "$artist | image=$artist_icon length=$display_length"
  echo "$album | image=$album_icon length=$display_length"

  if [[ $current_source = "$PIANOBAR" ]]; then
    echo "$station | image=$station_icon"

    # Experimental mode: TODO (sebbas): Cover art
    cover_url="${lines[5]}"
    if [[ $EXPERIMENTAL_MODE = 1 && $cover_url != "" ]]; then
      echo "---"
      cover_url="${lines[5]}"
      highres_pattern="500W_500H"
      lowres_pattern="130W_130H"
      thumb_url=${cover_url/$highres_pattern/$lowres_pattern}
      base64=$(curl -s "$thumb_url" | openssl base64 | tr -d '\n')
      echo " | image=$base64 length=$display_length trim=false"
    fi

    if [[ "$rating" = 1 ]]; then
      echo "Favorite Pandora song | image=$star_icon length=$display_length"
    fi
  fi
  return 0
}

function library_info
{
  if [[ $current_source = "$CMUS" ]]; then
    echo "My cmus library"

    exif=$(xxd -p "$cmus_cachefile" | tr -d '\n' | awk '{print $1"0"}' | perl -ne '$_.="0000";@exif=$_=~/(?<=f{112})(.*?)(?=0000)/g;print join"\n",@exif' | awk '{print "0066696c6500"$1}')

    # Experimental mode: TODO (sebbas): Album tag
    if [[ $EXPERIMENTAL_MODE = 1 ]]; then
      data=$(echo "$exif" | perl -ne '$_=~s/\n/00\n/g;@a=$_=~/(?<=0066696c6500)(.*?)(?=00)|(?<=0061727469737400)(.*?)(?=00)|(?<=00616c62756d00)(.*?)(?=00)|(?<=007469746c6500)(.*?)(?=00)/g; print "@a\n"; ')
      sorted=$(echo "$data" | sort -f -k 2)
      echo -e "$sorted" | perl -ne '$display_length=40; @a=split(/\s+/,$_); $a[0]=~s/\G..\K(?=.)/\\x/sg; $file="\\x".$a[0]; $artist=$a[1]; $album=$a[2]; $track=$a[3]; $file.="0" if(length($file) % 2 == 1); $artist.="0" if(length($artist) % 2 == 1); $album.="0" if(length($album) % 2 == 1); $track.="0" if(length($track) % 2 == 1); use Encode; binmode STDOUT, ":utf8"; $artist=Encode::decode("UTF-8", pack(q{H*}, $artist)); $album=Encode::decode("UTF-8", pack(q{H*}, $album)); $track=Encode::decode("UTF-8", pack(q{H*}, $track)); next if ($file eq "" || $artist eq "" || $album eq "" || $track eq ""); print "$artist | length=$display_length\n" if("$old_artist" ne "$artist"); print "--$album\n" if ("$old_album" ne "$album"); print "--$track | bash='"'$0'"' param1=\"cmus_song\" param2=\"$file\" terminal=false refresh=false length=$display_length\n"; $old_artist=$artist; $old_album=$album;'

    else
      data=$(echo "$exif" | perl -ne '$_=~s/\n/00\n/g;@a=$_=~/(?<=0066696c6500)(.*?)(?=00)|(?<=0061727469737400)(.*?)(?=00)|(?<=007469746c6500)(.*?)(?=00)/g; print "@a\n"; ')
      sorted=$(echo "$data" | sort -f -k 2)
      echo -e "$sorted" | perl -ne '$display_length=30; @a=split(/\s+/,$_); $a[0]=~s/\G..\K(?=.)/\\x/sg; $file="\\x".$a[0]; $artist=$a[1]; $track=$a[2]; $file.="0" if(length($file) % 2 == 1); $artist.="0" if(length($artist) % 2 == 1); $track.="0" if(length($track) % 2 == 1); use Encode; binmode STDOUT, ":utf8"; $artist=Encode::decode("UTF-8", pack(q{H*}, $artist)); $track=Encode::decode("UTF-8", pack(q{H*}, $track)); next if ($file eq "" || $artist eq "" || $track eq ""); print "$artist | length=$display_length\n" if("$oa" ne "$artist"); print "--$track | bash='"'$0'"' param1=\"cmus_song\" param2=\"$file\" terminal=false refresh=false length=$display_length\n"; $oa=$artist;'
    fi

  elif [[ $current_source = "$PIANOBAR" ]]; then
    echo "My Pandora stations"
    while read -r line; do

      # Split station number and name from each other, closing bracket is delimiter
      station_num="$(sed 's/).*//' <<< "$line")"
      station_name="$(sed 's/^[^)]*)//' <<< "$line")"

      # Remove leading and trailing white space
      station_num=$(echo "$station_num" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
      station_name=$(echo "$station_name" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')

      # Checked items are still beta, leaving this in anyways
      if [[ "$station_name" = "$station" ]]; then
        echo -e "$station_name | bash='$0' param1='pianobar_station' param2='$station_num' terminal=false refresh=false checked=true length=$display_length"
      else
        echo -e "$station_name | bash='$0' param1='pianobar_station' param2='$station_num' terminal=false refresh=false length=$display_length"
      fi
    done < "$pianobar_stationsfile"
  fi
}

# Set menu icon based on dark mode setup and display info sections
if [[ "$BitBarDarkMode" ]]; then
  echo "| image=$list_icon_dark"
  echo "---"
else
  echo "| image=$list_icon_light"
  echo "---"
fi

if playing_info; then
  echo "---"
  library_info
fi
