#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Active GPU</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Eric Ripa</bitbar.author>
# <bitbar.author.github>eripa</bitbar.author.github>
# <bitbar.desc>Displays an image based on the active GPU in multi-GPU machines, such as the MacBook Pro</bitbar.desc>
# <bitbar.image>http://i.imgur.com/v2MuPOi.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

from __future__ import print_function
import subprocess
import plistlib

# Base64 encoded icon for displaying the built-in GPU
BUILTIN_GPU_ICON = ('iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJ'
                    'QAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD'
                    '0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1'
                    'sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAg'
                    'ICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0ia'
                    'HR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj'
                    '4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkR'
                    'GPgo8L3g6eG1wbWV0YT4KTMInWQAAA3JJREFUWAntlsmKFEEQhtt9X8cVlZHxKvgQend5HG+efAof'
                    'QRS9ztw9DvgAIigqMrjv6/dV1+9kTVdXV40gDPjD3xkVGRkZERlZXaPRf3RXYFP3dOfsrLW/Oldvl'
                    'MlZWXblsZfJn1MMNqN/P2WuU72egLbi8Tu8CV9Anz0efTka5FF4HcYWsR9cMBRJ4hgLv8Ft0CCi/4'
                    'HsnIhu/NTjdz0BpVmf4v85bAsoRxnbHqGMTYZmoL10o9NjF5Vci9UQn094im3vwLK4dNhH3ofRHXg'
                    'CfoXx48bb4TN4FQ5u7DhibS/sxMoj8hYtwC3QIOJH2Tl76xH06JQ/w16Ioy5jN9CxVVmC89DMrYwB'
                    'tEG/VspXw2N4Eb6D8YU4DDa7lXDUiTgCDcQghtA1c1Doq/RdKcsfJ9vgeyYw2xyNWe6BizAvRucN0'
                    'DFQ/gCtzNtaqQ8rXfqup1aHZK8mDt3Il94teAMuwHvwJLQfhFnvgG5iUmGePS6DFwbg2vvwHNSnvt'
                    '3D5ET2rhyNVau/ZuEb2M29LQZ9FrppEriC/ADaV74IdZgqKVsdbYTPrp2vZX2qW4Hu1YCGbVBvD0k'
                    '38pZkQ8QKVsmKtAXkXJBgv9SK+HV0roFk3FDy4ObSDFyUhW4uluAp6LFZ9l31uLuWPSJtRAKOj/h2'
                    'nMC0CiWIOFm70P74CD1WbXRejiaaHkJsIL4byjzMqpB2yUjZIxL2h81qD1kVL4Kj1VJ2TXrINaWPU'
                    'maqiWkV8qgsdcqd89ZZYAVMKDbOJfuyOtFnr/h1nECMygk38XvGT4i8M14hG2RuxSLyGeiRaZ9eQ6'
                    'ywn19tLsGse13NjEbHGe0xK2cCDUwoGrPjBxcegm68DHV2Fx6Aa6uDqoJ+rdJl6DW/AK2UibVWBn0'
                    'nrJzH5GggYg6+gToeQitzGAp9lb4rZfnjZBtyVM4lIHUvoVfc90yCQpxAesmmd03pr5RbF04oOxT2'
                    'lu8ecRAm2EpR/Ng3Vkb4QjSoXujTQ22OvOK34bQPND9tr8FPcBCGBpSKOJ6Hjh5d/ES2Qg/rOYY/t'
                    '1O5E9N6aNoiN3KNfZAbZPOrT1DeIq/2MowtYj8MDUiv2Xge2fVtAflqELEdP/X4XU9AHovwE8VPCH'
                    '2srVCONraY9MPgDAq3/mcZSBv06zfRxsffVGjW2sHHtfHL+S8y+A3HDtV7aIFb0gAAAABJRU5ErkJ'
                    'ggg==')
PCIE_GPU_ICON = ('iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAA'
                 'FiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRv'
                 'YmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRm'
                 'PSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpE'
                 'ZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFk'
                 'b2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVu'
                 'dGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4K'
                 'TMInWQAABPxJREFUWAnt1GtoV2UcwPE5l9Oc5nJemmxG1MrKsqvpXEUFSVcJiwgTDCqpV5l0I2OYRYYS'
                 'kpoZNLB3hb3wRXQxIyJdKqhRWJGsbKTlyktecu7S93v+5zfOjpu92iDyB5//ec5znvM8v+dy/kVFp+M/'
                 'tgIDcvmWcG9dZ1rflnveb7f5xPpt4PxAJhIrMozysxiOdjRjOY4h2lDs2yjOde+Wmcw41KMMRr+vYH7A'
                 'UpLYgbFmQ+QTL9T2wa8rYsQhHkS5FaPhs0j0DMquXETUx328H/fZ66na+sx3uz6eSCg6iAcxeEf64Hg0'
                 '6OtrPqFIwKwte4buRw1+hwkPgQd/IGxn8n/Bw29dzNr6fFv7PISjcNUr0IzXYd2ASCg6KadyNtbDAc7G'
                 'c/gKjyHifQrfwQQm4C70FrbdCRNwYjMQsY6C472DJKE4rLHPI3iwAGfB1XAFWuDqWH4DzmoZmmCiL8Ov'
                 '0r8II7Z3BeUxsO1PaMRiVGIVXC37tX/L3SIS8jDPwyXYgknYDZNZCLfKJA5iJqbhWjTAgV6AbZ2U/2ub'
                 'cAD3oBbR1gksgm1/g+MasUCFu8yvn/t2TISDO7tR2A87WYIbcB9M2romVMPEL8AfsP411MG2l8K6n1EG'
                 'J+fRGAkjSShWZygVr8LML8JWXIZDmIUZiFltpHwm3FoTcRCthvEo6mHdl4i2u9I661/E3fBodCVUwk1v'
                 '4UuD4Gz2oArPYxsOw+3yoD6AEbCtidTAd+vxNY7Ctj6fBScxBHOwDrvhsyRMyJeNI3gqKRX+of2C5Cft'
                 '1cGnYzDW4gu040a4HS1wALfFetu6Mn5ltnWc6+HZ+xMfweQsdy1MsmdUxLZVUHaW5WiDHTt7t+1WnEAz'
                 'rsIrWISb4YptgO/4id+JVmTbLuT+Fjhx2/pOB0zK95IoSa8m5Aw8R09iB0zGTsfDQ/4D1sPYhWtggh/j'
                 'SsyHq2lCHuhPYdj2akRbJ/M0vkclquE43SJWysFteB0acTkOYDOG4ReYuPUzMQ3LYJ2fr1+XK1EFz571'
                 'V8C2dVgK6/ahAo7R7SuLRGxkuJxLcBAD4cq5Xa7GMzDRt1GKPXDlZsMvyWQexALMwRQ0wLO3F2PwEDbi'
                 'fMzHZNi/Y50yHGgb3IpvsBru9yrYsR0564cxFc52JZyYX5XXFTgHtr0Xtq3FSLyFdryJH+F4RrErIDvw'
                 '/NTDvV6TepzrB/AP7HZEfEhhJ/7GRNyB3iLamsDFuC3T0Gdu+4X4FcVxqCkn4Sp4wEzQpd6Pl3AensAx'
                 'lMHP2f8Ov8Am2MZnh9EGt8B+bGd77+1zCz6Bzzwuo/EZPCJGp6tjxCoV7gpLuIGbm+D+G3MxFiY5HM1o'
                 'QDYe4aYSngtX3AQHw200Oa9+HHF1Vdw+I59DUumsDTv9FuO8ISbgCN6Dg/hntw/xnGLRuTDZtbCNbeMd'
                 'E3gXrqL13sdzV99wxU6KqIyEvBo18Mz45TSiFlvhikVUU9gOn22CZ8O2cbV+c3rv1b7sczyMZOzYskJV'
                 'odJzZCIeuOlwWavgXnuuPHe2cd/r4KoYo/A5PDO2cxWGwlXKX92+1pR97IUJdeQPNXVJ+LA8LXvxP2cy'
                 'HMhD6/U4/NOMaKHgapTiBOzbL8sDnb/ah89tFxNykidFrJgHby6cbb9GJJAd1LrObEVajvOVfZSfVU9t'
                 'su17Kuf76KlNUvevf+e9vnn6wf9pBf4BZd5O0exv758AAAAASUVORK5CYII=')

def main():
    ''' Main function '''
    output = subprocess.check_output(["system_profiler", "-xml", "SPDisplaysDataType"])
    plist = plistlib.readPlistFromString(output)
    cards = [g for g in plist[0]['_items']]

    gpu_in_use = [x for x in cards if 'spdisplays_ndrvs' in x][0]
    if gpu_in_use['sppci_bus'] == 'spdisplays_builtin':
        template_image_icon = BUILTIN_GPU_ICON
    if gpu_in_use['sppci_bus'] == 'spdisplays_pcie_device':
        template_image_icon = PCIE_GPU_ICON
    print('| templateImage={}'.format(template_image_icon))
    print('---')
    print(gpu_in_use['sppci_model'])

if __name__ == '__main__':
    main()
