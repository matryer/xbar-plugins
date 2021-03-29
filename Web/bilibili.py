#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# coding=utf-8
#
# <xbar.title>B站UP主粉丝和投稿视频播放信息获取</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>胖哥带你入坑带你飞</xbar.author>
# <xbar.author.github>bihell</xbar.author.github>
# <xbar.desc>实时显示B站粉丝数和最新20次投稿的视频信息，请把userId改为你自己的。</xbar.desc>
# <xbar.image>https://bihell.com/media/2020/02/bilibili-bitbar.jpg</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
#
# by tpxcer

import requests
from datetime import datetime

# 请把下面88900889改为你的用户Id
userId = 88900889

userInfoUrl = "https://api.bilibili.com/x/web-interface/card?mid=" + str(userId)
userInfoRes = requests.request("GET", userInfoUrl).json()
print("粉丝：" + str(userInfoRes['data']['follower']))

subMitVideosUrl = "http://space.bilibili.com/ajax/member/getSubmitVideos?mid=" + str(userId)
subMitVideosRes = requests.request("GET", subMitVideosUrl).json()
print("---")
for video in subMitVideosRes["data"]["vlist"]:
    print(video["title"] + "| color=#123def href=http://bilibili.com/video/av" + str(video["aid"]))
    print("    投稿时间:" + datetime.fromtimestamp(video["created"]).strftime("%Y-%m-%d %H:%M:%S") +
          "    时长:" + str(video["length"]) +
          "    播放:{0:6}".format(video["play"]) +
          "    收藏:" + str(video["favorites"]) +
          "    评论:" + str(video["comment"]) +
          "    弹幕:" + str(video["video_review"]))
