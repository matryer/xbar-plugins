#!/usr/bin/python

# <bitbar.title>trello list</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Karim Boumedhel</bitbar.author>
# <bitbar.author.github>karmab</bitbar.author.github>
# <bitbar.desc>Lists trello cards in New/In Progress</bitbar.desc>


import os
import requests
import sys

key = os.environ['TRELLO_KEY'] if 'TRELLO_KEY' in os.environ else None
token = os.environ['TRELLO_TOKEN'] if 'TRELLO_TOKEN' in os.environ else None
board = os.environ['TRELLO_BOARD'] if 'TRELLO_BOARD' in os.environ else None
boardname = os.environ['TRELLO_BOARDNAME'] if 'TRELLO_BOARDNAME' in os.environ else None
states = os.environ['TRELLO_STATES'] if 'TRELLO_STATES' in os.environ else 'Next,In Progress'
user = os.environ['TRELLO_USER'] if 'TRELLO_USER' in os.environ else None

states = states.split(',')
error = None
if key is None:
    error = "Missing key\nGet it at https://trello.com/app-key"
if token is None:
    error = "Missing token\nGet it at https://trello.com/app-key"
if boardname is None and board is None:
    error = "Missing board information\nDefine board short or long name"
if user is None:
    error = "Missing username"

if error is not None:
    warning = u"\u26A0"
    warning = warning.encode('utf-8')
    print("%s\n" % warning)
    print("---\n")
    print(error)
    sys.exit(1)

boardsurl = "https://api.trello.com/1/members/%s/boards?key=%s&token=%s" % (user, key, token)
r = requests.get(boardsurl)
if board:
    board = [b for b in r.json() if b['shortLink'] == board]
    if board:
        boardname = board[0]['name']
elif boardname:
    board = [b for b in r.json() if b['name'] == boardname]
if len(board) != 1:
    print "Missing board information"
    sys.exit(0)
boardid = board[0]['id']

statesurl = "https://api.trello.com/1/boards/%s/lists?key=%s&token=%s" % (boardid, key, token)
r = requests.get(statesurl)
statesinfo = {}
for l in r.json():
    if l['name'] in states:
        statesinfo[l['id']] = l['name']

cardsurl = "https://api.trello.com/1/members/%s/cards?key=%s&token=%s" % (user, key, token)
r = requests.get(cardsurl)
cards = [card for card in r.json() if card['idBoard'] == boardid and card['idList'] in statesinfo]
if cards:
    print("%s Trello cards in %s" % (len(cards), boardname))
else:
    print("No Trello cards in %s" % boardname)
    sys.exit(0)
print("---\n")
oldcolumn = None
for card in sorted(cards, key=lambda c: (statesinfo[c['idList']], c['name'])):
    currentcolumn = statesinfo[card['idList']]
    if oldcolumn is None:
        oldcolumn = currentcolumn
        print currentcolumn
    if currentcolumn != oldcolumn:
        print("---\n")
        print currentcolumn
    url = card['url']
    name = card['name']
    print("%s | href=%s" % (name, url))
