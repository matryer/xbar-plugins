#!/usr/local/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Live Tennis Scores</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Anup Sam Abraham</bitbar.author>
# <bitbar.author.github>anupsabraham</bitbar.author.github>
# <bitbar.desc>Show live scores for tennis matches using ATP World Tour api</bitbar.desc>
# <bitbar.image>http://i.imgur.com/5kOPKVv.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

import urllib2
import json

atpworldtour_base_url = "http://www.atpworldtour.com"
inital_scores_url = atpworldtour_base_url + "/en/-/ajax/Scores/GetInitialScores"

nbsp = "&nbsp;"
team_keys = ['TeamOne', 'TeamTwo']
set_key_names = ['SetOne', 'SetTwo', 'SetThree', 'SetFour', 'SetFive']

inital_scores_response = urllib2.urlopen(inital_scores_url)
initial_scores_data = json.load(inital_scores_response)

tournaments = initial_scores_data['liveScores']['Tournaments']

match_info = []
for each_tournament in tournaments:
    matches = each_tournament['Matches']
    for match in matches:
        match_data = {}
        teams = []
        for team_name in team_keys:
            team_data = {}
            
            # get the player(s) name for each team
            player_name = match[team_name]['PlayerOneName']
            if match[team_name]['PlayerTwoName'].strip():    
                player_name += " / " + match[team_name]['PlayerTwoName']
            if match[team_name]['TeamStatus'] == "now-serving":
                player_name += "*"
            team_data['player_name'] = player_name

            # get the scores
            score_string = ""
            set_score_list = []
            for set_name in set_key_names:
                if set_name in match[team_name]['Scores'] and match[team_name]['Scores'][set_name] != nbsp:
                    score_string += match[team_name]['Scores'][set_name]
                    set_score_list.append(int(match[team_name]['Scores'][set_name]))

                score_string += " "

            if "CurrentScore" in match[team_name]['Scores'] and match[team_name]['Scores']['CurrentScore'] != nbsp:
                score_string += match[team_name]['Scores']["CurrentScore"]

            team_data['score'] = score_string
            team_data['set_score_list'] = set_score_list

            teams.append(team_data)
        
        set_lead = [0,0]
        if not match['MatchInfo'].strip():
            for x in xrange(5):
                if len(teams[0]['set_score_list']) > x and len(teams[0]['set_score_list']) > x:
                    if (teams[0]['set_score_list'][x] - 2) >= teams[1]['set_score_list'][x] - 2 and teams[0]['set_score_list'][x] >= 6:
                        set_lead[0] += 1
                    elif (teams[1]['set_score_list'][x] - 2) >= teams[0]['set_score_list'][x] and teams[1]['set_score_list'][x] >= 6:
                        set_lead[1] += 1
            if set_lead[0] > set_lead[1]:
                match_data['info'] = "%s leads by %s set%s to %s" %(teams[0]['player_name'], set_lead[0], "s" if set_lead[0] > 1 else "", set_lead[1])
            elif set_lead[1] > set_lead[0]:
                match_data['info'] = "%s leads by %s set%s to %s" %(teams[1]['player_name'], set_lead[1], "s" if set_lead[1] > 1 else "", set_lead[0])
            elif not (set_lead[0] & set_lead[1]):
                match_data['info'] = "First set in progress"
            else:
                match_data['info'] = "Both won %s set%s each" %(set_lead[0], "s" if set_lead[0] > 1 else "")
        else:
            match_data['info'] = match['MatchInfo']
            
        match_data['url'] = atpworldtour_base_url + match['StatsLink']
        match_data['team_data'] = teams
        match_info.append(match_data)

if match_info:
    print "🎾%s" % len(match_info)
    print "---"
    for match in match_info:
        print match['info'] + " | size=15 color=blue href=" + match['url']
        for team in match['team_data']:
            print team['score'] + " " + team['player_name'] + " | size=13"
        print "---"

else:
    print "🎾"
