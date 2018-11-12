#!/usr/bin/env python
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

nbsp = "&nbsp;"  # for stripping &nbsp; from data
team_keys = ['TeamOne', 'TeamTwo']
set_key_names = ['SetOne', 'SetTwo', 'SetThree', 'SetFour', 'SetFive']

inital_scores_response = urllib2.urlopen(inital_scores_url)
initial_scores_data = json.load(inital_scores_response)

tournaments = initial_scores_data['liveScores']['Tournaments']

final_matches_list = []
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
                    if match[team_name]['Scores'][set_name]:
                        set_score_list.append(int(match[team_name]['Scores'][set_name]))
                    else:
                        set_score_list.append(0)

                score_string += " "

            if "CurrentScore" in match[team_name]['Scores'] and match[team_name]['Scores']['CurrentScore'] != nbsp:
                score_string += match[team_name]['Scores']["CurrentScore"]

            team_data['score'] = score_string
            team_data['set_score_list'] = set_score_list

            teams.append(team_data)
        
        set_lead = [0,0]
        if not match['MatchInfo'].strip():
            # if matchinfo is not present in the json response, generate a match info
            # Calculate the total number of sets won by each team/player
            for x in xrange(5):
                if len(teams[0]['set_score_list']) > x:
                    team1_games = teams[0]['set_score_list'][x]
                    team2_games = teams[1]['set_score_list'][x]
                    if x == 4:
                        # if 5th set, there should be a 2 game difference to be the winner
                        if team1_games >= 6 and team2_games < 5:
                            set_lead[0] += 1
                        elif team2_games >= 6 and team1_games < 5:
                            set_lead[1] += 1
                    else:
                        if (team1_games >= 6 and team2_games < 5) or team1_games >= 7:
                            set_lead[0] += 1
                        elif (team2_games >= 6 and team1_games < 5) or team2_games >= 7:
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
        final_matches_list.append(match_data)

if final_matches_list:
    print "🎾%s" % len(final_matches_list)
    print "---"
    for match in final_matches_list:
        print match['info'] + " | size=15 color=blue href=" + match['url']
        for team in match['team_data']:
            print team['score'] + " " + team['player_name'] + " | size=13"
        print "---"

else:
    print "🎾"
