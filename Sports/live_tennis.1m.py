#!/usr/bin/env python3

# <xbar.title>Live Tennis Scores</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Anup Sam Abraham</xbar.author>
# <xbar.author.github>anupsabraham</xbar.author.github>
# <xbar.desc>Show live scores for tennis matches using ATP World Tour api</xbar.desc>
# <xbar.image>https://i.imgur.com/5kOPKVv.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

import sys
import getopt
import os
import json
import http.client
import gzip

FAVORITE_PLAYERS_FILE = "/var/tmp/bitbar_tennis_favourites.data"

ATP_WORLD_TOUR_BASE_URL = "www.atptour.com"
INITIAL_SCORES_URL = "/en/-/ls/liveMatches/web/tour"

if len(sys.argv) > 1:
    opts, args = getopt.getopt(sys.argv[1:], "i:d:", [])
    for opt, arg in opts:
        if opt == "-i":
            # this is for inserting the player name into our file
            with open(FAVORITE_PLAYERS_FILE, "a") as f:
                f.write(arg+"\n")

        elif opt == "-d":
            # this option is for deleting a player name from the file
            if os.path.exists(FAVORITE_PLAYERS_FILE):
                with open(FAVORITE_PLAYERS_FILE, "r") as f:
                    current_players = [x.strip() for x in f.readlines()]
                    if arg in current_players:
                        current_players.remove(arg)
                with open(FAVORITE_PLAYERS_FILE, "w") as f:
                    f.writelines([x+"\n" for x in current_players])
    exit()

if os.path.exists(FAVORITE_PLAYERS_FILE):
    with open(FAVORITE_PLAYERS_FILE, "r") as f:
        favorite_players = [x.strip() for x in f.readlines()]
else:
    # create an empty file if it doesn't exist
    open(FAVORITE_PLAYERS_FILE, "a").close()
    favorite_players = []

untracked_players = []
if "*" in favorite_players:
    track_all_players = True
else:
    track_all_players = False


conn = http.client.HTTPSConnection(ATP_WORLD_TOUR_BASE_URL)
headers = {
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Sec-Fetch-Site': 'none',
    'Cookie': '__cf_bm=random-value',
    'Accept-Encoding': 'gzip, deflate, br',
    'Sec-Fetch-Mode': 'navigate',
    'Host': 'www.atptour.com',
    'Accept-Language': 'en-GB,en;q=0.9',
    'Sec-Fetch-Dest': 'document',
    'Connection': 'keep-alive',
}
conn.request('GET', INITIAL_SCORES_URL, headers=headers)
response = conn.getresponse()

data = response.read()
initial_scores_data = json.loads(gzip.decompress(data))

tournaments = initial_scores_data['LiveMatchesTournaments']

nbsp = "&nbsp;"  # for stripping &nbsp; from data
team_keys = ['PlayerTeam', 'OpponentTeam']

final_matches_list = []
for each_tournament in tournaments:
    matches = each_tournament['LiveMatches']
    for match in matches:
        match_data = {}
        teams = []
        match_track = False
        match_hour, match_minutes, match_seconds = map(int, match["MatchTimeTotal"].split(":"))
        duration_in_words = ""
        if match_hour:
            duration_in_words += f"{match_hour} hour{'s' if match_hour > 1 else ''} and "
        if match_minutes:
            duration_in_words += f"{match_minutes} minute{'s' if match_minutes > 1 else ''}"
        for team_name in team_keys:
            team_data = {}

            # get the player(s) name for each team
            player_name = (f"{match[team_name]['Player']['PlayerFirstName'][0]}. "
                           f"{match[team_name]['Player']['PlayerLastName']}")
            if player_name in favorite_players or track_all_players:
                match_track = True
            else:
                untracked_players.append(player_name)
            if match[team_name]['Partner']['PlayerFirstName']:
                p2_name = (f"{match[team_name]['Partner']['PlayerFirstName'][0]}. "
                           f"{match[team_name]['Partner']['PlayerLastName']}")
                player_name += " / " + p2_name
                if p2_name in favorite_players or track_all_players:
                    match_track = True
                else:
                    untracked_players.append(p2_name)
            if match['ServerTeam'] == 0 and team_name == "PlayerTeam":
                player_name += "*"
            elif match['ServerTeam'] == 1 and team_name == "OpponentTeam":
                player_name += "*"
            team_data['player_name'] = player_name

            # get the scores
            score_string = ""
            set_score_list = []
            for set_name in match[team_name]['SetScores']:
                if set_name["SetScore"] is not None:
                    score_string += str(set_name["SetScore"])
                    set_score_list.append(set_name["SetScore"])
                else:
                    set_score_list.append(0)

                score_string += " "
            score_string += str(match[team_name]['GameScore'] or '')

            team_data['score'] = score_string
            team_data['set_score_list'] = set_score_list

            teams.append(team_data)
        if not match_track:
            continue

        set_lead = [0, 0]
        for x in range(5):
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

        if "ExtendedMessage" in match and match["ExtendedMessage"]:
            info = match["ExtendedMessage"].replace('\r', '').split('\n')[:2]
            match_data['info'] = info[0]
            if len(info) > 1:
                if "WinningPlayerId" in match and match["WinningPlayerId"]:
                    match_data['info'] = "👏" + match_data['info'] + "👏"
                match_data['info2'] = info[1]
        else:
            if "WinningPlayerId" in match and match["WinningPlayerId"]:
                if set_lead[0] > set_lead[1]:
                    match_data['info'] = f"👏Game Set and Match {teams[0]['player_name']}👏 "
                    match_data['info2'] = (f"{teams[0]['player_name']} wins the match by {set_lead[0]} sets to "
                                           f"{set_lead[1]}")
                else:
                    match_data['info'] = f"👏Game Set and Match {teams[1]['player_name']}👏 "
                    match_data['info2'] = (f"{teams[1]['player_name']} wins the match by {set_lead[1]} sets to "
                                           f"{set_lead[0]}")
            else:
                if set_lead[0] > set_lead[1]:
                    match_data['info'] = (f"{teams[0]['player_name']} leads by {set_lead[0]} set"
                                          f"{'s' if set_lead[0] > 1 else ''} to {set_lead[1]}")
                elif set_lead[1] > set_lead[0]:
                    match_data['info'] = (f"{teams[1]['player_name']} leads by {set_lead[1]} set"
                                          f"{'s' if set_lead[1] > 1 else ''} to {set_lead[0]}")
                elif not (set_lead[0] & set_lead[1]):
                    match_data['info'] = "First set in progress"
                else:
                    match_data['info'] = f"Both won {set_lead[0]} set{'s' if set_lead[0] > 1 else ''} each"
        if duration_in_words:
            match_data['info'] += "  [" + duration_in_words + "]"

        match_data['url'] = "https://" + ATP_WORLD_TOUR_BASE_URL + match['MatchStatsUrl']
        match_data['team_data'] = teams
        final_matches_list.append(match_data)

if final_matches_list:
    print(f"🎾{len(final_matches_list)}")
    print("---")
    for match in final_matches_list:
        print(match['info'] + " | size=15 color=blue href=" + match['url'])
        if 'info2' in match:
            print(match['info2'] + " | size=15 color=blue href=" + match['url'])
        for team in match['team_data']:
            print(team['score'] + " " + team['player_name'] + " | size=13")
        print("---")

else:
    print("🎾")
print("---")
print("Favorite Players | color=green")
print("--Click on any to remove from favorites")
print("-----")
if favorite_players:
    for each_player in favorite_players:
        print(f"--{each_player if each_player != '*' else 'TRACK ALL PLAYERS'} | "
              f"terminal=false bash=\"{sys.argv[0]}\" param1=\"-d\" param2=\"{each_player}\" refresh=true")
else:
    print("--You don't have any favorite players set")

print("Other Players | color=red")
print("--Click on any to add to favorites")
print("-----")
for each_player in untracked_players:
    print(f"--{each_player} | "
          f"terminal=false bash=\"{sys.argv[0]}\" param1=\"-i\" param2=\"{each_player}\" refresh=true")
print(f"--TRACK ALL PLAYERS | terminal=false bash=\"{sys.argv[0]}\" param1=\"-i\" param2=\"*\" refresh=true")
