#!/usr/bin/env python3

# <xbar.title>Live Tennis Scores</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Anup Sam Abraham</xbar.author>
# <xbar.author.github>anupsabraham</xbar.author.github>
# <xbar.desc>Show live scores for tennis matches using ATP World Tour api</xbar.desc>
# <xbar.image>https://i.imgur.com/5kOPKVv.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

import json
import http.client
import gzip

atp_world_tour_base_url = "www.atptour.com"
initial_scores_url = "/en/-/ls/liveMatches/web/tour"

conn = http.client.HTTPSConnection(atp_world_tour_base_url)
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
conn.request('GET', initial_scores_url, headers=headers)
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
        match_hour, match_minutes, match_seconds = map(int, match["MatchTimeTotal"].split(":"))
        duration_in_words = ""
        if match_hour:
            duration_in_words += f"{match_hour} hour{'s' if match_hour > 1 else ''} and "
        if match_minutes:
            duration_in_words += f"{match_minutes} minute{'s' if match_minutes > 1 else ''}"
        for team_name in team_keys:
            team_data = {}

            # get the player(s) name for each team
            player_name = match[team_name]['Player']['PlayerFirstName'][0] + ". " + match[team_name]['Player'][
                'PlayerLastName']
            if match[team_name]['Partner']['PlayerFirstName']:
                player_name += " / " + match[team_name]['Partner']['PlayerFirstName'][0] + ". " + \
                               match[team_name]['Partner']['PlayerLastName']
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

        match_data['url'] = "https://" + atp_world_tour_base_url + match['MatchStatsUrl']
        match_data['team_data'] = teams
        final_matches_list.append(match_data)

if final_matches_list:
    print("🎾%s" % len(final_matches_list))
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
