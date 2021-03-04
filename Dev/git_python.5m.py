#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Gitty</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Timothy Barnard</bitbar.author>
# <bitbar.author.github>collegboi</bitbar.author.github>
# <bitbar.image>https://raw.githubusercontent.com/collegboi/my-bitbar-plugins/master/Gitty/image1.png</bitbar.image>
# <bitbar.desc>Shows the current status of local repos. https://github.com/collegboi/my-bitbar-plugins</bitbar.desc>
# <bitbar.dependencies>python, GitPython</bitbar.dependencies>
#

import git
from os.path import expanduser
import os

def split_path(path):
    direc = path.split('.')[0]
    direc_name = path.split('/')[-1]
    return (direc_name, direc )


def get_list_dir(start_path):
    paths = []
    for root, directories, filenames in os.walk(start_path):
        if any(directory.endswith('.git') for directory in directories):
            if ".build" not in root:
                paths.append(root)
    return paths

print("Gitty | font='PilGi Regular' image=iVBORw0KGgoAAAANSUhEUgAAABgAAAAQCAYAAAF7I48DAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5JfxUYAAARgSURBVHjaYt6/fz+Dr68vw9WrVw8xMDExfYqMjGRgYGD4DwAAAP//BMFBAQAwEMKwSprBU8MPbXjoEu7utUWFJGxD5QMAAP//Yrhw4QJECRL+//8/A9OVK1e079+/z/j48WNGBgYGBmZm5j8MDAwMAAAAAP//ZM2hEYAgGIDRj4PACtxJoHksYeNgNBormYkE7VSSC/wGowu8p5xzd61111rTez9SSmcphR8zxvgOQOac5JxprWGtfUQEE0K4vPey1lLGGGKMG8ALAAD//zyPsQqCUBiFv1sQxnWopF4hArdewAcQfIkGX8DBuUdoC+7uIAhtzvkU3gYX15ZwutLfUHamwwfng0NVVX+d1voJiFLq7Xnea+JlWSIiiAjzYRiubdseAZxzS75R4zgufh2ttUuS5A4ws9Yeoii6GGO2AE3TKGutAqjrWoVheOu6bj+N6fueOI7PQRA88jzfFEWxMsassyzb+b7/TNP0ND0WET58UTFLhlAUve8RKA0fukboM9pc1N8gNApOD/+ASzj7A5z6CTUIIoiTgwQNLraJS5A4tUUgTjaloLwWX0hEZzmXwz2He7hQFMWJEFLFcQxRFEHXdRCGISRJArIsv1ZV9bPMGAOglN7ycpIkfez8zrUgCG6OBjzP8xU/b5qmi50vubZt2zUcgPakf8EYQ3zGAAC2bT+0bYvSNEWu66Isy1DTNMiyrKc/A+q6BkEQvnzf9/M8B0rpnaIob/ypxw5nAACEENB1/dnzvEoURXAc53FZlnNN0/DvdDyOI1JVlZmmeV+W5ee6rqe+7zuM8YthGNswDOho+Cad/EGXCKA4/u64K/HuB78KlAYbft0ZaaAYLQ7XkIODiy5CDeogSU5tNgiC8VsaRA3iQBdBxEVQ4kQQEc7JQ7RB0YQLlKgO/1CYoHd6LfmD0iUa3vJ4jy/f9/08hOd5aDabj6PRaPPQpGm6FgwGnymKMsdx/I7BYLguiuJ6s9l8xnGcYlm2OJ1ObYf5VCp1QdP0J6fTeWw4kUiAw+GI/c3/v5TH43kaj8f/AOIKDI7jHiAIov6PgF6vlwRBOD8lgOh0urEkSVewaLXar2az+QOKoqgoijdlWT7b7/fYfr/fkiT5w2g0LpfLJTYYDB7udrvzw57Vam10u90nR6GtVqszAACKovrj8Rjp9Xq3TSZTgSCIj4IgPEomk/d8Pt/dQqFwv9FoOCVJ+ub3+98oinKj1WrdIknyOwDAYrHQn6Q2EAhcAoDKMEya53nIZDIXB+sul+tVqVSCbDYLlUoF7HZ79vdvzYvF4rVarQYWi+U9AKiRSOT5yQxmsxmEQqHXAKCGw+GXo9EIcrkcsCwL/X4f6vU6tNttKJfL0Ol0IJ1OA8dxMBwOwe12vwUANRaLBSaTycmQkdFoBLIsw3w+h2q1asrn8+8Wi4VVVdUVhmErgiB+4ji+22632Hq9JmVZJhEE0VIU1fB6vS8Yhvmi0WgARVGw2WxHF/o1AOIORIB/vrb+AAAAAElFTkSuQmCC")

print("---")

home = expanduser("~")
quotes = '"'
path = home + '/Documents/'

print("---")

content = get_list_dir(path)

for file in content:
    direc_name, direc = split_path(file)
    repo = git.Repo(direc)
    branches = repo.branches
    try:
        active_branch = repo.active_branch.name
    except:
        active_branch = 'DETACHED_' + repo.head.object.hexsha
    tags = repo.tags
    head = repo.head
    cur_tag = next((tag for tag in repo.tags if tag.commit == repo.head.commit), None)
    
    commits_ahead = repo.iter_commits('origin/master..master')
    count1 = sum(1 for c in commits_ahead)
    
    commits_behind = repo.iter_commits('master..origin/master')
    count2 = sum(1 for c in commits_behind)
    
    changedFiles = [ item.a_path for item in repo.index.diff(None) ]
    
    count3 = sum(1 for c in changedFiles)
    if count3 > 0:
        print(direc_name + "| color=red")
    else:
        print(direc_name + "| color=green")
    print("--" + "Copy path | bash='echo "+direc+" | pbcopy '")
    print("--" + "Open location | bash='open "+direc+"'")
    print("--" + file)
    print("-----")
    print("--" + "Branches:")
    for branch in branches:
        print("----" + `branch.name`)
        print("------ Checkout | bash='cd "+direc+" && git checkout "+branch.name+"' ")
        print("------ Pull origin | bash='cd "+direc+" && git pull origin "+branch.name+"' ")
    print("--" + "Tags:")
    for tag in tags:
        print("----" + `tag.name`)
        print("------ Checkout | bash='cd "+direc+" && git checkout "+tag.name+"' ")
    print("-----")
    print("--"+ "Cur. branch: " + `active_branch` )
    if cur_tag is not None:
        print("--"+ "Cur. tag: " + `cur_tag.name` )
    print("--" + "No. commits ahead: " + str(count1) )
    print("--" + "No. commits behind: " + str(count2) )
    print("--" + "No. changed files: " + str(count3) )
    for changedFile in changedFiles:
        print("----" + changedFile)

print("---")


print("Refresh | refresh=true image='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAADAFBMVEX///8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmJiYnJycoKCgpKSkqKiorKyssLCwtLS0uLi4vLy8wMDAxMTEyMjIzMzM0NDQ1NTU2NjY3Nzc4ODg5OTk6Ojo7Ozs8PDw9PT0+Pj4/Pz9AQEBBQUFCQkJDQ0NERERFRUVGRkZHR0dISEhJSUlKSkpLS0tMTExNTU1OTk5PT09QUFBRUVFSUlJTU1NUVFRVVVVWVlZXV1dYWFhZWVlaWlpbW1tcXFxdXV1eXl5fX19gYGBhYWFiYmJjY2NkZGRlZWVmZmZnZ2doaGhpaWlqampra2tsbGxtbW1ubm5vb29wcHBxcXFycnJzc3N0dHR1dXV2dnZ3d3d4eHh5eXl6enp7e3t8fHx9fX1+fn5/f3+AgICBgYGCgoKDg4OEhISFhYWGhoaHh4eIiIiJiYmKioqLi4uMjIyNjY2Ojo6Pj4+QkJCRkZGSkpKTk5OUlJSVlZWWlpaXl5eYmJiZmZmampqbm5ucnJydnZ2enp6fn5+goKChoaGioqKjo6OkpKSlpaWmpqanp6eoqKipqamqqqqrq6usrKytra2urq6vr6+wsLCxsbGysrKzs7O0tLS1tbW2tra3t7e4uLi5ubm6urq7u7u8vLy9vb2+vr6/v7/AwMDBwcHCwsLDw8PExMTFxcXGxsbHx8fIyMjJycnKysrLy8vMzMzNzc3Ozs7Pz8/Q0NDR0dHS0tLT09PU1NTV1dXW1tbX19fY2NjZ2dna2trb29vc3Nzd3d3e3t7f39/g4ODh4eHi4uLj4+Pk5OTl5eXm5ubn5+fo6Ojp6enq6urr6+vs7Ozt7e3u7u7v7+/w8PDx8fHy8vLz8/P09PT19fX29vb39/f4+Pj5+fn6+vr7+/v8/Pz9/f3+/v7///87ptqzAAAAJXRSTlMAgA5ABAHjYRLswnooVM0CyLDK2mCpIMSvX5AFm5SRscBeH2Kql1edqgAAAGdJREFUeJyNjUcSgCAUQ1GKSlGw9879r6j4Wbogm0zeTBKEgkQSnrFmQBhDTstcyLbu+jH6cqENdT7NFoCq4s+t9QDFYU+/8t3oXYNcKQ8W4pwaXQBYt/04pcjLFCoY0+tmGU9I2NMDXoEEmA7BEvIAAAAASUVORK5CYII=' ")

