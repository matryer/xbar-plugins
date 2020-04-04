#!/usr/bin/env python3

# <bitbar.title>AI deadlines counter</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Jiseob Kim</bitbar.author>
# <bitbar.author.github>nzer0</bitbar.author.github>
# <bitbar.desc>Count the days to the submission deadlines of AI conferences.</bitbar.desc>
# <bitbar.image>https://github.com/nzer0/bitbar-aideadlines/blob/master/aid-screenshot.png?raw=true</bitbar.image>
# <bitbar.dependencies>python3 (pyyaml, pytz, tzlocal, wget)</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/nzer0/bitbar-aideadlines</bitbar.abouturl>


import os, re, sys
import datetime as dt

ME_PATH = os.path.realpath(__file__)
ROOT = os.path.dirname(ME_PATH)
DL_FNAME = os.path.join(ROOT, '.aid.aideadlines.yaml')
DLG_FNAME = os.path.join(ROOT, '.aid.processed.yaml')
SEL_FNAME = os.path.join(ROOT, '.aid.seldl')
SHB_FNAME = os.path.join(ROOT, '.aid.ammend_shebang')
AID_URL = "https://raw.githubusercontent.com/abhshkdz/ai-deadlines/gh-pages/_data/conferences.yml"

'''Write bash script to ammend the shebang'''
if not os.path.exists(SHB_FNAME):
	with open(SHB_FNAME, "w") as sf:
		sf.write('#!/bin/bash\n')
		sf.write('SB=`which python3`\n')
		sf.write('sed -i "" -e "1s:#!/usr/bin/env python3:#!/usr/bin/env $SB:" "$1"\n')
		sf.write('echo ">>> Now refresh the Bitbar <<<"')
	os.chmod(SHB_FNAME, 0o755)

'''Display instructions if the dependencies are not installed'''
try:
	import yaml
	from pytz import timezone
	from tzlocal import get_localzone
	import wget
except:
	print("Install Dependencies")
	print("---")
	print("Please install the dependencies by clicking below | color=black")
	print("$ pip3 install pyyaml pytz tzlocal wget | color=green bash='pip3 install pyyaml pytz tzlocal wget' refresh=true")
	print("---")
	print("If it is still not working, click below | color=black")
	print(f"Ammend shebang | color=green bash='{SHB_FNAME}' param1={ME_PATH}")
	print("then refresh the BitBar | color=black")
	sys.exit(1)

'''Replace UTC with Etc/GMT to be handled by pytz'''
def normalize_tz(tz):
	mo = re.search(r'UTC([+-]\d+)', tz)
	if mo is None:
		return tz
	else:
		return f"Etc/GMT{-int(mo.group(1)):+d}"

'''Parse the string to make a datetime obj with an appropriate local'''
def make_datetime(c, abs=False):
	dl_key = 'abstract_deadline' if abs else 'deadline'
	return timezone(normalize_tz(c['timezone'])).localize(dt.datetime.strptime(c[dl_key], '%Y-%m-%d %H:%M:%S'))

'''Compute the datetime diff with now'''
def get_diff(due):
	return due - get_localzone().localize(dt.datetime.now())

'''main Bitbar display'''
def main():
	if not os.path.exists(DL_FNAME):
		getdl()
	with open(DLG_FNAME, 'r') as yf:
		dlg = yaml.safe_load(yf)

	try:
		'''If there is a selected deadline'''
		with open(SEL_FNAME, 'r') as sf:
			sel = sf.readline().rstrip('\n')

		conf = dlg['full'][sel]
		name = conf['title']
		dl = make_datetime(conf)
		diff = get_diff(dl)
		days = f"+{-diff.days}" if diff.days < 0 else f"-{diff.days}"
		hours = f"{diff.seconds//3600}"
		print(f"{name} D{days} {hours}h+")
		print("---")
		print(f"About {name} {conf['year']}")
		print(f"--:date: {conf['date']}")
		print(f"--:round_pushpin: {conf['place']}")
		print(f"--:house: Go to Website | href={conf['link']}")
		print("-----")
		if 'abstract_deadline' in conf.keys():
			abs_dl = make_datetime(conf, abs=True)
			print(f"--Abs: {abs_dl.strftime('%Y-%m-%d %H:%M')} ({conf['timezone']})")	
		print(f"--Due: {dl.strftime('%Y-%m-%d %H:%M')} ({conf['timezone']})")
	except:
		name = "N/A"
		print("Select the conference")


	print("---")
	for urg in dlg['urgent'][:10]:
		conf = dlg['full'][urg]
		outstr = f"{conf['title']} | bash={ME_PATH} param1=seldl param2={conf['title']} terminal=false refresh=true"
		if conf['title'] == name:
			outstr += " checked=true"
		print(outstr)
	print("More...")
	for sub_name, sub in dlg['subs'].items():
		print(f"--{sub_name}")
		for t in sub:
			conf = dlg['full'][t]
			outstr = f"----{conf['title']} | bash={ME_PATH} param1=seldl param2={conf['title']} terminal=false refresh=true"
			if get_diff(make_datetime(conf)).days < 0:
				outstr += " color=red"
			print(outstr)
	print("---")
	print(f"Update Conferences Info | bash={ME_PATH} param1=getdl terminal=false refresh=true")
	print("Go to aideadlin.es | href='https://aideadlin.es'")
	print("About this plugin | href='https://github.com/nzer0/bitbar-aideadlines")


'''Select a particular deadline'''
def seldl(conf_title):
	with open(SEL_FNAME, 'w') as sf:
		sf.write(conf_title)

'''Get the deadlines list from aideadlin.es and process it'''
def getdl():
	if os.path.exists(DL_FNAME):
			os.remove(DL_FNAME)
	wget.download(AID_URL, DL_FNAME, False)

	with open(DL_FNAME, 'r') as yf:
		dl = yaml.safe_load(yf)
	full = {}
	subs = {}
	for conf in dl:
		full[conf['title']] = conf
		sub = conf['sub']
		if sub in subs.keys():
			subs[sub].append(conf['title'])
		else:
			subs[sub] = [conf['title']]

	urgent = sorted(list(full.keys()), key=lambda t: make_datetime(full[t]))
	urgent = [t for t in urgent if get_diff(make_datetime(full[t])).days > -7]

	for sub in subs.values():
	 	sub.sort(key=lambda t: make_datetime(full[t]))

	with open(DLG_FNAME, 'w') as yf:
		yaml.safe_dump(dict(full=full, subs=subs, urgent=urgent), yf)


if __name__ == "__main__":
	if len(sys.argv) == 1:
		main()

	elif sys.argv[1] == 'seldl':
		seldl(sys.argv[2])

	elif sys.argv[1] == 'getdl':
		getdl()

