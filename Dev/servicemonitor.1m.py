#!/usr/bin/env python3
# coding=utf-8
#
# <xbar.title>Service Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Cristian</xbar.author>
# <xbar.author.github>cmaluend</xbar.author.github>
# <xbar.desc>Ping the services and create a dropdown report</xbar.desc>
# <xbar.image>https://cmaluend.github.io/images/xbar/servicemonitor1.0.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
#
# by Cristian
import sqlite3
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta
from functools import partial
from threading import Thread

try:
	import requests
except:
	subprocess.check_call([sys.executable, "-m", "pip", "install", "requests"])


#Documentation
# 🟢: Healthy service
# 🔴: Service with issues
# ⚠️: Service recovered.

TITLE = "My Services"
'''
SERVICES = {
	"tier: [
		{
			"name": "Service name",
			"endpoint": "https://httpstat.us/200",
			"headers": {}							#optional
			"method": "GET",						#optional
			"body": {} 								#optional 
			"status_code": 200
		}
	]
}
'''
SERVICES = {
	"dev": [
		{ 
			"name": "service 1",
			"endpoint": "https://httpstat.us/200",
			"headers": {
				"Content-type": "text/html"
			},
			"status_code": 200
		},
		{ 
			"name": "service 2",
			"endpoint": "https://httpstat.us/400",
			"status_code": 200
		},
		{ 
			"name": "service 3",
			"endpoint": "https://httpstat.us/200",
			"headers": {
				"Content-type": "text/html"
			},
			"status_code": 200
		},
	],
	"uat": [
		{ 
			"name": "service 1",
			"endpoint": "https://httpstat.us/200",
			"status_code": 200
		},
	]
}

# Max number of issues in the menu
LIMIT_TOTAL_ISSUES=5
# Recovery time in Hours
RECOVERY_TIME=12

ALLOW_NOTIFICATIONS = True

class ServiceMonitor:

	def __init__(self, reporter) -> None:
		self.reporter = reporter
		self.notificator = Notificator(reporter)

	def process_environments(self, environments):
		envs = environments.keys()
		result = {}
		for env in envs:
			result[env] = self.process_services(env, environments[env])
		return result

	def process_services(self, env, services):
		servicesnames = [ x["name"] for x in services]
		with ThreadPoolExecutor(max_workers=5) as executor:
			result = executor.map(partial(self.call_service), services)
		
		serviceStatus = dict(zip(servicesnames, result))
		#Add issues to the reporter
		for service in serviceStatus:
			if serviceStatus[service]["healthy"] == False:
				self.reporter.add_issue(env, service, serviceStatus[service]["httpStatus"], serviceStatus[service]["errorMessage"])
				self.notificator.send_message(env, service)
			else:
				self.reporter.update_notification_status(env, service, False)

		return serviceStatus

	def call_service(self, service):
		method =  service["method"] if "method" in service.keys() else "GET"
		data = service["body"] if "body" in service.keys() else None
		headers = {"user-agent":"xbar"}
		if "headers" in service.keys():
			headers.update(service["headers"])
		
		healthy = False
		errorMessage = ""
		httpStatus = ""
		try:
			response = requests.request(method=method, url=service["endpoint"], headers=headers, data=data, timeout=10)
			if response.status_code == service["status_code"]:
				healthy = True
			else:
				errorMessage = response.text
				httpStatus = response.status_code
		except Exception as err:
			errorMessage = err
			httpStatus = "NA"
		return { 'healthy': healthy, 'errorMessage': errorMessage, "httpStatus": httpStatus}

class Reporter:

	def __init__(self) -> None:
		self.conn = sqlite3.connect("."+sys.argv[0]+".db", isolation_level=None)
		self.conn.execute("CREATE TABLE IF NOT EXISTS issues (env NOT NULL, service NOT NULL, http_status, error_message, timestamp timestamp)")
		self.conn.execute("CREATE TABLE IF NOT EXISTS notifications (env NOT NULL, service NOT NULL, triggered BOOLEAN, timestamp timestamp, PRIMARY KEY (env, service))")

	def __del__(self):
		self.clean_old_records()

	def add_issue(self, env, service, http_status, message):
		self.conn.execute("INSERT INTO issues VALUES(?, ?, ?, ?, ?)", (env, service, http_status, message, datetime.now()))
	
	def read_issues_by_service(self, env, service):
		fromDate = datetime.now() - timedelta(hours=RECOVERY_TIME)
		r = self.conn.execute("SELECT * FROM issues WHERE env=? AND service=? AND timestamp > ? ORDER BY timestamp DESC LIMIT ?", (env, service, fromDate, LIMIT_TOTAL_ISSUES))
		return r.fetchall()
	
	def clean_old_records(self):
		fromDate = datetime.now() - timedelta(hours=RECOVERY_TIME)
		self.conn.execute("DELETE FROM issues WHERE timestamp < ?", [fromDate])

	def update_notification_status(self, env, service, hasBeenNotified):
		try:
			self.conn.execute("INSERT INTO notifications VALUES(?, ?, ?, ?) ON CONFLICT (env, service) DO UPDATE SET triggered = ?, timestamp = ?", (env, service, hasBeenNotified, datetime.now(), hasBeenNotified, datetime.now()))
			self.conn.commit()
		except Exception as err:
			# print(err)
			pass

	def read_notification_status_by_service(self, env, service):
		r = self.conn.execute("SELECT * FROM notifications WHERE env=? AND service=?", (env, service))
		values = r.fetchone()
		return values
	
	def read_notification_has_been_triggered(self, env, service):
		result = self.read_notification_status_by_service(env, service)
		if not result:
			return False
		return bool(result[2])

class Notificator:

	def __init__(self, reporter):
		self.reporter = reporter

	def send_alert(self, message):
		subprocess.check_call(["osascript", "-e", "tell application (path to frontmost application as text) to display dialog \""+message+"\" buttons {\"OK\"} with icon stop"])

	def send_notification(sefl, title, message):
		subprocess.check_call(["osascript", "-e", f"display notification \"{message}\" with title \"{title}\""])

	def send_message(self, env, service):
		title = f"{TITLE}"
		message = f"[{env}] '{service}' has issues."

		if not ALLOW_NOTIFICATIONS:
			return
		
		hasBeenTriggered = self.reporter.read_notification_has_been_triggered(env, service)
		if hasBeenTriggered:
			return

		self.reporter.update_notification_status(env, service, True)
		if ALLOW_NOTIFICATIONS:
			t = Thread(target=self.send_notification, args=[title, message])
			t.start()

		
	
class MenuGenerator:

	# green: \x1b[42m
	# red: \x1b[41m
	# yellow: \x1b[43m
	HEALTHY='\x1b[42m'
	UNHEALTHY='\x1b[41m'
	ALERTED='\x1b[43m'
	NC='\x1b[0m'

	def __init__(self, reporter) -> None:
		self.reporter = reporter
	
	def create_dropdown_report(self, values):
		summary = ""
		menu = "\n---"
		for env in values.keys():
			healthyEnv = True
			alertedEnv = False
			envmenu = ""
			servicemenu = ""
			for servicename in values[env].keys():
				healthy = True
				service = values[env][servicename]
				alerted = len(self.reporter.read_issues_by_service(env,servicename)) > 0
				if not service["healthy"]:
					healthy = False
				servicemenu += f'\n--{self.get_icon(healthy, alerted)} {servicename}'
				servicemenu += self.create_issues_report(env, servicename)
				if not alertedEnv:
					alertedEnv = alerted
				if healthyEnv:
					healthyEnv = healthy
			envmenu = f'\n{self.get_icon(healthyEnv, alertedEnv)} {env}{servicemenu}'
			menu+= envmenu
			summary += self.get_env_colored(env, healthyEnv, alertedEnv)

		return f'{summary}{menu}'
	
	def create_issues_report(self, env, service):
		issues = self.reporter.read_issues_by_service(env, service)
		menu = ""
		for issue in issues:
			date = issue[4][:issue[4].rfind(".")]
			message = issue[3].replace("\"", "\\\"")
			menu += f'\n----[{date}] ({issue[2]}) : {message} | length=100 shell="/bin/bash" param1="-c" param2=\'echo "{message}" | open -a TextEdit -f\''
		return menu
		
	def get_icon(self, healthy, alerted):
		if not healthy:
			return "🔴"
		elif healthy and alerted:
			return "⚠️ "
		else:
			return "🟢"

	def get_env_colored(self, env, healthy, alerted):
		color = MenuGenerator.HEALTHY
		if not healthy:
			color = MenuGenerator.UNHEALTHY
		elif healthy and alerted:
			color = MenuGenerator.ALERTED

		return f'{color} {env} {MenuGenerator.NC}'
	
	def print(self, servicesStatus):
		report = self.create_dropdown_report(servicesStatus)
		print(f'{TITLE}: {report}')
	

if __name__ == "__main__":
	reporter = Reporter()
	servicesStatus = ServiceMonitor(reporter).process_environments(SERVICES)
	MenuGenerator(reporter).print(servicesStatus)
