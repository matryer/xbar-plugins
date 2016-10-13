#!/usr/bin/env python

# <bitbar.title>Java Version</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sanjiv Sahayam</bitbar.author>
# <bitbar.author.github>ssanj</bitbar.author.github>
# <bitbar.desc>Displays the current version of Java installed and a clickable list of the latest versions available.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/ssanj/java_version_bitbar_plugin/master/image.png</bitbar.image>
# <bitbar.dependencies>python2, java</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/ssanj/java_version_bitbar_plugin</bitbar.abouturl>
#
# Java must be installed in your OS. Running java -version in a terminal must return the installed version of Java.
# This plugin currently only works with Python2.
# Displays the current Java version installed on your system and a list of newer versions (if any). If you have
# the latest version, it will be displayed in green on the task bar. If there are newer versions available your
# Java version will be displayed in orange and will have a list of newer versions within the context menu. Clicking on
# these menu items will take you to a download page on the java.dashversion.com site which will direct you to the Oracle
# download page for the latest version.
import subprocess
import re
import json
import logging
from urllib2 import urlopen

FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
logging.basicConfig(format=FORMAT)
logger = logging.getLogger('java_version.bitbar')

try:
    url              = 'http://java.dashversion.com'
    apiUrl           = url + '/api'
    try:
        response     = urlopen(apiUrl).read()
        version_json = json.loads(response.decode('utf-8'))
    except Exception as e:
        logger.exception("could not retrieve latest java version from java.dashversion.com")
        raise

    try:
        version          = subprocess.check_output(['java', '-version'], stderr = subprocess.STDOUT)
        pattern          = '\"(\d+\.\d+\.?.*).*\"'.encode('utf-8')
        java_version     = re.search(pattern, version).groups()[0].decode('utf-8')
    except Exception as e:
        logger.exception("could not retrieve current java version. Do you have java installed on the current path? Try running 'java -version'")
        raise

    def findNewestVersion():
      versionsList = []
      for v in version_json['versions']:
        version = v['version_string']
        if version != java_version:
          versionsList.append(version)

      return versionsList

    results = findNewestVersion()
    if not results:
      print (java_version + ' | color=green')

    else:
      print (java_version + ' | color=orange')
      print ('---')
      for v in results:
        print (v + ' | href=' + url + ' color=green')
except:
    print ('java | color=red')
    logger.exception("failed: ")
