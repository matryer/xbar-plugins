#!/usr/local/bin/python3
# -*- coding: UTF-8 -*-

# <xbar.title>AWS Profiles Login</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Daniel Ferrari</xbar.author>
# <xbar.author.github>FerrariDG</xbar.author.github>
# <xbar.desc>List AWS profiles and login status (if session token is valid). Trigger MFA login script.</xbar.desc>
# <xbar.dependencies>python3, awscli, aws-mfa-tools</xbar.dependencies>

# Dependencies
# - AWS CLI is installed, 'pip3 install awscli', and 'aws configure' has been ran.
# - AWS MFA Tools is installed, 'pip3 install aws-mfa-tools' (https://github.com/FerrariDG/aws-mfa-tools).

# Installation:
# 1. Install AWS Cli and AWS MFA Tools
# 2. Configure AWS profiles and MFA credentials.
# 3. Copy this script to your xbar plugin folder.
# 4. Ensure the plugin file is executable by running chmod +x awsprofile.1h.sh


from configparser import ConfigParser
from datetime import datetime
from os.path import (
    expanduser,
    join
)


class AWSProfiles:
    config_file = join(expanduser("~"), ".aws", "config")
    credentials_file = join(expanduser("~"), ".aws", "credentials")
    mfa_file = join(expanduser("~"), ".aws", "mfa_credentials")

    def __init__(self) -> None:
        self.config = ConfigParser()
        self.config.read(self.config_file)

        self.credentials = ConfigParser()
        self.credentials.read(self.credentials_file)

        self.mfa = ConfigParser()
        self.mfa.read(self.mfa_file)

    def print(self) -> None:
        sections = self.config.sections()
        for section in sections:
            print(self.build_item(section))

    def build_item(self, profile: str) -> str:

        # The :key: emoji means that the profile has MFA configured
        has_mfa = ":key:" if profile in self.mfa.sections() else ":grey_question:"

        # White means that the profile does not have a session token
        color = "white"

        if profile in self.credentials.sections():
            expiration = self.credentials[profile].get("aws_session_token_expiration", None)
            if expiration is not None:
                exp_date = datetime.strptime(expiration, "%Y-%m-%dT%H:%M:%S%z")
                # Green means that the profile has a valid session token
                # Red means that the profile has an expired session token
                color = "green" if exp_date > datetime.now(exp_date.tzinfo) else "red"

        item = f"{has_mfa} {profile} | color={color} | shell=awslogin | param1=--profile | param2={profile} | terminal=true"

        return item


if __name__ == '__main__':
    print('AWS Profiles')
    print('---')

    profiles = AWSProfiles()
    profiles.print()
