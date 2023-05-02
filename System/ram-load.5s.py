#!/usr/bin/python3
#
#  <xbar.title>RAM Load</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Gustavo Ilha Morais</xbar.author>
#  <xbar.author.github>gustavoilhamorais</xbar.author.github>
#  <xbar.desc>Displays RAM load in bytes, KB, MB or GB. Can display free amount or used amount in percentage.</xbar.desc>
#  <xbar.image>https://imgur.com/a/oZx1iBq</xbar.image>
#  <xbar.dependencies>python3,psutil</xbar.dependencies>
#  <xbar.var>select(VAR_DISPLAY_METRIC="Used %"): Which metric to display. [bytes, KB, MB, GB, Free %, Used %]</xbar.var>

def main():
    try:
        import os
        import psutil

        class RamLoad:
            def __init__(self):
                self.displayOption = os.environ.get("VAR_DISPLAY_METRIC")

            def __createUsageText(self, memory):
                if self.displayOption == "bytes":
                    return f"{memory.used} bytes"
                elif self.displayOption == "KB":
                    return f"{memory.used / 1024:.2f} KB"
                elif self.displayOption == "MB":
                    return f"{memory.used / (1024 * 1024):.2f} MB"
                elif self.displayOption == "GB":
                    return f"{memory.used / (1024 * 1024 * 1024):.2f} GB"
                elif self.displayOption == "Free %":
                    return f"{100 * (memory.available / memory.total):.1f}%"
                elif self.displayOption == "Used %":
                    return f"{100 * (memory.total - memory.available) / memory.total:.1f}%"
                else:
                    return None

            def show_usage(self):
                memory_usage = psutil.virtual_memory()
                usageText = self.__createUsageText(memory_usage)
                print("Invalid metric." if usageText == None else usageText)

            def on_click(self):
                print("---")
                print("Refresh | refresh=true")

        ramLoad = RamLoad()
        ramLoad.show_usage()
        ramLoad.on_click()

    except ModuleNotFoundError:
        print("Error: missing 'psutil' library.")
        print("---")
        import sys
        import subprocess
        subprocess.run('pbcopy', universal_newlines=True,
                       input=f"{sys.executable} -m pip install psutil")
        print("Fix copied to clipboard. Paste on terminal and run.")


main()
