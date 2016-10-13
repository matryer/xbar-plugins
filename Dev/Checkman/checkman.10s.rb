#!/usr/bin/env ruby

# <bitbar.title>Checkman Simulator</bitbar.title>
# <bitbar.version>v0.1.1</bitbar.version>
# <bitbar.author>Deluan Quintao</bitbar.author>
# <bitbar.author.github>deluan</bitbar.author.github>
# <bitbar.desc>This plugin reuses Checkman's configurations and plugins, so you don't need to install it :) It has checks for: HTTP, GoCD, Concourse, Jenkins, Travis, Semaphore, Codeship, CircleCI, Airbrake, GitHub, Pivotal Tracker, TDDium and SnapCI, and you can even create your own plugins to leverage Checkman's streamlined UI and configuration files. More info: https://gist.github.com/deluan/3f6fa6bcff2a355ae89181bb15590b88#file-readme-md</bitbar.desc>
# <bitbar.image>http://i.imgur.com/irmlsOX.jpg</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://goo.gl/SdNXj2</bitbar.abouturl>

# This plugin simulates Checkman functionality, allowing you to use all its
# plugins and creating checks in external files. It downloads Checkman's plugins
# on demand, so you don't need to install Checkman itself.
#
# To learn more about Checkman, and learn about its existing plugins, see:
# https://github.com/cppforlife/checkman#included-check-scripts
#
# Configuration
# Same as for Checkman: https://github.com/cppforlife/checkman#configuring-checkman-via-checkfiles
#
# Missing/Not yet implemented:
# - Notifications/Stickies

require 'json'
require 'fileutils'
require 'open-uri'

CHECKS_DIR="#{ENV['HOME']}/.bitbar-checkman-plugins/"
CHECKS_URL="https://raw.githubusercontent.com/cppforlife/checkman/master/scripts/"
CONFIG_DIR="#{ENV['HOME']}/Checkman/"

README_URL="https://goo.gl/SdNXj2"

ICON_OK="iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAEJGlDQ1BJQ0MgUHJvZmlsZQAAOBGFVd9v21QUPolvUqQWPyBYR4eKxa9VU1u5GxqtxgZJk6XtShal6dgqJOQ6N4mpGwfb6baqT3uBNwb8AUDZAw9IPCENBmJ72fbAtElThyqqSUh76MQPISbtBVXhu3ZiJ1PEXPX6yznfOec7517bRD1fabWaGVWIlquunc8klZOnFpSeTYrSs9RLA9Sr6U4tkcvNEi7BFffO6+EdigjL7ZHu/k72I796i9zRiSJPwG4VHX0Z+AxRzNRrtksUvwf7+Gm3BtzzHPDTNgQCqwKXfZwSeNHHJz1OIT8JjtAq6xWtCLwGPLzYZi+3YV8DGMiT4VVuG7oiZpGzrZJhcs/hL49xtzH/Dy6bdfTsXYNY+5yluWO4D4neK/ZUvok/17X0HPBLsF+vuUlhfwX4j/rSfAJ4H1H0qZJ9dN7nR19frRTeBt4Fe9FwpwtN+2p1MXscGLHR9SXrmMgjONd1ZxKzpBeA71b4tNhj6JGoyFNp4GHgwUp9qplfmnFW5oTdy7NamcwCI49kv6fN5IAHgD+0rbyoBc3SOjczohbyS1drbq6pQdqumllRC/0ymTtej8gpbbuVwpQfyw66dqEZyxZKxtHpJn+tZnpnEdrYBbueF9qQn93S7HQGGHnYP7w6L+YGHNtd1FJitqPAR+hERCNOFi1i1alKO6RQnjKUxL1GNjwlMsiEhcPLYTEiT9ISbN15OY/jx4SMshe9LaJRpTvHr3C/ybFYP1PZAfwfYrPsMBtnE6SwN9ib7AhLwTrBDgUKcm06FSrTfSj187xPdVQWOk5Q8vxAfSiIUc7Z7xr6zY/+hpqwSyv0I0/QMTRb7RMgBxNodTfSPqdraz/sDjzKBrv4zu2+a2t0/HHzjd2Lbcc2sG7GtsL42K+xLfxtUgI7YHqKlqHK8HbCCXgjHT1cAdMlDetv4FnQ2lLasaOl6vmB0CMmwT/IPszSueHQqv6i/qluqF+oF9TfO2qEGTumJH0qfSv9KH0nfS/9TIp0Wboi/SRdlb6RLgU5u++9nyXYe69fYRPdil1o1WufNSdTTsp75BfllPy8/LI8G7AUuV8ek6fkvfDsCfbNDP0dvRh0CrNqTbV7LfEEGDQPJQadBtfGVMWEq3QWWdufk6ZSNsjG2PQjp3ZcnOWWing6noonSInvi0/Ex+IzAreevPhe+CawpgP1/pMTMDo64G0sTCXIM+KdOnFWRfQKdJvQzV1+Bt8OokmrdtY2yhVX2a+qrykJfMq4Ml3VR4cVzTQVz+UoNne4vcKLoyS+gyKO6EHe+75Fdt0Mbe5bRIf/wjvrVmhbqBN97RD1vxrahvBOfOYzoosH9bq94uejSOQGkVM6sN/7HelL4t10t9F4gPdVzydEOx83Gv+uNxo7XyL/FtFl8z9ZAHF4bBsrEwAAAVdJREFUKBWVU1FOwkAUfPvahgL+mHgMLyF+kngUQwieQlHDUYx+qpfwGCb+iFba3XXmwUojJNaG0nbfzJt526mLMQqP0W051lxnrpDjLHNHora8/gsi3sfXWMtLaMLV06R6YMGRfLoYXOZ9mRYHqnmJxQyncz9sYqIXaSqR+j2E5lOuH88/LtzJTW9cDPWuPFQthk40BweqLa6YOaiHBuRllOothHoZznJapSKJWW9XNclTnY6AEF+r+i+ZKWekVSr+tpuIRoEV1omz0cBTbo51NKvbOdvEdG/7QByakKc2H6rtGRN435U4k0AT/HB0ZaZuG/yabNuZKh2uG7wKXgFj0pVPnMUKPLXkIADW5I8OlkaKAU+eMnJMDgPARQPscZ5SRhzx5KlfhTkjx+TgxUtYoWsd0Wx72jPXUSeOeGbcsj1aDOZFXyb/znay2fWrotPnaXXPyb4BZsjMvJjQ4YcAAAAASUVORK5CYII="
ICON_FAIL="iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAEJGlDQ1BJQ0MgUHJvZmlsZQAAOBGFVd9v21QUPolvUqQWPyBYR4eKxa9VU1u5GxqtxgZJk6XtShal6dgqJOQ6N4mpGwfb6baqT3uBNwb8AUDZAw9IPCENBmJ72fbAtElThyqqSUh76MQPISbtBVXhu3ZiJ1PEXPX6yznfOec7517bRD1fabWaGVWIlquunc8klZOnFpSeTYrSs9RLA9Sr6U4tkcvNEi7BFffO6+EdigjL7ZHu/k72I796i9zRiSJPwG4VHX0Z+AxRzNRrtksUvwf7+Gm3BtzzHPDTNgQCqwKXfZwSeNHHJz1OIT8JjtAq6xWtCLwGPLzYZi+3YV8DGMiT4VVuG7oiZpGzrZJhcs/hL49xtzH/Dy6bdfTsXYNY+5yluWO4D4neK/ZUvok/17X0HPBLsF+vuUlhfwX4j/rSfAJ4H1H0qZJ9dN7nR19frRTeBt4Fe9FwpwtN+2p1MXscGLHR9SXrmMgjONd1ZxKzpBeA71b4tNhj6JGoyFNp4GHgwUp9qplfmnFW5oTdy7NamcwCI49kv6fN5IAHgD+0rbyoBc3SOjczohbyS1drbq6pQdqumllRC/0ymTtej8gpbbuVwpQfyw66dqEZyxZKxtHpJn+tZnpnEdrYBbueF9qQn93S7HQGGHnYP7w6L+YGHNtd1FJitqPAR+hERCNOFi1i1alKO6RQnjKUxL1GNjwlMsiEhcPLYTEiT9ISbN15OY/jx4SMshe9LaJRpTvHr3C/ybFYP1PZAfwfYrPsMBtnE6SwN9ib7AhLwTrBDgUKcm06FSrTfSj187xPdVQWOk5Q8vxAfSiIUc7Z7xr6zY/+hpqwSyv0I0/QMTRb7RMgBxNodTfSPqdraz/sDjzKBrv4zu2+a2t0/HHzjd2Lbcc2sG7GtsL42K+xLfxtUgI7YHqKlqHK8HbCCXgjHT1cAdMlDetv4FnQ2lLasaOl6vmB0CMmwT/IPszSueHQqv6i/qluqF+oF9TfO2qEGTumJH0qfSv9KH0nfS/9TIp0Wboi/SRdlb6RLgU5u++9nyXYe69fYRPdil1o1WufNSdTTsp75BfllPy8/LI8G7AUuV8ek6fkvfDsCfbNDP0dvRh0CrNqTbV7LfEEGDQPJQadBtfGVMWEq3QWWdufk6ZSNsjG2PQjp3ZcnOWWing6noonSInvi0/Ex+IzAreevPhe+CawpgP1/pMTMDo64G0sTCXIM+KdOnFWRfQKdJvQzV1+Bt8OokmrdtY2yhVX2a+qrykJfMq4Ml3VR4cVzTQVz+UoNne4vcKLoyS+gyKO6EHe+75Fdt0Mbe5bRIf/wjvrVmhbqBN97RD1vxrahvBOfOYzoosH9bq94uejSOQGkVM6sN/7HelL4t10t9F4gPdVzydEOx83Gv+uNxo7XyL/FtFl8z9ZAHF4bBsrEwAAAUJJREFUKBWVk1tOwzAQRccTIlFVUCgsozupxDaAP4TKKqB8FrENBDvpMiDioQrU1DZz4rjiA6JgaWzHc8/YTm5cjFFod+PRtFSdlU4mhXPHrllNHQof43MdZVmHcHNevT2RccD3RwfXA3WXe6q665wU4sSGbaO+lyhfNvkIIXyGeHv68nrlFof702GhD+Oi0KERpYVSdYuKYSLBojZ4ZVF5H1Y+nOxwVHYETLsa+HPbtggntOs0T7Xp11Fmyh2B2LGw1G8gBOvk0SW9TJSXwx2bo1qiq1EAHXo4RZ6jC8y5rGWkUKKbSc8O0lqC06dOK336Vq+MOfpyWa84BwPwHbPb/ipCHh16OMVyOAcD+I4CgOTRJb0sdRP8HMvhHBbXrWBj8xwArJNHhx6Pt94ezQeqF//2dr5n37+Kk55V7492EPkGePnLtKbOBRoAAAAASUVORK5CYII="
ICON_UNDETERMINED="iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAEJGlDQ1BJQ0MgUHJvZmlsZQAAOBGFVd9v21QUPolvUqQWPyBYR4eKxa9VU1u5GxqtxgZJk6XtShal6dgqJOQ6N4mpGwfb6baqT3uBNwb8AUDZAw9IPCENBmJ72fbAtElThyqqSUh76MQPISbtBVXhu3ZiJ1PEXPX6yznfOec7517bRD1fabWaGVWIlquunc8klZOnFpSeTYrSs9RLA9Sr6U4tkcvNEi7BFffO6+EdigjL7ZHu/k72I796i9zRiSJPwG4VHX0Z+AxRzNRrtksUvwf7+Gm3BtzzHPDTNgQCqwKXfZwSeNHHJz1OIT8JjtAq6xWtCLwGPLzYZi+3YV8DGMiT4VVuG7oiZpGzrZJhcs/hL49xtzH/Dy6bdfTsXYNY+5yluWO4D4neK/ZUvok/17X0HPBLsF+vuUlhfwX4j/rSfAJ4H1H0qZJ9dN7nR19frRTeBt4Fe9FwpwtN+2p1MXscGLHR9SXrmMgjONd1ZxKzpBeA71b4tNhj6JGoyFNp4GHgwUp9qplfmnFW5oTdy7NamcwCI49kv6fN5IAHgD+0rbyoBc3SOjczohbyS1drbq6pQdqumllRC/0ymTtej8gpbbuVwpQfyw66dqEZyxZKxtHpJn+tZnpnEdrYBbueF9qQn93S7HQGGHnYP7w6L+YGHNtd1FJitqPAR+hERCNOFi1i1alKO6RQnjKUxL1GNjwlMsiEhcPLYTEiT9ISbN15OY/jx4SMshe9LaJRpTvHr3C/ybFYP1PZAfwfYrPsMBtnE6SwN9ib7AhLwTrBDgUKcm06FSrTfSj187xPdVQWOk5Q8vxAfSiIUc7Z7xr6zY/+hpqwSyv0I0/QMTRb7RMgBxNodTfSPqdraz/sDjzKBrv4zu2+a2t0/HHzjd2Lbcc2sG7GtsL42K+xLfxtUgI7YHqKlqHK8HbCCXgjHT1cAdMlDetv4FnQ2lLasaOl6vmB0CMmwT/IPszSueHQqv6i/qluqF+oF9TfO2qEGTumJH0qfSv9KH0nfS/9TIp0Wboi/SRdlb6RLgU5u++9nyXYe69fYRPdil1o1WufNSdTTsp75BfllPy8/LI8G7AUuV8ek6fkvfDsCfbNDP0dvRh0CrNqTbV7LfEEGDQPJQadBtfGVMWEq3QWWdufk6ZSNsjG2PQjp3ZcnOWWing6noonSInvi0/Ex+IzAreevPhe+CawpgP1/pMTMDo64G0sTCXIM+KdOnFWRfQKdJvQzV1+Bt8OokmrdtY2yhVX2a+qrykJfMq4Ml3VR4cVzTQVz+UoNne4vcKLoyS+gyKO6EHe+75Fdt0Mbe5bRIf/wjvrVmhbqBN97RD1vxrahvBOfOYzoosH9bq94uejSOQGkVM6sN/7HelL4t10t9F4gPdVzydEOx83Gv+uNxo7XyL/FtFl8z9ZAHF4bBsrEwAAAUlJREFUKBWdk01ugzAQhW0TECCkbjhG9t31ApF6CViwrtJT9GdXCSEuQdUeg11XPUOXCBA/pvNcT4VQ1EaxZJOM53sz2A+5LIvASNP04Lru0XGcPc1YSmniWJAzz/MXzY9xHB/LsnxHXGIjy7IHz/PugiBQJCCUUmILa60FgaLrOj0Mw3Oe5/cySZJDGIavURQp3/cFVTXgFrbVRd/3omka3bbt7Q6toiLAU1XRHgZgdIRB7Svq4qjwjoBQcduuybQLOsE+8mz+HnDM0LrVNci/sb8SiRUH/gO3AkaEg5c8f07gEpIYhVPkeY4G55rTh3NgAA7+JcA5yAenrOXwR7DIKQGAFjJOA7ej5Yksd0NXZt4f98g3wCJcEQXgMFgUHnfquv6squqKNq4pSbI6Eqdp+p3wNUCypfF2URQv5sNAhXO/KnRK4BuYbwDN9qHScGVnAAAAAElFTkSuQmCC"

ICON_OK_CHANGING="iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAEJGlDQ1BJQ0MgUHJvZmlsZQAAOBGFVd9v21QUPolvUqQWPyBYR4eKxa9VU1u5GxqtxgZJk6XtShal6dgqJOQ6N4mpGwfb6baqT3uBNwb8AUDZAw9IPCENBmJ72fbAtElThyqqSUh76MQPISbtBVXhu3ZiJ1PEXPX6yznfOec7517bRD1fabWaGVWIlquunc8klZOnFpSeTYrSs9RLA9Sr6U4tkcvNEi7BFffO6+EdigjL7ZHu/k72I796i9zRiSJPwG4VHX0Z+AxRzNRrtksUvwf7+Gm3BtzzHPDTNgQCqwKXfZwSeNHHJz1OIT8JjtAq6xWtCLwGPLzYZi+3YV8DGMiT4VVuG7oiZpGzrZJhcs/hL49xtzH/Dy6bdfTsXYNY+5yluWO4D4neK/ZUvok/17X0HPBLsF+vuUlhfwX4j/rSfAJ4H1H0qZJ9dN7nR19frRTeBt4Fe9FwpwtN+2p1MXscGLHR9SXrmMgjONd1ZxKzpBeA71b4tNhj6JGoyFNp4GHgwUp9qplfmnFW5oTdy7NamcwCI49kv6fN5IAHgD+0rbyoBc3SOjczohbyS1drbq6pQdqumllRC/0ymTtej8gpbbuVwpQfyw66dqEZyxZKxtHpJn+tZnpnEdrYBbueF9qQn93S7HQGGHnYP7w6L+YGHNtd1FJitqPAR+hERCNOFi1i1alKO6RQnjKUxL1GNjwlMsiEhcPLYTEiT9ISbN15OY/jx4SMshe9LaJRpTvHr3C/ybFYP1PZAfwfYrPsMBtnE6SwN9ib7AhLwTrBDgUKcm06FSrTfSj187xPdVQWOk5Q8vxAfSiIUc7Z7xr6zY/+hpqwSyv0I0/QMTRb7RMgBxNodTfSPqdraz/sDjzKBrv4zu2+a2t0/HHzjd2Lbcc2sG7GtsL42K+xLfxtUgI7YHqKlqHK8HbCCXgjHT1cAdMlDetv4FnQ2lLasaOl6vmB0CMmwT/IPszSueHQqv6i/qluqF+oF9TfO2qEGTumJH0qfSv9KH0nfS/9TIp0Wboi/SRdlb6RLgU5u++9nyXYe69fYRPdil1o1WufNSdTTsp75BfllPy8/LI8G7AUuV8ek6fkvfDsCfbNDP0dvRh0CrNqTbV7LfEEGDQPJQadBtfGVMWEq3QWWdufk6ZSNsjG2PQjp3ZcnOWWing6noonSInvi0/Ex+IzAreevPhe+CawpgP1/pMTMDo64G0sTCXIM+KdOnFWRfQKdJvQzV1+Bt8OokmrdtY2yhVX2a+qrykJfMq4Ml3VR4cVzTQVz+UoNne4vcKLoyS+gyKO6EHe+75Fdt0Mbe5bRIf/wjvrVmhbqBN97RD1vxrahvBOfOYzoosH9bq94uejSOQGkVM6sN/7HelL4t10t9F4gPdVzydEOx83Gv+uNxo7XyL/FtFl8z9ZAHF4bBsrEwAAAZRJREFUKBXFkj1LA0EQhmf29j7yYWU6/4K1RUxjUlpZKyG/QCRaWwipNPgFNpZ21hZWWqm9+APsFUGMl7vc3Ywzm4ARLCSNCwO3O/O88+7sITPDrMvMCir3f7D9q+16DxfCatBgwHlg+ijS7BHre9HF/W7SlsH9OrnWabCExvZNCMvWR9RmRQqQvotMoxexjfAuMcONhy1+nnaychauB4F3HlRNyUYAaIWVFlnMkLwJ3DwusV9RQY6J8JIov5ZdYjy75kXQDiqIIg7GF1jGy8UYHr4wWC9ECOY0iWUm6FDud9SckWlozgTy7QmoIQmS1s68CFk9VFVbQqesttySpIP0MUVMQV06GVdCwlDBr8BYcyLSbVLjCqehMSiYQGq9EM5wBk+5TI9zlZXQLkZdSEwp6WMoRFKn01bOUE4H2YAo+2R3SCNJZixF3+H2ei6Q1qUD4mJEfVTF1kl535ag61eNcU8yGc7Yu3SZdHXvK2Aew+HNZrztYC1qHkWrxpod9GHR87D246+Xe+od1ap2vO0mV8p8Afhywbpd85/uAAAAAElFTkSuQmCC"
ICON_FAIL_CHANGING="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"
ICON_UNDETERMINED_CHANGING="iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAEJGlDQ1BJQ0MgUHJvZmlsZQAAOBGFVd9v21QUPolvUqQWPyBYR4eKxa9VU1u5GxqtxgZJk6XtShal6dgqJOQ6N4mpGwfb6baqT3uBNwb8AUDZAw9IPCENBmJ72fbAtElThyqqSUh76MQPISbtBVXhu3ZiJ1PEXPX6yznfOec7517bRD1fabWaGVWIlquunc8klZOnFpSeTYrSs9RLA9Sr6U4tkcvNEi7BFffO6+EdigjL7ZHu/k72I796i9zRiSJPwG4VHX0Z+AxRzNRrtksUvwf7+Gm3BtzzHPDTNgQCqwKXfZwSeNHHJz1OIT8JjtAq6xWtCLwGPLzYZi+3YV8DGMiT4VVuG7oiZpGzrZJhcs/hL49xtzH/Dy6bdfTsXYNY+5yluWO4D4neK/ZUvok/17X0HPBLsF+vuUlhfwX4j/rSfAJ4H1H0qZJ9dN7nR19frRTeBt4Fe9FwpwtN+2p1MXscGLHR9SXrmMgjONd1ZxKzpBeA71b4tNhj6JGoyFNp4GHgwUp9qplfmnFW5oTdy7NamcwCI49kv6fN5IAHgD+0rbyoBc3SOjczohbyS1drbq6pQdqumllRC/0ymTtej8gpbbuVwpQfyw66dqEZyxZKxtHpJn+tZnpnEdrYBbueF9qQn93S7HQGGHnYP7w6L+YGHNtd1FJitqPAR+hERCNOFi1i1alKO6RQnjKUxL1GNjwlMsiEhcPLYTEiT9ISbN15OY/jx4SMshe9LaJRpTvHr3C/ybFYP1PZAfwfYrPsMBtnE6SwN9ib7AhLwTrBDgUKcm06FSrTfSj187xPdVQWOk5Q8vxAfSiIUc7Z7xr6zY/+hpqwSyv0I0/QMTRb7RMgBxNodTfSPqdraz/sDjzKBrv4zu2+a2t0/HHzjd2Lbcc2sG7GtsL42K+xLfxtUgI7YHqKlqHK8HbCCXgjHT1cAdMlDetv4FnQ2lLasaOl6vmB0CMmwT/IPszSueHQqv6i/qluqF+oF9TfO2qEGTumJH0qfSv9KH0nfS/9TIp0Wboi/SRdlb6RLgU5u++9nyXYe69fYRPdil1o1WufNSdTTsp75BfllPy8/LI8G7AUuV8ek6fkvfDsCfbNDP0dvRh0CrNqTbV7LfEEGDQPJQadBtfGVMWEq3QWWdufk6ZSNsjG2PQjp3ZcnOWWing6noonSInvi0/Ex+IzAreevPhe+CawpgP1/pMTMDo64G0sTCXIM+KdOnFWRfQKdJvQzV1+Bt8OokmrdtY2yhVX2a+qrykJfMq4Ml3VR4cVzTQVz+UoNne4vcKLoyS+gyKO6EHe+75Fdt0Mbe5bRIf/wjvrVmhbqBN97RD1vxrahvBOfOYzoosH9bq94uejSOQGkVM6sN/7HelL4t10t9F4gPdVzydEOx83Gv+uNxo7XyL/FtFl8z9ZAHF4bBsrEwAAAWtJREFUKBXFUj1Lw1AUvS95bRrByW7+BWcRFP+Ak3Mk7VAo3fszDC4urUMHyeIWcPAPuHaQbi7+gC4hNEm/8jwn9IGIgnTxws3Lu+ece25enjLGyL7h7Cuk7v/E+q9jdzqd42azeaGUOoIm2263byoIgsc4jkMc3I8n1+v1TiGKtNbnSEWzzWYji8VCVBiGBuArCjeTyeSDoI3BYBAAe2i1Wj5WcV1X6LFcLiXLMlH9ft8AJD+vquoJ4AuyxHjXnueFwBSFcBXUBJxanKap6EajIb7vEzwA0MW3dEmiCwXEHcepk3UGV6YmQBK7W9ASiFmixTi2TQ2nOTZtEun2NazQ1qyIo1Pn4DFbr9fc1B0p+D4mxRTuRLLjzzRebouiuIRrfdvo/psjDcqyFPAN3iN3Op2+J0lyiM0ZuivbnUT+T5t0ozDPc7Nare5Go9G94jgMXIYrnOwQzifItj0gYuSg2Rw5Q0bj8fiZ9U9rAuOyymXbswAAAABJRU5ErkJggg=="

ICONS = {
  true: {
    false: ICON_OK,
    true: ICON_OK_CHANGING
  },
  false: {
    false: ICON_FAIL,
    true: ICON_FAIL_CHANGING
  },
}

PRIORITY_ORDER = [ICON_OK, ICON_OK_CHANGING, ICON_FAIL, ICON_FAIL_CHANGING, ICON_UNDETERMINED, ICON_UNDETERMINED_CHANGING]

DARK_MODE=`defaults read -g AppleInterfaceStyle 2> /dev/null`.strip
NO_DIM = " color=#{DARK_MODE == 'Dark' ? 'white' : 'black'} "

@output = ""
@failed = 0
@undetermined = 0
@aggregated_status = ICON_OK

def help
  puts " | image=#{ICON_UNDETERMINED}"
  puts "---"
  puts "Checkman Simulator v0.1.1|href=#{README_URL}"
  puts "More info...|href=#{README_URL}"
  puts "---"
  puts "No configuration files found in #{CONFIG_DIR} | bash=/usr/bin/open param1=\"#{CONFIG_DIR}\" terminal=false"
  puts "Click here to learn how to write configuration files... | href=https://github.com/cppforlife/checkman#configuring-checkman-via-checkfiles"
  puts "---\nRefresh... | refresh=true"
  exit
end

def plugin_path(plugin)
  FileUtils.mkdir_p(CHECKS_DIR)
  plugin_path = "#{CHECKS_DIR}/#{plugin}"
  return plugin_path if File.executable? plugin_path
  File.write(plugin_path, open("#{CHECKS_URL}/#{plugin}").read)
  File.chmod(0755, plugin_path)
  plugin_path
end

def format_info(info)
  lines = info[1].split("\n")
  s = "--#{info[0]}: #{lines[0]} | #{NO_DIM}"
  lines.each_with_index do |line, i|
    s += "\n--#{line} | #{NO_DIM}" if i > 0 
  end
  s += "href=#{info[1]}" if info[0].downcase == "url"
  s += "\n"
end

def parse_output(check_name, check_output)
  if check_output.strip == ""
    @output += "#{check_name}| #{NO_DIM} image= #{ICON_UNDETERMINED} \n"
    @undetermined += 1
    return ICON_UNDETERMINED
  end
  r = JSON.parse(check_output)
  icon = ICONS[r["result"].to_s.to_sym][(!!r["changing"]).to_s.to_sym]
  @output += "#{check_name}| image=#{icon} "
  @output += r['url'].nil? ? NO_DIM : "href=#{r['url']}"
  @output += "\n"
  @failed += 1 unless r["result"]
  unless r["info"].nil?
    r["info"].each do |i|
      if i[0] != '-'
        @output += format_info i
      else
        @output += "-----\n"
      end
    end
  end

  icon
end

check_files = Dir["#{CONFIG_DIR}/*"]
help if check_files.count == 0

check_files.each do |checkfile|
  check = File.open(checkfile).read
  check.each_line do |line|
    case line.strip
    when /^#-(.+)/
      @output += "#{$1.strip}| size=12\n"
    when '#-'
      @output += "---\n"
    when /^#.*/, ''
    else
      item = line.split(":", 2)
      cmd = item[1].strip.split(" ", 2)
      output = `(#{plugin_path cmd[0]} #{cmd[1]}) 2>/dev/null`
      icon = parse_output item[0], output
      if PRIORITY_ORDER.index(icon) > PRIORITY_ORDER.index(@aggregated_status)
        @aggregated_status = icon
      end
    end
  end
  @output += "---\n"
end

if @undetermined > 0
  print @undetermined
elsif @failed > 0
    print @failed
end
puts " | image=#{@aggregated_status}"
puts "---\n#{@output}"

puts "---\nRefresh... | refresh=true"
