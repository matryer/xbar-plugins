#!/usr/local/bin/python3
# <bitbar.title>hours</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Udey Rishi</bitbar.author>
# <bitbar.author.github>udeyrishi</bitbar.author.github>
# <bitbar.desc>A simple command line tool for managing your work hours and the money you make</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/udeyrishi/hours/master/assets/shift_ongoing.png</bitbar.image>
# <bitbar.dependencies>Python3</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/udeyrishi/hours</bitbar.abouturl>

from argparse import ArgumentParser
import csv
from enum import Enum, auto
import os
from distutils.util import strtobool
from math import isclose
import sys
import time

LOG_FILE_PATH = os.path.join(os.path.expanduser('~'), '.hours', 'log.csv')

class ModeFailException(Exception):
    pass

def prompt_until_success(question, parser_fn, default=None):
    while True:
        print(question, end='')
        try:
            return parser_fn(input())
        except ValueError:
            if default is None:
                print('Not a valid response.')
            else:
                return default

def script_path():
    return os.path.realpath(__file__)

def script_name():
    return os.path.basename(__file__)

class LogEvent(Enum):
    WAGE_SET = auto()
    PAYMENT = auto()
    START = auto()
    END = auto()

def positive_float(val):
    num = float(val)
    if num < 0:
        raise ValueError(f'{val} is a negative number.')
    return num

class LogReport:
    def __init__(self, active_wage=None, current_shift_started_at=None, earned_past_shifts=0, total_paid=0):
        self.active_wage = active_wage
        self.current_shift_started_at = current_shift_started_at
        self.earned_past_shifts = earned_past_shifts
        self.total_paid = total_paid

    @property
    def total_earned(self):
        return self.earned_past_shifts + (((time.time() - self.current_shift_started_at)/60/60*self.active_wage) if self.current_shift_started_at is not None else 0)

    @property
    def outstanding_payment(self):
        return self.total_earned - self.total_paid

    @property
    def has_outstanding_payment(self):
        return not isclose(self.total_earned, self.total_paid, abs_tol=0.01)

    @property
    def in_shift(self):
        return self.current_shift_started_at != None

    @property
    def has_active_wage(self):
        return self.active_wage != None

    @property
    def current_shift_duration(self):
        if self.current_shift_started_at is None:
            return None
        else:
            duration = time.time() - self.current_shift_started_at
            if duration < 0:
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; the ongoing shift seems to have been started in the future.')
            m, s = divmod(duration, 60)
            h, m = divmod(m, 60)
            return f'{int(h):02d}:{int(m):02d}:{int(s):02d}'

def prepare_report():
    report = LogReport()
    
    for event, value in read_log():
        if event == LogEvent.WAGE_SET:
            report.active_wage = value
        elif event == LogEvent.PAYMENT:
            report.total_paid += value
        elif event == LogEvent.START: 
            if report.in_shift:
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; found two successive {LogEvent.START.name}s without a {LogEvent.END.name} in between. Try fixing or deleting it.')
            if report.active_wage is None:
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; A shift {event.name} event occurred before any {LogEvent.WAGE_SET.name} event.')
            report.current_shift_started_at = value
        elif event == LogEvent.END:
            if not report.in_shift:
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; found two successive {LogEvent.END.name}s without a {LogEvent.START.name} in between. Try fixing or deleting it.')
            if report.active_wage is None:
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; A shift {event.name} event occurred before any {LogEvent.WAGE_SET.name} event.')
            
            seconds = value - report.current_shift_started_at
            report.current_shift_started_at = None
            if (seconds < 0):
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; A shift\'s duration cannot be negative. Try fixing or deleting it.')
            
            report.earned_past_shifts += (seconds/60/60) * report.active_wage
        else:
            assert False, f'Support for new LogEvent {event.name} not added.'

    return report


def read_log():
    with open(LOG_FILE_PATH, 'r') as log_file:
        csv_reader = csv.reader(log_file)
        for log in csv_reader:
            event = next((e for e in LogEvent if e.name == log[0]), None)
            if event is None:
                raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; found an unknown log event: {log}')
            value = float(log[1])
            yield event, value

def write_log(event, value):
    with open(LOG_FILE_PATH, 'a') as log_file:
        csv_writer = csv.writer(log_file)
        csv_writer.writerow([event.name, value])

def read_sanitized_report(expected_in_shift=None, if_shift_err=None):
    if (expected_in_shift is None and if_shift_err is not None) or (expected_in_shift is not None and if_shift_err is None):
        raise ValueError('Either both, or neither of expected_in_shift and if_shift_err should be null.')

    report = prepare_report()
    if not report.has_active_wage:
        raise ModeFailException(f'Log file at {LOG_FILE_PATH} is corrupted; no {LogEvent.WAGE_SET.name} events found. Try fixing or deleting it.')

    if expected_in_shift is not None and report.in_shift != expected_in_shift:
        raise ModeFailException(if_shift_err)

    return report

def configure_as_new(ask_permission=True):
    if ask_permission:
        should_configure = prompt_until_success(question=f'Looks like you have never configured {script_name()} before. Would you like to do so now? [Y/n] ', parser_fn=lambda x: strtobool(x) == 1, default=True)
        if not should_configure:
            raise ModeFailException(f'{script_name()} cannot run without configuring.')

    wage = prompt_until_success(question='What is your hourly wage? ', parser_fn=positive_float)

    if not os.path.exists(os.path.dirname(LOG_FILE_PATH)):
        os.makedirs(os.path.dirname(LOG_FILE_PATH))

    write_log(LogEvent.WAGE_SET, wage)
    return LogReport(active_wage=wage)

class App:
    class Mode:
        def __init__(self, name, runner, help, is_default):
            self.name = name
            self.runner = runner
            self.help = help
            self.is_default = is_default

    def __init__(self):
        self.__registered_modes = []

    def run(self):
        assert len(self.__registered_modes) > 0, 'No modes were registered'
        default_modes = [mode for mode in self.__registered_modes if mode.is_default]
        assert len(default_modes) == 1, 'Exactly 1 mode should be registered as the default'
        default_mode = default_modes[0]

        parser = ArgumentParser(description='A tool for managing your work hours and the money you make.')
        group = parser.add_mutually_exclusive_group()

        for mode in self.__registered_modes:
            group.add_argument(f'-{mode.name[0]}', f'--{mode.name}', action='store_true', help=mode.help)

        args = parser.parse_args()

        matching_mode = next((mode for mode in self.__registered_modes if getattr(args, mode.name)), default_mode)
        try:
            matching_mode.runner()
            return 0
        except ModeFailException as e:
            print(str(e))
            return 3

    def register_mode(self, expected_in_shift=None, if_shift_err=None, help=None, configure_if_needed=True, is_default=False):
        def wrapper(mode_fn):
            report_param_name = next((param[0] for param in mode_fn.__annotations__.items() if param[1] == LogReport), None)
            num_other_params = len([param for param in mode_fn.__annotations__.items() if param[1] != LogReport])
            assert num_other_params == 0, 'mode functions can only optionally request the current report. Everything else must be gathered via user input for bitbar compatibility.'

            def mode_runner():
                if os.path.isfile(LOG_FILE_PATH):
                    report = read_sanitized_report(expected_in_shift, if_shift_err)
                elif configure_if_needed:
                    report = configure_as_new()
                else:
                    report = None

                kwargs = dict()
                if report_param_name is not None:
                    kwargs[report_param_name] = report
                
                mode_fn(**kwargs)

            self.__registered_modes.append(App.Mode(name=mode_fn.__name__, runner=mode_runner, help=help, is_default=is_default))
            return mode_runner
        return wrapper

app = App()

@app.register_mode(help='see the current status summary in a bitbar compatible syntax', configure_if_needed=False, is_default=True)
def bitbar(report: LogReport):
    if report is None:
        print(f'⚙️{script_name()} needs a one-time configuration.')
        print(f'Configure | refresh=true bash="{script_path()}" param1=-i terminal=true')
        return 0

    if report.in_shift:
        print(f'🕒 {report.current_shift_duration}')
    else:
        print('🏠')

    print('---')
    if report.in_shift:
        print(f'End Shift | refresh=true bash="{script_path()}" param1=-e terminal=false')
    else:
        print(f'Start Shift | refresh=true bash="{script_path()}" param1=-s terminal=false')

    print('---')
    if not report.in_shift:
        print(f'Update wage | refresh=true bash="{script_path()}" param1=-w terminal=true')
    print(f'Receive payment | refresh=true bash="{script_path()}" param1=-p terminal=true')

    print(f'Open log | refresh=true bash="less" param1={LOG_FILE_PATH} terminal=true')

    if report.has_outstanding_payment:
        print('---')
        if report.outstanding_payment > 0:
            print(f'💰 {report.outstanding_payment:.2f} pending')
        else:
            print(f'💰 {-report.outstanding_payment:.2f} overpaid')

@app.register_mode(help='see the current status summary info')
def info(report: LogReport):
    if report.in_shift:
        print(f'🕒 {report.current_shift_duration}', end='')
    else:
        print('🏠', end='')

    if report.has_outstanding_payment:
        print(' | ', end='')
        if report.outstanding_payment > 0:
            print(f'💰 {report.outstanding_payment:.2f} pending', end='')
        else:
            print(f'💰 {-report.outstanding_payment:.2f} overpaid', end='')
    print()

@app.register_mode(expected_in_shift=False, if_shift_err='Cannot change the wage while a shift is ongoing.', help='update the hourly wage moving forward', configure_if_needed=False)
def wage(report: LogReport):
    if report is None:
        # User is trying to use this mode as the first-time setup itself. Do not ask for wage 2x
        configure_as_new(ask_permission=False)
    else:
        wage = prompt_until_success(question='What is your new hourly wage? ', parser_fn=positive_float)
        write_log(LogEvent.WAGE_SET, wage)

@app.register_mode(help='add a received payment')
def payment():
    amount = prompt_until_success(question='How much amount did you receive? ', parser_fn=positive_float)
    write_log(LogEvent.PAYMENT, amount)

@app.register_mode(expected_in_shift=False, if_shift_err='Cannot start a shift while one is ongoing.', help='start a shift')
def start():
    write_log(LogEvent.START, time.time())

@app.register_mode(expected_in_shift=True, if_shift_err='Cannot end a shift when none is ongoing.', help='end a shift')
def end():
    write_log(LogEvent.END, time.time())

@app.register_mode(help='prints the path to the log file')
def log():
    print(LOG_FILE_PATH)

if __name__ == '__main__':
    sys.exit(app.run())