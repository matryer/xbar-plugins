#!/usr/bin/env python3

# <xbar.title>EmailPwn</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Henry Giddens</xbar.author>
# <xbar.desc>A simple xbar python plugin to generate disposable guerrillamail email addresses and read their messages</xbar.desc>
# <xbar.dependencies>python3.11</xbar.dependencies>

import sys
import subprocess
import os

# 1. Define a hidden venv folder right next to the plugin
PLUGIN_DIR = os.path.dirname(os.path.realpath(__file__))
VENV_DIR = os.path.join(PLUGIN_DIR, ".xbar_venv")
VENV_PYTHON = os.path.join(VENV_DIR, "bin", "python3.11")

if sys.executable != VENV_PYTHON:
    if not os.path.exists(VENV_PYTHON):
        # Create the venv silently
        subprocess.run([sys.executable, "-m", "venv", VENV_DIR], check=True)
        # Install direct dependencies; pip resolves their transitive requirements.
        subprocess.run(
            [
                VENV_PYTHON,
                "-m",
                "pip",
                "install",
                "guerrillamail-api==1.0.0",
                "macos-notifications==0.2.1",
                "tkinterweb==4.25.5",
            ],
            check=True,
        )
    
    # Relaunch this exact script using the new venv python interpreter
    os.execv(VENV_PYTHON, [VENV_PYTHON] + sys.argv)

from guerrillamail import GuerrillaMailClient
from mac_notifications import client
import tkinter as tk
import time
import queue
import threading
import AppKit
import signal
import re
import webbrowser
from html.parser import HTMLParser
from tkinterweb import HtmlFrame
from functools import partial

PID_FILE = "/tmp/EmailPwn.pid"
EMAIL_FILE = "/tmp/EmailPwn.email"
gm = None
email = ""
email_messages = queue.Queue()
pending_notifications = queue.Queue()
checker_finished = threading.Event()
expiration_pending = threading.Event()
stop_event = threading.Event()
instance_pid = str(os.getpid())

def notifyMac(title, text, subtitle=""):
    def escapeAppleScriptString(value):
        return (
            str(value)
            .replace("\\", "\\\\")
            .replace('"', '\\"')
            .replace("\r", "\\r")
            .replace("\n", "\\n")
        )

    script = (
        f'display notification "{escapeAppleScriptString(text)}" '
        f'with title "{escapeAppleScriptString(title)}" '
        f'subtitle "{escapeAppleScriptString(subtitle)}"'
    )
    subprocess.run(["osascript", "-e", script], check=True)

def stopPreviousInstance():
    try:
        with open(PID_FILE) as pid_file:
            previous_pid = int(pid_file.read().strip())
    except (FileNotFoundError, ValueError):
        previous_pid = None

    if previous_pid and previous_pid != os.getpid():
        try:
            os.kill(previous_pid, signal.SIGTERM)
        except ProcessLookupError:
            pass

        for _ in range(50):
            try:
                os.kill(previous_pid, 0)
            except ProcessLookupError:
                break
            time.sleep(0.1)

    with open(PID_FILE, "w") as pid_file:
        pid_file.write(instance_pid)

def releaseInstance():
    try:
        with open(PID_FILE) as pid_file:
            if pid_file.read().strip() == instance_pid:
                os.remove(PID_FILE)
    except FileNotFoundError:
        pass

def readCurrentEmail():
    try:
        with open(EMAIL_FILE) as email_file:
            return email_file.read().strip()
    except FileNotFoundError:
        return ""

def removeCurrentEmail():
    current_email = readCurrentEmail()
    previous_pid = None
    try:
        with open(PID_FILE) as pid_file:
            previous_pid = int(pid_file.read().strip())
        if previous_pid != os.getpid():
            os.kill(previous_pid, signal.SIGTERM)
            for _ in range(100):
                try:
                    os.kill(previous_pid, 0)
                except ProcessLookupError:
                    break
                time.sleep(0.1)
    except (FileNotFoundError, ProcessLookupError, ValueError):
        pass

    try:
        os.remove(EMAIL_FILE)
    except FileNotFoundError:
        pass
    try:
        with open(PID_FILE) as pid_file:
            if pid_file.read().strip() == str(previous_pid):
                os.remove(PID_FILE)
    except FileNotFoundError:
        pass
    client.create_notification(
        title="EmailPwn",
        subtitle=f"Address expired: {current_email}",
        text="This email address has expired. Request a new address to continue receiving emails.",
    )

def clearCurrentEmail():
    try:
        if readCurrentEmail() == email:
            os.remove(EMAIL_FILE)
    except FileNotFoundError:
        pass

def handleStop(signum, frame):
    stop_event.set()

def plainTextFromHTML(source):
    parser = NotificationHTMLParser()
    parser.feed(source)
    parser.close()
    return re.sub(r"\n[ \t]*\n+", "\n\n", "".join(parser.text)).strip()

class NotificationHTMLParser(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.text = []
        self.ignored_tags = 0

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.ignored_tags += 1
        elif tag in {"br", "div", "p", "li", "tr", "h1", "h2", "h3", "h4", "h5", "h6"}:
            self.text.append("\n")

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.ignored_tags = max(0, self.ignored_tags - 1)
        elif tag in {"div", "p", "li", "tr", "h1", "h2", "h3", "h4", "h5", "h6"}:
            self.text.append("\n")

    def handle_data(self, data):
        if not self.ignored_tags:
            self.text.append(data)

def cleanMessageBody(msg):
    return plainTextFromHTML(str(msg.mail_body or msg.mail_excerpt or ""))

def messageSubject(msg):
    subject = plainTextFromHTML(str(msg.mail_subject or ""))
    if subject:
        return subject

    plain_body = cleanMessageBody(msg)
    subject_match = re.search(
        r"(?im)^\s*subject\s*:\s*(.+?)\s*$",
        plain_body,
    )
    return subject_match.group(1).strip() if subject_match else "(No subject)"

def checkEmails():
    deadline = time.monotonic() + 125
    gm.check_inbox()

    try:
        while not stop_event.is_set() and time.monotonic() < deadline:
            try:
                msg = gm.wait_for_message(
                    timeout=min(deadline - time.monotonic(), 3),
                    poll_interval=1,
                )
            except TimeoutError:
                continue

            pending_notifications.put(msg)

    finally:
        gm.forget_address()
        gm.close()
        if not stop_event.is_set() and time.monotonic() >= deadline:
            expiration_pending.set()
            clearCurrentEmail()
        checker_finished.set()

def runEmailChecker():
    root = tk.Tk()
    root.withdraw()
    open_windows = set()
    AppKit.NSApp.setActivationPolicy_(
        AppKit.NSApplicationActivationPolicyAccessory
    )
    checker_thread = threading.Thread(target=checkEmails, name="email-checker")
    checker_thread.start()

    def process_messages():
        while True:
            try:
                msg = pending_notifications.get_nowait()
            except queue.Empty:
                break

            client.create_notification(
                title="EmailPwn",
                subtitle=messageSubject(msg),
                text=cleanMessageBody(msg),
                action_button_str="Open Email",
                action_callback=partial(onEmailRsvClick, msg)
            )

        while True:
            try:
                msg = email_messages.get_nowait()
            except queue.Empty:
                break
            showEmailWindow(msg, root, open_windows)

        if checker_finished.is_set():
            if expiration_pending.is_set():
                client.create_notification(
                    title="EmailPwn",
                    subtitle=f"Address expired: {email}",
                    text="This email address has expired. Request a new address to continue receiving emails.",
                )
                for window in tuple(open_windows):
                    window.destroy()
                open_windows.clear()
            root.quit()
            root.destroy()
            return

        root.after(100, process_messages)

    root.after(100, process_messages)
    try:
        root.mainloop()
    finally:
        stop_event.set()
        checker_thread.join(timeout=10)
        if checker_thread.is_alive():
            raise RuntimeError("Email checker thread did not stop cleanly")
        client.get_notification_manager().cleanup()

def onEmailRsvClick(msg):
    email_messages.put(msg)

def openEmailLink(url):
    if not webbrowser.open(url, new=2):
        raise RuntimeError(f"Unable to open email link: {url}")

def showEmailWindow(msg, root, open_windows):
    window = tk.Toplevel(root)
    open_windows.add(window)
    window.protocol("WM_DELETE_WINDOW", lambda: closeEmailWindow(window, open_windows))
    window.title("Email")
    window.geometry("600x500")
    window.configure(background="white")
    text_color = "#111111"
    body_html = str(msg.mail_body or msg.mail_excerpt or "")
    subject = messageSubject(msg)
    header = tk.Frame(window, background="white")
    header.pack(fill="x", padx=20, pady=(15, 5))

    close_button = tk.Button(
        header,
        text="×",
        command=lambda: closeEmailWindow(window, open_windows),
        font=("Arial", 16),
        relief="flat",
        background="white",
        activebackground="#eeeeee",
    )
    close_button.pack(side="right", anchor="ne")

    tk.Label(
        header,
        text=f"From: {msg.mail_from}",
        font=("Arial", 12),
        background="white",
        foreground=text_color,
        anchor="w",
    ).pack(fill="x", pady=(5, 8))

    tk.Label(
        header,
        text=f"Subject: {subject}",
        font=("Arial", 14, "bold"),
        background="white",
        foreground=text_color,
        anchor="w",
        justify="left",
        wraplength=540,
    ).pack(fill="x", pady=(8, 0))

    body = HtmlFrame(
        window,
        messages_enabled=False,
        on_link_click=openEmailLink,
    )
    body.load_html(body_html or "<p>(This email has no body.)</p>")
    body.pack(fill="both", expand=True, padx=20, pady=15)

def closeEmailWindow(window, open_windows):
    open_windows.discard(window)
    window.destroy()

def generateEmail():
    global gm, email
    stopPreviousInstance()
    signal.signal(signal.SIGTERM, handleStop)
    gm = GuerrillaMailClient()
    email = gm.create_email().address
    with open(EMAIL_FILE, "w") as email_file:
        email_file.write(email)

    # Use macOS pbcopy via stdin
    process = subprocess.Popen('pbcopy', env={'LANG': 'en_US.UTF-8'}, stdin=subprocess.PIPE)
    process.communicate(email.encode('utf-8'))

    notifyMac(
        title="Copied to Clipboard!",
        subtitle=f"Address: {email}",
        text="Expires in 2 minutes.\n\nEmails received will send notifications",
    )
    try:
        runEmailChecker()
    finally:
        releaseInstance()

current_email = readCurrentEmail()
print("EmailPwn")
print("---")
if current_email:
    print(f"Current Email: {current_email}")
else:
    print(f"Generate New Email | bash='{sys.argv[0]}' param1='--copy' terminal=false | color=#40ff53 | refresh=true | disabled=false")
if current_email:
    print(
        f"Delete current email | color=#fc2b2b | "
        f"bash='{sys.argv[0]}' param1='--remove' terminal=false | "
        f"refresh=true"
    )

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == '--copy':
        generateEmail()
    elif len(sys.argv) > 1 and sys.argv[1] == '--remove':
        removeCurrentEmail()