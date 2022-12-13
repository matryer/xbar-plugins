#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Kubeconfig Context Changer</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Chris Opland</xbar.author>
# <xbar.author.github>copland</xbar.author.github>
# <xbar.desc>Displays active kubeconfig context and allows you to easily change contexts.</xbar.desc>
# <xbar.dependencies>python,kubectl</xbar.dependencies>

from collections import namedtuple
from distutils import spawn
import os
import subprocess

Context = namedtuple('Context', ['name', 'active'])

os.environ['PATH'] = '/usr/local/bin:/usr/bin:%s' % os.getenv('PATH')
KUBECTL_PATH = spawn.find_executable('kubectl')


def get_active(contexts):
    return next((x for x in contexts if x.active), 'CONTEXT_NOT_SET')


def load_contexts():
    cmd = [
        KUBECTL_PATH,
        'config',
        'get-contexts',
        '--no-headers'
    ]
    out = subprocess.check_output(cmd)
    lines = out.split('\n')
    contexts = []
    for line in lines:
        columns = line.split()
        if columns == []:
            continue
        elif columns[0] == "*":
            contexts.append(Context(columns[1], True))
        else:
            contexts.append(Context(columns[0], False))
    return contexts


def display(contexts):
    active = get_active(contexts)
    print(active.name)
    print('---')
    for context in sorted(contexts, key=lambda x: x.name):
        vardict = {
            'context': context.name,
            'kubectl': KUBECTL_PATH
        }
        print("{context} | bash={kubectl} param1=config param2=use-context param3={context} terminal=false".format(**vardict))


if __name__ == '__main__':
    CONTEXTS = load_contexts()
    display(CONTEXTS)
