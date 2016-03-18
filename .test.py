#!/usr/bin/env python
DEBUG = False
import re
import os
import sys
import subprocess
import urllib2

allowed_image_content_types = [ 'image/png', 'image/jpeg' ]
required_metadata = [ 'author', 'author.github', 'title' ]
recommended_metadata = [ 'image', 'desc', 'version' ]
required_shebangs = {
    '.sh': '(bash|ksh|zsh|sh|fish)$',
    '.py': 'python(|2|3)$',
    '.rb': 'ruby$',
    '.js': 'node$',
    '.php': 'php$',
    '.pl': 'perl( -[wW])?$',
    '.swift': 'swift$',
}
linter_command = {
    '.sh': [ 'shellcheck' ],
    '.py': [ 'pyflakes' ],
    '.rb': [ 'rubocop', '-l' ],
    '.js': [ 'jshint' ],
    '.php': [ 'php', '-l' ],
    '.pl': [ 'perl', '-MO=Lint'],
    '.swift': [ 'xcrun', '-sdk', 'macosx', 'swiftc' ],
}
error_count = 0
def debug(s):
    if DEBUG:
        print "\033[1;44mDBG!\033[0;0m %s\n" % s

def warn(s):
    print "\033[1;43mWRN!\033[0;0m %s\n" % s

def error(s):
    global error_count
    error_count += 1
    print "\033[1;41mERR!\033[0;0m %s\n" % s

def check_file(file_full_path):
    file_short_name, file_extension = os.path.splitext(file_full_path)

    if not required_shebangs.get(file_extension, False):
        error("%s unrecognized file extension" % file_full_path)
        return

    if not os.access(file_full_path, os.R_OK):
        error("%s not readable" % file_full_path)
        return

    if not os.access(file_full_path, os.X_OK):
        error("%s not executable" % file_full_path)

    metadata = {}
    with open(file_full_path, "r") as fp:
        first_line = fp.readline().strip()
        shebang_re = required_shebangs.get(file_extension, '')
        if first_line[0:3] != '#!/' or re.search(shebang_re, first_line) is None:
            error("%s has incorrect shebang.\n  Got %s\n  Wanted %s" % (file_full_path, first_line, shebang_re))

        for line in fp:
            match = re.search("<bitbar.(?P<lho_tag>[^>]+)>(?P<value>[^<]+)</bitbar.(?P<rho_tag>[^>]+)>", line)
            if match is not None:
                if match.group('lho_tag') != match.group('rho_tag'):
                    error('%s includes mismatched metatags: %s' % (file_full_path, line))
                else:
                    metadata[match.group('lho_tag')] = match.group('value')

    for key in required_metadata:
        if key not in metadata:
            error('%s missing required metadata for %s' % (file_full_path, key))

    for key in recommended_metadata:
        if key not in metadata:
            warn('%s missing recommended metadata for %s' % (file_full_path, key))

    if metadata.get('image', False):
        try:
            response = urllib2.urlopen(metadata['image'])
            response_content_type = response.info().getheader('Content-Type')
            if response_content_type not in allowed_image_content_types:
                error('%s image metadata has bad content type: %s' % (file_full_path, response_content_type))
        except:
            error('%s cannot fetch image' % file_full_path)

    if linter_command.get(file_extension, False):
        command = list(linter_command[file_extension])
        command.append(file_full_path)
        debug('running %s' % command)
        lint_exit_code = subprocess.call(command)
        if lint_exit_code > 0:
            error('%s failed linting, see above errors' % file_full_path)

files = sys.argv[1:]
if len(files) == 0:
    for root, dirs, files_in_folder in os.walk("."):
        for file in files_in_folder:
            files.append(os.path.join(root, file).strip())

        skip = [d for d in dirs if d.startswith(".")]
        for d in skip:
            debug('skipping directory %s' % d)
            dirs.remove(d)

elif files[0] == '--pr':
    output = subprocess.check_output('git diff --name-only origin/master..HEAD | cat', shell=True)
    files = output.strip().split('\n')

for file in files:
    file_name, file_ext = os.path.splitext(file)
    if any(s[0] == '.' for s in (file.split('/')[1:])) or file_ext == '.md':
        debug('skipping file %s' % file)
    else:
        debug('checking file %s' % file)
        check_file(os.path.join(os.getcwd(), file))

if error_count > 0:
    error('failed with %i errors' % error_count)
    exit(1)
