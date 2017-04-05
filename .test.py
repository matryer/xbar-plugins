#!/usr/bin/env python
import re
import os
import subprocess
import urllib2
import argparse
import warnings
from distutils.spawn import find_executable

allowed_image_content_types = ['image/png', 'image/jpeg', 'image/gif']
required_metadata = ['author', 'author.github', 'title']
recommended_metadata = ['image', 'desc', 'version']
error_count = 0


def debug(s):
    global args
    if args.debug:
        print "\033[1;44mDBG!\033[0;0m %s\n" % s


def passed(s):
    global args
    if args.verbose:
        print "\033[1;42mPASS\033[0;0m %s\n" % s


def warn(s):
    print "\033[1;43mWRN!\033[0;0m %s\n" % s


def error(s):
    global error_count
    error_count += 1
    print "\033[1;41mERR!\033[0;0m %s\n" % s


class Language(object):
    _languages = {}

    def __init__(self, exts, shebang, linter, trim_shebang=False):
        self.extensions = exts
        self.shebang = shebang
        self.cmd = linter
        self.trim = trim_shebang

        self.enabled = True
        if not find_executable(self.cmd[0]):
            error("Linter %s not present, skipping %s files" % (self.cmd[0], ', '.join(exts)))
            self.enabled = False

    @staticmethod
    def registerLanguage(lang):
        for extension in lang.extensions:
            if extension in Language._languages:
                Language._languages[extension].append(lang)
            else:
                Language._languages[extension] = [lang, ]

    @staticmethod
    def getLanguagesForFileExtension(ext):
        if ext in Language._languages:
            return Language._languages[ext]
        else:
            return None

    def validShebang(self, bang):
        return re.search(self.shebang, bang) is not None

    def lint(self, file):
        if not self.enabled:
            return None
        if self.trim:
            with open(file, 'r') as fp:
                lines = fp.readlines()[1:]
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore")
                    tmpfile = os.tmpnam()
                with open(tmpfile, 'w') as tp:
                    tp.writelines(lines)
                file = tmpfile
        command = list(self.cmd)
        command.append(file)
        return subprocess.check_output(command, stderr=subprocess.STDOUT)


Language.registerLanguage(Language(['.sh'], '(bash|ksh|zsh|sh|fish)$', ['shellcheck', '-e', 'SC2196', '-e', 'SC2197']))
Language.registerLanguage(Language(['.py', '.py2'], 'python(|2)$', ['python2', '-m', 'pyflakes']))
Language.registerLanguage(Language(['.py', '.py3'], 'python(|3)$', ['python3', '-m', 'pyflakes']))
Language.registerLanguage(Language(['.rb'], 'ruby$', ['rubocop', '-l']))
Language.registerLanguage(Language(['.js'], 'node$', ['jshint']))
Language.registerLanguage(Language(['.php'], 'php$', ['php', '-l']))
Language.registerLanguage(Language(['.pl'], 'perl( -[wW])?$', ['perl', '-MO=Lint']))
Language.registerLanguage(Language(['.swift'], 'swift$', ['xcrun', '-sdk', 'macosx', 'swiftc', '-o', '/dev/null']))
Language.registerLanguage(Language(['.lisp', '.clisp'], 'clisp$', ['clisp']))
# go does not actually support shebang on line 1.  gorun works around this, so we need to strip it before we lint
Language.registerLanguage(Language(['.go'], 'gorun$', ['golint', '-set_exit_status'], trim_shebang=True))


def check_file(file_full_path):

    file_short_name, file_extension = os.path.splitext(file_full_path)
    candidates = Language.getLanguagesForFileExtension(file_extension)

    if not candidates:
        error("%s unrecognized file extension" % file_full_path)
        return
    else:
        passed("%s has a recognized file extension" % file_full_path)

    if not os.access(file_full_path, os.R_OK):
        error("%s not readable" % file_full_path)
        return

    if not os.access(file_full_path, os.X_OK):
        error("%s not executable" % file_full_path)
    else:
        passed("%s is executable" % file_full_path)

    metadata = {}
    linters = []
    with open(file_full_path, "r") as fp:
        first_line = fp.readline().strip()

        for candidate in candidates:
            if candidate.validShebang(first_line):
                linters.append(candidate)

        if not re.match(r'#! ?/', first_line):
            error("'%s' does not look like a valid shebang" % first_line)

        if not linters:
            error("%s has incorrect shebang.\n  Got %s\n  Wanted %s" % (
                  file_full_path, first_line,
                  ' or '.join(["'%s'" % candidate.shebang for candidate in candidates])))
        else:
            passed("%s has a good shebang (%s)" % (file_full_path, first_line))

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
        else:
            passed('%s has required metadata for %s (%s)' % (file_full_path, key, metadata[key]))

    for key in recommended_metadata:
        if key not in metadata:
            warn('%s missing recommended metadata for %s' % (file_full_path, key))
        else:
            passed('%s has recommended metadata for %s (%s)' % (file_full_path, key, metadata[key]))

    if metadata.get('image', False):
        try:
            response = urllib2.urlopen(metadata['image'])
            response_content_type = response.info().getheader('Content-Type')
            if response_content_type not in allowed_image_content_types:
                error('%s image metadata has bad content type: %s' % (file_full_path, response_content_type))
            else:
                passed('%s image content type looks good: %s' % (file_full_path, response_content_type))
        except Exception:
            warn('%s cannot fetch image: %s' % (file_full_path, metadata['image']))

    errors = []
    for linter in linters:
        try:
            debug('running %s' % " ".join(linter.cmd))
            linter.lint(file_full_path)
        except subprocess.CalledProcessError as cpe:
            debug('%s failed linting with "%s"' % (file_full_path, " ".join(linter.cmd)))
            errors.append({'linter': linter, 'output': cpe.output})
        else:
            errors = []
            passed('%s linted successfully with "%s"' %
                   (file_full_path, " ".join(list(linter.cmd))))
            break

    for e in errors:
        error('%s failed linting with "%s", please correct the following:' %
              (file_full_path, " ".join(e['linter'].cmd)))
        print e['output']


def boolean_string(string):
    if string.lower() == "false":
        return False
    return True


parser = argparse.ArgumentParser()
parser.add_argument(
    '--pr', action='store', nargs='?', const="True",
    default=os.environ.get('TRAVIS_PULL_REQUEST', "False"),
    type=boolean_string,
    help='Run tests on changes from the root branch to HEAD.  verbose is implied!')
parser.add_argument('--verbose', '-v', action='store_true', help='Turn on success and other non-critical messages')
parser.add_argument('--debug', action='store_true', help='Turn on debug messages')
parser.add_argument('files', nargs=argparse.REMAINDER)
args = parser.parse_args()

if args.pr:
    output = subprocess.check_output(['git', 'diff', '--name-only', '--diff-filter=ACMR',
                                      'origin/%s..HEAD' %
                                      os.environ.get('TRAVIS_BRANCH', 'master')]).strip()
    if not output:
        warn('No changed files in this PR... weird...')
        exit(0)
    else:
        args.files = output.split('\n')
    args.verbose = True
elif not args.files:
    for root, dirs, files_in_folder in os.walk("."):
        for _file in files_in_folder:
            args.files.append(os.path.join(root, _file).strip())

        skip = [d for d in dirs if d.startswith(".")]
        for d in skip:
            debug('skipping directory %s' % d)
            dirs.remove(d)

for _file in args.files:
    file_name, file_ext = os.path.splitext(_file)
    components = _file.split('/')
    if components[0] == ".":
        del components[0]
    if any(s[0] == '.' for s in components) or file_ext == '.md':
        debug('skipping file %s' % _file)
    else:
        debug('checking file %s' % _file)
        check_file(os.path.join(os.getcwd(), _file))

if error_count > 0:
    error('failed with %i errors' % error_count)
    exit(1)
