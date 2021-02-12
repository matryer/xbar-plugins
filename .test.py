#!/usr/bin/env python3
import re
import os
import subprocess
import argparse
import warnings
from distutils.spawn import find_executable
from tempfile import NamedTemporaryFile
from urllib.request import urlopen

ignore_file_types = ['.md']
ignore_file_names = ['package.json', 'package-lock.json']
allowed_image_content_types = ['image/png', 'image/jpeg', 'image/gif']
required_metadata = ['author', 'author.github', 'title']
recommended_metadata = ['image', 'desc', 'version']
error_count = 0


def debug(s):
    global args
    if args.debug:
        print("\033[1;44mDBG!\033[0;0m %s\n" % s)


def passed(s):
    global args
    if args.verbose:
        print("\033[1;42mPASS\033[0;0m %s\n" % s)


def warn(s):
    global args
    if args.warn:
        print("\033[1;43mWRN!\033[0;0m %s\n" % s)


def error(s):
    global error_count
    error_count += 1
    print("\033[1;41mERR!\033[0;0m %s\n" % s)


class Language(object):
    _languages = {}

    def __init__(self, exts, shebang, linter, trim_shebang=False, full_options=[], pr_options=[]):
        self.extensions = exts
        self.shebang = shebang
        self.cmd = linter
        self.trim = trim_shebang
        self.full = full_options
        self.pr = pr_options

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

    def lint(self, file, is_pr):
        if not self.enabled:
            return None
        if self.trim:
            with open(file, 'r') as fp:
                lines = fp.readlines()[1:]
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore")
                with NamedTemporaryFile(mode='w', delete=False) as t:
                    file = t.name
                    t.writelines(lines)
        command = list(self.cmd)
        if is_pr and self.pr:
            command.extend(self.pr)
        elif not is_pr and self.full:
            command.extend(self.full)
        command.append(file)
        result = subprocess.check_output(command, stderr=subprocess.STDOUT)
        if self.trim:
            os.remove(file)
        return result


class Python2(Language):
    def __init__(self):
        super().__init__(['.py', '.py2'], r'python(|2(\.\d+)?)$', ['python2', '-m', 'pyflakes'])

    def lint(self, file, pr):
        if pr:
            raise subprocess.CalledProcessError(
                cmd="", returncode=1,
                output=b"python3 support required for all PRs")
        else:
            return super(Python2, self).lint(file, pr)


class Rscript(Language):
    def __init__(self):
        super().__init__(['.r', '.R'], '(r|R)script$', ['Rscript'])

    def lint(self, file, pr):
        if not self.enabled:
            return None
        result = subprocess.check_output([
            'Rscript',
            '-e',
            'library(lintr); l=lint("%s", with_defaults(line_length_linter = NULL));l; quit(status=if (length(l) > 0) { 1 } else { 0 })' % file
        ], stderr=subprocess.STDOUT)
        return result


Language.registerLanguage(Language(['.sh'], '(bash|ksh|zsh|sh|fish)$', ['shellcheck'], full_options=['-e', 'SC1117,SC2164,SC2183,SC2196,SC2197,SC2206,SC2207,SC2215,SC2219,SC2230,SC2236']))
Language.registerLanguage(Python2())
Language.registerLanguage(Language(['.py', '.py3'], r'python(|3(\.\d+)?)$', ['python3', '-m', 'pyflakes']))
Language.registerLanguage(Language(['.rb'], 'ruby$', ['rubocop', '-l'], full_options=['--except', 'Lint/RedundantStringCoercion,Lint/BigDecimalNew']))
Language.registerLanguage(Language(['.js'], 'node$', ['jshint']))
Language.registerLanguage(Language(['.php'], 'php$', ['php', '-l']))
Language.registerLanguage(Language(['.pl'], 'perl( -[wW])?$', ['perl', '-MO=Lint']))
Language.registerLanguage(Language(['.swift'], 'swift$', ['xcrun', '-sdk', 'macosx', 'swiftc', '-o', '/dev/null']))
Language.registerLanguage(Language(['.lisp', '.clisp'], 'clisp$', ['clisp']))
Language.registerLanguage(Language(['.rkt'], 'racket$', ['raco', 'make']))
# go does not actually support shebang on line 1.  gorun works around this, so we need to strip it before we lint
Language.registerLanguage(Language(['.go'], 'gorun$', ['golint', '-set_exit_status'], trim_shebang=True))
Language.registerLanguage(Language(['.lua'], 'lua$', ['luacheck']))
Language.registerLanguage(Rscript())


def check_file(file_full_path, pr=False):

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
            response = urlopen(metadata['image'])
            response_content_type = response.info().get('Content-Type')
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
            linter.lint(file_full_path, pr)
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
        print(e['output'].decode('UTF-8'))


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
parser.add_argument('--no-warn', action='store_false', dest='warn', help='Disable warnings', default=True)
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
        args.files = output.decode("UTF-8").split('\n')
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
    if any(s[0] == '.' for s in components):
        debug('skipping file %s' % _file)
    elif file_ext in ignore_file_types:
        debug('ignoring file by type %s' % _file)
    elif components[-1] in ignore_file_names:
        debug('ignoring file by name %s' % _file)
    else:
        debug('checking file %s' % _file)
        check_file(os.path.join(os.getcwd(), _file), args.pr)

if error_count > 0:
    error('failed with %i errors' % error_count)
    exit(1)
