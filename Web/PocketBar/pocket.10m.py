#!/usr/bin/env -S PATH="${PATH}:/usr/local/bin" python3

# <bitbar.title>Pocket Bar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sergey Shlyapugin</bitbar.author>
# <bitbar.author.github>inbalboa</bitbar.author.github>
# <bitbar.desc>Basic Pocket client.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/xxtHqaW.png</bitbar.image>
# <bitbar.dependencies>python3,pocket-api</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/inbalboa/pocketbar</bitbar.abouturl>

from argparse import ArgumentParser
from dataclasses import dataclass
import subprocess
import sys

CONSUMER_KEY = 'YOUR CONSUMER KEY'
ACCESS_TOKEN = 'YOUR ACCESS TOKEN'


@dataclass(frozen=True)
class Article:
    id: str
    link: str
    title: str
    cmd: str

    def __str__(self):
        title_ = self.title.replace("|", "—").strip()
        return f'''{title_ if title_ else self.link}|href={self.link} length=60
➖ {title_ if title_ else self.link}|alternate=true length=60 bash={self.cmd} param1=--delete param2={self.id} terminal=false refresh=true'''


def parse_args():
    parser = ArgumentParser(description='Pocket Bar')
    parser.add_argument('-d', '--delete', type=str, help='delete item')
    parser.add_argument('-a', '--add', action="store_true", help='add item')
    args = parser.parse_args()
    return args


def pocket_icon():
    return "iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAQAAABuvaSwAAABIElEQVR4Xs3OvS9DYRQH4KdKfaViRWq2SUpCzKJithKrpISko6E6WJBg8L8ws0g6dJMIg6nNbZcmEkLEcG96qduZ31nec/LknJd/lVkVF84T6kLF7HeaU/Xoxm1C3XhUlYvxisCyQSMJNWhZYCXGa+ryeiWvYe03HjdjoDNNmzSCvHoSPlJX1A/6bHhw0BvvedO2o1/KpqbPCP/4xqqGeQwpe9W2Y0vTh3NZzGtYjfFipw35i7b3iIarFmI84V45eof83VlEKbs3EeOUSzVTUTds276xqJtScynlW+Y8O5HRnYxTz+a6x0Uth519YcZUtBS7KRklgWvrcrKypq27Figl3ENawZXAkzt3ngSuFKSTaJhRS3YdO7ZryWhv+If5AkpGXVSbf9oEAAAAAElFTkSuQmCC"


def get_url():
    osa_bin = 'osascript'
    osa_params = "-e 'Tell application \"System Events\" to display dialog \"Save an item to Pocket:\" default answer \"\"' -e 'text returned of result'"

    task = subprocess.Popen(f'{osa_bin} {osa_params}', shell=True, stdout=subprocess.PIPE)
    answer_text = task.stdout.read()
    assert task.wait() == 0

    return answer_text.decode('utf-8').strip().replace('\n', '').replace('\r', '')


def print_error(error):
    print('✦ !|color=#ECB935')
    print('---')
    print(f'Exception: {error}')


def print_refresh():
    print('---')
    print('🔄 Refresh|refresh=yes')
    print('---')
    print('🌐 Open Pocket|href="https://getpocket.com/" refresh=no')


def main(argv):
    parsed_args = parse_args()

    try:
        from pocket import Pocket, PocketException
    except ImportError:
        print_error('You need to `pip3 install pocket-api`')
        print_refresh()
        sys.exit(1)

    pocket = Pocket(consumer_key=CONSUMER_KEY, access_token=ACCESS_TOKEN)

    if parsed_args.delete:
        pocket.delete(parsed_args.delete).commit()
        sys.exit()
    elif parsed_args.add:
        new_url = get_url()
        if new_url:
            pocket.add(url=new_url)
        sys.exit()

    try:
        raw_answer = pocket.retrieve(sort='newest')
    except (Exception, PocketException) as error:
        print_error(error)
        print_refresh()
        sys.exit(1)

    adapted_articles = [Article(i.get('item_id'), i.get('resolved_url', i.get('given_url')), i.get('resolved_title', i.get('given_title')), argv[0])
                        for i in raw_answer['list'].values()]

    print(f'{len(adapted_articles)}|font=Verdana templateImage={pocket_icon()}')
    print('---')
    print(*adapted_articles, sep='\n')
    print('---')
    print(f'➕ Save a URL|bash={argv[0]} param1=--add terminal=false refresh=true')
    print_refresh()


if __name__ == "__main__":
    main(sys.argv)
