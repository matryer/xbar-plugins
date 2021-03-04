#!/usr/bin/env -S PATH="${PATH}:/usr/local/bin" python3

# <bitbar.title>Pocket Bar</bitbar.title>
# <bitbar.version>v1.6.1</bitbar.version>
# <bitbar.author>Sergey Shlyapugin</bitbar.author>
# <bitbar.author.github>inbalboa</bitbar.author.github>
# <bitbar.desc>Basic Pocket client.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/XQnh7US.png</bitbar.image>
# <bitbar.dependencies>python3,pocket-api,keyring</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/inbalboa/pocketbar</bitbar.abouturl>

from argparse import ArgumentParser
from dataclasses import dataclass
import json
from pathlib import Path
import subprocess
import sys

APPNAME = 'pocketbar'
CMD = sys.argv[0]
CACHE_PATH = f'~/Library/Caches/{APPNAME}/articles.json'


@dataclass(frozen=True)
class Article:
    id: str
    link: str
    title: str
    cmd: str

    def __str__(self):
        title_ = self.title.replace('|', '—').strip() if self.title else self.link
        return f'''{title_}|href={self.link} length=60\n➖ {title_}|alternate=true length=60 bash={self.cmd} param1=--delete param2={self.id} terminal=false refresh=true'''


def get_secrets():
    consumer_key = keyring.get_password(APPNAME, 'consumer_key')
    access_token = keyring.get_password(APPNAME, 'access_token')
    return consumer_key, access_token


def update_secrets():
    consumer_key = get_input('\"Enter your consumer key from\\n\\"https://getpocket.com/developer/apps/\\"\"', hidden=True)
    if not consumer_key:
        return None, None
    keyring.set_password(APPNAME, 'consumer_key', consumer_key)

    pocket = Pocket(consumer_key=consumer_key)
    redirect_uri = 'https://getpocket.com/connected_applications'
    request_token = pocket.get_request_token(redirect_uri)
    auth_url = f'https://getpocket.com/auth/authorize?request_token={request_token}&redirect_uri={redirect_uri}'
    subprocess.Popen(['open', auth_url])
    get_ok('\"Press the Authorize button in the opened browser tab, then close this dialog.\"')
    access_token = pocket.get_access_token(request_token)
    keyring.set_password(APPNAME, 'access_token', access_token)


def parse_args():
    parser = ArgumentParser(description='Pocket Bar')
    parser.add_argument('-a', '--add', action='store_true', help='add item')
    parser.add_argument('-d', '--delete', type=str, help='delete item')
    parser.add_argument('-f', '--full', action='store_true', help='full retrieve')
    parser.add_argument('-s', '--secrets', action='store_true', help='update secrets')
    args = parser.parse_args()
    return args


def pocket_icon():
    return 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAQAAABuvaSwAAABIElEQVR4Xs3OvS9DYRQH4KdKfaViRWq2SUpCzKJithKrpISko6E6WJBg8L8ws0g6dJMIg6nNbZcmEkLEcG96qduZ31nec/LknJd/lVkVF84T6kLF7HeaU/Xoxm1C3XhUlYvxisCyQSMJNWhZYCXGa+ryeiWvYe03HjdjoDNNmzSCvHoSPlJX1A/6bHhw0BvvedO2o1/KpqbPCP/4xqqGeQwpe9W2Y0vTh3NZzGtYjfFipw35i7b3iIarFmI84V45eof83VlEKbs3EeOUSzVTUTds276xqJtScynlW+Y8O5HRnYxTz+a6x0Uth519YcZUtBS7KRklgWvrcrKypq27Figl3ENawZXAkzt3ngSuFKSTaJhRS3YdO7ZryWhv+If5AkpGXVSbf9oEAAAAAElFTkSuQmCC'


def get_ok(caption):
    osa_bin = 'osascript'
    osa_params = f"-e 'Tell application \"System Events\" to display alert \"Pocket Bar\" message {caption} buttons \"Close\" default button \"Close\"'"
    task = subprocess.Popen(f'{osa_bin} {osa_params} > /dev/null', shell=True)
    task.wait()


def get_input(caption, hidden=False):
    osa_bin = 'osascript'
    hidden_text = ' with hidden answer' if hidden else ''
    osa_params = f"-e 'Tell application \"System Events\" to display dialog {caption} default answer \"\" with title \"Pocket Bar\" with icon 1 {hidden_text}' -e 'text returned of result'"
    task = subprocess.Popen(f'{osa_bin} {osa_params}', shell=True, stdout=subprocess.PIPE)
    answer_text = task.stdout.read()
    task.wait()

    return answer_text.decode().replace('\n', '').replace('\r', '').strip()


def print_error(error):
    print('!|color=#ECB935')
    print('---')
    print(f'Exception: {error}')


def print_refresh():
    print('---')
    print('Refresh|refresh=yes')
    print(f'Full refresh|alternate=true bash={CMD} param1=--full terminal=false refresh=yes')
    print('---')
    print('Open Pocket|href="https://getpocket.com/" refresh=no')
    print(f'Re-authorize...|alternate=true bash={CMD} param1=--secrets terminal=false refresh=true')


def print_secrets_error():
    print('!|color=#ECB935')
    print('---')
    print('Need authorization')
    print('---')
    print(f'Authorize...|bash={CMD} param1=--secrets terminal=false refresh=true')


def print_import_error():
    print('!|color=#ECB935')
    print('---')
    print('Need to install pocket-api or/and keyring packages')
    print('---')
    print('Install (with PIP)...|bash=pip3 param1=install param2=-U param3=pocket-api param4=keyring terminal=true refresh=true')


def get_cache(cache_path):
    try:
        with open(Path(cache_path).expanduser()) as json_file:
            return json.load(json_file)
    except:
        return {}


def set_cache(cache_path, json_data):
    expanded_cache_path = Path(cache_path).expanduser()
    expanded_cache_path.parent.mkdir(exist_ok=True)
    with open(expanded_cache_path, 'w+') as json_file:
        json.dump(json_data, json_file)


def update_from_cache(main_dict, update_dict):
    if update_dict['status'] == 2:
        return main_dict
    res = dict(main_dict)
    res_list = res.get('list', {})
    res_list.update(update_dict.get('list', {}))
    res.update(update_dict)
    res['list'] = res_list
    return res


def main():
    parsed_args = parse_args()

    try:
        global keyring, Pocket, PocketException
        import keyring
        from pocket import Pocket, PocketException
    except ImportError:
        print_import_error()
        print_refresh()
        return

    consumer_key, access_token = get_secrets()
    pocket = Pocket(consumer_key=consumer_key, access_token=access_token)

    if parsed_args.add:
        new_url = get_input('\"Save an item to Pocket:\"')
        if new_url:
            pocket.add(url=new_url)
        return
    elif parsed_args.delete:
        pocket.delete(parsed_args.delete).commit()
        return
    elif parsed_args.secrets:
        update_secrets()
        return

    raw_articles = {} if parsed_args.full else get_cache(CACHE_PATH)
    try:
        raw_answer = pocket.retrieve(detailType='simple', since=raw_articles.get('since'))
    except PocketException as e:
        if e.http_code in (400, 401):
            print_secrets_error()
        else:
            print_error(e)
        print_refresh()
        return
    except Exception as e:
        print_error(e)
        print_refresh()
        return

    raw_articles = update_from_cache(raw_articles, raw_answer)
    set_cache(CACHE_PATH, raw_articles)

    adapted_articles = [Article(
                            id=i.get('item_id'),
                            link=i.get('resolved_url', i.get('given_url')),
                            title=i.get('resolved_title', i.get('given_title')),
                            cmd=CMD
                        )
                        for i in sorted(raw_articles['list'].values(), key=lambda x: x.get('time_added', ''), reverse=True) if i['status'] == '0']
    print(f'{len(adapted_articles)}|font=Verdana size=14 templateImage={pocket_icon()}')
    print('---')
    print(*adapted_articles, sep='\n')
    print('---')
    print(f'➕ Save a URL|bash={CMD} param1=--add terminal=false refresh=true')
    print_refresh()


if __name__ == '__main__':
    main()
