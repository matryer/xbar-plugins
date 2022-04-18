#!/usr/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>Currency Tracker Google</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Alp Sayin</xbar.author>
# <xbar.author.github>alpsayin</xbar.author.github>
# <xbar.desc>Keep an eye on currency exchange rates from google</xbar.desc>
# <xbar.dependencies>python3, requests</xbar.dependencies>
# <xbar.abouturl>https://alpsayin.com/#xbar-plugins</xbar.abouturl>
# <xbar.image>https://github.com/alpsayin/alpsayin.github.com/raw/master/img/xbar_currency_tracker_google.png</xbar.image> 

import requests
import traceback
from datetime import datetime
from pprint import pprint

# Google uses these as keys for its currency api
currency_keys = {
    "afghan afghani": "/m/019vxc",
    "albanian lek": "/m/01n64b",
    "algerian dinar": "/m/04wcz0",
    "angolan kwanza": "/m/03c7mb",
    "argentine peso": "/m/024nzm",
    "armenian dram": "/m/033xr3",
    "aruban florin": "/m/08s1k3",
    "australian dollar": "/m/0kz1h",
    "azerbaijani manat": "/m/04bq4y",
    "bahamian dollar": "/m/01l6dm",
    "bahraini dinar": "/m/04wd20",
    "bajan dollar": "/m/05hy7p",
    "bangladeshi taka": "/m/02gsv3",
    "belarusian ruble": "/m/05c9_x",
    "belize dollar": "/m/02bwg4",
    "bermudan dollar": "/m/04xb8t",
    "bhutan currency": "/m/02gt45",
    "bolivian boliviano": "/m/04tkg7",
    "bosnia-herzegovina convertible mark": "/m/02lnq3",
    "botswanan pula": "/m/02nksv",
    "brazilian real": "/m/03385m",
    "brunei dollar": "/m/021x2r",
    "bulgarian lev": "/m/01nmfw",
    "burundian franc": "/m/05jc3y",
    "cfp franc": "/m/01qyjx",
    "cambodian riel": "/m/03_m0v",
    "canadian dollar": "/m/0ptk_",
    "cape verdean escudo": "/m/06plyy",
    "cayman islands dollar": "/m/04xbgl",
    "central african cfa franc": "/m/025sw2b",
    "chilean peso": "/m/0172zs",
    "chilean unit of account (uf)": "/m/0775_k",
    "chinese yuan": "/m/0hn4_",
    "chinese yuan (offshore)": "/g/11c54p47s9",
    "colombian peso": "/m/034sw6",
    "comorian franc": "/m/05yxq3",
    "congolese franc": "/m/04h1d6",
    "costa rican colón": "/m/04wccn",
    "croatian kuna": "/m/02z8jt",
    "cuban peso": "/m/049p2z",
    "czech koruna": "/m/04rpc3",
    "danish krone": "/m/01j9nc",
    "djiboutian franc": "/m/05yxn7",
    "dominican peso": "/m/04lt7_",
    "east caribbean dollar": "/m/02r4k",
    "egyptian pound": "/m/04phzg",
    "ethiopian birr": "/m/02_mbk",
    "euro": "/m/02l6h",
    "fijian dollar": "/m/04xbp1",
    "gambian dalasi": "/m/04wctd",
    "georgian lari": "/m/03nh77",
    "ghanaian cedi": "/m/01s733",
    "guatemalan quetzal": "/m/01crby",
    "guinean franc": "/m/05yxld",
    "guyanaese dollar": "/m/059mfk",
    "haitian gourde": "/m/04xrp0",
    "honduran lempira": "/m/04krzv",
    "hong kong dollar": "/m/02nb4kq",
    "hungarian forint": "/m/01hfll",
    "icelandic króna": "/m/012nk9",
    "indian rupee": "/m/02gsvk",
    "indonesian rupiah": "/m/0203sy",
    "iranian rial": "/m/034n11",
    "iraqi dinar": "/m/01kpb3",
    "israeli new shekel": "/m/01jcw8",
    "jamaican dollar": "/m/04xc2m",
    "japanese yen": "/m/088n7",
    "jordanian dinar": "/m/028qvh",
    "kazakhstani tenge": "/m/01km4c",
    "kenyan shilling": "/m/05yxpb",
    "kuwaiti dinar": "/m/01j2v3",
    "kyrgystani som": "/m/04k5c6",
    "laotian kip": "/m/04k4j1",
    "lebanese pound": "/m/025tsrc",
    "lesotho loti": "/m/04xm1m",
    "liberian dollar": "/m/05g359",
    "libyan dinar": "/m/024xpm",
    "macanese pataca": "/m/02fbly",
    "macedonian denar": "/m/022dkb",
    "malagasy ariary": "/m/04hx_7",
    "malawian kwacha": "/m/0fr4w",
    "malaysian ringgit": "/m/01_c9q",
    "maldivian rufiyaa": "/m/02gsxf",
    "mauritanian ouguiya (1973–2017)": "/m/023c2n",
    "mauritian rupee": "/m/02scxb",
    "mexican peso": "/m/012ts8",
    "moldovan leu": "/m/02z6sq",
    "moroccan dirham": "/m/06qsj1",
    "mozambican metical": "/m/05yxqw",
    "myanmar kyat": "/m/04r7gc",
    "namibian dollar": "/m/01y8jz",
    "nepalese rupee": "/m/02f4f4",
    "netherlands antillean guilder": "/m/08njbf",
    "new taiwan dollar": "/m/01t0lt",
    "new zealand dollar": "/m/015f1d",
    "nicaraguan córdoba": "/m/02fvtk",
    "nigerian naira": "/m/018cg3",
    "norwegian krone": "/m/0h5dw",
    "omani rial": "/m/04_66x",
    "pakistani rupee": "/m/02svsf",
    "panamanian balboa": "/m/0200cp",
    "papua new guinean kina": "/m/04xblj",
    "paraguayan guarani": "/m/04w7dd",
    "philippine peso": "/m/01h5bw",
    "poland złoty": "/m/0glfp",
    "pound sterling": "/m/01nv4h",
    "qatari rial": "/m/05lf7w",
    "romanian leu": "/m/02zsyq",
    "russian ruble": "/m/01hy_q",
    "rwandan franc": "/m/05yxkm",
    "salvadoran colón": "/m/04wcnp",
    "saudi riyal": "/m/02d1cm",
    "serbian dinar": "/m/02kz6b",
    "seychellois rupee": "/m/01lvjz",
    "sierra leonean leone": "/m/02vqvn",
    "singapore dollar": "/m/02f32g",
    "sol": "/m/0b423v",
    "solomon islands dollar": "/m/05jpx1",
    "somali shilling": "/m/05yxgz",
    "south african rand": "/m/01rmbs",
    "south korean won": "/m/01rn1k",
    "sovereign bolivar": "/g/11bc5b_s84",
    "sri lankan rupee": "/m/02gsxw",
    "sudanese pound": "/m/08d4zw",
    "surinamese dollar": "/m/02dl9v",
    "swazi lilangeni": "/m/02pmxj",
    "swedish krona": "/m/0485n",
    "swiss franc": "/m/01_h4b",
    "tajikistani somoni": "/m/0370bp",
    "tanzanian shilling": "/m/04s1qh",
    "thai baht": "/m/0mcb5",
    "tongan paʻanga": "/m/040qbv",
    "trinidad &amp; tobago dollar": "/m/04xcgz",
    "tunisian dinar": "/m/04z4ml",
    "turkish lira": "/m/04dq0w",
    "turkmenistani manat": "/m/0425kx",
    "ugandan shilling": "/m/04b6vh",
    "ukrainian hryvnia": "/m/035qkb",
    "united arab emirates dirham": "/m/02zl8q",
    "united states dollar": "/m/09nqf",
    "uruguayan peso": "/m/04wblx",
    "uzbekistani som": "/m/04l7bl",
    "vietnamese dong": "/m/03ksl6",
    "west african cfa franc": "/m/025sw2q",
    "yemeni rial": "/m/05yxwz",
    "zambian kwacha": "/m/0fr4f",
}

# this list is extracted from wikipedia
# https://en.wikipedia.org/wiki/List_of_circulating_currencies#List_of_circulating_currencies_by_state_or_territory
currency_codes = {
    'afn': 'afghan afghani',
    'all': 'albanian lek',
    'dzd': 'algerian dinar',
    'aoa': 'angolan kwanza',
    'ars': 'argentine peso',
    'amd': 'armenian dram',
    'awg': 'aruban florin',
    'aud': 'australian dollar',
    'azn': 'azerbaijani manat',
    'bsd': 'bahamian dollar',
    'bhd': 'bahraini dinar',
    'bdt': 'bangladeshi taka',
    'bbd': 'barbadian dollar',
    'byn': 'belarusian ruble',
    'bzd': 'belize dollar',
    'bmd': 'bermudian dollar',
    'btn': 'bhutanese ngultrum',
    'bob': 'bolivian boliviano',
    'bam': 'bosnia and herzegovina convertible mark',
    'bwp': 'botswana pula',
    'brl': 'brazilian real',
    'gbp': 'pound sterling',
    'bnd': 'brunei dollar',
    'bgn': 'bulgarian lev',
    'mmk': 'burmese kyat',
    'bif': 'burundian franc',
    'khr': 'cambodian riel',
    'cad': 'canadian dollar',
    'cve': 'cape verdean escudo',
    'kyd': 'cayman islands dollar',
    'xaf': 'central african cfa franc',
    'xpf': 'cfp franc',
    'clp': 'chilean peso',
    'cop': 'colombian peso',
    'kmf': 'comorian franc',
    'cdf': 'congolese franc',
    'ckd': 'cook islands dollar',
    'crc': 'costa rican colón',
    'hrk': 'croatian kuna',
    'cuc': 'cuban convertible peso',
    'cup': 'cuban peso',
    'czk': 'czech koruna',
    'dkk': 'danish krone',
    'djf': 'djiboutian franc',
    'dop': 'dominican peso',
    'xcd': 'eastern caribbean dollar',
    'egp': 'egyptian pound',
    'ern': 'eritrean nakfa',
    'etb': 'ethiopian birr',
    'eur': 'euro',
    'fkp': 'falkland islands pound',
    'fok': 'faroese króna',
    'fjd': 'fijian dollar',
    'gmd': 'gambian dalasi',
    'gel': 'georgian lari',
    'ghs': 'ghanaian cedi',
    'gip': 'gibraltar pound',
    'gtq': 'guatemalan quetzal',
    'ggp': 'guernsey pound',
    'gnf': 'guinean franc',
    'gyd': 'guyanese dollar',
    'htg': 'haitian gourde',
    'hnl': 'honduran lempira',
    'hkd': 'hong kong dollar',
    'huf': 'hungarian forint',
    'isk': 'icelandic króna',
    'inr': 'indian rupee',
    'idr': 'indonesian rupiah',
    'irr': 'iranian rial',
    'iqd': 'iraqi dinar',
    'ils': 'israeli new shekel',
    'jmd': 'jamaican dollar',
    'jpy': 'japanese yen',
    'jep': 'jersey pound',
    'jod': 'jordanian dinar',
    'kzt': 'kazakhstani tenge',
    'kes': 'kenyan shilling',
    'kid': 'kiribati dollar',
    'kwd': 'kuwaiti dinar',
    'kgs': 'kyrgyz som',
    'lak': 'lao kip',
    'lbp': 'lebanese pound',
    'lsl': 'lesotho loti',
    'lrd': 'liberian dollar',
    'lyd': 'libyan dinar',
    'mop': 'macanese pataca',
    'mkd': 'macedonian denar',
    'mga': 'malagasy ariary',
    'mwk': 'malawian kwacha',
    'myr': 'malaysian ringgit',
    'mvr': 'maldivian rufiyaa',
    'imp': 'manx pound',
    'mru': 'mauritanian ouguiya',
    'mur': 'mauritian rupee',
    'mxn': 'mexican peso',
    'mdl': 'moldovan leu',
    'mnt': 'mongolian tögrög',
    'mad': 'moroccan dirham',
    'mzn': 'mozambican metical',
    'nad': 'namibian dollar',
    'npr': 'nepalese rupee',
    'ang': 'netherlands antillean guilder',
    'twd': 'new taiwan dollar',
    'nzd': 'new zealand dollar',
    'nio': 'nicaraguan córdoba',
    'ngn': 'nigerian naira',
    'kpw': 'north korean won',
    'nok': 'norwegian krone',
    'omr': 'omani rial',
    'pkr': 'pakistani rupee',
    'pab': 'panamanian balboa',
    'pgk': 'papua new guinean kina',
    'pyg': 'paraguayan guaraní',
    'pen': 'peruvian sol',
    'php': 'philippine peso',
    'pnd': 'pitcairn islands dollar',
    'pln': 'polish złoty',
    'qar': 'qatari riyal',
    'cny': 'renminbi/chinese yuan',
    'ron': 'romanian leu',
    'zwb': 'rtgs dollar',
    'rub': 'russian ruble',
    'rwf': 'rwandan franc',
    'shp': 'saint helena pound',
    'wst': 'samoan tālā',
    'stn': 'são tomé and príncipe dobra',
    'sar': 'saudi riyal',
    'rsd': 'serbian dinar',
    'scr': 'seychellois rupee',
    'sll': 'sierra leonean leone',
    'sgd': 'singapore dollar',
    'sbd': 'solomon islands dollar',
    'sos': 'somali shilling',
    'sls': 'somaliland shilling',
    'zar': 'south african rand',
    'krw': 'south korean won',
    'ssp': 'south sudanese pound',
    'lkr': 'sri lankan rupee',
    'sdg': 'sudanese pound',
    'srd': 'surinamese dollar',
    'szl': 'swazi lilangeni',
    'sek': 'swedish krona',
    'chf': 'swiss franc',
    'syp': 'syrian pound',
    'tjs': 'tajikistani somoni',
    'tzs': 'tanzanian shilling',
    'thb': 'thai baht',
    'top': 'tongan paʻanga',
    'prb': 'transnistrian ruble',
    'ttd': 'trinidad and tobago dollar',
    'tnd': 'tunisian dinar',
    'try': 'turkish lira',
    'tmt': 'turkmenistan manat',
    'tvd': 'tuvaluan dollar',
    'ugx': 'ugandan shilling',
    'uah': 'ukrainian hryvnia',
    'aed': 'united arab emirates dirham',
    'usd': 'united states dollar',
    'uyu': 'uruguayan peso',
    'uzs': 'uzbekistani som',
    'vuv': 'vanuatu vatu',
    'ved': 'venezuelan bolívar digital',
    'ves': 'venezuelan bolívar soberano',
    'vnd': 'vietnamese đồng',
    'xof': 'west african cfa franc',
    'yer': 'yemeni rial',
    'zmw': 'zambian kwacha',
}

# Only 1 currency pair is supported for now
# Duplicate the script if more pairs are needed
currency_from = 'USD'
currency_to = 'TRY'

headers = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:99.0) Gecko/20100101 Firefox/99.0',
    'Accept': '*/*',
    'Accept-Language': 'en-GB,en;q=0.5',
    'Accept-Encoding': 'text',
    'Referer': 'https://www.google.com/',
    'DNT': '1',
    'Alt-Used': 'www.google.com',
    'Connection': 'keep-alive',
    'Sec-Fetch-Dest': 'empty',
    'Sec-Fetch-Mode': 'cors',
    'Sec-Fetch-Site': 'same-origin',
    'Sec-GPC': '1',
}

url = f'https://www.google.com/async/currency_v2_update?yv=3&async=source_amount%3A1%2Csource_currency%3A{currency_keys[currency_codes[currency_from.lower()]]}%2Ctarget_currency%3A{currency_keys[currency_codes[currency_to.lower()]]}%2Clang%3Aen%2Ccountry%3Atr%2Cdisclaimer_url%3Ahttps%3A%2F%2Fwww.google.com%2Fintl%2Fen%2Fgooglefinance%2Fdisclaimer%2F%2Cperiod%3A1M%2Cinterval%3A86400%2C_id%3Acurrency-v2-updatable_2%2C_pms%3As%2C_fmt%3Apc'


def main():
    result = requests.get(url, headers=headers)
    if result.status_code != 200:
        print(f'HTTP {result.status_code}')
        return

    # pprint(result.text)

    try:
        first_marker = 'data-value="'
        equals_index = result.text.index(first_marker)
        stripped = result.text[equals_index + len(first_marker):]
        next_apostrophe = stripped.index('"')
        data_value_str = stripped[:next_apostrophe]
        # print(f'{data_value_str=}')
        timestamp_marker = 'UTC'
        utc_index = stripped.index(timestamp_marker)
        stripped = stripped[:utc_index]
        span_marker = '<span>'
        last_span_index = stripped.index(span_marker)
        timestamp_str = stripped[last_span_index:].strip(span_marker) + timestamp_marker
        # print(timestamp_str)
    except Exception as exc:
        print('Data parse error')
        print('---')
        pprint(result.text)
        print(f'{exc}')
        traceback.print_exc()
        return

    try:
        result = float(data_value_str)
        timestamp = datetime.strptime(timestamp_str, '%b %d, %H:%M %Z')  # Apr 18, 12:32 UTC
        timestamp = timestamp.replace(year=datetime.now().year)
        timestamp = timestamp + (datetime.now() - datetime.utcnow())
        new_timestamp_str = timestamp.strftime('%d %b, %H:%M')
    except Exception as exc:
        print('Number/Date parse error')
        print('---')
        pprint(data_value_str)
        print(f'{exc}')
        traceback.print_exc()
        return

    print(f'{currency_from.upper()}→{currency_to.upper()} {result:.3f}')
    print('---')
    print(f'{currency_to.upper()}→{currency_from.upper()} {1.0/result:.3f}')
    print(f'{new_timestamp_str}')


if __name__ == '__main__':
    main()
