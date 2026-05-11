#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# <xbar.title>Open Access PDF Search</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Michelle G Dyason</xbar.author>
# <xbar.desc>Search OpenAlex for open-access papers and provider PDF links by keyword.</xbar.desc>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://openalex.org</xbar.abouturl>
# <xbar.var>string(OA_KEYWORDS="climate change,public health,food security"): Comma-separated keywords to search.</xbar.var>
# <xbar.var>number(OA_RESULTS_PER_KEYWORD=5): Number of open-access results per keyword, max 10.</xbar.var>
# <xbar.var>string(OA_LANGUAGE="en"): OpenAlex language code, for example en for English. Leave blank for all languages.</xbar.var>
# <xbar.var>string(OA_SORT="relevance_score:desc"): OpenAlex sort, for example relevance_score:desc or publication_date:desc.</xbar.var>
# <xbar.var>string(OPENALEX_MAILTO=""): Optional email for OpenAlex polite pool requests.</xbar.var>

"""
xbar/SwiftBar plugin for finding open-access papers and direct PDF links.

OpenAlex aggregates open-access locations from many providers. When a provider
exposes a direct PDF URL, it appears as a PDF submenu item. Otherwise, the item
opens the best open-access landing page.
"""

import json
import os
import sys
from datetime import date
from html import unescape
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen


DEFAULT_KEYWORDS = ["climate change", "public health", "food security"]
DEFAULT_RESULTS_PER_KEYWORD = 5
DEFAULT_LANGUAGE = "en"
DEFAULT_SORT = "relevance_score:desc"
MAX_RESULTS_PER_KEYWORD = 10
OPENALEX_WORKS_URL = "https://api.openalex.org/works"
REQUEST_TIMEOUT_SECONDS = 20


def menu_text(value, max_length=96):
    text = unescape(str(value or "")).replace("\n", " ").replace("|", "-")
    text = " ".join(text.split())
    if len(text) <= max_length:
        return text
    return text[: max_length - 1].rstrip() + "..."


def menu_url(value):
    return str(value or "").replace(" ", "%20")


def configured_keywords():
    raw = os.environ.get("OA_KEYWORDS", "")
    keywords = [item.strip() for item in raw.split(",") if item.strip()]
    return keywords or DEFAULT_KEYWORDS


def configured_result_count():
    raw = os.environ.get("OA_RESULTS_PER_KEYWORD", "")
    try:
        count = int(raw)
    except ValueError:
        count = DEFAULT_RESULTS_PER_KEYWORD
    return min(max(count, 1), MAX_RESULTS_PER_KEYWORD)


def configured_language():
    return os.environ.get("OA_LANGUAGE", DEFAULT_LANGUAGE).strip().lower()


def configured_sort():
    return os.environ.get("OA_SORT", DEFAULT_SORT).strip() or DEFAULT_SORT


def openalex_search_query(keyword):
    query = " ".join(str(keyword or "").split())
    if " " not in query:
        return query

    advanced_tokens = ['"', "(", ")", "~", "*", "?"]
    boolean_query = any(f" {operator} " in f" {query} " for operator in ["AND", "OR", "NOT"])
    if boolean_query or any(token in query for token in advanced_tokens):
        return query

    return f'"{query}"'


def request_headers():
    email = os.environ.get("OPENALEX_MAILTO", "").strip()
    user_agent = "xbar Open Access PDF Search"
    if email:
        user_agent = f"{user_agent} ({email})"
    return {"Accept": "application/json", "User-Agent": user_agent}


def openalex_query(keyword, per_page):
    filters = [
        "open_access.is_oa:true",
        f"to_publication_date:{date.today().isoformat()}",
    ]

    language = configured_language()
    if language:
        filters.append(f"language:{language}")

    params = {
        "search": openalex_search_query(keyword),
        "filter": ",".join(filters),
        "per-page": per_page,
        "sort": configured_sort(),
    }

    email = os.environ.get("OPENALEX_MAILTO", "").strip()
    if email:
        params["mailto"] = email

    url = f"{OPENALEX_WORKS_URL}?{urlencode(params)}"
    request = Request(url, headers=request_headers())

    with urlopen(request, timeout=REQUEST_TIMEOUT_SECONDS) as response:
        return json.loads(response.read().decode("utf-8"))


def location_label(location):
    source = (location or {}).get("source") or {}
    display_name = source.get("display_name") or source.get("host_organization_name")
    return menu_text(display_name or "Open access provider", max_length=48)


def add_location(seen, locations, location):
    if not location:
        return

    pdf_url = location.get("pdf_url")
    landing_url = location.get("landing_page_url")
    oa_url = pdf_url or landing_url
    if not oa_url or oa_url in seen:
        return

    seen.add(oa_url)
    locations.append(
        {
            "label": location_label(location),
            "url": oa_url,
            "is_pdf": bool(pdf_url),
        }
    )


def open_access_locations(work):
    seen = set()
    locations = []

    open_access = work.get("open_access") or {}
    add_location(
        seen,
        locations,
        {
            "pdf_url": open_access.get("oa_url")
            if str(open_access.get("oa_url", "")).lower().endswith(".pdf")
            else None,
            "landing_page_url": open_access.get("oa_url"),
            "source": {"display_name": "Best open access copy"},
        },
    )

    add_location(seen, locations, work.get("best_oa_location"))
    add_location(seen, locations, work.get("primary_location"))

    for location in work.get("locations") or []:
        add_location(seen, locations, location)

    locations.sort(key=lambda item: (not item["is_pdf"], item["label"].lower()))
    return locations


def doi_url(work):
    doi = work.get("doi")
    if doi:
        return doi
    return work.get("id") or ""


def print_keyword_menu(keyword, payload):
    meta = payload.get("meta") or {}
    works = payload.get("results") or []
    count = meta.get("count", 0)
    search_filters = ["open_access.is_oa:true"]
    language = configured_language()
    if language:
        search_filters.append(f"language:{language}")
    search_url = f"https://openalex.org/works?{urlencode({'search': openalex_search_query(keyword), 'filter': ','.join(search_filters), 'sort': configured_sort()})}"

    print(f"{menu_text(keyword)} - {count:,} OA matches | href={menu_url(search_url)}")

    if not works:
        print("--No open-access matches found")
        return

    for work in works:
        title = menu_text(work.get("display_name") or "Untitled work")
        year = work.get("publication_year") or "unknown year"
        locations = open_access_locations(work)
        primary_url = locations[0]["url"] if locations else doi_url(work)

        print(f"--{title} ({year}) | href={menu_url(primary_url)}")

        for location in locations[:4]:
            label = "PDF" if location["is_pdf"] else "OA page"
            print(f"---{label}: {location['label']} | href={menu_url(location['url'])}")


def print_error(keyword, error):
    print(f"{menu_text(keyword)} - search failed")
    print(f"--{menu_text(error, max_length=120)}")


def main():
    keywords = configured_keywords()
    per_page = configured_result_count()

    print("OA PDFs")
    print("---")
    print("Configure keywords with OA_KEYWORDS | color=gray")
    print("Refresh | refresh=true")
    print("---")

    for keyword in keywords:
        try:
            print_keyword_menu(keyword, openalex_query(keyword, per_page))
        except HTTPError as error:
            print_error(keyword, f"OpenAlex HTTP {error.code}")
        except URLError as error:
            print_error(keyword, f"Network error: {error.reason}")
        except Exception as error:
            print_error(keyword, error)


if __name__ == "__main__":
    try:
        main()
    except BrokenPipeError:
        sys.exit(0)
