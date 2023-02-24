#!/usr/bin/env -S -P/${HOME}/.deno/bin:/opt/homebrew/bin:/usr/local/bin deno run --allow-all
// <xbar.title>GitHub Review Requests TS</xbar.title>
// <xbar.desc>Shows a list of PRs that need to be reviewed (heavily inspired by Adam Bogdał's Python implementation)</xbar.desc>
// <xbar.version>v0.1</xbar.version>
// <xbar.author>Alfonso Gomez</xbar.author>
// <xbar.author.github>victorstein</xbar.author.github>
// <xbar.image>https://github.com/victorstein/xbar-review-requests/raw/main/preview-img.png</xbar.image>
// <xbar.dependencies>Deno</xbar.dependencies>

//  Variables
//  <xbar.var>string(TOKEN=""): Github token. https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/</xbar.var>
//  <xbar.var>string(USERNAME=""): Github User name.</xbar.var>
//  <xbar.var>string(ORGANIZATION=""): Filter PRs by Organization name. (optional)</xbar.var>
//  <xbar.var>string(WIP_FILTER=""): Title included in a Work in Progress PR to exclude from the list. (optional)</xbar.var>

import { xbar, separator } from "https://deno.land/x/xbar@v2.1.0/mod.ts";
import axios from 'https://deno.land/x/axiod@0.20.0-0/mod.ts'
import formatDistance from 'https://deno.land/x/date_fns@v2.22.1/formatDistance/index.ts'
import parseISO from 'https://deno.land/x/date_fns@v2.22.1/parseISO/index.js'

class DataFetcher {
  buildQuery (filters: Init['filters'], login: string) {
    const orgFilter = filters.org ? `org:${filters.org}` : ''

    return `
      query searchPRs {
        search(query: "type:pr ${orgFilter} review-requested:${login} state:open", type: ISSUE, first: 100) {
          issueCount
          edges {
            node {
              ... on PullRequest {
                repository {
                  nameWithOwner
                }
                author {
                  login
                }
                createdAt
                number
                url
                reviewDecision
                mergeable
                title
              }
            }
          }
        }
      }
    `
  }

  async fetch({ token, username, filters }: Init): Promise<SearchResults> {
    const { data } = await axios.post(
      `https://api.github.com/graphql`,
      { query: this.buildQuery(filters, username) },
      {
        headers: {
          Authorization: `bearer ${token}`,
          "Content-Type": "application/json",
        }
      }
    )
    return data
  }
}

class Main {
  constructor (
    private readonly dataFetcher: DataFetcher,
  ) {}

  statusEmoji(status: ReviewDecision): string {
    switch (status) {
      case 'APPROVED':
        return '✅'
      case 'REVIEW_REQUIRED':
        return '⏳ 👀'
      case 'CHANGES_REQUESTED':
        return '📝'
      default:
        return '❓'
    }
  }

  mergeStatusColor (status: Mergeable): string {
    switch (status) {
      case 'MERGEABLE': 
        return '#8df76f'
      case 'CONFLICTING':
        return '#eda43e'
      default:
        return 'gray'
    }
  }

  buildMenu(data: Data, wipFilter: string) {
    return data.search.edges.reduce((acc: Record<string, unknown>[], { node }) => {
      const { repository, author, createdAt, number, url, title, mergeable, reviewDecision } = node
      let wipValidation: boolean
      wipValidation = title.match(new RegExp(wipFilter, 'i')) !== null
      if (!wipFilter) { wipValidation = false }

      if (mergeable === 'MERGEABLE' && reviewDecision === 'APPROVED' || wipValidation) {
        return acc
      }

      acc.push(
        {
          text: `${title}#${number} ${this.statusEmoji(reviewDecision)}`,
          href: url,
          color: this.mergeStatusColor(mergeable),
          size: 15,
          font: 'HelveticaNeue-Light',
        },
        {
          text: `Organization: ${repository.nameWithOwner}`,
          size: 11,
          font: 'TrebuchetMS',
        },
        {
          text: `mergeable: ${mergeable} status: ${reviewDecision}`,
          size: 11,
          font: 'TrebuchetMS',
        },
        {
          text: `requested by: ${author.login} ${formatDistance(Date.now(), parseISO(createdAt))} ago`,
          size: 11,
          font: 'TrebuchetMS',
        },
        {
          text: `---`,
        }
      )

      return acc
    }, [])
  }

  async init(initData: Init) {
    if (!initData.token || !initData.username) {
      return xbar([{
        text: `username or token not set`,
        color: '#e35d4b',
      }])
    }

    const { data } = await this.dataFetcher.fetch(initData)
    const menu = this.buildMenu(data, initData.filters.wipFilter)
    const filteredReviews = menu.filter(item => item.href)

    return xbar([
      {
        text: `${filteredReviews.length} PRs pending review`,
      },
      separator,
      ...menu
    ]);
  }
}

const main = new Main(new DataFetcher())
const env = Deno.env.toObject();
const { TOKEN: token, USERNAME: username, ORGANIZATION: org, WIP_FILTER: wipFilter } = env;

main.init({
  token,
  username,
  filters: {
    org,
    wipFilter,
  }
})

/*
TYPES
*/
export interface Init {
  token: string
  username: string
  filters: {
    org: string
    wipFilter: string
  }
}

export interface SearchResults {
  data: Data;
}

export interface Data {
  search: SearchClass;
}

export interface SearchClass {
  issueCount: number;
  edges:      Edge[];
}

export interface Edge {
  node: Node;
} 

export enum ReviewDecision {
  REVIEW_REQUIRED = 'REVIEW_REQUIRED',
  CHANGES_REQUESTED = 'CHANGES_REQUESTED',
  APPROVED = 'APPROVED',
}

export enum Mergeable {
  MERGEABLE = 'MERGEABLE',
  CONFLICTING = 'CONFLICTING',
}

export interface Node {
  repository:     Repository;
  author:         Author;
  createdAt:      Date;
  number:         number;
  url:            string;
  reviewDecision: ReviewDecision;
  mergeable:      Mergeable;
  title:          string;
}

export interface Author {
  login: string;
}

export interface Repository {
  nameWithOwner: string;
}
