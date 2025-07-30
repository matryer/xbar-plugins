#!/usr/bin/env -S -P/${HOME}/.deno/bin:/opt/homebrew/bin deno run --allow-net=api.github.com --allow-env
 
// <xbar.version>v1.0.0</xbar.version>
// <xbar.author>haradakunihiko</xbar.author>
// <xbar.author.github>haradakunihiko</xbar.author.github>
// <xbar.desc>Display GitHub Pull Requests in your menu bar</xbar.desc>
// <xbar.title>GitHub Notifications</xbar.title>
// <xbar.image></xbar.image>
// <xbar.dependencies>deno</xbar.dependencies>
// <xbar.abouturl>https://github.com/haradakunihiko/xbar</xbar.abouturl>
// <xbar.image>https://github.com/haradakunihiko/xbar/blob/main/images/screenshot.png</xbar.image>
// <xbar.var>string(VAR_GITHUB_TOKEN=""): GITHUB API token to get access to remote data.</xbar.var>


import { xbar, separator } from "https://deno.land/x/xbar@v2.1.0/mod.ts";

const STATUS_CHECK_STATE = {
	SUCCESS: 'SUCCESS',
	FAILURE: 'FAILURE',
	PENDING: 'PENDING',
	ERROR: 'ERROR'
} as const;

const REVIEW_DECISION = {
	APPROVED: 'APPROVED',
	CHANGES_REQUESTED: 'CHANGES_REQUESTED',
	REVIEW_REQUIRED: 'REVIEW_REQUIRED'
} as const;

const MERGEABLE_STATE = {
	MERGEABLE: 'MERGEABLE',
	CONFLICTING: 'CONFLICTING',
	UNKNOWN: 'UNKNOWN'
} as const;

const ICON_MERGE = 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAYAAAAmlE46AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMBAJqcGAAAATJJREFUKJG9krFOAlEQRe/MLpqHnd/ATyBWsLWF+hcLPYXibvwCE/gLC2MNVrD7AbbGnpjYboBlro2QXWATabzVy5x338ydPKBC4awTVTEA8Huz9i1FI5gZBINh6+0lnHUiEXkAUGmWMGm/c7Fues4pLU9IPv+aAABm9i2qT6Pm+BECbuoqpnZWW4lmmRLG3ZdV9VyAOEyD+1JdVO4yqX+ua7UPofRHrUlEMgaA4cVYALkEACF6e/N2k4DdJCh1Ky7nENftyVACo9akcjElox3I9yfjsdoaVfFPHbtpcG2GOUy/wjS42r0QTjs3ZpibYV7kPmkxVuuGOKfIl1MAr0WjqMRcrBqec8oCVwDwnNOFnwugezkJ4+ZnFbkPeANanpwsAQr7+2m8Qab1FKcAeYgfqR/3P4pMOYR15QAAAABJRU5ErkJggg==';
const ICON_MERGE_RED = 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAYAAAAmlE46AAABE0lEQVR4AZ1Sy27CMBCk976+pz2AbFcq5AFtUe/9kYo/4xDHXLgA+QeQgD8AwozBQguJEESeZHdmZ5PY26i7bKwHdZrnbVv188QUWVtNbUf1SNIErmRcCxep2fCj+Tjqfz6juAgmxCWRRWqdp+a//Gs8iCY20ZPsrfU0bDVf2eTcSLMHzMLoUpPkiV6iwcpGOqIYzIxd17x7I2qYCxwEI/6J5lBUpXuNAt6480nFjTpxIZHEJmwvhCNBnTimpwfJu42uozanVjJiY0KyyEjebMRxfNuuXmBz5tj6FH3EsrH68TpqhM5DD5PDsRMuJPiaolKnkeM2/tUveOsEtWJRD5MldNczX+xK2FjHwoXkmo6S29Yed6S/wL/h084AAAAASUVORK5CYII=';
const ICON_CAUTION = 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAYAAAAmlE46AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMBAJqcGAAAAZxJREFUKJGdkjFoU2EUhb97k9jNRzEFoWvJVHXoZCqIaQaH7JaUbtpi2zc4ORWJYKGTYJLBroFaiGPo0hBwyAOhU51Cd8EOOid53uugL4SnkOK3/dx7OPccfvhPZPqxHZUWc0gIUjFs6c/CJdDBtN580P36lzCM1jZ+mr9HuFKkJfgXABO9i/mmqOTd7VlztXcyEYbR2oabtFx5kx+Oa7VHn+LpS7bOV3K5UfBaTF+6WLW52juR7ai0mDEGovq2WezuJ8t7UdkBGsXu5KqdfvlQhOeeGRc0h4QIV/nhuDarkIVgft+xHxJnQwWpKNJKn/cvasvtkcCxuFTUsKWkiOsgrhcoBb2uIE1W4NKRO8DH6cF0KSnLewYDBTqGb26dr+Sm53tR2ZNmE8LTx3O4V9XpKKZ1FV24MZyf2aoH8YE5garXBWC3X1oX12NXO8zfvPWqttwepZ08iA9wXqDypFE8a09y7PZL64geCXx37IO4XiSZcK/+dso8bRTP2pD65DufH96WOBuKSwWlAGD4QJ2Oqtff3e99mxVnJr8AXSGi02ni0+YAAAAASUVORK5CYII=';
const ICON_GITHUB = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABHklEQVR4AY1SAXHDMAw0hEDQnZM7xZs9QyiDQdgYjMIYbAw6BoNSCIUQCKletnr2NW3TiyS/9P+unbitHxENPsTjGNIyzmmVOHlOH1vcrgehkCG4Hy/xpxMZmELIT8Xl36x+TmfTae13jv93jcqRTph3JmhYqKMkYiYp14coDwDEmYyrHMp5sAYqSI9i5HwAr8bi/Bz/Klj9npsWd+OjuuZVrTLb9YhG76EY1JtVsEvusGljENIVyMfzvscDm1k4z/HTAOozg2lOX+BZKF8Bp2+8W6xB0kGTpinn9uzgeX47KkWEv2gQM8n6jPXIrwcd1qS95r6A66gUExZ0myFogzh3H5oqzAREbTQJPYtNsXExBNGwVTn/4u3M1nTOXQBHyKzj60yPgQAAAABJRU5ErkJggg==';
const ICON_GITHUB_SUCCESS = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGUlEQVR4AZVS21HDMBBUCfrkwx83I2fmbEYalUAHlAAdwB8TfqAD6AA6SAkpwSWkhJTg3Mo5JUpsx/F4dXvn3fVDNmMHEVnX+r+6Dfu6Cb2gcxxexrTFDEYRwzCNR/9TmLRZtW28aR6epndN2Kkv1fLOfjMZNLxSh+tFCAaKlCgLMZOUfBJFi4Y4kmqThmK0OkCFaA41xyfojtgb1/j/Y9O7JV9a0lUv1JizrerTYMEini7LNA01D+8hSIMZkJ/n+R5v0jr2rzAr0nDJ8ll9mY+HYaeSmcM39hZ81YS32QyY19Xpm4nxF0ZiJuE78Bzwbm3mIJdmzIArI4YA7gQTOCp68DFoSPEEEMK0rrYGFf0ciGP6Va80E+YD2xqP12tVGLkAAAAASUVORK5CYII=';
const ICON_GITHUB_FAILURE = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABJ0lEQVR4AZWS0U3EMAyG88gDSBnBUnqSW5QoI9wGjAAbsAIbwAbwdg956Cg3wo1wIwT/aVyRKgWuihvb8ffXSWp6DxFZN/nPYQrXYQxZ7Ow4PPdqmxxAKQawb4/+vYE0OExT/BNeusluDBflytx+2c+7QsuWzlhvRJBQK4ryImaSaR1E0SIgjqS1pYZitJrAjKLfbOB4RF21q3Gj/6pBdv85aVHXeszmx1VlWbt9QEXtdloI6aCcKkTk53mSVHfM5q451NnYJXbsXwCr9eiTsZTMQ1YRwIhPKlpgDm+4W/iHMbxuhVQkmftjErEVRqGAHwCJmcS/wEd+a6nCmLdrcp1hF0Sxtg04SQe6Haytpl/fdqCwtq3b6YoQx/KrrqrVUbiGBiLwvwHtQ525mYCkmAAAAABJRU5ErkJggg==';
const ICON_GITHUB_PENDING = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABF0lEQVR4AZVTwXGDMBDkYf8p4WaEZw4y0qgEd5ASkg5SQlJBSCpIOsgvPF2CS3AJlKDcCk4WY+zIDMvd6XZXQohq7SKi2nT2q+nc2LQuCI6G3dMadzEGoZAhuI4H2y9EWuy6zv8rnlYTTOtOqotxObP9uWo0vdIR/YUJBhTRUR7ETBLSTeRrFMSelBs55H2tA4gg3ULDfg/ejLEyrf2ei2BKdlrclY9YZZ8qSK/oFk3ch8lg3tlYFMkrTJoZdC4VcngeCz3ONMP2GbMrzp07sihm94Zvi3zXupc1+fC+eR36bVAkjgg/ICRmkvyEPDWzRIVZPKT2LaGSMuHlKkBSk9IV/H5uL/9QYh+PKgzXMPSbg2AU7NH/A1pvtWWw89UmAAAAAElFTkSuQmCC';


async function githubGraphQL<T>(query: string, variables: Record<string, unknown> = {}): Promise<GraphQLResponse<T>> {
	const TOKEN = Deno.env.get('VAR_GITHUB_TOKEN');
	
	const headers = {
		'Authorization': `token ${TOKEN}`,
		'Content-Type': 'application/json',
	};
	
	const res = await fetch('https://api.github.com/graphql', {
		method: 'POST',
		headers,
		body: JSON.stringify({ query, variables }),
	});
	
	if (!res.ok) {
		throw new Error(`GraphQL API error: ${res.statusText}`);
	}
	
	return await res.json();
}

async function fetchIssues(params: {[index: string]: string}): Promise<IssueResponse> {
	const searchQuery = Object.keys(params).map(k => `${k}:${params[k]}`).join(' ');
	
	const query = `
		query SearchIssues($query: String!) {
			search(query: $query, type: ISSUE, first: 100) {
				issueCount
				edges {
					node {
						... on PullRequest {
							title
							url
							number
							createdAt
							author {
								login
							}
							repository {
								nameWithOwner
								url
							}
							milestone {
								title
							}
							reviewDecision
							mergeable
							headRefName
							baseRefName
							commits(last: 1) {
								nodes {
									commit {
										statusCheckRollup {
											state
										}
									}
								}
							}
						}
					}
				}
			}
		}
	`;
	
	const result = await githubGraphQL<SearchIssuesResponse>(query, { query: searchQuery });
	
	if (!result.data) {
		throw new Error('Failed to fetch issues');
	}
	
	const items: IssueItem[] = result.data.search.edges.map(edge => {
		const node = edge.node;
		const repoUrl = node.repository.url.replace('https://github.com/', 'https://api.github.com/repos/');
		
		return {
			title: node.title,
			html_url: node.url,
			repository_url: repoUrl,
			user: {
				login: node.author?.login || 'unknown'
			},
			milestone: node.milestone ? { title: node.milestone.title } : undefined,
			statusCheckRollup: node.commits?.nodes[0]?.commit?.statusCheckRollup || undefined,
			reviewDecision: node.reviewDecision,
			mergeable: node.mergeable as keyof typeof MERGEABLE_STATE | undefined,
			headRefName: node.headRefName,
			baseRefName: node.baseRefName
		};
	});
	
	return {
		total_count: result.data.search.issueCount,
		items: items as [IssueItem]
	};
}

function printIssues(issues: IssueResponse, {title, color, shouldFold = false} : {title: string, color: string, icon?: string, shouldFold?: boolean }) {
  const groupedByRepo = issues.items.reduce<{[repoKey: string]: IssueItem[]}>((acc, current) => {
    const partial = current.repository_url.split('/');
    const repo = partial.pop() || '';
    const org = partial.pop() || '';
    const repoKey = `${org} - ${repo}`;
    if (!acc[repoKey]) {
      acc[repoKey] = [];
    }
    acc[repoKey].push(current);
    return acc;
  }, {});

  xbar([
    {
      text: title,
      color: color,
    },
    ...Object.keys(groupedByRepo).flatMap((repoKey) => {
      const issuesInRepo = groupedByRepo[repoKey];
	  
      const groupedByMilestone = issuesInRepo.reduce<{[milestoneKey: string]: IssueItem[]}>((acc, current) => {
        const milestoneTitle = current.milestone ? current.milestone.title : "";
        if (!acc[milestoneTitle]) {
          acc[milestoneTitle] = [];
        }
        acc[milestoneTitle].push(current);
        return acc;
      }, {});

      // マイルストーンをソート（空のマイルストーンは最後に）
      const sortedMilestoneKeys = Object.keys(groupedByMilestone).sort((a, b) => {
        if (a === "") return 1;  // 空のマイルストーンは最後
        if (b === "") return -1;
        return a.localeCompare(b);  // その他は文字列順
      });

      return [
        { text: fold(repoKey, shouldFold) },
        ...sortedMilestoneKeys.flatMap((milestoneKey) => [
          ...(milestoneKey ? [{ text: fold(`${milestoneKey}`, shouldFold) }]: [{ text: fold(`no-milestone`, shouldFold) }]),
          ...groupedByMilestone[milestoneKey].map(e => ({
            text: fold(`${e.title} by ${e.user.login}`, shouldFold),
            href: e.html_url,
            image: getIconForItem(e),
          }))
        ])
      ];
    })
  ]);
}

function fold(text: string, fold = false) {
	return  `${(fold ? '--' : '')}${text}`
}

function getIconForItem(item: IssueItem): string {
	// merge status
	if (item.mergeable === MERGEABLE_STATE.CONFLICTING) {
		return ICON_GITHUB_FAILURE;
	}

	// CI status
	switch (item.statusCheckRollup?.state) {
		case STATUS_CHECK_STATE.SUCCESS:
			return ICON_GITHUB_SUCCESS;
		case STATUS_CHECK_STATE.FAILURE:
		case STATUS_CHECK_STATE.ERROR:
			return ICON_GITHUB_FAILURE;
		default:
			return ICON_GITHUB_PENDING;
	}
}


async function main() {
	const TOKEN = Deno.env.get('VAR_GITHUB_TOKEN')
	if (!TOKEN) {
		
		xbar([
			{
				text: '●',
				color: '#FF0000',
			},
			separator,
			{
				text: 'Open Plugin... in menu bar and set github token'
			}
		]);
		return;
	}
	const prs = await fetchIssues({'type': 'pr', 'author': '@me', 'state': 'open'});
	const reviews = await fetchIssues({'type': 'pr', 'state': 'open', 'review-requested': '@me'});

	xbar([
		{
			text: '●',
			color: (prs.total_count + reviews.total_count) == 0  ? '#7d7d7d' : '#4078C0',
		},
		separator,
		{
			text: `Last updated: ${new Date().toLocaleString()}`,
			color: '#aaaa99',
			refresh: true
		}
	]);
	printIssues(prs, {title: `Pull Requests (${prs.items.length})`, color: "#58BE89" } );
	xbar([
		{
			text: '---'
		}
	])
	printIssues(reviews, {title: `Awaiting Reviews (${reviews.items.length})`, color: '#ff0000' });
}

try {
	await main();
} catch(e) {
	if (e instanceof Error) {
		console.log(e.message);
	}
}

interface IssueResponse {
	total_count: number,
	items: [IssueItem]
}

interface IssueItem {
	title: string,
	html_url: string,
	repository_url: string,
	user: {
		login: string
	},
	milestone?: {
		title: string,
	},
	statusCheckRollup?: {
		state: keyof typeof STATUS_CHECK_STATE | null
	},
	reviewDecision?: string,
	mergeable?: keyof typeof MERGEABLE_STATE,
	headRefName?: string,
	baseRefName?: string
}

interface GraphQLResponse<T> {
	data?: T;
	errors?: Array<{
		message: string;
		path?: string[];
	}>;
}

interface SearchIssuesResponse {
	search: {
		issueCount: number;
		edges: Array<{
			node: {
				title: string;
				url: string;
				number: number;
				createdAt: string;
				author?: {
					login: string;
				};
				repository: {
					nameWithOwner: string;
					url: string;
				};
				milestone?: {
					title: string;
				};
				reviewDecision?: string;
				mergeable?: string;
				headRefName?: string;
				baseRefName?: string;
				commits?: {
					nodes: Array<{
						commit: {
							statusCheckRollup?: {
								state: keyof typeof STATUS_CHECK_STATE | null;
							};
						};
					}>;
				};
			};
		}>;
	};
}
