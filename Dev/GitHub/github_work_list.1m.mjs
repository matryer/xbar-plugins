#!/usr/bin/env /usr/local/bin/node
// jshint asi:true
// jshint esversion: 11
// jshint module:true
// jshint node:true
// <xbar.title>GitHub Work List</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Simeon Cheeseman</xbar.author>
// <xbar.author.github>simeonc</xbar.author.github>
// <xbar.desc>List of your assigned, authored and review requested pull requests. Also shows any branches pushed that haven't had pull requests created yet.
// Each Pull in the dropdown is grouped by project and displays `"Title"; "Mergeable Status"`, clicking on an Pull opens it in the browser</xbar.desc>
// <xbar.dependencies>node.js</xbar.dependencies>
// <xbar.image>https://i.imgur.com/SwvRjft.png</xbar.image>

// <xbar.var>string(GITHUB_API_TOKEN=""): Github token with repo:status scope which is required for checks</xbar.var>
// <xbar.var>string(GITHUB_HOST="github.com"): The domain your instance is hosted on. Leave the default if using github.com</xbar.var>
// <xbar.var>number(FONT_SIZE=15): Font size of the MR title and status</xbar.var>
// <xbar.var>number(PROJECT_FONT_SIZE=13): Font size of the project heading</xbar.var>
// <xbar.var>select(DARK_MODE="auto"): Choose dark mode settings [auto, dark, light]</xbar.var>

/**
 * Information
 * @see   BitBar Node Module Docs     https://github.com/sindresorhus/xbar
 */

const private_token = process.env.GITHUB_API_TOKEN;
const font_size = process.env.FONT_SIZE;
// <xbar.var>boolean(MENU_BAR_SHOW_NEW_BRANCH=true): show count and icon 📝 of new branches pushed without a Pull icon in the menubar (note that this basically is from when the plugin is installed, not all branches)</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_REQUESTED_REVIEWER=true): show count and icon 🔎 of requested reviews in the menubar</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_DRAFT=true): show count and icon 🛠️ of Draft pulls in the menubar</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_UNMERGABLE=true): show count and icon ⛔ of unmergable Pulls in the menubar - Unmergable means the head ref is out of date or the merge commit cannot be cleanly created</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_BLOCKED=true): show count and icon 🚧 of Pulls that are blocked in the menubar</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_APPROVED=true): show count and icon ❇️ of Pulls with Mergeable and passing commit status in the menubar</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_UNSTABLE=true): show count and icon ⚠️ of Pulls that are mergeable with non-passing commit status in the menubar</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_HAS_HOOKS=true): show count and icon 🚀 of Pulls that are mergeable with passing commit status and pre-receive hooks in the menubar</xbar.var>
// <xbar.var>boolean(MENU_BAR_SHOW_UNKNOWN=true): show count and icon 💬 of pulls that have an unknown mergable state in the menubar</xbar.var>

/**
 * Change this array to choose which statuses show and in what order on the menu bar.
 *
 * @type {string[]}
 */
const MENU_BAR_ORDER = [
  process.env.MENU_BAR_SHOW_NEW_BRANCH === "true" ? "new" : false,
  process.env.MENU_BAR_SHOW_REQUESTED_REVIEWER === "true" ? "reviewer" : false,
  process.env.MENU_BAR_SHOW_DRAFT === "true" ? "draft" : false,
  process.env.MENU_BAR_SHOW_UNMERGABLE === "true" ? "unmergable" : false,
  process.env.MENU_BAR_SHOW_BLOCKED === "true" ? "blocked" : false,
  process.env.MENU_BAR_SHOW_APPROVED === "true" ? "approved" : false,
  process.env.MENU_BAR_SHOW_UNSTABLE === "true" ? "failed" : false,
  process.env.MENU_BAR_SHOW_HAS_HOOKS === "true" ? "ciRunning" : false,
  process.env.MENU_BAR_SHOW_UNKNOWN === "true" ? "pending" : false,
].filter((k) => !!k);

/**
 * MAX length of the title string in the toolbar
 *
 * @type {number}
 */
const MAX_LENGTH = 60;

/////////////////////////////////////////////////////////////////////////
// Do not edit below this line unless you know what you're doing. :)  //
///////////////////////////////////////////////////////////////////////
import fs from "fs";
import path from "path";
import childProcess from "child_process";

const [node, thisFilePath, ...args] = process.argv;
const SETTINGS_FILE_PATH = `${thisFilePath.replace(
  /\/(?!.*\/)/gi,
  "/."
)}.settings.json`;

(async () => {
  let _settings;

  async function updateSettings() {
    if (!_settings) return;
    await fs.writeFileSync(SETTINGS_FILE_PATH, JSON.stringify(_settings));
  }
  /**
   * @returns {Promise<{ username: string, currentBranches: {id: string, repoId: string, repo: string, branch: string, description: string, url: string}[], ignoredBranches: {id: string, repoId: string, ref: string}[]}>}
   */
  async function getSettings() {
    if (_settings) return _settings;
    if (fs.existsSync(SETTINGS_FILE_PATH)) {
      _settings = JSON.parse(fs.readFileSync(SETTINGS_FILE_PATH)) || {};
    } else {
      _settings = {};
    }
    if (!_settings.username) {
      const myUser = await octokit.rest.users.getAuthenticated();
      _settings.username = myUser.data.login;
    }
    if (!_settings.currentBranches) {
      _settings.currentBranches = [];
    }
    if (!_settings.ignoredBranches) {
      _settings.ignoredBranches = [];
    }
    return _settings;
  }
  if (args.length) {
    const [command, ...params] = args;
    switch (command) {
      case "remove_branch":
        const settings = await getSettings();
        const branchPredicate = (branch) => branch.id === params[0];
        const branchToRemove = settings.currentBranches.find(branchPredicate);
        if (!branchToRemove) return;
        const index = settings.currentBranches.findIndex(branchPredicate);
        if (index !== -1) {
          settings.currentBranches.splice(index, 1);
        }
        if (!settings.ignoredBranches.find(branchPredicate)) {
          settings.ignoredBranches.push({
            id: branchToRemove.id,
            repoId: branchToRemove.repoId,
            branch: branchToRemove.branch,
          });
        }
        await updateSettings();
        return;
      default:
        console.error(`Unknown command ${args.join(" ")}`);
        return;
    }
  }
  const xbar = await installAndRequire("@sindresorhus/xbar");
  const { Octokit } = await installAndRequire("octokit");

  const textColor = (() => {
    switch (process.env.DARK_MODE) {
      case "dark":
        return "white";
      case "light":
        return "black";
      case "auto":
      default:
        return xbar.darkMode ? "white" : "black";
    }
  })();

  const githubIconBase64 =
    textColor === "black"
      ? "iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEqADAAQAAAABAAAAEgAAAACaqbJVAAABlklEQVQ4EZWUOS9EURiGrzViC4JILBmTiE4iljEaNBRKChGVUqMRiUIUConwD/wACrWIimQqGjIKIaGgtMYe2/OOOXLOzTXLmzwz51vO9t3vXs9zdYT5BBsQhRbohQHohnoYgV34gk3IAUd5WJ9wAtfwnYJLYmfwDOXgqAZLk0ehDFZhFrogDH2wAhNQAIvwAlXgSNfQcecc7//GNqFXqLZT8jGO4Qaa7UCK8RAx3WDazqnF0OoLtjPNWDXVxufKy9UPKgIF4jIylB7MIYSUbxb6YKz6NMiZhVrJvbPzSzDUP3u2M824jrhqtO7PW04Gxv2BALsQ3xLoep0Bce8Ap3ZRZ7eDOtlcX082BMOwD1pEnW3iiWZTp/aDdloDLSbeQa+GNAaqpfxvMAXO69GE4xRU7EGQ1JyaoHevGCSdTi2yBRUQqEq8F/AAMzAJWmgejLT7PcSMw/7XvaVbaIMdUBGNX6c00sKSc51fl1UoHDpNBHS6DshKfxW3Zj0yvkra+kzYUrH1ADKWjt8Dpb4ZYexGny9h/gAjnlV3yrzolgAAAABJRU5ErkJggg=="
      : "iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEqADAAQAAAABAAAAEgAAAACaqbJVAAABvklEQVQ4EZWUvS8EQRjGb91FBIUTkYuvLCI6iZDzUZ2GQqkRiUap0UhERJSXSPgL9CjUIioSFQ2hIqGg9XFBzuXu1u/Zvdnbw+XOk/x23nnn2dmZ3Xc2FArIcZwr+IB9GIM+GIcJiEM7zMAJ5OEALE3hXhSQCNNk4A5aoBnK6YmBNLRBzLKsVE3AqRvVXwMbtmEF4tALCdiCeeiGXdBCIlAUK9I2tNzVYrZ8hO8I0qDVe6ITgRt4Bj2tovBNgbTkm+m0gmbf8JMVArxh0IPvZTXvqI5YL/tayWrEC87huwRbfjNRljgPHUr+Q/14X30/y2sA1c+pn6wQ4I2BtFdiJbHpph1nrmTgjw6+WkhCDoZ/WUhegKTKHgRVsrt9Wn1ZG6bhHDSJKtt7PQQ98AgJ0JN2wChDoIJU5c9CtjDwRbsIKkhPdLrgFlSMk8rSqjglnb36Qk6rU4kcQpN3948rA1F4gBQswwJI68ZKbMEbnJlcsHXPCTXxgmGAgWNIgjk/KglXeLA4iovb8Ybcq6mjkE4wjJCNwlDAU1XoT2TcTPZOrN+E9Ok1/lWFq19NdWIL2scoNAbvoK8v3BnMmfgbpQWDYNC92VIAAAAASUVORK5CYII=";
  const octokit = new Octokit({
    userAgent: "xbar/github_work-list - node " + process.version,
    auth: private_token,
  });

  const statusIconMap = {
    reviewer: "🔎",
    unmergable: "⛔",
    failed: "⚠️",
    ciRunning: "🚀",
    pending: "💬",
    approved: "❇️",
    draft: "🛠️",
    blocked: "🚧",
    new: "📝",
  };

  async function searchPullRequests() {
    const { username } = await getSettings();
    const responses = await Promise.all([
      octokit.rest.search.issuesAndPullRequests({
        q: `is:open is:pr review-requested:${username} archived:false `,
        per_page: 20,
      }),
      octokit.rest.search.issuesAndPullRequests({
        q: `is:open is:pr assignee:${username} archived:false `,
        per_page: 20,
      }),
      octokit.rest.search.issuesAndPullRequests({
        q: `is:open is:pr author:${username} archived:false `,
        per_page: 20,
      }),
    ]);
    const uniqueResponses = [];
    responses.forEach((response) => {
      response.data.items.forEach((item) => {
        if (!uniqueResponses.find((uniqueItem) => uniqueItem.id === item.id)) {
          uniqueResponses.push(item);
        }
      });
    });
    return await Promise.all(
      uniqueResponses.slice(0, 20).map(async (item) => {
        const [owner, repo] = item.repository_url.split("/").slice(-2);
        const result = await octokit.graphql(
          `query($owner: String!, $repo: String!, $pull_number: Int!) {
            repository(owner: $owner, name:$repo) {
              pullRequest(number:$pull_number) {
                id
                baseRepository {
                  id
                  nameWithOwner
                }
                headRepository {
                  id
                  nameWithOwner
                }
                author {
                  login
                }
                assignees(first: 10) {
                  nodes {
                    login
                  }
                }
                mergeStateStatus
                reviewDecision
                isDraft
                title
                url
                headRefName
                bodyText
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
          }`,
          {
            headers: {
              Accept: "application/vnd.github.merge-info-preview+json",
            },
            owner,
            repo,
            pull_number: item.number,
          }
        );
        const { pullRequest } = result.repository;
        const [{ commit: lastCommit }] = pullRequest.commits.nodes;
        const pull = {
          number: item.number,
          title: pullRequest.title,
          body: pullRequest.bodyText,
          url: pullRequest.url,
          headRef: pullRequest.headRefName,
          reviewDecision: pullRequest.reviewDecision,
          mergeStateStatus: pullRequest.mergeStateStatus,
          author: pullRequest.author.login,
          assignees: pullRequest.assignees.nodes.map((node) => node.login),
          repoName: pullRequest.baseRepository.nameWithOwner,
          repoId: pullRequest.headRepository.id,
          isDraft: pullRequest.isDraft,
        };
        if (lastCommit.statusCheckRollup) {
          const { state } = lastCommit.statusCheckRollup;
          switch (state) {
            case "ERROR":
            case "FAILURE":
              pull.latestCheck = "failed";
              break;
            case "EXPECTED":
            case "PENDING":
              pull.latestCheck = "running";
              break;
            case "SUCCESS":
              pull.latestCheck = "passed";
              break;
          }
        }
        return pull;
      })
    );
  }

  async function updateCurrentBranches() {
    const { ignoredBranches, currentBranches, username } = await getSettings();
    const activities =
      await octokit.rest.activity.listEventsForAuthenticatedUser({
        username,
      });
    for (let i = activities.data.length - 1; i >= 0; i -= 1) {
      const activity = activities.data[i];
      const findPredicate = (event) =>
        event.repoId === activity.repo.id &&
        event.branch === activity.payload.ref;
      if (
        activity.type === "CreateEvent" &&
        activity.payload.ref_type === "branch"
      ) {
        if (
          ignoredBranches.find(
            (branch) =>
              branch.id === activity.id && branch.repoId === activity.repo.id
          )
        )
          continue;
        const oldBranchIndex = currentBranches.findIndex(
          (branch) => branch.id === activity.id
        );
        const branch = {
          id: activity.id,
          repoId: activity.repo.id,
          repo: activity.repo.full_name || activity.repo.name,
          branch: activity.payload.ref,
          description: activity.payload.description,
          url: `https://github.com/${activity.repo.name}/compare/${activity.payload.ref}?expand=1`,
        };
        if (oldBranchIndex >= 0) {
          currentBranches.splice(oldBranchIndex, 1, branch);
        } else {
          currentBranches.push(branch);
        }
      } else if (
        activity.type === "DeleteEvent" &&
        activity.payload.ref_type === "branch"
      ) {
        const branchIndex = currentBranches.findIndex(findPredicate);
        if (branchIndex >= 0) {
          currentBranches.splice(branchIndex, 1);
        }
        const ignoredBranchIndex = ignoredBranches.findIndex(findPredicate);
        if (ignoredBranchIndex >= 0) {
          ignoredBranches.splice(ignoredBranchIndex, 1);
        }
      } else if (
        activity.type === "PullRequestEvent" &&
        activity.payload.action === "opened"
      ) {
        const branch = currentBranches.find(
          (event) =>
            event.repoId === activity.payload.pull_request.head.repo.id &&
            event.branch === activity.payload.pull_request.head.ref
        );
        if (branch) {
          currentBranches.splice(currentBranches.indexOf(branch), 1);
        }
      }
    }
    return currentBranches;
  }

  function get(object, keyPath) {
    const keys = keyPath.split(".");
    let result = object;
    while (keys.length && typeof result === "object") {
      const currentKey = keys.shift();
      result = result[currentKey];
    }
    return result;
  }
  function createMapByKey(items, keyPath) {
    const result = {};
    items.forEach((item) => {
      const key = get(item, keyPath);
      result[key] = result[key] || [];
      result[key].push(item);
    });
    return result;
  }

  async function getPulls() {
    const { username } = await getSettings();

    const statusCount = {};
    function incrementStatusCount(key) {
      statusCount[key] = statusCount[key] || 0;
      statusCount[key] += 1;
    }

    const [currentBranches, pullRequestSearch] = await Promise.all([
      updateCurrentBranches(),
      searchPullRequests(),
    ]);
    if (currentBranches.length === 0 && pullRequestSearch.length === 0) {
      // Hide when no open MRs
      return xbar([
        {
          image: githubIconBase64,
          text: `0 Pulls`,
          color: textColor,
          dropdown: true,
        },
      ]);
    }
    const currentBranchesByProject = createMapByKey(currentBranches, "repo");
    const pullRequestsByProject = createMapByKey(pullRequestSearch, "repoName");
    const projectIds = Object.keys(currentBranchesByProject);
    Object.keys(pullRequestsByProject).forEach((projectId) => {
      if (projectIds.indexOf(projectId) === -1) {
        projectIds.push(projectId);
      }
    });
    function getProjectName(projectId) {
      const projectBranches = currentBranchesByProject[projectId] || [];
      const pulls = pullRequestsByProject[projectId] || [];
      if (projectBranches.length) {
        return projectBranches[0].repo;
      }
      return pulls[0].repoName;
    }
    projectIds.sort((projectIdA, projectIdB) => {
      const nameA = getProjectName(projectIdA);
      const nameB = getProjectName(projectIdB);
      if (nameA < nameB) return -1;
      if (nameA > nameB) return 1;
      return 0;
    });
    const content = [];
    projectIds.forEach((projectId) => {
      const projectBranches = currentBranchesByProject[projectId] || [];
      const pulls = pullRequestsByProject[projectId] || [];
      content.push(xbar.separator);
      content.push({
        text: getProjectName(projectId),
        size: process.env.PROJECT_FONT_SIZE,
      });
      projectBranches.forEach((branch) => {
        incrementStatusCount("new");
        const submenu = (
          branch.description
            ? [
                {
                  text: "Description",
                  size: process.env.PROJECT_FONT_SIZE,
                },
                {
                  text: branch.description,
                  size: font_size,
                },
              ]
            : []
        ).concat([
          {
            text: "Remove from list",
            size: font_size,
            shell: thisFilePath,
            param1: `remove_branch`,
            param2: branch.id,
            terminal: false,
          },
        ]);
        content.push({
          text: `${statusIconMap.new}: ${branch.branch}`,
          href: branch.url,
          size: font_size,
          submenu,
        });
      });
      pulls.forEach((pull) => {
        let icon;
        if (
          pull.author !== username &&
          !pull.assignees.find((assignee) => assignee === username)
        ) {
          icon = "reviewer";
        } else if (pull.isDraft) {
          icon = "draft";
        } else {
          icon = (() => {
            switch (pull.mergeStateStatus) {
              case "BEHIND": // The head ref is out of date.
              case "DIRTY": // The merge commit cannot be cleanly created.
                return "unmergable";
              case "CLEAN": // Mergeable and passing commit status.
                return "approved";
              case "DRAFT": // The merge is blocked due to the pull request being a draft.
                return "draft";
              case "HAS_HOOKS": // Mergeable with passing commit status and pre-receive hooks.
                return "ciRunning";
              case "UNSTABLE": // Mergeable with non-passing commit status.
                if (pull.latestCheck === "running") return "ciRunning";
                return "failed";
              case "UNKNOWN": // The state cannot currently be determined.
              case "BLOCKED": // The merge is blocked.
              default:
                if (pull.reviewDecision === "CHANGES_REQUESTED")
                  return "blocked";
                switch (pull.latestCheck) {
                  case "running":
                    return "ciRunning";
                  case "failed":
                    return "failed";
                }
                return "pending";
            }
          })();
        }
        incrementStatusCount(icon);
        let submenu = [
          {
            text: "Branch",
            size: process.env.PROJECT_FONT_SIZE,
          },
          {
            text: pull.headRef,
            size: font_size,
          },
        ];
        if (pull.body) {
          submenu = submenu.concat([
            {
              text: "Description",
              size: process.env.PROJECT_FONT_SIZE,
            },
            { text: pull.body, size: font_size },
          ]);
        }
        content.push({
          text: `#${pull.number} ${pull.title} ${statusIconMap[icon]}`,
          href: pull.url,
          size: font_size,
          submenu: submenu,
        });
      });
    });
    const statusString = MENU_BAR_ORDER.map((key) => {
      if (statusCount[key]) {
        return `${statusIconMap[key]}[${statusCount[key]}]`;
      }
    })
      .filter((s) => !!s)
      .join(", ");
    content.unshift({
      image: githubIconBase64,
      text: ` ${statusString}`,
      color: textColor,
      dropdown: true,
    });

    // Update settings file for speed later
    await updateSettings();

    function sanitizeText(items) {
      return items.map((element) => {
        if (typeof element !== "object") return element;
        let { text, submenu, ...result } = element;
        if (text) result.text = text.replace(/\|/gi, "┃");
        if (submenu) result.submenu = sanitizeText(submenu);
        return result;
      });
    }
    try {
      await xbar.default(sanitizeText(content));
    } catch (error) {
      console.log(JSON.stringify(content, null, 2));
      throw error;
    }
  }

  getPulls().catch((error) => {
    console.error(error);
  });
})();

// These utility functions taken from another plugin;
// https://github.com/matryer/xbar-plugins/blob/f1004a74a0d887b9655c71a1d52ba4e02b37fd77/Dev/Gitlab/gitlab_projects.js

/**
 * Sets up the ability to require global node packages.
 *
 * @return     {object}  Returns the required node package object
 */
async function globalRequire(packageName) {
  const env = Object.assign({}, process.env);
  env.PATH = path.resolve("/usr/local/bin") + ":" + env.PATH;

  const globalNodeModulesDir =
    childProcess
      .execSync(npmBin() + " root -g", { env: env })
      .toString()
      .trim() + "/";
  let packageDir = path.join(globalNodeModulesDir, packageName, "/");

  //find package required by older versions of npm
  if (!fs.existsSync(packageDir)) {
    packageDir = path.join(
      globalNodeModulesDir,
      "npm/node_modules/",
      packageName
    );
  }

  // Package not found
  if (!fs.existsSync(packageDir)) {
    throw new Error("Cannot find global module '" + packageName + "'");
  }

  const packageMeta = JSON.parse(
    fs.readFileSync(path.join(packageDir, "package.json")).toString()
  );
  const main = path.join(packageDir, packageMeta.main || packageMeta.files[0]);

  return await import(main);
}

async function installAndRequire(module) {
  try {
    return await import(module);
  } catch (e) {
    try {
      return await globalRequire(module);
    } catch (e) {
      installModule(module);

      // Not catching error if one is thrown.
      return await globalRequire(module);
    }
  }
}

/**
 * Installs node module if it doesn't exit.)
 */
function installModule(module) {
  // Allows one to run the npm command as if on the command line.
  const execSync = childProcess.execSync;

  const env = Object.assign({}, process.env);
  env.PATH = path.resolve("/usr/local/bin") + ":" + env.PATH;

  // Get the path to npm bin
  const npm = npmBin();

  // The install command
  const cmd = npm + " install -g " + module;

  console.log("Installing the " + module + " Node module...");

  execSync(cmd, {
    cwd: process.cwd(),
    env: env,
  })
    .toString("utf8")
    .trim();

  console.log("Installation complete.");
}

/**
 * Gets the path to your npm executable.
 *
 * @return  {string}  The full path to your npm executable
 */
function npmBin() {
  const execSync = childProcess.execSync;
  const env = Object.assign({}, process.env);
  env.PATH = path.resolve("/usr/local/bin") + ":" + env.PATH;

  // Get the path to npm bin
  return execSync("which npm", { env: env }).toString("utf8").trim();
}
