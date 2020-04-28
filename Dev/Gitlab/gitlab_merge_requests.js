#!/usr/bin/env /usr/local/bin/node
// jshint asi:true
// jshint esversion: 8
// <bitbar.title>GitLab Merge Requests</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Simeon Cheeseman</bitbar.author>
// <bitbar.author.github>simeonc</bitbar.author.github>
// <bitbar.desc>List of your assigned and created merge requests.
// The menu bar shows a count of MRs requiring your approval (🔎), WIP MRs (🛠️), Un-mergable MRs (⛔), MRs with failed pipelines (⚠️), MRs with unresolved discussions (🚧), MRs with running pipeline (🚀), MRs pending approval (💬) and Approved/Ready to Merge (❇️).
// Each MR in the dropdown is grouped by project and displays `<Unmergable> "Title"; "Pipeline Status" : "Approval Status"`, clicking on an MR opens it in the browser</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>https://i.imgur.com/t0TtQXO.png</bitbar.image>

/**
 * Information
 *
 * This was inspired by the work of Shelton Koskie (@eightygrit), who made the
 * first version of the "GITLAB Projects" for API v4.
 *
 * @see   GitLab API Documentation    https://docs.gitlab.com/ee/api/README.html
 * @see   Create GitLab Access Token  https://gitlab.com/profile/personal_access_tokens
 * @see   BitBar Node Module Docs     https://github.com/sindresorhus/bitbar
 */

/**
 * The domain your instance is hosted on. Leave the default if using gitlab.com
 *
 * @const       {string}
 */
const gitlab_domain = 'gitlab.com';

/**
 * Your private access token.
 *
 * @const       {string}
 * @see       Create a token  https://gitlab.com/profile/personal_access_tokens
 */
const private_token = '7Tkm2aLMxk8q-M9TaJz1';

/**
 * Font size of the project list
 *
 * @const       {string}
 */
const font_size = '15';

/**
 * Change this array to choose which statuses show and in what order on the menu bar.
 *
 * @type {string[]}
 */
const MENU_BAR_ORDER = [
  'newMR',
  'reviewer',
  'wip',
  'unmergable',
  'failed',
  'unresolved',
  'ciRunning',
  'pending',
  'approved'
];

/**
 * MAX length of the title string in the toolbar
 *
 * @type {number}
 */
const MAX_LENGTH = 60;

/////////////////////////////////////////////////////////////////////////
// Do not edit below this line unless you know what you're doing. :)  //
///////////////////////////////////////////////////////////////////////
let bitbar;
const fs = require('fs');
const path = require('path');
const SETTINGS_FILE_PATH = path.resolve(
  __dirname,
  './.gitlab_merge_requests.settings.json'
);

// Verify bitbar node module is available or try to install it globally.
try {
  bitbar = require('bitbar');
} catch (e) {
  try {
    bitbar = globalRequire('bitbar');
  } catch (e) {
    installBitbarModule();

    // Not catching error if one is thrown.
    bitbar = globalRequire('bitbar');
  }
}

// Converts the gitlab status to emoji
// see https://emojipedia.org/ for customisation
function stateIcon(status) {
  return {
    created: '💤',
    pending: '💤',
    running: '🚀',
    failed: '⚠️',
    success: '✅',
    skipped: '🚀',
    manual: '⏯️'
  }[status];
}

const gitlabIconBase64 =
  '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';

function request(path) {
  const httpTransport = require('https');
  const responseEncoding = 'utf8';
  const httpOptions = {
    hostname: gitlab_domain,
    port: '443',
    path,
    method: 'GET',
    headers: { 'PRIVATE-TOKEN': private_token }
  };
  httpOptions.headers['User-Agent'] =
    'bitbar/gitlab_projects - node ' + process.version;

  return new Promise((resolve, reject) => {
    const request = httpTransport
    .request(httpOptions, (res) => {
      let responseBufs = [];
      let responseStr = '';

      res
      .on('data', (chunk) => {
        if (Buffer.isBuffer(chunk)) {
          responseBufs.push(chunk);
        } else {
          responseStr = responseStr + chunk;
        }
      })
      .on('end', () => {
        if (responseBufs.length > 0) {
          responseStr = Buffer.concat(responseBufs).toString(responseEncoding);
        } else {
          responseStr = responseStr;
        }

        resolve(JSON.parse(responseStr));
      });
    })
    .setTimeout(0)
    .on('error', (error) => {
      reject(error);
    });
    request.write('');
    request.end();
  });
}

const cachedProjectNames = {};
async function registerProject(project_id) {
  if (!cachedProjectNames[project_id]) {
    const project = await request(`/api/v4/projects/${project_id}`);
    cachedProjectNames[project_id] = project.name_with_namespace;
  }
}

async function getMRs() {
  if (!fs.existsSync(SETTINGS_FILE_PATH)) {
    const myUser = await request('/api/v4/user');
    fs.writeFileSync(
      SETTINGS_FILE_PATH,
      JSON.stringify({
        userId: myUser.id
      })
    );
  }

  const statusCount = {};
  function incrementStatusCount(key) {
    statusCount[key] = statusCount[key] || 0;
    statusCount[key] += 1;
  }

  const mergeRequestsByProjectId = {};

  const yesterday = new Date();
  yesterday.setDate(yesterday.getDate() - 1);
  const yesterdayParam = `${yesterday.getFullYear()}-${yesterday.getMonth() + 1}-${yesterday.getDate()}`;

  const { userId, todaysUUID, todaysExcludedEvents = [] } = JSON.parse(fs.readFileSync(SETTINGS_FILE_PATH));
  let excludedEvents = [];
  if (todaysUUID === yesterdayParam) {
    excludedEvents = todaysExcludedEvents;
  }

  const createdMRs = await request(
    '/api/v4/merge_requests?scope=created_by_me&state=opened'
  );
  const assignedMRs = await request(
    '/api/v4/merge_requests?scope=assigned_to_me&state=opened'
  );
  const approvalMRs = await request(
    `/api/v4/merge_requests?scope=all&state=opened&approver_ids[]=${userId}`
  );

  const allMRs = assignedMRs
  .concat(approvalMRs, createdMRs)
  .reduce((uniqueMRs, currentMR) => {
    if (!uniqueMRs.find(({ id }) => id === currentMR.id)) {
      uniqueMRs.push(currentMR);
    }
    return uniqueMRs;
  }, []);

  const bitbar = globalRequire('bitbar');

  if (allMRs.length === 0) {
    // Hide when no open MRs
    return bitbar([
      {
        image: gitlabIconBase64,
        text: `0 MR`,
        color: bitbar.darkMode ? 'white' : 'black',
        dropdown: true
      }
    ]);
  }


  await Promise.all(
    allMRs.map(async (mergeRequest) => {
      const {
        web_url,
        merge_when_pipeline_succeeds,
        merge_status,
        title,
        project_id,
        iid,
        blocking_discussions_resolved,
        work_in_progress,
        assignee,
        assignees,
        author,
        source_branch
      } = mergeRequest;
      excludedEvents.push(`${project_id}_${source_branch}`);

      const isReviewer =
        (!author || author.id !== userId) &&
        (!assignee || assignee.id !== userId) &&
        (!assignees || !assignees.find(({ id }) => id === userId));

      await registerProject(project_id);

      mergeRequestsByProjectId[project_id] =
        mergeRequestsByProjectId[project_id] || [];

      const pipelines = await request(
        `/api/v4/projects/${project_id}/merge_requests/${iid}/pipelines`
      );
      let pipelineStatus = '';
      if (pipelines.length) {
        pipelineStatus = stateIcon(pipelines[0].status);
      }
      const approvals = await request(
        `/api/v4/projects/${project_id}/merge_requests/${iid}/approval_state`
      );
      const isApproved = approvals.rules.reduce(
        (isApproved, { approved }) => isApproved && approved,
        true
      );
      const hasCurrentUserApproved = approvals.rules.reduce(
        (currentUserHasApproved, { approved_by }) =>
          currentUserHasApproved ||
          !!approved_by.find(({ id }) => id === userId),
        false
      );
      const canMerge = merge_status === 'can_be_merged';
      if (isReviewer && (work_in_progress || !canMerge || isApproved || hasCurrentUserApproved))
        return;
      let mergeStatus = '  ';
      if (!canMerge) {
        mergeStatus = '⛔ ';
      } else if (isReviewer) {
        mergeStatus = '🔎 ';
      }
      let approvalStatus = '💬';
      if (work_in_progress) {
        approvalStatus = '🛠️';
      } else if (!blocking_discussions_resolved) {
        approvalStatus = '🚧';
      } else if (isApproved) {
        approvalStatus = '❇️';
      }
      if (isReviewer) {
        incrementStatusCount('reviewer');
      } else if (work_in_progress) {
        incrementStatusCount('wip');
      } else if (merge_status !== 'can_be_merged') {
        incrementStatusCount('unmergeable');
      } else if (pipelines.length && pipelines[0].status === 'failed') {
        incrementStatusCount('failed');
      } else if (!blocking_discussions_resolved) {
        incrementStatusCount('unresolved');
      } else if (pipelines.length && pipelines[0].status === 'running') {
        incrementStatusCount('ciRunning');
      } else if (!isApproved) {
        incrementStatusCount('pending');
      } else {
        incrementStatusCount('approved');
      }

      let statusString = '';
      if (pipelineStatus || approvals.rules.length) {
        const statusStrings = [];
        if (pipelineStatus) statusStrings.push(pipelineStatus);
        if (approvals.rules.length) statusStrings.push(approvalStatus);
        statusString = `  ${statusStrings.join(' ')}`;
      }
      let trimmedTitle = title.substring(0, MAX_LENGTH);
      if (title.length > MAX_LENGTH) {
        trimmedTitle += '...';
      }
      mergeRequestsByProjectId[project_id].push(
        `${mergeStatus}!${iid}: ${trimmedTitle}${statusString} | href="${web_url}" size=${font_size}`
      );
    })
  );

  const todaysEvents = await request(
    `/api/v4/users/4557473/events?after=${yesterdayParam}&action=pushed`
  );
  for (let i = 0; i < todaysEvents.length; i += 1) {
    const { project_id, push_data: {action, ref, ref_type, commit_to} } = todaysEvents[i];
    if (excludedEvents.indexOf(`${project_id}_${ref}`) !== -1) continue;
    if (!(action === 'created' && ref_type === 'branch')) continue;

    const mrCheck = await request(`/api/v4/projects/${project_id}/repository/commits/${commit_to}/merge_requests`);
    if (mrCheck.length) {
      excludedEvents.push(`${project_id}_${ref}`);
      continue;
    }
    await registerProject(project_id);
    const { web_url } = await request(`/api/v4/projects/${project_id}`);
    const params = [
      `merge_request%5Bsource_branch%5D=${ref}`,
      `merge_request%5Bsource_project_id%5D=${project_id}`,
      `merge_request%5Btarget_project_id%5D=${project_id}`
    ];
    const createMRUrl = `${web_url}/-/merge_requests/new?${params.join('&')}`;
    mergeRequestsByProjectId[project_id] = mergeRequestsByProjectId[project_id] || [];
    mergeRequestsByProjectId[project_id].unshift(
      `📝: ${ref} | href="${createMRUrl}" size=${font_size}`
    );
    incrementStatusCount('newMR');
  }

  const content = [];

  const statusMap = {
    reviewer: '🔎',
    unmergable: '⛔',
    failed: '⚠️',
    ciRunning: '🚀',
    pending: '💬',
    approved: '❇️',
    wip: '🛠️',
    unresolved: '🚧',
    newMR: '📝'
  };
  const statusString = MENU_BAR_ORDER
  .map((key) => {
    if (statusCount[key]) {
      return `${statusMap[key]}[${statusCount[key]}]`;
    }
  })
  .filter((s) => !!s)
  .join(', ');

  content.push({
    image: gitlabIconBase64,
    text: `${statusString}`,
    color: bitbar.darkMode ? 'white' : 'black',
    dropdown: true
  });

  Object.keys(mergeRequestsByProjectId).forEach((projectId) => {
    if (!mergeRequestsByProjectId[projectId].length) return;
    content.push(bitbar.separator);
    content.push({
      text: cachedProjectNames[projectId]
    });
    mergeRequestsByProjectId[projectId].forEach((text) =>
      text ? content.push({ text }) : null
    );
  });

  // Update settings file for speed later
  fs.writeFileSync(
    SETTINGS_FILE_PATH,
    JSON.stringify({
      userId,
      todaysUUID: yesterdayParam,
      todaysExcludedEvents: excludedEvents.reduce((uniqueEvents, event) => {
        if (uniqueEvents.indexOf(event) === -1) {
          uniqueEvents.push(event);
        }
        return uniqueEvents;
      }, [])
    })
  );

  try {
    bitbar(content);
  } catch (error) {
    console.log(content);
    throw error;
  }
}

getMRs().catch((error) => {
  console.error(error);
});

/**
 * Sets up the ability to require global node packages.
 *
 * @return     {object}  Returns the required node package object
 */
function globalRequire(package) {
  const childProcess = require('child_process');
  const path = require('path');
  const fs = require('fs');
  const env = Object.assign({}, process.env);
  env.PATH = path.resolve('/usr/local/bin') + ':' + env.PATH;

  const globalNodeModulesDir =
    childProcess
    .execSync(npmBin() + ' root -g', { env: env })
    .toString()
    .trim() + '/';
  let packageDir = path.join(globalNodeModulesDir, package, '/');

  //find package required by older versions of npm
  if (!fs.existsSync(packageDir)) {
    packageDir = path.join(globalNodeModulesDir, 'npm/node_modules/', package);
  }

  // Package not found
  if (!fs.existsSync(packageDir)) {
    throw new Error("Cannot find global module '" + package + "'");
  }

  const packageMeta = JSON.parse(
    fs.readFileSync(path.join(packageDir, 'package.json')).toString()
  );
  const main = path.join(packageDir, packageMeta.files[0]);

  return require(main);
}

/**
 * Installs Bitbar node module if it doesn't exits.
 *
 * @see    BitBar node module on github    https://github.com/sindresorhus/bitbar
 */
function installBitbarModule() {
  // Allows one to run the npm command as if on the command line.
  const childProcess = require('child_process');
  const execSync = childProcess.execSync;
  const path = require('path');
  const fs = require('fs');

  const env = Object.assign({}, process.env);
  env.PATH = path.resolve('/usr/local/bin') + ':' + env.PATH;

  // Get the path to npm bin
  const npm = npmBin();

  // The install command
  const cmd = npm + ' install -g bitbar';

  console.log('Installing the BitBar Node module...');

  const output = execSync(cmd, {
    cwd: process.cwd(),
    env: env
  })
  .toString('utf8')
  .trim();

  console.log('Installation complete.');
}

/**
 * Gets the path to your npm executable.
 *
 * @return  {string}  The full path to your npm executable
 */
function npmBin() {
  const path = require('path');
  const childProcess = require('child_process');
  const execSync = childProcess.execSync;
  const env = Object.assign({}, process.env);
  env.PATH = path.resolve('/usr/local/bin') + ':' + env.PATH;
  const buffs = [];

  // Get the path to npm bin
  return execSync('which npm', { env: env })
  .toString('utf8')
  .trim();
}
