#!/usr/bin/env /usr/local/bin/node
// jshint esversion: 8
// <bitbar.title>RSync Backup Bitbar Plugin</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Gregory S. Read</bitbar.author>
// <bitbar.author.github>readgs</bitbar.author.github>
// <bitbar.desc>Schedule and monitor rsync backups via BitBar</bitbar.desc>
// <bitbar.dependencies>node npm/path npm/untildify npm/yargs npm/bitbar npm/mkdirp npm/jsonc npm/lockfile npm/execa npm/moment</bitbar.dependencies>
// <bitbar.abouturl></bitbar.abouturl>

/**
 * Quick Install
 * -Make sure Node is installed (this also installs npm)
 * -Copy script to your bitbar plugins folder.
 *      NOTE: Be sure to name it something like rsyncbackup.5s.js so that it will refresh
 *      backup status (and trigger backups) often enough to be useful.
 * -Open up a terminal and navigate to plugin folder
 * -Install required npm packages...
 *      NOTE: This will only install the required dependencies into the plugins folder.
 *      Specifically, into a subfolder called node_modules.
 * 
 *      From Terminal: npm install --no-package-lock path untildify yargs bitbar mkdirp jsonc lockfile execa moment
 * -Refresh your plugins
 * -Select "Configure" option to open config file up in your default editor
 * -Make whatever changes necessary for your backup needs.
 *      NOTE: It is recommended that you don't set a schedule for your backup until
 *      you execute a "dry run" to make sure you like what it's backing up.
 * -Select "Dry Run"
 * -If you like the results, select Back Up Now
 * -If you still like the results, update the schedule in the configuration.
 * -Enjoy.
 */


const path = require('path');
const untildify = require('untildify');
const yargs = require('yargs');
const bitbar = require('bitbar');
const fs = require('fs');
const mkdirp = require('mkdirp');
const jsonc = require('jsonc');
const lockFile = require('lockfile');
const execa = require('execa');
const moment = require('moment');

/**
 * Enumeration of valid backup statuses
 */
const BackupStatus = {
    /**
     * No status available on backup (i.e. it hasn't run ever)
     */
    None: 'None',
    /**
     * Last backup was successful
     */
    Succeeded: 'Succeeded',
    /**
     * Last backup failed
     */
    Failed: 'Failed',
    /**
     * Backup currently in progress
     */
    Running: 'Running'
}

/**
 * Values that don't change :)
 */
const constants = {
    /**
     * Main folder where we store backup related stuff (like lock files, logs, etc)
     */
    WORKINGFOLDER: '~/.backup',
    /**
     * Name of lockfile we'll be using
     */
    LOCKFILE: 'backup.lock',
    /**
     * Name of file that will indicate the start of a backup
     */
    STARTFILE: 'start.flag',
    /**
     * Name of file that will indicate an error status
     */
    ERRORFILE: 'error.flag',
    /**
     * Name of file that will indicate a success status
     */
    SUCCESSFILE: 'success.flag',
    /**
     * Config file for user-configurable settings (source, destination, etc.)
     */
    CONFIGFILE: 'config.jsonc',
    /**
     * Name of excludes file for rsync
     */
    EXCLUDESFILE: 'excludes.txt',
    /**
     * Name of file where errors are logged
     */
    ERRORLOGFILE: 'errorlog.txt',
    /**
     * Name of file where rsync output is logged
     */
    LOGFILE: 'log.txt',
    /**
     * Default content for the rsync excludes file
     */
    DEFAULTEXCLUDES: 
`.backup/
.Trash/
.DS_Store`,
    /**
     * Default configuration content to use if no config file exists.
     */
    DEFAULTCONFIG:
`/**
* Configuration settings for backup
*/
{
    /**
     * Path to the rsync program.  The version included with macOS is ancient, so you
     * may want to install a newer version via homebrew.  In this case, you would want
     * to likely change this path to /usr/local/bin/rsync
     */
    "rsyncPath": "/usr/bin/rsync",
    /**
     * How often a backup should be executed.  Can expressed as a number, followed
     * by the time unit (seconds, hours or days).  For example...
     *      10s     Every 10 seconds
     *       5m     Every 5 minutes
     *       1h     Every 1 hour
     *       2d     Every 2 days
     *   manual     Backup is only done on-demand
     */
    "frequency": "1h",
    /**
     * Source to pass along to rsync for syncing data from
     */
    // ***UNCOMMENT LINE BELOW TO SPECIFY A SOURCE*** 
    //"source": "~",
    /**
     * Destination to pass along to rsync for syncing data to
     */
    // ***UNCOMMENT LINE BELOW TO SPECIFY A DESTINATION***
    //"destination": "/tmp/rsyncbackup/",
    /**
     * Additional arguments to pass to rsync.  Default arguments are reasonable
     * for a standard archival copy of the source to the destination (excluding
     * permissions, ACLs, etc.).  See documentation for 'rsync' for more info
     * on arguments.
     * 
     * NOTE: The rsyncbackup.js script already passed along the --exclude-from
     * argument if the 'excludes' configuration is specified above.
     */ 
    "rsyncAdditionalArguments": [
        "--archive",
        "--no-perms",
        "--no-acls",
        "--stats",
        "--delete",
        "--delete-excluded"
    ]
}`
}

/**
 * Variables that we just want accessible from anywhere
 */
const globals = {
    /**
     * Path of main working folder for rsyncbackup script
     */
    workingFolder: untildify(constants.WORKINGFOLDER),
    /**
     * Configuration settings for our script go here.
     */
    configFile: getBackupPath(constants.CONFIGFILE),
    /**
     * Excluded files and folders for rsync will be in this file.
     */
    excludesFile: getBackupPath(constants.EXCLUDESFILE),
    /**
     * Used for ensuring only one instance of the backup is running.
     */
    lockFile: getBackupPath(constants.LOCKFILE),
    /**
     * Created when a backup starts (mostly to figure out when our backup started)
     */
    startFile: getBackupPath(constants.STARTFILE),
    /**
     * If an error occured, this file will exist
     */
    errorFile: getBackupPath(constants.ERRORFILE),
    /**
     * If backup succeeded, this file will exist
     */
    successFile: getBackupPath(constants.SUCCESSFILE),
    /**
     * Logs from rsync go here
     */
    logFile: getBackupPath(constants.LOGFILE),
    /**
     * Any errors that rsync output go here
     */
    errorLogFile: getBackupPath(constants.ERRORLOGFILE),
    /**
     * Arguments retrieved from commandline
     */
    args: {},
    /**
     * Configuration loaded from config.jsonc.  If not loaded or validation error
     * with configuration, then this value will be null.
     */
    configuration: null,
    /**
     * If configuration is null, this value is set to a message indicating why
     * the configuration was not loaded.
     */
    configurationError: '',
    /**
     * Status of the last completed backup.
     */
    backupStatus: BackupStatus.None,
    /**
     * Last date/time (formatted) of the last backup (regardless of outcome).
     * If null, that means we never started a backup before.
     */
    backupDate: null,
    /**
     * Number of milliseconds the backup has been running, or ran (depending on status)
     */
    backupDuration: 0,
    /**
     * Date and time of the next scheduled backup
     */
    nextScheduledBackup: null,
    /**
     * Arguments to pass to the rsync command
     */
    rsyncArgs: []
}

/**
 * Bitbar items that we show as the main item (shows on menubar) for a given bitBar view
 */
const bitbarHeaders = {
    /**
     * Show when backup is running
     */
    backupRunning: {
        text: ':running:'
    },
    /**
     * Show when last backup ended in error
     */
    backupError: {
        text: ':rage:'
    },
    /**
     * Show if last backup ended in success
     */
    backupSuccess: {
        text: ':smile:'
    },
    /**
     * Show if there is no status for the backup (i.e. it hasn't run yet) */    
    backupNoStatus: {
        text: ':expressionless:'
    }
}

const bitbarItems = {
    configurationError: {
        text: 'Error in config file: '
    }
}

const bitbarActions = {
    /**
     * Shows a "Configure..." option and brings up the config file in a text editor when selected.
     */
    configure: {
        text: 'Configure...',
        bash: '/usr/bin/open',
        param1: '-t',
        param2: globals.configFile,
        terminal: false
    },
    /**
     * Shows a "View Logs..." option and brings up the last log in a text when selected.
     */
    viewLog: {
        text: 'View Log...',
        bash: '/usr/bin/open',
        param1: '-t',
        param2: globals.logFile,
        terminal: false
    },
    /**
     * Shows a "View Logs..." option and brings up the last log in a text when selected.
     */
    viewErrorLog: {
        text: 'View Error Log...',
        bash: '/usr/bin/open',
        param1: '-t',
        param2: globals.errorLogFile,
        terminal: false
    },
    /**
     * Shows a "Start backup" option and manually starts backup process when selected.
     */
    startBackup: {
        text: 'Back Up Now',
        //bash: See init()
        param1: '--start',
        terminal: false
    },
    /**
     * Shows a "Stop backup" option and manually stops backup process if running, when selected.
     */
    stopBackup: {
        text: 'Stop Backup',
        //bash: See init(),
        param1: '--stop',
        terminal: false
    },
    /**
     * Starts a backup, but not actually doing the copy.  Will run in a terminal as well
     * so user can observe what will be copied, etc.
     */
    dryRun: {
        text: 'Dry Run...',
        //bash: See init()
        param1: '--start',
        param2: '--dry-run',
        terminal: true
    }
}

/**
 * Do any initalization required for the script at startup
 */
async function init() {
    // Pull in commandline arguments
    globals.args = yargs
        .boolean(['start', 'stop'])
        .default('start', false)
        .default('stop', false)
        .describe('start', 'Starts the backup')
        .describe('stop', 'Stops the backup')
        .describe('dry-run', 'If starting backup, runs it in --dry-run rsync mode with a terminal')
        .argv;

    // Create missing items as needed
    await createFolderIfNeeded(globals.workingFolder);
    createFileIfNeeded(globals.configFile, constants.DEFAULTCONFIG);
    createFileIfNeeded(globals.excludesFile, constants.DEFAULTEXCLUDES);

    // Load in configuration
    loadConfiguration();

    // Initilize the rsync arguments based on configuration
    if(globals.configuration) {
        // Use the source, Luke...
        let source = untildify(globals.configuration.source);
        // If destination includes a colon, we'll treat it as a ssh server path.
        // Otherwise, make sure the tilde gets translated.
        let destination = globals.configuration.destination.includes(':') ?
            globals.configuration.destination
            : untildify(globals.configuration.destination);
        // Merge additional arguments with our default arguments
        globals.rsyncArgs = [
            ...globals.configuration.rsyncAdditionalArguments,
            `--exclude-from=${globals.excludesFile}`,
            source,
            destination
        ];
        // Enable dry-run mode if requested
        if(globals.args.dryRun) {
            globals.rsyncArgs.push('--dry-run');
        }
    }

    // Setup any bitbar items with additional info after init
    bitbarActions.startBackup.bash = __filename;
    bitbarActions.stopBackup.bash = __filename;
    bitbarActions.dryRun.bash = __filename;
    bitbarItems.configurationError.text += globals.configurationError;

    // Get and store our lastest backup status
    getBackupStatus();

    // Figure out when our next backup should occur, if applicable
    getNextScheduledBackup();
}

/**
 * Starts the backup process
 */
async function startBackup() {
    // Ensure we can get a lock on the lockfile before we proceed
    lockFile.lockSync(globals.lockFile, {});
    // Indicate that we've started a backup
    touch(globals.startFile);
    // Remove the error and success flags since we are just starting
    untouch(globals.errorFile);
    untouch(globals.successFile);
    // Run rsync
    let exitCode = await executeRsync();
    if(exitCode != 0) {
        // Failed!
        touch(globals.errorFile);
    }
    else {
        // We succeeded!!
        touch(globals.successFile);
    }
    // We're all done, unlock the lock file
    lockFile.unlockSync(globals.lockFile);
}

/**
 * Stops the backup process, if it's running
 */
function stopBackup() {
    console.log('STOP BACKUP!');
}

/**
 * Returns BitBar-friendly status of the backup.  This is the default when no
 * arguments are passed to the script.
 */
function defaultOutput() {
    const formattedFileDate = !globals.backupDate ? 'never' : formatDate(globals.backupDate);
    const formattedNextBackupDate = !globals.nextScheduledBackup ? 'never' : formatDate(globals.nextScheduledBackup);
    const backupDurationInMinutes = (globals.backupDuration / 60000).toFixed(2);

    // If lockfile exists, we're running
    if(globals.backupStatus == BackupStatus.Running) {
        bitbar([
            bitbarHeaders.backupRunning,
            bitbar.separator,
            { text: `Backup running...` },
            { text: `Started at ${formattedFileDate}` },
            { text: `Running for ${backupDurationInMinutes}` },
            bitbar.separator,
            bitbarActions.stopBackup
        ]);
    }
    // If error file exists, something is wrong
    else if(globals.backupStatus == BackupStatus.Failed) {
        bitbar([
            bitbarHeaders.backupError,
            bitbar.separator,
            { text: `Backup failed!` },
            { text: `Started at ${formattedFileDate}` },
            { text: `Ran for ${backupDurationInMinutes} minutes` },
            { text: `Next backup at ${formattedNextBackupDate}` },
            bitbar.separator,
            bitbarActions.configure,
            bitbarActions.viewLog,
            bitbarActions.viewErrorLog,
        ]);
    }
    // If success file exists, the last backup succeeded
    else if (globals.backupStatus == BackupStatus.Succeeded) {
        bitbar([
            bitbarHeaders.backupSuccess,
            bitbar.separator,
            { text: `Backup succeeded!` },
            { text: `Started at ${formattedFileDate}` },
            { text: `Ran for ${backupDurationInMinutes} minutes` },
            { text: `Next backup at ${formattedNextBackupDate}` },
            bitbar.separator,
            bitbarActions.configure,
            bitbarActions.viewLog
        ]);
    }
    // Otherwise, we don't have any status (i.e. backup has never been run)
    else {
        bitbar([
            bitbarHeaders.backupNoStatus,
            bitbar.separator,
            { text: `Next backup at ${formattedNextBackupDate} minutes` },
            bitbar.separator,
            bitbarActions.configure
        ]);        
    }

    // Always show run/configuration error if we're in any status other that running
    if (globals.backupStatus != BackupStatus.Running) {
        if(!globals.configuration) {
            bitbar([bitbarItems.configurationError]);
        }
        else {
            bitbar([bitbarActions.dryRun]);
            bitbar([bitbarActions.startBackup]);
        }
    }
}

/**
 * Run the actual rsync program with the configured arguments
 */
async function executeRsync() {
    const subProcess = execa(globals.configuration.rsyncPath, globals.rsyncArgs, {});

    // If not dry run, send output to log files.  Otherwise output will just
    // go to terminal.
    if(!globals.args.dryRun) {
        // Create our error and output logs
        const errStream = fs.createWriteStream(globals.errorLogFile);
        const outStream = fs.createWriteStream(globals.logFile);
        // We don't want any output from rsync (spit out to our respective files)
        subProcess.stderr.pipe(errStream);
        subProcess.stdout.pipe(outStream);
    } else {
        // We're interactive dry run, send output to parent process
        subProcess.stderr.pipe(process.stderr);
        subProcess.stdout.pipe(process.stdout);
    }

    return (await subProcess).exitCode;
}

/**
 * Create the folder if it doesn't exist
 * @param {*} folderPath 
 */
async function createFolderIfNeeded(folderPath) {
    if(!fs.existsSync(folderPath)) {
        await mkdirp(folderPath);
    }
}

/**
 * Creates the specified file with default content, if it doesn't exist.
 * @param {string} filePath 
 * @param {string} defaultContent 
 */
function createFileIfNeeded(filePath, defaultContent) {
    if(!fs.existsSync(filePath)) {
        fs.writeFileSync(filePath, defaultContent);
    }
}

/**
 * Sets the date/time of when the next backup should take place
 * based on the configuration and status.
 */
function getNextScheduledBackup() {
    let now = new Date();
    let frequencyInMinutes = getFrequencyInMinutes(globals.configuration.frequency);

    // If we haven't done a backup before, schedule it for now!
    if(!globals.backupDate) {
        globals.nextScheduledBackup = now;
    }
    // If there's no frequency, assume it's manual
    else if(!frequencyInMinutes) {
        globals.nextScheduledBackup = null;
    }
    else {
        // Get the date/time for the next scheduled backup
        globals.nextScheduledBackup = moment(globals.backupDate).add(frequencyInMinutes, 'minutes').toDate();
        // If next scheduled backup is in the past, set it to happen now
        if(globals.nextScheduledBackup.getTime() < now.getTime()) {
            globals.nextScheduledBackup = now;
        }
    }
}



/**
 * Starts an instance of the backup if we are scheduled to do so
 * @returns {boolean} True if backup started, otherwise false.
 */
function startBackupIfScheduled() {
    let now = new Date();
    // If the time has come!
    if(globals.nextScheduledBackup && globals.nextScheduledBackup.getTime() <= now) {
        // Execute ourselves with the "--start" parameter so we start the backup
        // but don't wait for the process to finish.
        const subProcess = execa(__filename, ['--start'], {
            detached: true,
            cleanup: false
        });

        subProcess.unref();
        return true;
    }

    return false;
}

/**
 * Retrieve and store information related to the last successful, failed or in-progress backup
 */
function getBackupStatus() {
    let now = new Date();
    // Only get modify date for startFile if the file exists
    if(fs.existsSync(globals.startFile)) {
        globals.backupDate = getFileDate(globals.startFile);
    }
    // If no start file exists, then should assume we never did a backup
    else {
        return;
    }

    // If a lockfile exists, then our backup is currently running
    if(fs.existsSync(globals.lockFile)) { 
        // Duration is start of backup to now
        globals.backupStatus = BackupStatus.Running;
        globals.backupDuration = moment(now).diff(globals.backupDate);
    }
    // Else if error file exists, our last backup failed
    else if(fs.existsSync(globals.errorFile)) {
        // Duration is start of backup to date/time of error file
        let errorFileDate = getFileDate(globals.errorFile);
        globals.backupStatus = BackupStatus.Failed;
        globals.backupDuration = moment(errorFileDate).diff(globals.backupDate);
    }
    // Else if success file exists, our last backup succeeded
    else if(fs.existsSync(globals.successFile)) {
        // Duration is start of backup to date/time of error file
        let successFileDate = getFileDate(globals.successFile);
        globals.backupStatus = BackupStatus.Succeeded;
        globals.backupDuration = moment(successFileDate).diff(globals.backupDate);
    }

    // Else, we're in an unknown state (don't set any status)
}

/**
 * Loads and verifies 
 */
function loadConfiguration() {
    let content = fs.readFileSync(globals.configFile).toString();
    let configuration = {};
    try {
        configuration = jsonc.parse(content);
    } catch(e) {
        globals.configurationError = 'Error parsing configuration file';
        return;
    }

    // By default, no error
    let error = null;
    
    // If rsync path is bad
    if(!configuration.rsyncPath || !fs.existsSync(configuration.rsyncPath)) {
        error = 'rsyncPath is invalid or file does not exist';
    }
    // If frequency isn't specified, or it's not a valid frequency
    else if(!configuration.frequency || !(configuration.frequency === 'manual' || getFrequencyInMinutes(configuration.frequency))) {
        error = 'frequency is invalid';
    }
    // Must have a valid source 
    else if(!configuration.source) {
        error = 'source must be set';
    }
    // Must have a valid destination
    else if(!configuration.destination) {
        error = 'destination must be set';
    }
    // Check whether our rsync arguments is a legit string array
    else if(!configuration.rsyncAdditionalArguments ||
        !Array.isArray(configuration.rsyncAdditionalArguments) ||
        !configuration.rsyncAdditionalArguments.reduce((prev, curr) => typeof prev === 'string' || typeof curr === 'string')) {
        error = 'rsyncAdditionalArguments is invalid'
    }

    if(error) {
        // Just save the error that we got
        globals.configurationError = error;
    }
    else {
        // Config is good, let's save it as our actual config
        globals.configuration = configuration;
    }
}

/**
 * Parses a frequency string into number of minutes
 * @param {string} frequency 
 */
function getFrequencyInMinutes(frequency) {
    let values = frequency.match(/^\s*(\d*)\s*([sSmMhHdD])\s*$/);
    // If frequency was in a valid format
    if(values) {
        let unit = values[2].toLowerCase();
        let value = values[1];
        switch(unit) {
            case 's':
                return value / 60;
            case 'm':
                return value;
            case 'h':
                return value * 60;
            case 'd':
                return value * 1440;
        }
    }

    return null;
}

/**
 * Gets the last time the specified file was modified.
 * @param {string} filePath 
 */
function getFileDate(filePath) {
    const { mtime } = fs.statSync(filePath);
    return mtime;
}

/**
 * Formats specified to display as "M/D/YY h:mm AM/PM"
 * @param {Date} fileDate 
 */
function formatDate(fileDate) {
    return moment(fileDate).format('M/D/YY h:mm A');
}

/**
 * Returns a full path and file using the configured backup path ()
 * @param {string} fileName 
 */
function getBackupPath(fileName) {
    return untildify(path.join(constants.WORKINGFOLDER, fileName));
}

/**
 * Creates an empty file and/or updates the modified date.
 * @param {string} filePath 
 */
function touch(filePath) {
    fs.closeSync(fs.openSync(filePath, 'w'));
}

/**
 * Removes the file if it exists.
 * @param {string} filePath 
 */
function untouch(filePath) {
    if(fs.existsSync(filePath)) {
        fs.unlinkSync(filePath);
    }
}

// START HERE
(async () => {
    await init();

    if(globals.args.start) {
        await startBackup();
    }
    else if(globals.args.stop) {
        stopBackup();
    }
    else {
        let isBackupStarted = startBackupIfScheduled();
        defaultOutput();
        if(isBackupStarted) {
            // If we started a backup, we want to kill our process.  Otherwise
            // it will wait for the other backup process that we started, which
            // we don't want (so status gets reported back right away).
            process.exit(0);
        }
    }
})();