#!/usr/bin/env php
<?php
/**
 * <bitbar.title>Github CI Status</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Jordan Andree</bitbar.author>
 * <bitbar.author.github>jordanandree</bitbar.author.github>
 * <bitbar.desc>Displays Github Pull Request CI Check statuses</bitbar.desc>
 * <bitbar.dependencies>php</bitbar.dependencies>
 * <bitbar.abouturl>https://github.com/jordanandree/bitbar-github-ci</bitbar.abouturl>
 *
 * Icon sourced from feather icons: https://feathericons.com/
 *
 * To setup, create or edit your ~/.bitbarrc file with a new section:
 *
 * [github_ci]
 * access_token=xxx # Personal Access Token
 * username=jordanandree
 * repos[]=jordanandree/bitbar-github-ci
 * repos[]=jordanandree/dotfiles
 */

class GithubCIStatus
{
    /**
     * Default config values
     *
     * @var array
     */
    protected $default_config = [
        "hostname" => "github.com",
    ];

    /**
     * Config options sourced from ~/.bitbarrc
     *
     * @var stdClass
     */
    protected $config;

    /**
     * Status line template for each check
     *
     * @var string
     */
    protected $status_line = "%s %s | href=%s";

    /**
     * State of CI checks
     *
     * @var string
     */
    protected $state = "success";

    /**
     * Lock param for mutating state
     *
     * @var bool
     */
    protected $state_lock = false;

    /**
     * Menu bar icon
     *
     * @var string
     */
    protected $icon = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAABDtpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIKICAgICAgICAgICAgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiCiAgICAgICAgICAgIHhtbG5zOnRpZmY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vdGlmZi8xLjAvIj4KICAgICAgICAgPHhtcDpDcmVhdG9yVG9vbD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNyAoTWFjaW50b3NoKTwveG1wOkNyZWF0b3JUb29sPgogICAgICAgICA8eG1wTU06RG9jdW1lbnRJRD54bXAuZGlkOjVEMzE5OTBGREQzRTExRTdCNjU1Q0M4MUYwMENBMTNDPC94bXBNTTpEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06RGVyaXZlZEZyb20gcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICA8c3RSZWY6aW5zdGFuY2VJRD5hZG9iZTpkb2NpZDpwaG90b3Nob3A6ODMzYTI0NjgtMjVhOC0xMTdiLTkxNzEtZjU1MDA2YWFhMDcyPC9zdFJlZjppbnN0YW5jZUlEPgogICAgICAgICAgICA8c3RSZWY6ZG9jdW1lbnRJRD5hZG9iZTpkb2NpZDpwaG90b3Nob3A6ODMzYTI0NjgtMjVhOC0xMTdiLTkxNzEtZjU1MDA2YWFhMDcyPC9zdFJlZjpkb2N1bWVudElEPgogICAgICAgICA8L3htcE1NOkRlcml2ZWRGcm9tPgogICAgICAgICA8eG1wTU06SW5zdGFuY2VJRD54bXAuaWlkOjVEMzE5OTBFREQzRTExRTdCNjU1Q0M4MUYwMENBMTNDPC94bXBNTTpJbnN0YW5jZUlEPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4K4XbDnwAAAxFJREFUWAnNl1uITlEUxweTonG/y2UYIsUUUSIP84zwIELNk9LwwpsXmhEvk8sTKZq8eBJJXqQUuV+SUJPmySXjEpM7g9//m73OrDnO/r75Lg9W/WatvfZa6+y9z977fFNVlS0NuD/BH+iAw1AHxUgNwdvhBnSDarVCHxnQp9XbOI+5qreZs1TgOLTBfJgNI2EUfIH38BwegAZ7AMaAlx80xsNH78yyVVAP7AJbCbVL4Td5r1zuSuxEBiZWrzEWc0hoXkRPhF3wIfj6qzTYNpgDO8BkqhnS1b4RbD8oFdEKHIRTsBO05Frmh9AJGthQGA1zYVGwT6BvgWRBj8r9jb32JEQB9graE295Rgvp9vr67C0/W3vEYozBoTHDnGVqbViTpWbE9BU6bLTNsaAi/ZrU91BXJ2FaLL8+BGkAes+DYoEl+HeTYxPbH8v3QY2xoBL9I8jT7DWI+7EaJ0OAgqbHgsrw3w71P1uN9CbUUTIpeFtZYBHa7hId25ykB6BbyyR9jZq/HO0nmKuTHsBLV32Ssytl2u5/bQXTA3hiHegVzq6EqbtgXCj01AqmB3DZOtBrnF0Jc60r4p/j3D3mdZSd1/X/9JbmmEDam1D3F7oWotJAjw1A34TV0cj+deg43wGreaw/aYdcghIvgL7jyfHBzie6QXX96ivqf09ojw2DgqK9cQRs1KZ1k90FzWIyeFHOXrgGXWA5pnW1T4FM2YxXH4t7sNxFrMNuByvitQboZRkN32+2BrMP7EeOz0ns01iWoFlqH5hoZlp+f00rdqMFBK1fUp1gdbQS22A4FJR5RFwFS36HPTOVtdD1a09kySacVqM1KyCfTxvnrCvwDHuWS9jg+rY6vzc1WxvAGd8Rs6tdRzf2FtDS1UMdPIZLoDMsn4leU5bofZtoQiWJrstHYDPJ0o15Klv8uTwxSZc2WFo02yWwB16kOyvdzhqAnvEVmkHnVue9Fpqg4uL3QKy4/quR6FquuMRWIOtBfoN5Ox37Mzi0igWlmJ3aQbUauAlHQacmSxT3DVrgbVbAf+X7C2311IYwO5eYAAAAAElFTkSuQmCC";

    /**
     * Output the Pull Request checks
     *
     * @return string
     */
    public function run()
    {
        $lines = [];

        try {
            $pull_requests = $this->searchPullRequests();

            if (empty($pull_requests)) {
                $this->sendOutput("No Pull Requests. Get to work!");
                exit;
            }

            foreach ($pull_requests as $pr) {
                $repo_name = substr($pr->repository_url, strlen($this->getConfig()->base_uri . "repos/"));

                $pr_info = $this->getPullRequest($repo_name, $pr->number);
                $status  = $this->getCommitStatus($repo_name, $pr_info->head->sha);
                $lines[] = $this->formatLine($status->state, $pr_info->title, $pr_info->html_url);

                if ($status->state !== $this->state && !$this->state_lock) {
                    $this->state = $status->state;
                    $this->state_lock = true;
                }

                foreach ($status->statuses as $check) {
                    $lines[] = "--" . $this->formatLine($check->state, $check->context, $check->target_url);
                }
            }
        } catch (RuntimeException $e) {
            $this->state = "failure";
            $this->sendOutput($e->getMessage());
            exit;
        }

        $this->sendOutput($lines);
    }

    /**
     * echo back output to bitbar
     *
     * @param string[]|string
     *
     * @return void
     */
    public function sendOutput($lines)
    {
        echo $this->statusIcon($this->state) . " | templateImage=" . $this->icon;
        echo "\n---\n";

        if (is_array($lines)) {
            echo implode($lines, "\n");
        } else {
            echo $lines . "\n";
        }
    }

    /**
     * Format a line for output
     *
     * @param mixed ...$text
     *
     * @return string
     */
    public function formatLine(...$args)
    {
        $icon = $this->statusIcon($args[0]);
        $args[0] = $icon;

        return sprintf($this->status_line, ...$args);
    }

    /**
     * Get the memoized configuration struct or set it
     *
     * @return stdClass
     *
     * @throws RuntimeException
     */
    protected function getConfig()
    {
        if (is_null($this->config)) {
            $bitbarrc = getenv('HOME') . "/.bitbarrc";

            if (!file_exists($bitbarrc)) {
                throw new RuntimeException("~/.bitbarrc is missing");
            }

            $config = parse_ini_file($bitbarrc, true);

            if (!array_key_exists("github_ci", $config)) {
                throw new RuntimeException("[github_ci] section is missing in ~/.bitbarrc");
            }

            $config = array_merge($this->default_config, $config["github_ci"]);
            $config["base_uri"] = "https://" . $config["hostname"] . "/api/v3/";
            $this->config = (object) $config;
        }

        return $this->config;
    }

    /**
     * Send a Request to the Github API
     *
     * @param string $endpoint
     * @param array $options
     *
     * @return stdClass
     *
     * @throws RuntimeException
     */
    protected function sendRequest($endpoint, $params = [])
    {
        $url = $this->getConfig()->base_uri . $endpoint;
        $params["access_token"] = $this->getConfig()->access_token;
        $url .= "?" . http_build_query($params);

        $headers = null;
        $body = null;
        $ch = curl_init();
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_HEADER, true);
        curl_setopt($ch, CURLOPT_URL, $url);
        $response = curl_exec($ch);

        if (!empty($response)) {
            list($headers, $body) = explode("\r\n\r\n", $response, 2);
        }

        curl_close($ch);

        if (is_null($body)) {
            throw new RuntimeException("Error making request to the Github API. Check your configuration.");
        } else {
            return json_decode($body);
        }
    }

    /**
     * Get a single Pull Request
     *
     * @param string $repo
     * @param int $id
     *
     * @return stdClass
     */
    protected function getPullRequest($repo, $id)
    {
        return $this->sendRequest("repos/$repo/pulls/$id");
    }

    /**
     * Get the status of a single Commit
     *
     * @param string $repo
     * @param string $sha
     *
     * @return stdClass
     */
    protected function getCommitStatus($repo, $sha)
    {
        return $this->sendRequest("repos/$repo/commits/$sha/status");
    }

    /**
     * Perform search for open issues by the author in the repos
     *
     * @return stdClass
     */
    protected function searchPullRequests()
    {
        $q = "state:open author:" . $this->getConfig()->username;
        foreach ($this->getConfig()->repos as $repo) {
          $q .= " repo:$repo";
        }

        return $this->sendRequest("search/issues", [
            "q" => $q,
        ])->items;
    }

    /**
     * Get the icon for the status
     *
     * @param string $status
     *
     * @return string
     */
    protected function statusIcon($status)
    {
        $map = [
            "success" => "\033[0;32m●\033[0m",
            "failure" => "\033[0;31m●\033[0m",
            "pending" => "\033[0;34m●\033[0m",
            "warning" => "\033[0;33m●\033[0m",
        ];

        return $map[$status];
    }
}

echo (new GithubCIStatus())->run();
