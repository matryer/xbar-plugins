#!/usr/bin/env php
<?php
// <bitbar.title>Simple RSS reader</bitbar.title>
// <bitbar.version>1.0</bitbar.version>
// <bitbar.author>Aleš Farčnik</bitbar.author>
// <bitbar.author.github>alesf</bitbar.author.github>
// <bitbar.desc>Simple RSS "reader" that will list latest feed items and link to them.</bitbar.desc>
// <bitbar.image>http://i.imgur.com/JDdgcGH.png</bitbar.image>
// <bitbar.dependencies>php >= 5.4</bitbar.dependencies>

define('FEED_URL', 'http://feedpress.me/sixcolors?type=xml');
define('ITEM_LIMIT', 25);

// You should not need to change anything below this part

// TODO:
// - multiple feeds
// - keep older items in log file for some time

define('FEED_LOG', dirname(__FILE__).'/.rss.log');
define('SCRIPT_PATH', __FILE__);
define('PHP_PATH', exec("which php"));

$feed_image = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRT".
    "b2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAATBJREFUeNqc0b1KQzEYxvFQq4i2kwiCD".
    "jo4dZIKQm+hm5tUUHDxBnQSF1cXh1IR3AqdBK+gLl5BURz82hy0Iq2lYkHqP/BGHsKxFgM/Tv".
    "vk5D3Jm7Rzro8OHtHABc7x6oYc/QSfqCH33wLBF8rIDiowj2Ws4wQvCYVuh92NH6NW7D4q8oa".
    "VpAVH2EMRk5KP4zChSG5QD/xtVDAr82voRcfJ/tXEd5SiIjpfDhMjmEETc0hbPoZVu85LXNlX".
    "Czafx5k1/GdMYAet6Gsl6Yk2tvbbLSziLjpO6MmG5L4vU7rIbysl/9vy8rFccVPyLR/uS1CXP".
    "uxGt5Ox/FTyqrMt6plDo/yCD8mLlm9K1kjZVnW07NmxGwhjyZ43ki34Att4QhcHuJYXHuT3tD".
    "2fJct8CzAAqHZ3QQFiFvsAAAAASUVORK5CYII=";

if (isset($argv[1])) {
    $log_data = @file_get_contents(FEED_LOG);
    $log_data = json_decode($log_data, true);

    if ($argv[1] == 'MARK_ALL_AS_READ') {
        foreach ($log_data as &$log_item) {
            $log_item['visited'] = true;
        }
        file_put_contents(FEED_LOG, json_encode($log_data));
        exit();
    }
    exec('open ' . $log_data[$argv[1]]['link']);
    $log_data[$argv[1]]['visited'] = true;
    file_put_contents(FEED_LOG, json_encode($log_data));
    exit();
}

class RSSParser
{
    protected $url;
    protected $feed;
    protected $feed_type;
    protected $feed_channel;
    protected $feed_items;
    protected $feed_guid;

    protected $log_data;

    public $title;
    public $items;
    public $unread_no = 0;

    public function __construct($url)
    {
        $this->load($url);
    }

    private function loadLogData()
    {
        $log_data = @file_get_contents(FEED_LOG);
        if ($log_data) {
            $this->log_data = json_decode($log_data, true);
        }
    }

    public function load($url)
    {
        $this->loadLogData();

        $feed_data = @file_get_contents(FEED_URL);
        $this->feed = new DOMDocument();
        $this->feed->loadXML($feed_data);

        if ($this->feed->getElementsByTagName('channel')->item(0)) {
            $this->feed_type = 'rss';
            $this->feed_channel = $this->feed->getElementsByTagName('channel')->item(0);
            $this->feed_items = $this->feed->getElementsByTagName('channel')
                ->item(0)->getElementsByTagName('item');
            $this->feed_guid = 'guid';
        } else {
            $this->feed_type = 'atom';
            $this->feed_channel = $this->feed;
            $this->feed_items = $this->feed->getElementsByTagName('entry');
            $this->feed_guid = 'id';
        }

        $this->setTitle();
        $this->setItems();
    }

    public function setTitle()
    {
        $this->title = $this->getItemElement($this->feed_channel, 'title');
    }

    public function setItems()
    {
        $item_count = 1;
        foreach ($this->feed_items as $item) {
            if ($item_count > ITEM_LIMIT) {
                break;
            }
            $title = $this->getItemElement($item, 'title');
            $guid = $this->getItemElement($item, $this->feed_guid);
            $link = $this->getItemElement($item, 'link');

            $this->items[$guid] = [
                'title' => $title,
                'link' => $link,
                'guid' => $guid
            ];
            if (!isset($this->log_data[$guid])) {
                $this->items[$guid]['visited'] = false;
                $this->unread_no++;
            } else {
                $this->items[$guid] = $this->log_data[$guid];
                if (!$this->items[$guid]['visited']) {
                    $this->unread_no++;
                }
            }
            $item_count++;
        }

        if (!empty($this->items)) {
            file_put_contents(FEED_LOG, json_encode($this->items));
        }
    }

    public function getUnreadCnt()
    {
        return $this->unread_no ? $this->unread_no : '';
    }

    private function getItemElement($item, $element)
    {
        if (!$item) {
            return '';
        }
        if ($this->feed_type == 'rss') {
            return trim($item->getElementsByTagName($element)->item(0)->firstChild->nodeValue);
        }

        if ($this->feed_type == 'atom') {
            if ($element == 'link') {
                return trim($item->getElementsByTagName($element)->item(0)->getAttribute('href'));
            }
            return trim($item->getElementsByTagName($element)->item(0)->nodeValue);
        }
    }
}

/**
 * BitBar display
 */
class BitBar
{
    public function divider()
    {
        echo "---\n";
    }

    public function icon($icon, $text = '')
    {
        if ($text) {
            echo "{$text} ";
        }
        echo "| size=10 templateImage={$icon}\n";
    }

    public function println($text)
    {
        echo "{$text}";
        echo "\n";
    }

    public function rssItem($title, $guid, $unread = false)
    {
        if ($unread) {
            echo "● ";
        }
        echo "$title | length=65 terminal=false refresh=true ";
        echo "bash=".PHP_PATH." param1=".SCRIPT_PATH." param2=$guid\n";
    }

    public function rssItems($items)
    {
        if (empty($items)) {
            return false;
        }
        foreach ($items as $item) {
            $this->rssItem($item['title'], $item['guid'], !$item['visited']);
        };
    }

    public function markAsRead()
    {
        echo "Mark all as read | terminal=false refresh=true ";
        echo "bash=".PHP_PATH." param1=".SCRIPT_PATH." param2=MARK_ALL_AS_READ\n";
    }

    public function refresh()
    {
        echo "Refresh | refresh=true";
    }
}

$parser = new RSSParser(FEED_URL);
$bb = new BitBar();

$bb->icon($feed_image, $parser->getUnreadCnt());
$bb->divider();
$bb->println($parser->title);
$bb->divider();
$bb->rssItems($parser->items);
$bb->divider();
$bb->markAsRead();
$bb->refresh();
