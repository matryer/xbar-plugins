#!/usr/bin/env php
<?php
/**
 * Provides travel distance and time for your favorite destination, with traffic conditions.
 *
 * A Google Developer Account is required with access to "Google Maps Distance Matrix API" and "Google Maps Geolocation API"
 *
 * How does it work:
 * - perform a Wifi Access Point scan using airport utility command
 * - Get current position (lat,lng) with Geolocation API using collected AP data
 * - Get distance, time and traffic delay using the Distance Matrix API
 *
 * @link https://console.developers.google.com/apis/enabled
 * @link https://developers.google.com/maps/documentation/geolocation/
 * @link https://developers.google.com/maps/documentation/distance-matrix/
 *
 * <bitbar.title>Travel time</bitbar.title>
 * <bitbar.version>1.1</bitbar.version>
 * <bitbar.author>Yann Milin</bitbar.author>
 * <bitbar.author.github>ymilin</bitbar.author.github>
 * <bitbar.desc>Provides travel distance and time to your favorite destination, with traffic conditions. A Google Developer Account is required with access to "Google Maps Distance Matrix API" and "Google Maps Geolocation API"</bitbar.desc>
 * <bitbar.image>http://i.imgur.com/Ui6I4YH.png</bitbar.image>
 * <bitbar.dependencies>php >= 5.4.0</bitbar.dependencies>
 */

namespace BitbarPlugins\Travel;

// Required : Your Google Developer Project's API Key
const API_KEY = "YOUR_API_KEY";
const DESTINATION = "Tour Eiffel";

/*
 * Use airport to perform a scan of nearby wifi access points, provides better geolocation accuracy but makes script
 * slower by a few seconds.
 * false: fallback to current IP, resulting in less accurate geolocation
 */
const SCAN_NEARBY_WIFI_ACCESS_POINTS = true;
const LANGUAGE = "en"; // list of supported languages https://developers.google.com/maps/faq#languagesupport
const UNITS = "metric"; // metric, imperial

const AIRPORT_PATH = "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport";
const DEBUG = false; // optimize output for console instead of bitbar

/**
 * Class TravelTimePlugin
 *
 * Renders plugin output
 *
 * @package BitbarPlugins\Travel
 */
class TravelTimePlugin
{
    const GOOGLE_MAP_URL                     = "https://www.google.com/maps";
    const GOOGLE_MAP_URL_SOURCE_ADDRESS      = 'saddr';
    const GOOGLE_MAP_URL_DESTINATION_ADDRESS = 'daddr';

    const ICON_PIN     = "📍";
    const ICON_FLAG    = "🏁";
    const ICON_CIRCLE  = "⭕️";
    const ICON_WARNING = "⚠️";
    const COLOR_BLACK  = "#000000";
    const COLOR_ORANGE = "#FF9900";
    const COLOR_RED    = "#FF0000";

    const ONE_MINUTES_IN_SECONDS     = 60;
    const FIVE_MINUTES_IN_SECONDS    = 300;
    const FIFTEEN_MINUTES_IN_SECONDS = 900;

    private $durationInTraffic = [];
    private $duration = [];
    private $distance = [];
    private $delay = null;
    private $originAddress = null;
    private $destinationAddress = null;
    private $latitude = null;
    private $longitude = null;
    private $accuracy = null;
    private $googleMapsLink = null;
    private $accessPointCount = 0;
    private $errors = [];
    private $warnings = [];

    private $distanceMatrixErrorMessages = [
        DistanceMatrixResponse::STATUS_CODE_INVALID_REQUEST => "Invalid Request.",
        DistanceMatrixResponse::STATUS_CODE_MAX_ELEMENTS_EXCEEDED => "Origins and destinations per-query limit exceeded.",
        DistanceMatrixResponse::STATUS_CODE_OVER_QUERY_LIMIT => "Too many requests.",
        DistanceMatrixResponse::STATUS_CODE_REQUEST_DENIED => "Service denied",
        DistanceMatrixResponse::STATUS_CODE_UNKNOWN_ERROR => "Server error, try again.",
    ];

    private $distanceMatrixElementErrorMessages = [
        DistanceMatrixResponseElement::STATUS_CODE_NOT_FOUND => "Destination could not be geocoded.",
        DistanceMatrixResponseElement::STATUS_CODE_ZERO_RESULTS => "No route could be found.",
    ];

    public function __construct()
    {
        // Scan and create WifiAccessPoints
        $wifiAccessPoints = [];
        if (SCAN_NEARBY_WIFI_ACCESS_POINTS === true) {
            try {
                $wifiAccessPoints = WifiAccessPoints::fromAccessPointScannerResponse(AccessPointScanner::scan());
                $this->accessPointCount = count($wifiAccessPoints);
            } catch (AccessPointScannerException $apse) {
                $this->warnings[] = $apse->getMessage();
            }

            if (count($wifiAccessPoints) < 2) {
                $this->warnings[] = "Less than two Wifi Access Points in proximity.";
            }
        }

        // Geolocation
        try {
            $geolocationApi = new GeolocationAPI();
            $geolocationResponse = $geolocationApi->call(GeolocationRequest::fromArrayDefinition([
                GeolocationRequest::DEFINITION_WIFI_ACCESS_POINTS => $wifiAccessPoints
            ]));

            $this->latitude = $geolocationResponse->getLatitude();
            $this->longitude = $geolocationResponse->getLongitude();
            $this->accuracy = $geolocationResponse->getAccuracy();
        } catch (GeolocationResponseException $gre) {
            $this->errors[] = "Geolocation API Error status {$gre->getStatusCode()}: {$gre->getMessage()}";
        }

        if ($this->latitude && $this->longitude) {
            // distanceMatrix
            try {
                $distanceMatrixAPI = new DistanceMatrixAPI();
                $distanceMatrixResponse = $distanceMatrixAPI->call(DistanceMatrixRequest::fromArrayDefinition([
                    DistanceMatrixRequest::DEFINITION_ORIGINS => $this->latitude . ',' . $this->longitude,
                    DistanceMatrixRequest::DEFINITION_DESTINATIONS => DESTINATION,
                    DistanceMatrixRequest::DEFINITION_KEY => API_KEY,
                    DistanceMatrixRequest::DEFINITION_LANGUAGE => LANGUAGE,
                    DistanceMatrixRequest::DEFINITION_UNITS => UNITS,
                ]));

                $this->durationInTraffic = $distanceMatrixResponse->getRows()->getDurationInTraffic();
                $this->duration = $distanceMatrixResponse->getRows()->getDuration();
                $this->distance = $distanceMatrixResponse->getRows()->getDistance();
                $this->originAddress = $distanceMatrixResponse->getOriginAddresses();
                $this->destinationAddress = $distanceMatrixResponse->getDestinationAddresses();

                $this->computeDelay();
                $this->computeGoogleMapsLink();
            } catch (DistanceMatrixResponseException $e) {
                $this->errors[] = "Distance Matrix API Error: " . $this->distanceMatrixErrorMessages[$e->getMessage()];
            } catch (DistanceMatrixResponseElementException $e) {
                $this->errors[] = "Distance Matrix API Error: " . $this->distanceMatrixElementErrorMessages[$e->getMessage()];
            }
        }

    }

    private function computeDelay()
    {
        if (is_array($this->duration)
            && is_array($this->durationInTraffic)
            && array_key_exists('value', $this->duration)
            && array_key_exists('value', $this->durationInTraffic)
        ) {
            $delay = $this->durationInTraffic['value'] - $this->duration['value'];
            $this->delay = $delay > 0 ? $delay : null;
        } else {
            $this->delay = null;
        }
    }

    private function computeGoogleMapsLink()
    {
        if (isset($this->originAddress)
            && is_string($this->originAddress)
            && isset($this->destinationAddress)
            && is_string($this->destinationAddress)
        ) {
            $this->googleMapsLink = self::GOOGLE_MAP_URL . "?" . http_build_query([
                    self::GOOGLE_MAP_URL_SOURCE_ADDRESS => $this->originAddress,
                    self::GOOGLE_MAP_URL_DESTINATION_ADDRESS => $this->destinationAddress,
                ]);
        } else {
            $this->googleMapsLink = null;
        }
    }

    /**
     * @return string
     */
    public function __toString()
    {
        return $this->render();
    }

    private function render()
    {
        return DEBUG ? $this->renderConsole() : $this->renderBitbar();
    }

    /**
     * render with Bitbar metadata
     * @return string
     */
    private function renderBitbar()
    {
        $return = "";

        if ($this->errors) {
            $return .= self::ICON_WARNING . " Error|color=" . self::COLOR_RED . "\n";
            $return .= "---\n";
            $return .= implode("|color=" . self::COLOR_RED . "\n", $this->errors) . "\n";
            $return .= "---\n";
            $return .= "Refresh | refresh=true \n";
            return $return;
        }

        $durationColor = self::COLOR_BLACK;
        if ($this->delay > self::FIVE_MINUTES_IN_SECONDS) {
            $durationColor = self::COLOR_ORANGE;
        }
        if ($this->delay > self::FIFTEEN_MINUTES_IN_SECONDS) {
            $durationColor = self::COLOR_RED;
        }

        $return .= self::ICON_PIN . " {$this->durationInTraffic['text']}|color=$durationColor\n";
        $return .= "---\n";

        if ($this->warnings) {
            foreach ($this->warnings as $warning) {
                $return .= self::ICON_WARNING . " $warning\n";
            }
            $return .= "---\n";
        }

        $return .= "{$this->durationInTraffic['text']} ({$this->distance['text']})|color=$durationColor\n";
        if ($this->delay > self::ONE_MINUTES_IN_SECONDS) {
            $return .= "{$this->duration['text']} without traffic\n";
        }
        $return .= "---\n";

        $return .= "Directions\n";
        $return .= self::ICON_CIRCLE . " {$this->originAddress} | color=" . self::COLOR_BLACK . "\n";
        $return .= self::ICON_FLAG . " {$this->destinationAddress} | color=" . self::COLOR_BLACK . "\n";
        $return .= "---\n";

        $return .= "Geolocation\n";
        $return .= "Latitude: {$this->latitude} | color=" . self::COLOR_BLACK . "\n";
        $return .= "Longitude: {$this->longitude} | color=" . self::COLOR_BLACK . "\n";
        $return .= "Accuracy: {$this->accuracy}m | color=" . self::COLOR_BLACK . "\n";
        $return .= "---\n";

        if ($this->googleMapsLink) {
            $return .= "View on Google maps|href={$this->googleMapsLink}\n";
        }
        $return .= "Refresh | refresh=true \n";

        return $return;
    }

    /**
     * render for console output
     * @return string
     */
    private function renderConsole()
    {
        $return = "";
        if ($this->errors) {
            return implode("\n", $this->errors);
        }

        if ($this->warnings) {
            $return .= "---\nWarning(s):\n";
            $return .= implode("\n", $this->warnings);
            $return .= "\n---\n\n";
        }

        $return .= "Duration: {$this->durationInTraffic['text']}\n";
        $return .= "Distance: {$this->distance['text']}\n";
        $return .= $this->delay ? "Traffic Delay: {$this->delay}s" : "No Traffic Delay";
        $return .= "\n\n";

        $return .= "Directions:\n";
        $return .= "\tFrom: {$this->originAddress}\n";
        $return .= "\tTo: {$this->destinationAddress}\n\n";

        $return .= "Geolocation:\n";
        $return .= "\tLatitude: {$this->latitude}\n";
        $return .= "\tLongitude: {$this->longitude}\n";
        $return .= "\tAccuracy: {$this->accuracy}\n\n";

        if ($this->googleMapsLink) {
            $return .= "Google Maps URL: {$this->googleMapsLink}\n\n";
        };

        return $return;
    }

}

/**
 * Class AccessPointScanner
 * @package BitbarPlugins\Travel
 */
final class AccessPointScanner
{
    /**
     * use `airport` utility command with --scan option : Perform a wireless broadcast scan.
     *
     * @return string raw command output
     * @throws AccessPointScannerException
     */
    public static function scan()
    {
        if (!self::airportProgramFound()) {
            throw new AccessPointScannerException("Airport utility command not found. Please check path.");
        }

        return shell_exec(AIRPORT_PATH . " -s");
    }

    private static function airportProgramFound()
    {
        if (!(is_file(AIRPORT_PATH)
            && file_exists(AIRPORT_PATH)
            && is_executable(AIRPORT_PATH))
        ) {
            return false;
        }

        return true;
    }
}

final class AccessPointScannerException extends \Exception
{

}

final class WifiAccessPoints implements \Countable, \JsonSerializable
{

    const PATTERN_AIRPORT_LINE_SCAN = '/^\s*(.+?)\s((?:[0-9A-Fa-f]{2}[:-]){5}(?:[0-9A-Fa-f]{2}))\s(.+?)\s+(\d+).+$/';

    /**
     * @var WifiAccessPoint[]
     */
    private $accessPoints = [];

    /**
     * {@inheritDoc}
     */
    public function count()
    {
        return count($this->accessPoints);
    }

    /**
     * {@inheritDoc}
     */
    function jsonSerialize()
    {
        return $this->accessPoints;
    }

    /**
     * @param string $response
     *
     * @return self
     */
    public static function fromAccessPointScannerResponse($response)
    {
        $instance = new self();

        foreach (explode("\n", $response) as $airportAccessPoint) {
            if (preg_match(self::PATTERN_AIRPORT_LINE_SCAN, $airportAccessPoint, $matches)) {
                $instance->accessPoints[] = WifiAccessPoint::fromArrayDefinition([
                    WifiAccessPoint::DEFINITION_MAC_ADDRESS => $matches[2],
                    WifiAccessPoint::DEFINITION_SIGNAL_TO_NOISE_RATION => intval($matches[3]),
                    WifiAccessPoint::DEFINITION_CHANNEL => intval($matches[4]),
                ]);
            }
        }

        return $instance;
    }

}

final class WifiAccessPoint implements \JsonSerializable
{
    const DEFINITION_MAC_ADDRESS = "macAddress";
    const DEFINITION_SIGNAL_STRENGTH = "signalStrength";
    const DEFINITION_AGE = "age";
    const DEFINITION_CHANNEL = "channel";
    const DEFINITION_SIGNAL_TO_NOISE_RATION = "signalToNoiseRatio";

    const PATTERN_MAC_ADDRESS = '/^(?:[0-9A-Fa-f]{2}[:-]){5}(?:[0-9A-Fa-f]{2})$/';

    /**
     * @var string
     */
    private $macAddress = null;

    /**
     * @var int
     */
    private $signalStrength = null;

    /**
     * @var int
     */
    private $age = null;

    /**
     * @var int
     */
    private $channel = null;

    /**
     * @var int
     */
    private $signalToNoiseRatio = null;

    /**
     * {@inheritDoc}
     */
    function jsonSerialize()
    {
        $return = [self::DEFINITION_MAC_ADDRESS => $this->macAddress];

        if ($this->signalStrength !== null) {
            $return[self::DEFINITION_SIGNAL_STRENGTH] = $this->signalStrength;
        }

        if ($this->age !== null) {
            $return[self::DEFINITION_AGE] = $this->age;
        }

        if ($this->channel !== null) {
            $return[self::DEFINITION_CHANNEL] = $this->channel;
        }

        if ($this->signalToNoiseRatio !== null) {
            $return[self::DEFINITION_SIGNAL_TO_NOISE_RATION] = $this->signalToNoiseRatio;
        }

        return $return;
    }

    /**
     * @param array $definition
     * @return WifiAccessPoint
     */
    public static function fromArrayDefinition(array $definition)
    {
        $instance = new self();

        $instance->macAddress = self::getMacAddressFromDefinition($definition);
        $instance->signalStrength = self::getSignalStrengthFromDefinition($definition);
        $instance->age = self::getAgeFromDefinition($definition);
        $instance->channel = self::getChannelFromDefinition($definition);
        $instance->signalToNoiseRatio = self::getSignalToNoiseRatioFromDefinition($definition);

        return $instance;
    }

    private static function getMacAddressFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_MAC_ADDRESS])
            && is_string($definition[self::DEFINITION_MAC_ADDRESS])
            && preg_match(self::PATTERN_MAC_ADDRESS, $definition[self::DEFINITION_MAC_ADDRESS]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_MAC_ADDRESS];
    }

    private static function getSignalStrengthFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_SIGNAL_STRENGTH])
            && is_int($definition[self::DEFINITION_SIGNAL_STRENGTH]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_SIGNAL_STRENGTH];
    }

    private static function getAgeFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_AGE])
            && is_int($definition[self::DEFINITION_AGE]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_AGE];
    }

    private static function getChannelFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_CHANNEL])
            && is_int($definition[self::DEFINITION_CHANNEL]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_CHANNEL];
    }

    private static function getSignalToNoiseRatioFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_SIGNAL_TO_NOISE_RATION])
            && is_int($definition[self::DEFINITION_SIGNAL_TO_NOISE_RATION]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_SIGNAL_TO_NOISE_RATION];
    }


}

/**
 * Class GeolocationAPI
 *
 * @link https://developers.google.com/maps/documentation/geolocation/
 * @package BitbarPlugins\Travel
 */
final class GeolocationAPI
{
    const GEOLOCATION_URL = "https://www.googleapis.com/geolocation/v1/geolocate";
    const METHOD = "POST";

    /**
     * @var resource curl handler
     */
    private $ch;

    private $headers = [
        "content-type: application/json",
        "Accept: application/json",
        "Cache-Control: no-cache",
        "Pragma: no-cache",
    ];

    public function __construct()
    {
        $this->initCurl();
    }

    private function initCurl()
    {
        $this->ch = curl_init();

        curl_setopt($this->ch, CURLOPT_RETURNTRANSFER, 1);
        curl_setopt($this->ch, CURLOPT_HTTPHEADER, $this->headers);
        curl_setopt($this->ch, CURLOPT_CUSTOMREQUEST, self::METHOD);
        curl_setopt($this->ch, CURLOPT_POSTFIELDS, $this->headers);
    }

    /**
     * Sends a Geolocation request
     *
     * @param GeolocationRequest $request
     * @return GeolocationResponse
     */
    public function call(GeolocationRequest $request)
    {
        $url = self::GEOLOCATION_URL . '?' . http_build_query(['key' => API_KEY]);

        curl_setopt($this->ch, CURLOPT_POSTFIELDS, json_encode($request));
        curl_setopt($this->ch, CURLOPT_URL, $url);

        $response = curl_exec($this->ch);

        return GeolocationResponse::fromApiResponse($response);
    }

}

/**
 * Class GeolocationRequest
 *
 * @todo make cellTower definition as an object and sanitize data
 * @link https://developers.google.com/maps/documentation/geolocation/intro#body
 * @package BitbarPlugins\Travel
 */
final class GeolocationRequest implements \JsonSerializable
{
    const DEFINITION_MMC = "homeMobileCountryCode";
    const DEFINITION_MNC = "homeMobileNetworkCode";
    const DEFINITION_RADIO_TYPE = "radioType";
    const DEFINITION_CARRIER = "carrier";
    const DEFINITION_CONSIDER_IP = "considerIp";
    const DEFINITION_CELL_TOWERS = "cellTowers";
    const DEFINITION_WIFI_ACCESS_POINTS = "wifiAccessPoints";

    /**
     * The mobile country code (MCC) for the device's home network.
     * @var int
     */
    private $homeMobileCountryCode = null;

    /**
     * The mobile network code (MNC) for the device's home network.
     * @var int
     */
    private $homeMobileNetworkCode = null;

    /**
     * The mobile radio type. Supported values are lte, gsm, cdma, and wcdma. While this field is optional,
     * it should be included if a value is available, for more accurate results.
     * @var string
     */
    private $radioType = null;

    /**
     * The carrier name.
     * @var string
     */
    private $carrier = null;

    /**
     * Specifies whether to fall back to IP geolocation if wifi and cell tower signals are not available.
     * Note that the IP address in the request header may not be the IP of the device. Defaults to true.
     * Set considerIp to false to disable fall back.
     * @var boolean
     */
    private $considerIp = true;

    /**
     * An array of cell tower objects.
     *
     * @var array
     */
    private $cellTowers;

    /**
     * An array of WiFi access point objects.
     * @var WifiAccessPoints
     */
    private $wifiAccessPoints;

    function jsonSerialize()
    {
        $return = [];

        if ($this->homeMobileCountryCode !== null) {
            $return[self::DEFINITION_MMC] = $this->homeMobileCountryCode;
        }

        if ($this->homeMobileNetworkCode !== null) {
            $return[self::DEFINITION_MNC] = $this->homeMobileNetworkCode;
        }

        if (in_array($this->radioType, ['lte', 'gsm', 'cdma', 'wcdma'], true)) {
            $return[self::DEFINITION_RADIO_TYPE] = $this->radioType;
        }

        if ($this->carrier) {
            $return[self::DEFINITION_CARRIER] = $this->carrier;
        }

        if ($this->considerIp === false) {
            $return[self::DEFINITION_CONSIDER_IP] = 'false';
        }

        if (count($this->cellTowers)) {
            $return[self::DEFINITION_CELL_TOWERS] = $this->cellTowers;
        }

        if (count($this->wifiAccessPoints)) {
            $return[self::DEFINITION_WIFI_ACCESS_POINTS] = $this->wifiAccessPoints;
        }

        return $return;
    }

    /**
     * @param array $definition
     *
     * @return self
     */
    public static function fromArrayDefinition(array $definition)
    {
        $instance = new self();

        $instance->homeMobileCountryCode = self::getMMCFromDefinition($definition);
        $instance->homeMobileNetworkCode = self::getMNCFromDefinition($definition);
        $instance->radioType = self::getRadioTypeFromDefinition($definition);
        $instance->carrier = self::getCarrierFromDefinition($definition);
        $instance->considerIp = self::getConsiderIpFromDefinition($definition);
        $instance->cellTowers = self::getCellTowersFromDefinition($definition);
        $instance->wifiAccessPoints = self::getWifiAccessPointsFromDefinition($definition);

        return $instance;
    }

    private static function getMMCFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_MMC])
            && is_int($definition[self::DEFINITION_MMC]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_MMC];
    }

    private static function getMNCFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_MNC])
            && is_int($definition[self::DEFINITION_MNC]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_MNC];
    }

    private static function getRadioTypeFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_RADIO_TYPE])
            && is_string($definition[self::DEFINITION_RADIO_TYPE])
            && in_array($definition[self::DEFINITION_RADIO_TYPE], ['lte', 'gsm', 'cdma', 'wcdma'], true))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_RADIO_TYPE];
    }

    private static function getCarrierFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_CARRIER])
            && is_string($definition[self::DEFINITION_CARRIER]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_CARRIER];
    }

    private static function getConsiderIpFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_CONSIDER_IP])
            && is_bool($definition[self::DEFINITION_CONSIDER_IP]))
        ) {
            return true;
        }

        return $definition[self::DEFINITION_CONSIDER_IP];
    }

    private static function getCellTowersFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_CELL_TOWERS])
            && is_array($definition[self::DEFINITION_CELL_TOWERS]))
        ) {
            return [];
        }

        return $definition[self::DEFINITION_CELL_TOWERS];
    }

    private static function getWifiAccessPointsFromDefinition(array $definition)
    {

        if (!(isset($definition[self::DEFINITION_WIFI_ACCESS_POINTS])
            && $definition[self::DEFINITION_WIFI_ACCESS_POINTS] instanceof WifiAccessPoints
            && count($definition[self::DEFINITION_WIFI_ACCESS_POINTS]) >= 2)
        ) {
            return [];
        }

        return $definition[self::DEFINITION_WIFI_ACCESS_POINTS];
    }

    /**
     * @param string $carrier
     * @return GeolocationRequest
     */
    public function setCarrier($carrier)
    {
        $this->carrier = $carrier;
        return $this;
    }

    /**
     * @param array $cellTowers
     * @return GeolocationRequest
     */
    public function setCellTowers($cellTowers)
    {
        $this->cellTowers = $cellTowers;
        return $this;
    }

    /**
     * @param boolean $considerIp
     * @return GeolocationRequest
     */
    public function setConsiderIp($considerIp)
    {
        $this->considerIp = $considerIp;
        return $this;
    }

    /**
     * @param int $homeMobileCountryCode
     * @return GeolocationRequest
     */
    public function setHomeMobileCountryCode($homeMobileCountryCode)
    {
        $this->homeMobileCountryCode = $homeMobileCountryCode;
        return $this;
    }

    /**
     * @param int $homeMobileNetworkCode
     * @return GeolocationRequest
     */
    public function setHomeMobileNetworkCode($homeMobileNetworkCode)
    {
        $this->homeMobileNetworkCode = $homeMobileNetworkCode;
        return $this;
    }

    /**
     * @param string $radioType
     * @return GeolocationRequest
     */
    public function setRadioType($radioType)
    {
        $this->radioType = $radioType;
        return $this;
    }

    /**
     * @param WifiAccessPoints $wifiAccessPoints
     * @return GeolocationRequest
     */
    public function setWifiAccessPoints($wifiAccessPoints)
    {
        $this->wifiAccessPoints = $wifiAccessPoints;
        return $this;
    }
}

/**
 * Class GeolocationResponse
 * @package BitbarPlugins\Travel
 */
final class GeolocationResponse
{
    const DEFINITION_LOCATION = 'location';
    const DEFINITION_LATITUDE = 'lat';
    const DEFINITION_LONGITUDE = 'lng';
    const DEFINITION_ACCURACY = 'accuracy';
    const DEFINITION_ERROR = 'error';
    const DEFINITION_MESSAGE = 'message';
    const DEFINITION_CODE = 'code';

    /**
     * @var float
     */
    private $latitude = null;

    /**
     * @var float
     */
    private $longitude = null;

    /**
     * @var float
     */
    private $accuracy = null;

    /**
     * Creates an instance of GeolocationResponse from raw API response in json
     * @param $response
     * @return GeolocationResponse
     * @throws GeolocationResponseException
     */
    public static function fromApiResponse($response)
    {
        $instance = new self();

        $geolocation = json_decode($response, true);

        if ($geolocationError = self::getErrorFromResponse($geolocation)) {
            throw $geolocationError;
        }

        $instance->latitude = self::getLatitudeFromResponse($geolocation);
        $instance->longitude = self::getLongitudeFromResponse($geolocation);
        $instance->accuracy = self::getAccuracyFromResponse($geolocation);

        return $instance;
    }

    private static function getErrorFromResponse(array $geolocation)
    {
        if (!(is_array($geolocation)
            && array_key_exists(self::DEFINITION_ERROR, $geolocation)
            && array_key_exists(self::DEFINITION_MESSAGE, $geolocation[self::DEFINITION_ERROR])
            && array_key_exists(self::DEFINITION_CODE, $geolocation[self::DEFINITION_ERROR])
            && is_string($geolocation[self::DEFINITION_ERROR][self::DEFINITION_MESSAGE])
            && is_int($geolocation[self::DEFINITION_ERROR][self::DEFINITION_CODE]))
        ) {
            return null;
        }

        return new GeolocationResponseException(
            $geolocation[self::DEFINITION_ERROR][self::DEFINITION_MESSAGE],
            $geolocation[self::DEFINITION_ERROR][self::DEFINITION_CODE]
        );
    }

    private static function getLatitudeFromResponse(array $geolocation)
    {
        if (!(is_array($geolocation)
            && array_key_exists(self::DEFINITION_LOCATION, $geolocation)
            && array_key_exists(self::DEFINITION_LATITUDE, $geolocation[self::DEFINITION_LOCATION])
            && is_float($geolocation[self::DEFINITION_LOCATION][self::DEFINITION_LATITUDE]))
        ) {
            return null;
        }

        return $geolocation[self::DEFINITION_LOCATION][self::DEFINITION_LATITUDE];
    }

    private static function getLongitudeFromResponse(array $geolocation)
    {
        if (!(is_array($geolocation)
            && array_key_exists(self::DEFINITION_LOCATION, $geolocation)
            && array_key_exists(self::DEFINITION_LONGITUDE, $geolocation[self::DEFINITION_LOCATION])
            && is_float($geolocation[self::DEFINITION_LOCATION][self::DEFINITION_LONGITUDE]))
        ) {
            return null;
        }

        return $geolocation[self::DEFINITION_LOCATION][self::DEFINITION_LONGITUDE];
    }

    private static function getAccuracyFromResponse(array $geolocation)
    {
        if (!(is_array($geolocation)
            && array_key_exists(self::DEFINITION_ACCURACY, $geolocation)
            && is_float($geolocation[self::DEFINITION_ACCURACY]))
        ) {
            return null;
        }

        return $geolocation[self::DEFINITION_ACCURACY];
    }

    /**
     * @return float
     */
    public function getAccuracy()
    {
        return $this->accuracy;
    }

    /**
     * @return float
     */
    public function getLatitude()
    {
        return $this->latitude;
    }

    /**
     * @return float
     */
    public function getLongitude()
    {
        return $this->longitude;
    }
}

final class GeolocationResponseException extends \Exception
{
    /**
     * @var int
     */
    private $statusCode;

    public function __construct($message, $code)
    {
        $this->statusCode = $code;
        $this->message = $message;
    }

    /**
     * @return int
     */
    public function getStatusCode()
    {
        return $this->statusCode;
    }
}

/**
 * Class DistanceMatrixAPI
 *
 * @link https://developers.google.com/maps/documentation/distance-matrix/
 * @package BitbarPlugins\Travel
 */
final class DistanceMatrixAPI
{
    const DISTANCE_MATRIX_URL = "https://maps.googleapis.com/maps/api/distancematrix/json";

    /**
     * @var resource curl handler
     */
    private $ch;

    private $headers = [
        "Accept: application/json",
        "Cache-Control: no-cache",
        "Pragma: no-cache",
    ];

    public function __construct()
    {
        $this->initCurl();
    }

    private function initCurl()
    {
        $this->ch = curl_init();

        curl_setopt($this->ch, CURLOPT_RETURNTRANSFER, 1);
        curl_setopt($this->ch, CURLOPT_HTTPHEADER, $this->headers);
    }

    /**
     * Send a DistanceMatrix Request
     *
     * @param DistanceMatrixRequest $request
     * @return DistanceMatrixResponse
     * @throws DistanceMatrixResponseException
     */
    public function call(DistanceMatrixRequest $request)
    {
        $url = self::DISTANCE_MATRIX_URL . '?' . $request->toQueryParameters();
        curl_setopt($this->ch, CURLOPT_URL, $url);
        $response = curl_exec($this->ch);

        return DistanceMatrixResponse::fromApiResponse($response);
    }

}

/**
 * Class DistanceMatrixRequest
 * @todo handle departure_time with unix timestamps
 * @package BitbarPlugins\Travel
 */
final class DistanceMatrixRequest
{
    const DEFINITION_ORIGINS = "origins";
    const DEFINITION_DESTINATIONS = "destinations";
    const DEFINITION_KEY = "key";
    const DEFINITION_MODE = "mode";
    const DEFINITION_LANGUAGE = "language";
    const DEFINITION_UNITS = "units";
    const DEFINITION_DEPARTURE_TIME = "departure_time";

    const PATTERN_LATITUDE_LONGITUDE = '/^\-?\d+(?:\.\d+)?,\-?\d+(?:\.\d+)?$/';

    /**
     * One or more addresses and/or textual latitude/longitude values, separated with the pipe (|) character,
     * from which to calculate distance and time.
     * @var string
     */
    private $origins = null;

    /**
     * One or more addresses and/or textual latitude/longitude values, separated with the pipe (|) character,
     * to which to calculate distance and time.
     * @var string
     */
    private $destinations = null;

    /**
     * Your application's API key. This key identifies your application for purposes of quota management.
     * @var string
     */
    private $key = null;

    /**
     * Optional
     * Specifies the mode of transport to use when calculating distance.
     * @var string
     */
    private $mode = 'driving'; // 'driving', 'walking', 'cycling', 'transit'

    /**
     * Optional
     * The language in which to return results.
     * @var string
     */
    private $language = 'en';

    /**
     * Optional
     * Specifies the unit system to use when expressing distance as text.
     * @var string
     */
    private $units = 'metric'; // 'metric', 'imperial'

    /**
     * Optional
     * The desired time of departure. You can specify the time as an integer in seconds since midnight,
     * January 1, 1970 UTC. Alternatively, you can specify a value of now
     * @var string
     */
    private $departureTime = 'now';

    /**
     * Convert Request Object to URL-encoded query string
     *
     * @return string
     */
    public function toQueryParameters()
    {
        return http_build_query([
            self::DEFINITION_ORIGINS => $this->origins,
            self::DEFINITION_DESTINATIONS => $this->destinations,
            self::DEFINITION_KEY => $this->key,
            self::DEFINITION_MODE => $this->mode,
            self::DEFINITION_LANGUAGE => $this->language,
            self::DEFINITION_UNITS => $this->units,
            self::DEFINITION_DEPARTURE_TIME => $this->departureTime,
        ]);
    }

    /**
     * @param array $definition
     *
     * @return self
     */
    public static function fromArrayDefinition(array $definition)
    {
        $instance = new self();

        $instance->origins = self::getOriginsFromDefinition($definition);
        $instance->destinations = self::getDestinationsFromDefinition($definition);
        $instance->key = self::getKeyFromDefinition($definition);
        $instance->mode = self::getModeFromDefinition($definition);
        $instance->language = self::getLanguageFromDefinition($definition);
        $instance->units = self::getUnitsFromDefinition($definition);
        $instance->departureTime = self::getDepartureTimeFromDefinition($definition);

        return $instance;
    }

    private static function getOriginsFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_ORIGINS])
            && is_string($definition[self::DEFINITION_ORIGINS])
            && preg_match(self::PATTERN_LATITUDE_LONGITUDE, $definition[self::DEFINITION_ORIGINS]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_ORIGINS];
    }

    private static function getDestinationsFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_DESTINATIONS])
            && is_string($definition[self::DEFINITION_DESTINATIONS]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_DESTINATIONS];
    }

    private static function getKeyFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_KEY])
            && is_string($definition[self::DEFINITION_KEY]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_KEY];
    }

    private static function getModeFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_MODE])
            && is_string($definition[self::DEFINITION_MODE])
            && in_array($definition[self::DEFINITION_MODE], ['driving', 'walking', 'cycling', 'transit']))
        ) {
            return 'driving';
        }

        return $definition[self::DEFINITION_MODE];
    }

    private static function getLanguageFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_LANGUAGE])
            && is_string($definition[self::DEFINITION_LANGUAGE])
            && in_array($definition[self::DEFINITION_LANGUAGE], ['ar', 'kn', 'bg', 'ko', 'bn', 'lt', 'ca', 'lv', 'cs',
                'ml', 'da', 'mr', 'de', 'nl', 'el', 'no', 'en', 'pl', 'en-AU', 'pt', 'en-GB', 'pt-BR', 'es', 'pt-PT',
                'eu', 'ro', 'eu', 'ru', 'fa', 'sk', 'fi', 'sl', 'fil', 'sr', 'fr', 'sv', 'gl', 'ta', 'gu', 'te', 'hi',
                'th', 'hr', 'tl', 'hu', 'tr', 'id', 'uk', 'it', 'vi', 'iw', 'zh-CN', 'ja', 'zh-TW',]))
        ) {
            return 'en';
        }

        return $definition[self::DEFINITION_LANGUAGE];
    }

    private static function getUnitsFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_UNITS])
            && is_string($definition[self::DEFINITION_UNITS])
            && in_array($definition[self::DEFINITION_UNITS], ['metric', 'imperial']))
        ) {
            return 'metric';
        }

        return $definition[self::DEFINITION_UNITS];
    }

    private static function getDepartureTimeFromDefinition(array $definition)
    {
        if (!(isset($definition[self::DEFINITION_DEPARTURE_TIME])
            && is_string($definition[self::DEFINITION_DEPARTURE_TIME])
            && $definition[self::DEFINITION_DEPARTURE_TIME] === 'now')
        ) {
            return 'now';
        }

        return $definition[self::DEFINITION_DEPARTURE_TIME];
    }
}

/**
 * Class DistanceMatrixResponse
 * @package BitbarPlugins\Travel
 */
final class DistanceMatrixResponse
{
    const DEFINITION_STATUS = 'status';
    const DEFINITION_ORIGIN_ADDRESSES = 'origin_addresses';
    const DEFINITION_DESTINATION_ADDRESSES = 'destination_addresses';
    const DEFINITION_ROWS = 'rows';

    const STATUS_CODE_OK = 'OK';
    const STATUS_CODE_INVALID_REQUEST = 'INVALID_REQUEST';
    const STATUS_CODE_MAX_ELEMENTS_EXCEEDED = 'MAX_ELEMENTS_EXCEEDED';
    const STATUS_CODE_OVER_QUERY_LIMIT = 'OVER_QUERY_LIMIT';
    const STATUS_CODE_REQUEST_DENIED = 'REQUEST_DENIED';
    const STATUS_CODE_UNKNOWN_ERROR = 'UNKNOWN_ERROR';

    /**
     * Contains metadata on the request.
     * @var string
     */
    private $status;

    /**
     * Contains an array of addresses as returned by the API from your original request.
     * These are formatted by the geocoder and localized according to the language parameter passed with the request.
     * @var string
     */
    private $originAddresses;

    /**
     * Contains an array of addresses as returned by the API from your original request.
     * As with origin_addresses, these are localized if appropriate.
     * @var string
     */
    private $destinationAddresses;

    /**
     * Contains an array of elements
     * @var DistanceMatrixResponseElement
     */
    private $rows;

    /**
     * Creates an instance of DistanceMatrixResponse from raw API response in json
     * @param $response
     * @return DistanceMatrixResponse
     * @throws DistanceMatrixResponseException
     */
    public static function fromApiResponse($response)
    {
        $instance = new self();

        $distanceMatrix = json_decode($response, true);

        if ($distanceMatrixError = self::getErrorFromResponse($distanceMatrix)) {
            throw $distanceMatrixError;
        }

        $instance->status = self::getStatusFromResponse($distanceMatrix);
        $instance->originAddresses = self::getOriginAddressesFromResponse($distanceMatrix);
        $instance->destinationAddresses = self::getDestinationAddressesFromResponse($distanceMatrix);
        $instance->rows = DistanceMatrixResponseElement::fromArrayDefinition(self::getRowsFromResponse($distanceMatrix));

        return $instance;
    }

    private static function getErrorFromResponse(array $distanceMatrix)
    {
        if (!(is_array($distanceMatrix)
            && array_key_exists(self::DEFINITION_STATUS, $distanceMatrix)
            && $distanceMatrix[self::DEFINITION_STATUS] === self::STATUS_CODE_OK)
        ) {
            return new DistanceMatrixResponseException($distanceMatrix[self::DEFINITION_STATUS]);
        }

        return null;
    }

    private static function getStatusFromResponse(array $distanceMatrix)
    {
        if (!(is_array($distanceMatrix)
            && array_key_exists(self::DEFINITION_STATUS, $distanceMatrix)
            && is_string($distanceMatrix[self::DEFINITION_STATUS]))
        ) {
            return null;
        }

        return $distanceMatrix[self::DEFINITION_STATUS];
    }

    private static function getOriginAddressesFromResponse(array $distanceMatrix)
    {
        if (!(is_array($distanceMatrix)
            && array_key_exists(self::DEFINITION_ORIGIN_ADDRESSES, $distanceMatrix)
            && is_array($distanceMatrix[self::DEFINITION_ORIGIN_ADDRESSES])
            && count($distanceMatrix[self::DEFINITION_ORIGIN_ADDRESSES]) === 1)
        ) {
            return null;
        }

        return $distanceMatrix[self::DEFINITION_ORIGIN_ADDRESSES][0];
    }

    private static function getDestinationAddressesFromResponse(array $distanceMatrix)
    {
        if (!(is_array($distanceMatrix)
            && array_key_exists(self::DEFINITION_DESTINATION_ADDRESSES, $distanceMatrix)
            && is_array($distanceMatrix[self::DEFINITION_DESTINATION_ADDRESSES])
            && count($distanceMatrix[self::DEFINITION_DESTINATION_ADDRESSES]) === 1)
        ) {
            return null;
        }

        return $distanceMatrix[self::DEFINITION_DESTINATION_ADDRESSES][0];
    }

    private static function getRowsFromResponse(array $distanceMatrix)
    {
        if (!(is_array($distanceMatrix)
            && array_key_exists(self::DEFINITION_ROWS, $distanceMatrix)
            && is_array($distanceMatrix[self::DEFINITION_ROWS])
            && count($distanceMatrix[self::DEFINITION_ROWS]) === 1)
        ) {
            return null;
        }

        return $distanceMatrix[self::DEFINITION_ROWS][0];
    }

    /**
     * @return string
     */
    public function getDestinationAddresses()
    {
        return $this->destinationAddresses;
    }

    /**
     * @return string
     */
    public function getOriginAddresses()
    {
        return $this->originAddresses;
    }

    /**
     * @return DistanceMatrixResponseElement
     */
    public function getRows()
    {
        return $this->rows;
    }

    /**
     * @return string
     */
    public function getStatus()
    {
        return $this->status;
    }

}

final class DistanceMatrixResponseElement
{
    const DEFINITION_ELEMENTS = 'elements';
    const DEFINITION_STATUS = 'status';
    const DEFINITION_DURATION = 'duration';
    const DEFINITION_DISTANCE = 'distance';
    const DEFINITION_DURATION_IN_TRAFFIC = 'duration_in_traffic';

    const STATUS_CODE_OK = 'OK';
    const STATUS_CODE_NOT_FOUND = 'NOT_FOUND';
    const STATUS_CODE_ZERO_RESULTS = 'ZERO_RESULTS';

    /**
     * Element level status of the request
     * @var string
     */
    private $status = null;

    /**
     * The length of time it takes to travel this route, expressed in seconds (the value field) and as text.
     * The textual representation is localized according to the query's language parameter.
     * @var array
     */
    private $duration = [];

    /**
     * The total distance of this route, expressed in meters (value) and as text. The textual value uses the unit
     * system specified with the unit parameter of the original request, or the origin's region.
     * @var array
     */
    private $distance = [];

    /**
     * The length of time it takes to travel this route, based on current and historical traffic conditions.
     * @var array
     */
    private $durationInTraffic = [];

    /**
     * @param array $definition
     * @return DistanceMatrixResponseElement
     * @throws DistanceMatrixResponseElementException
     */
    public static function fromArrayDefinition(array $definition)
    {
        $instance = new self();

        if ($distanceMatrixElementError = self::getErrorFromDefinition($definition)) {
            throw $distanceMatrixElementError;
        }

        $instance->status = self::getStatusFromDefinition($definition);
        $instance->duration = self::getDurationFromDefinition($definition);
        $instance->distance = self::getDistanceFromDefinition($definition);
        $instance->durationInTraffic = self::getDurationInTrafficFromDefinition($definition);

        return $instance;
    }

    public static function getErrorFromDefinition(array $definition)
    {
        if (!(is_array($definition)
            && array_key_exists(self::DEFINITION_ELEMENTS, $definition)
            && is_array($definition[self::DEFINITION_ELEMENTS])
            && count($definition[self::DEFINITION_ELEMENTS]) === 1
            && is_array($definition[self::DEFINITION_ELEMENTS][0])
            && array_key_exists(self::DEFINITION_STATUS, $definition[self::DEFINITION_ELEMENTS][0])
            && $definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_STATUS] === self::STATUS_CODE_OK)
        ) {
            return new DistanceMatrixResponseElementException($definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_STATUS]);
        }

        return null;
    }

    public static function getStatusFromDefinition(array $definition)
    {
        if (!(is_array($definition[self::DEFINITION_ELEMENTS][0])
            && array_key_exists(self::DEFINITION_STATUS, $definition[self::DEFINITION_ELEMENTS][0])
            && is_string($definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_STATUS]))
        ) {
            return null;
        }

        return $definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_STATUS];
    }

    public static function getDurationFromDefinition(array $definition)
    {
        if (!(is_array($definition[self::DEFINITION_ELEMENTS][0])
            && array_key_exists(self::DEFINITION_DURATION, $definition[self::DEFINITION_ELEMENTS][0])
            && is_array($definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_DURATION]))
        ) {
            return [];
        }

        return $definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_DURATION];
    }

    public static function getDistanceFromDefinition(array $definition)
    {
        if (!(is_array($definition[self::DEFINITION_ELEMENTS][0])
            && array_key_exists(self::DEFINITION_DISTANCE, $definition[self::DEFINITION_ELEMENTS][0])
            && is_array($definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_DISTANCE]))
        ) {
            return [];
        }

        return $definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_DISTANCE];
    }

    public static function getDurationInTrafficFromDefinition(array $definition)
    {
        if (!(is_array($definition[self::DEFINITION_ELEMENTS][0])
            && array_key_exists(self::DEFINITION_DURATION_IN_TRAFFIC, $definition[self::DEFINITION_ELEMENTS][0])
            && is_array($definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_DURATION_IN_TRAFFIC]))
        ) {
            return [];
        }

        return $definition[self::DEFINITION_ELEMENTS][0][self::DEFINITION_DURATION_IN_TRAFFIC];
    }

    /**
     * @return array
     */
    public function getDistance()
    {
        return $this->distance;
    }

    /**
     * @return array
     */
    public function getDuration()
    {
        return $this->duration;
    }

    /**
     * @return array
     */
    public function getDurationInTraffic()
    {
        return $this->durationInTraffic;
    }

    /**
     * @return string
     */
    public function getStatus()
    {
        return $this->status;
    }
}

final class DistanceMatrixResponseException extends \Exception
{

}

final class DistanceMatrixResponseElementException extends \Exception
{

}

echo new TravelTimePlugin();
