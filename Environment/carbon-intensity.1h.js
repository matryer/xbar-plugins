#!/opt/homebrew/bin/node

// Metadata
// <xbar.title>Carbon Intensity</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Jason Jones</xbar.author>
// <xbar.author.github>jasonm-jones</xbar.author.github>
// <xbar.desc>Shows real-time carbon intensity and grid cleanliness to help you minimize your carbon footprint by running energy-intensive tasks at cleaner times.</xbar.desc>
// <xbar.image>https://raw.githubusercontent.com/jasonm-jones/carbon-intensity-xbar/79b01fbcdb4c535b78718b1699c477484ddf8bdb/CarbonIntensityScreenshot.png</xbar.image>
// <xbar.dependencies>node, npm</xbar.dependencies>
// <xbar.abouturl>https://github.com/jasonm-jones/carbon-intensity-xbar</xbar.abouturl>
// <xbar.var>string(ELECTRICITY_MAPS_API_KEY=""): Your Electricity Maps API key from https://api-portal.electricitymaps.com/signup</xbar.var>
// <xbar.var>string(ELECTRICITY_MAPS_ZONE=""): Your Electricity Maps zone. Find your zone at https://app.electricitymaps.com/map</xbar.var>
// <xbar.var>string(WATTTIME_USERNAME=""): Your WattTime username from https://www.watttime.org/api-documentation/#register-new-user</xbar.var>
// <xbar.var>string(WATTTIME_PASSWORD=""): Your WattTime password</xbar.var>
// <xbar.var>string(WATTTIME_ZONE=""): Your WattTime Grid Region Abbreviation. Find yours at https://www.watttime.org/explorer/</xbar.var>


const https = require('https');


// Load variables from either .vars.jsonfile
if (!process.env.XBAR) {
  try {
    const vars = require(`${__filename}.vars.json`);
    Object.assign(process.env, vars);
  } catch (error) {
    console.error('Debug: Failed to load .vars.json:', error.message);
  }
}

// API Configuration
const ELECTRICITY_MAPS_API_KEY =  process.env.ELECTRICITY_MAPS_API_KEY;
const ELECTRICITY_MAPS_ZONE = process.env.ELECTRICITY_MAPS_ZONE;
const WATTTIME_USERNAME =  process.env.WATTTIME_USERNAME;
const WATTTIME_PASSWORD =  process.env.WATTTIME_PASSWORD;
const WATTTIME_ZONE =  process.env.WATTTIME_ZONE;

// Display Configuration
const EMOJIS = ["🌿", "🌱", "😑", "😫", "😡", "⛔", "❓"];

// Helper Functions
function makeRequest(path) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: 'api.electricitymap.org',
      path: path,
      headers: { 'auth-token': ELECTRICITY_MAPS_API_KEY }
    };
    https.get(options, handleResponse(resolve, reject));
  });
}

function getWattTimeToken() {
  return new Promise((resolve, reject) => {
    const auth = Buffer.from(`${WATTTIME_USERNAME}:${WATTTIME_PASSWORD}`).toString('base64');
    const options = {
      hostname: 'api2.watttime.org',
      path: '/v2/login',
      headers: { 'Authorization': `Basic ${auth}` }
    };
    https.get(options, handleResponse(resolve, reject, response => response.token));
  });
}

function getMOER(token) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: 'api2.watttime.org',
      path: `/v3/signal-index?region=${WATTTIME_ZONE}&signal_type=co2_moer`,
      headers: { 'Authorization': `Bearer ${token}` }
    };
    https.get(options, handleResponse(resolve, reject));
  });
}

function handleResponse(resolve, reject, transform = response => response) {
  return (resp) => {
    let data = '';
    resp.on('data', (chunk) => { data += chunk; });
    resp.on('end', () => {
      try {
        resolve(transform(JSON.parse(data)));
      } catch (error) {
        reject(error);
      }
    });
  };
}

function calculateFossilFuelPercentage(powerData) {
  if (!powerData?.powerConsumptionBreakdown) {
    console.error('Power data is missing or invalid:', powerData);
    return 'N/A';
  }

  const fossilFuels = ['coal', 'gas', 'oil', 'unknown'];
  const totalPower = Object.values(powerData.powerConsumptionBreakdown).reduce((a, b) => a + b, 0);
  const fossilPower = fossilFuels.reduce((sum, fuel) => 
    sum + (powerData.powerConsumptionBreakdown[fuel] || 0), 0);
  return Math.round((fossilPower / totalPower) * 100);
}

function calculateRenewablePercentage(powerData) {
  if (!powerData?.powerConsumptionBreakdown) {
    console.error('Power data is missing or invalid:', powerData);
    return 'N/A';
  }

  const renewables = ['wind', 'solar', 'hydro', 'biomass', 'geothermal'];
  const totalPower = Object.values(powerData.powerConsumptionBreakdown).reduce((a, b) => a + b, 0);
  const renewablePower = renewables.reduce((sum, source) => 
    sum + (powerData.powerConsumptionBreakdown[source] || 0), 0);
  return Math.round((renewablePower / totalPower) * 100);
}

function getEmoji(percentile) {
  if (percentile === undefined || percentile === 'N/A') return EMOJIS[6];
  if (percentile <= 20) return EMOJIS[0];  // Cleanest 20%
  if (percentile <= 40) return EMOJIS[1];  // Cleaner than average
  if (percentile <= 60) return EMOJIS[2];  // Average
  if (percentile <= 80) return EMOJIS[3];  // Dirtier than average
  if (percentile <= 90) return EMOJIS[4];  // Very dirty
  return EMOJIS[5];                        // Extremely dirty
}

function displayPowerBreakdown(powerData) {
  // Power source emojis
  const sourceEmojis = {
    wind: "💨️",
    solar: "☀️",
    hydro: "💧",
    biomass: "🌱",
    geothermal: "🌋",
    nuclear: "⚛️",
    coal: "🪨",
    gas: "⛽",
    oil: "🛢️",
    unknown: "❓"
  };

  const totalPower = Object.values(powerData.powerConsumptionBreakdown).reduce((a, b) => a + b, 0);
  Object.entries(powerData.powerConsumptionBreakdown)
    .sort(([, a], [, b]) => b - a)
    .forEach(([source, value]) => {
      const percentage = Math.round((value / totalPower) * 100);
      if (percentage > 0) {
        const emoji = sourceEmojis[source] || "❓";
        console.log(`${emoji} ${source}: ${percentage}%`);
      }
    });
}

// Validate Configuration
function validateConfig() {
  const errors = [];
  
  // Check for required Electricity Maps configuration
  if (!ELECTRICITY_MAPS_API_KEY) {
    errors.push("🔴 Missing Electricity Maps API key. Get one at https://api-portal.electricitymaps.com/signup");
  }
  if (!ELECTRICITY_MAPS_ZONE) {
    errors.push("🔴 Missing Electricity Maps zone. Find your zone at https://app.electricitymaps.com/map (click on the map and find your zone in the URL) or https://api.electricitymap.org/v3/zones");
  }

  if (!WATTTIME_USERNAME) {
    errors.push("🔴 Missing WattTime username. Sign up at https://www.watttime.org/api-documentation/#register-new-user");
  }
  if (!WATTTIME_PASSWORD) {
    errors.push("🔴 Missing WattTime password");
  }
  if (!WATTTIME_ZONE) {
    errors.push("🔴 Missing WattTime zone. Find your grid region at https://www.watttime.org/explorer/");
  }


  return errors;
}

// Main Execution
const configErrors = validateConfig();

if (configErrors.length > 0) {
  console.log('Config Errors');
  console.log('---');
  configErrors.forEach(error => {
    console.log(`${error} | color=red`);
  });
  console.log('---');
  console.log('📖 Quick Start Instructions | href=https://github.com/jasonm-jones/carbon-intensity-xbar#readme');
  process.exit(0);
}

// If WattTime is not configured, only make Electricity Maps requests
Promise.all([
  makeRequest(`/v3/carbon-intensity/latest?zone=${ELECTRICITY_MAPS_ZONE}`),
  makeRequest(`/v3/power-breakdown/latest?zone=${ELECTRICITY_MAPS_ZONE}`),
  getWattTimeToken().then(token => getMOER(token))
])
.then(([carbonData, powerData, moerData]) => {

  const fossilPercentage = calculateFossilFuelPercentage(powerData);
  const renewablePercentage = calculateRenewablePercentage(powerData);
  const moerValue = moerData?.data?.[0]?.value ?? 'N/A';
  const moerPercent = moerValue;
  const moerTimestamp = moerData?.data?.[0]?.point_time;

  const emoji = getEmoji(moerPercent);
  
  // Menu Bar Display
  console.log(`${emoji} (${100 - moerPercent}%) ${Math.round(carbonData.carbonIntensity)} gCO₂eq/kWh | size=12 font=UbuntuMono-Bold`);
  console.log('---');
  console.log(`Grid Carbon Intensity: ${Math.round(carbonData.carbonIntensity)} gCO₂eq/kWh`);
  console.log(`24hr Relative Cleanliness: ${100 - moerValue}th percentile`);
  console.log('---');
  console.log(`Power from Renewables: ${renewablePercentage}%`);
  console.log(`Power from Fossil Fuels: ${fossilPercentage}%`);
  console.log('---');
  console.log('Power Breakdown:');
  displayPowerBreakdown(powerData);  
  console.log('---');
  console.log(`Zone: ${ELECTRICITY_MAPS_ZONE}`);
  console.log(`Grid Power Breakdown Updated: ${new Date(powerData.datetime).toLocaleString('en-GB', {
    day: 'numeric',
    month: 'short',
    year: 'numeric',
    hour: 'numeric',
    minute: '2-digit',
    hour12: true
  })}`);
  console.log(`Cleanliness Forecast Updated: ${new Date(moerTimestamp).toLocaleString('en-GB', {
    day: 'numeric',
    month: 'short',
    year: 'numeric',
    hour: 'numeric',
    minute: '2-digit',
    hour12: true
  })}`);
  console.log('---');
  console.log(`⚡ Open Electricity Maps, ${ELECTRICITY_MAPS_ZONE} | href=https://app.electricitymaps.com/zone/${ELECTRICITY_MAPS_ZONE}`);
  console.log('📖 View Setup Instructions | href=https://github.com/jasonm-jones/carbon-intensity-xbar#readme');
})
.catch(error => {
  console.log('🔴 Error');
  console.log('---');
  console.log(`${error.message} | color=red`);
  console.log('---');
  console.log('📖 View Setup Instructions | href=https://github.com/jasonm-jones/carbon-intensity-xbar#readme');
}); 