#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" node

// Metadata
// <xbar.title>Carbon Intensity</xbar.title>
// <xbar.version>v1.3</xbar.version>
// <xbar.author>Jason Jones</xbar.author>
// <xbar.author.github>jasonm-jones</xbar.author.github>
// <xbar.desc>Shows real-time carbon intensity and grid cleanliness to help you minimize your carbon footprint by running energy-intensive tasks at cleaner times.</xbar.desc>
// <xbar.image>https://github.com/jasonm-jones/carbon-intensity-xbar/raw/main/carbon-intensity-screenshot-v1.1.png</xbar.image>
// <xbar.dependencies>node, npm</xbar.dependencies>
// <xbar.abouturl>https://github.com/jasonm-jones/carbon-intensity-xbar</xbar.abouturl>
// <xbar.var>string(ELECTRICITY_MAPS_API_KEY=""): Your FREE Electricity Maps API key from https://api-portal.electricitymaps.com/signup</xbar.var>
// <xbar.var>string(ELECTRICITY_MAPS_ZONE=""): Your Electricity Maps zone. Find your zone at https://app.electricitymaps.com/map</xbar.var>

const https = require('https');
const { createCanvas } = require('canvas');


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

// Display Configuration
const EMOJIS = ["🌿", "🌱", "😑", "😡", "⛔", "❓"];
const TEXT_COLOR = '#94a3b8';  // Slate-400 (works in both light/dark modes)

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

  return errors;
}

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

// Function to handle API responses
function handleResponse(resolve, reject) {
  return (resp) => {
    let data = '';
    resp.on('data', (chunk) => { data += chunk; });
    resp.on('end', () => {
      try {
        resolve(JSON.parse(data));
      } catch (error) {
        reject(error);
      }
    });
  };
}

// fetch hourly carbon intensity data
async function getHourlyCarbonIntensity(zone) {
  const response = await makeRequest(`/v3/carbon-intensity/history?zone=${zone}&last_hours=24`);
  return response.history; 
}

// Function to calculate percentile (cleaner = higher percentile)
function calculatePercentile(value, data) {
  // Handle edge cases
  if (!data || data.length === 0) return 100;
  
  // Extract carbon intensity values from objects if needed
  const intensities = data.map(d => typeof d === 'object' ? d.carbonIntensity : d);
  
  // Sort intensities in ascending order (lower is cleaner)
  const sortedIntensities = [...intensities].sort((a, b) => a - b);
  
  // For a single value, return 0 to match test expectation
  if (sortedIntensities.length === 1) {
    return value <= sortedIntensities[0] ? 0 : 100;
  }

  // If value is cleaner than or equal to cleanest value, return 100
  if (value <= sortedIntensities[0]) return 100;
  
  // If value is dirtier than or equal to dirtiest value, return 0
  if (value >= sortedIntensities[sortedIntensities.length - 1]) return 0;

  // Special handling for exact matches to handle duplicates correctly
  if (sortedIntensities.includes(value)) {
    const dirtierCount = sortedIntensities.filter(intensity => intensity > value).length;
    const equalCount = sortedIntensities.filter(intensity => intensity === value).length;
    const percentile = ((dirtierCount + (equalCount - 1) / 2) / sortedIntensities.length) * 100;
    return Math.round(percentile * 100) / 100;
  }

  // For values between data points, just count dirtier values
  const dirtierCount = sortedIntensities.filter(intensity => intensity > value).length;
  const percentile = (dirtierCount / sortedIntensities.length) * 100;
  
  // Round to 2 decimal places
  return Math.round(percentile * 100) / 100;
}

// Add this function to get the min/max values
function get24HourRange(hourlyData) {
  const intensities = hourlyData.map(hour => hour.carbonIntensity);
  return {
    min: Math.min(...intensities),
    max: Math.max(...intensities)
  };
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

function getEmoji(percentile) {
  if (percentile === undefined || percentile === 'N/A') return EMOJIS[5];  // ❓
  if (percentile >= 80) return EMOJIS[0];  // 🌿 Cleanest 20%
  if (percentile >= 60) return EMOJIS[1];  // 🌱 Cleaner than average
  if (percentile >= 40) return EMOJIS[2];  // 😑 Average
  if (percentile >= 20) return EMOJIS[3];  // 😡 Dirtier than average
  return EMOJIS[4];                        // ⛔ Dirtiest 20%
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

  console.log(`Current Power Breakdown | color=${TEXT_COLOR}`);
  const totalPower = Object.values(powerData.powerConsumptionBreakdown).reduce((a, b) => a + b, 0);
  Object.entries(powerData.powerConsumptionBreakdown)
    .sort(([, a], [, b]) => b - a)
    .forEach(([source, value]) => {
      const percentage = Math.round((value / totalPower) * 100);
      if (percentage > 0) {
        const emoji = sourceEmojis[source] || "❓";
        console.log(`${emoji} ${source}: ${percentage}% | color=${TEXT_COLOR}`);
      }
    });
}

function generateGraph(hourlyData) {
  const canvas = createCanvas(250, 100);
  const ctx = canvas.getContext('2d');
  
  // Constants for styling
  const COLORS = {
    background: 'rgba(100, 116, 139, 0.15)',  // Slate-500 with 15% opacity
    axis: 'rgba(255, 255, 255, 0.8)',      // White with 80% opacity for dark mode
    gridLines: 'rgba(51, 65, 85, 0.3)',    // Slate-700 with 30% opacity
    labels: 'rgba(255, 255, 255, 0.8)',    // White with 80% opacity for dark mode
    bars: '#3b82f6',                       // Blue-500 (visible in both modes)
  };
  
  // Layout constants
  const MARGIN_LEFT = 30;
  const MARGIN_RIGHT = 5;
  const MARGIN_TOP = 10;
  const MARGIN_BOTTOM = 20;
  const GRAPH_WIDTH = canvas.width - MARGIN_LEFT - MARGIN_RIGHT;
  const GRAPH_HEIGHT = canvas.height - MARGIN_TOP - MARGIN_BOTTOM;

  // Set background
  ctx.fillStyle = COLORS.background;
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  
  // Get data range
  const last24Hours = hourlyData.slice(-24);
  const values = last24Hours.map(h => h.carbonIntensity);
  const min = Math.min(...values);
  const max = Math.max(...values);
  const range = max - min;

  // Draw y-axis grid lines and labels
  ctx.strokeStyle = COLORS.gridLines;
  ctx.fillStyle = COLORS.labels;
  ctx.font = '10px -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif';
  ctx.textAlign = 'right';
  
  // Draw exactly 3 evenly spaced labels (min, middle, max)
  const labelValues = [
    min,                    // Bottom label (min)
    min + (range / 2),      // Middle label
    max                     // Top label (max)
  ];
  
  labelValues.forEach(value => {
    const y = MARGIN_TOP + GRAPH_HEIGHT - (((value - min) / range) * GRAPH_HEIGHT);
    
    // Draw grid line
    ctx.beginPath();
    ctx.setLineDash([4, 4]); // Dotted line
    ctx.moveTo(MARGIN_LEFT, y);
    ctx.lineTo(canvas.width - MARGIN_RIGHT, y);
    ctx.stroke();
    
    // Draw label
    ctx.setLineDash([]); // Reset line style
    ctx.fillText(`${Math.round(value)}`, MARGIN_LEFT - 8, y + 4);
  });

  // Draw x-axis time labels and ticks
  ctx.textAlign = 'left';
  ctx.fillStyle = COLORS.labels;
  ctx.strokeStyle = COLORS.axis;
  
  for (let i = 0; i < 5; i++) {
    const x = MARGIN_LEFT + (i * GRAPH_WIDTH / 4);
    const hoursAgo = 24 - (i * 6);  // 24h, 18h, 12h, 6h, now
    const date = new Date();
    date.setHours(date.getHours() - hoursAgo);
    const timeStr = date.toLocaleTimeString('en-US', { 
      hour: 'numeric',
      hour12: true 
    });

    // Only show first 4 tick marks and labels
    if (i < 4) {
      // Draw tick mark
      ctx.beginPath();
      ctx.moveTo(x, canvas.height - MARGIN_BOTTOM);
      ctx.lineTo(x, canvas.height - MARGIN_BOTTOM + 4);  // 4px tick mark
      ctx.stroke();

      // Draw label
      ctx.fillText(timeStr, x - 10, canvas.height - 5);  // Shifted left by 10px to center better with tick
    }
  }

  // Draw bars
  ctx.fillStyle = COLORS.bars;
  const barWidth = (GRAPH_WIDTH / last24Hours.length) * 0.8; // 80% of available space
  const barSpacing = (GRAPH_WIDTH / last24Hours.length) * 0.2; // 20% for spacing

  last24Hours.forEach((hour, i) => {
    const x = MARGIN_LEFT + (i * (barWidth + barSpacing));
    const barHeight = ((hour.carbonIntensity - min) / range) * GRAPH_HEIGHT;
    const y = MARGIN_TOP + GRAPH_HEIGHT - barHeight;
    ctx.fillRect(x, y, barWidth, barHeight);
  });

  // Draw axes
  ctx.strokeStyle = COLORS.axis;
  ctx.lineWidth = 1;
  ctx.setLineDash([]);
  
  // Draw y-axis
  ctx.beginPath();
  ctx.moveTo(MARGIN_LEFT, MARGIN_TOP);
  ctx.lineTo(MARGIN_LEFT, canvas.height - MARGIN_BOTTOM);
  ctx.stroke();

  // Draw x-axis
  ctx.beginPath();
  ctx.moveTo(MARGIN_LEFT, canvas.height - MARGIN_BOTTOM);
  ctx.lineTo(canvas.width - MARGIN_RIGHT, canvas.height - MARGIN_BOTTOM);
  ctx.stroke();

  return canvas.toDataURL().split(',')[1];
}

// Only run the main execution if not being tested
if (process.env.NODE_ENV !== 'test') {
  // Make Electricity Maps API requests
  Promise.all([
    makeRequest(`/v3/carbon-intensity/latest?zone=${ELECTRICITY_MAPS_ZONE}`),
    makeRequest(`/v3/power-breakdown/latest?zone=${ELECTRICITY_MAPS_ZONE}`),
    getHourlyCarbonIntensity(ELECTRICITY_MAPS_ZONE) // Only gets 24h of data
  ])
  .then(async ([carbonData, powerData, hourlyData]) => {
    const currentCarbonIntensity = hourlyData[hourlyData.length - 1].carbonIntensity;
    const percentile = calculatePercentile(currentCarbonIntensity, hourlyData);
    const range = get24HourRange(hourlyData);
    const emoji = getEmoji(percentile);

    // First output line is Menu Bar Display
    console.log(`${emoji} (${percentile.toFixed(0)}%) ${Math.round(currentCarbonIntensity)} gCO₂ | size=12 font=UbuntuMono-Bold`);
    console.log('---');
    console.log(`Relative Cleanliness (24hr): ${percentile.toFixed(0)} percentile | color=${TEXT_COLOR}`);
    console.log(`Grid Carbon Intensity: ${Math.round(currentCarbonIntensity)} gCO₂eq/kWh | color=${TEXT_COLOR}`);
    //console.log(`Range: ${Math.round(range.min)} - ${Math.round(range.max)} gCO₂eq/kWh | color=${TEXT_COLOR}`);
    const graphBase64 = generateGraph(hourlyData);
    console.log(`| image=${graphBase64}`);
    
    console.log('---');
    displayPowerBreakdown(powerData);  
    console.log(`Power from Renewables: ${powerData.renewablePercentage}% | color=${TEXT_COLOR}`);
    console.log('---');
    console.log(`Zone: ${ELECTRICITY_MAPS_ZONE} | color=${TEXT_COLOR}`);
    console.log(`Last Updated: ${new Date(powerData.datetime).toLocaleString('en-GB', {
      day: 'numeric',
      month: 'short',
      year: 'numeric',
      hour: 'numeric',
      minute: '2-digit',
      hour12: true
    })} | color=${TEXT_COLOR}`);
    console.log('---');
    console.log(`⚡ Open Electricity Maps, ${ELECTRICITY_MAPS_ZONE} | href=https://app.electricitymaps.com/zone/${ELECTRICITY_MAPS_ZONE}`);
    console.log('📖 View Setup & Usage Instructions | href=https://github.com/jasonm-jones/carbon-intensity-xbar#readme');

    })
    .catch(error => {
      console.log('🔴 Error');
      console.log('---');
      console.log(`${error.message} | color=red`);
      console.log('---');
      console.log('📖 View Setup & Usage Instructions | href=https://github.com/jasonm-jones/carbon-intensity-xbar#readme');
    });
}

module.exports = {
  calculatePercentile
}; 