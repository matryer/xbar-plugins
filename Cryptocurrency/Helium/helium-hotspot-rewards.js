#!/usr/bin/env /usr/local/bin/node

// <xbar.title>Helium Hotspot Rewards Aggregator</xbar.title>
// <xbar.description>Sums the rewards for a given list of Helium Hotspot addresses</xbar.description>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Joseph Schultz</xbar.author>
// <xbar.author.github>acupofjose</xbar.author.github>
// <xbar.dependencies>node</xbar.dependencies>

// <xbar.var>string(VAR_HOTSPOT_ADDRESSES=""): Helium Hotspot Addresses (comma separated).</xbar.var>
// <xbar.var>select(VAR_TIMEFRAME="24h"): Timeframe. [24h, 48h, 1w, 1m]</xbar.var>

const https = require("https")
const HOTSPOT_ADDRESSES = process.env.VAR_HOTSPOT_ADDRESSES
const TIMEFRAME = process.env.VAR_TIMEFRAME || "24h"

if (!HOTSPOT_ADDRESSES) {
  console.log(`Specify Addresses in Options.`)
  process.exit(0)
}

/**
 * Promisified wrapper for Node's Request API
 * @param {string} url
 * @returns
 */
async function get(url) {
  return new Promise((resolve, reject) => {
    https
      .get(url, (response) => {
        let str = ""
        response.on("data", (chunk) => (str += chunk))
        response.on("end", () => {
          resolve(JSON.parse(str))
        })
        response.on("error", (err) => reject(err))
      })
      .end()
  })
}

/**
 * Creates a Date object and calculates a negative offset based on timeframe
 * @returns ISO String for TIMEFRAME
 */
function getISOForTimeframe() {
  const date = new Date()

  switch (TIMEFRAME) {
    case "24h":
      date.setDate(date.getDate() - 1)
      break
    case "48h":
      date.setDate(date.getDate() - 2)
      break
    case "1w":
      date.setDate(date.getDate() - 7)
      break
    case "1m":
      date.setDate(date.getDate() - 30)
      break
  }

  return date.toISOString()
}

/**
 * Encodes an object into HTTP query params
 * @param {[key:string]: string|number|bool} obj
 * @returns encoded string
 */
const encodeObj = function (obj) {
  var str = []
  for (var p in obj)
    if (obj.hasOwnProperty(p)) {
      str.push(encodeURIComponent(p) + "=" + encodeURIComponent(obj[p]))
    }
  return str.join("&")
}

/**
 * Requests a hotspot's info
 * @param {string} address
 * @returns
 */
const getHotspot = async (address) => get(`https://api.helium.io/v1/hotspots/${address}`)

/**
 * Requests a hotspot's rewards for a specified timeframe
 * @param {string} address
 * @returns
 */
const getHotspotRewards = async (address) =>
  get(`https://api.helium.io/v1/hotspots/${address}/rewards/sum?${encodeObj({ min_time: getISOForTimeframe() })}`)

/**
 * Requests a hotspot's witnesses
 * @param {string} address
 * @returns
 */
const getHotspotWitnesses = async (address) => get(`https://api.helium.io/v1/hotspots/${address}/witnesses`)

/**
 * Requests a hotspot's witnessed hotspots
 * @param {string} address
 * @returns
 */
const getHotspotWitnessed = async (address) => get(`https://api.helium.io/v1/hotspots/${address}/witnessed`)

/**
 * Requests the oracle's current price and converts to USD
 * @returns {number} current hotspot price in USD
 */
const getOraclePriceInUSD = async () => {
  const result = await get(`https://api.helium.io/v1/oracle/prices/current`)
  return (result.data.price / 100000000).toFixed(2)
}

/**
 * Runs API requests for each address and returns an aggregate object of them.
 * @param {string} address
 * @returns
 */
const aggregateInfoForAddress = async (address) => {
  const info = await getHotspot(address)
  const rewards = await getHotspotRewards(address)
  const witnesses = await getHotspotWitnesses(address)
  const witnessed = await getHotspotWitnessed(address)

  const rewardsTotal = rewards.data.total

  return { address, info, rewards, witnessed, witnesses, rewardsTotal }
}

/**
 * Logs a submenu item for a single address
 * @param {*} param: response from {aggregateInfoForAddress}
 */
const echoAddressInfo = ({ address, info, rewards, witnessed, witnesses, rewardsTotal }) => {
  console.log(
    `[${rewardsTotal.toFixed(2)} HNT] ${
      info.data.name
    } | color=#fff | href=https://explorer.helium.com/hotspots/${address}`
  )
  console.log(`--Online: ${info.data.status.online === "online" ? "TRUE" : "FALSE"}`)
  console.log(`--Scale: ${info.data.reward_scale.toFixed(3)}`)
  console.log(`--Witnesses: ${witnesses.data.length}`)
  console.log(`--Witnessed: ${witnessed.data.length}`)
  console.log(`--Helium Explorer | href=https://explorer.helium.com/hotspots/${address}`)
}

/**
 * Main loop
 */
async function main() {
  let sum = 0.0
  let addresses = []
  const oraclePrice = await getOraclePriceInUSD()

  // Loop over addresses from ENV and aggregate their info
  for (const address of HOTSPOT_ADDRESSES.split(",")) {
    const result = await aggregateInfoForAddress(address)
    addresses.push(result)
  }

  // Sum total rewards for all addresses
  for (const address of addresses) {
    sum = sum + address.rewardsTotal
  }

  // Construct output
  console.log(`${sum.toFixed(2)} HNT / \$${(sum * oraclePrice).toFixed(2)}`)
  console.log(`---`)

  for (const address of addresses) {
    echoAddressInfo(address)
  }

  console.log(`Oracle Price: \$${oraclePrice}`)
  console.log(`Last Updated: ${new Date().toLocaleTimeString()}`)
  console.log(`CoinMarketCap (HNT) | href=https://coinmarketcap.com/currencies/helium`)
}

// Run it!
main()
