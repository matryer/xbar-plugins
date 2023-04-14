#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" php

<?php
	// NOTE: You may need #!/usr/bin/php above depending on where you have php installed.

	// xbar metadata
	// <xbar.title>Metal Prices</xbar.title>
	// <xbar.version>v1.0</xbar.version>
	// <xbar.author>Brendon Cheves</xbar.author>
	// <xbar.author.github>misfitius</xbar.author.github>
	// <xbar.desc>Fetches metal prices in USD</xbar.desc>
	// <xbar.dependencies>php >= 5.3 (with --enable-intl and --with-curl)</xbar.dependencies>

	// xbar variables
	// <xbar.var>select(VAR_API="metals.live"): [metals.live, metals-API.com, live-metal-prices.p.rapidapi.com]</xbar.var>
	// <xbar.var>string(VAR_API_KEY=""):  The API key for metals-API.com or live-metal-prices.p.rapidapi.com</xbar.var>

	$output = 'Metals' . "\n";
	$output .= '---' . "\n";

	$fmt = new NumberFormatter( 'en-US', NumberFormatter::CURRENCY );

	$ch = curl_init();
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

	$api = getenv('VAR_API');
	$api_key = getenv('VAR_API_KEY');

	if(strcmp($api, 'metals.live') == 0)
	{
		// metals.live does not currently require a key and is therefore set as default

		curl_setopt($ch, CURLOPT_URL, "https://api.metals.live/v1/spot");
		$json = curl_exec($ch);

		$metals_array = json_decode($json, true);
		// last element is a timestamp we don't care about
		array_pop($metals_array);

		foreach($metals_array as $key => $name_price_array)
		{
			foreach($name_price_array as $name => $price)
			{
				$output .= ucfirst($name) . ' ' . $fmt->formatCurrency($price, 'USD') . "\n";
			}
		}

		// ---------------------------------------------------------------------------------
		// Include this section if you want commodity metals such as Copper, Lead, Zinc, etc.
		/*
		$output .= '---' . "\n";

		curl_setopt($ch, CURLOPT_URL, "https://api.metals.live/v1/spot/commodities");
		$json = curl_exec($ch);

		$metals_array = json_decode($json, true);
		// last element is a timestamp we don't care about
		array_pop($metals_array);

		foreach($metals_array as $key => $name_price_array)
		{
			foreach($name_price_array as $name => $price)
			{
				$output .= ucfirst($name) . ' ' . $fmt->formatCurrency($price, 'USD') . "\n";
			}
		}
		*/
		// ---------------------------------------------------------------------------------

		$output .= '---' . "\n";
		$output .= 'Price data from metals.live' . "\n";
		$output .= 'Refresh now | refresh=true';
	}
	else if(strcmp($api, 'metals-API.com') == 0)
	{
		curl_setopt($ch, CURLOPT_URL, "https://metals-api.com/api/latest?access_key=" . $api_key);
		$json = curl_exec($ch);

		$metals_array = json_decode($json, true);

		$output .= 'Gold ' . $fmt->formatCurrency(1/$metals_array['rates']['XAU'], 'USD') . "\n";
		$output .= 'Platinum ' . $fmt->formatCurrency(1/$metals_array['rates']['XPT'], 'USD') . "\n";
		$output .= 'Silver ' . $fmt->formatCurrency(1/$metals_array['rates']['XAG'], 'USD') . "\n";
		$output .= 'Palladium ' . $fmt->formatCurrency(1/$metals_array['rates']['XPD'], 'USD') . "\n";
		$output .= '---' . "\n";
		$output .= 'Price data from metals-API.com' . "\n";
		$output .= 'Refresh now | refresh=true';
	}
	else if(strcmp($api, 'live-metal-prices.p.rapidapi.com') == 0)
	{
		curl_setopt($ch, CURLOPT_HTTPHEADER,
			[
				"x-rapidapi-host: live-metal-prices.p.rapidapi.com",
				"x-rapidapi-key: " . $api_key
			]);

		curl_setopt($ch, CURLOPT_URL, "https://live-metal-prices.p.rapidapi.com/v1/latest/XAU,XAG,PL,PA");
		$json = curl_exec($ch);

		$metals_array = json_decode($json, true);

		$output .= 'Gold ' . $fmt->formatCurrency($metals_array['rates']['XAU'], 'USD') . "\n";
		$output .= 'Platinum ' . $fmt->formatCurrency($metals_array['rates']['PL'], 'USD') . "\n";
		$output .= 'Silver ' . $fmt->formatCurrency($metals_array['rates']['XAG'], 'USD') . "\n";
		$output .= 'Palladium ' . $fmt->formatCurrency($metals_array['rates']['PA'], 'USD') . "\n";
		$output .= '---' . "\n";
		$output .= 'Price data from live-metal-prices.p.rapidapi.com' . "\n";
		$output .= 'Refresh now | refresh=true';
	}

	curl_close($ch);

	echo $output;

