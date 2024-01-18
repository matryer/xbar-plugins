//usr/bin/env go run $0 $@; exit
//<xbar.title>Options Call Prices</xbar.title>
//<xbar.version>1.0</xbar.version>
//<xbar.author>Aaron Edell</xbar.author>
//<xbar.author.github>aaronedell</xbar.author.github>
//<xbar.desc>Track Specific Option Call Quotes</xbar.desc>
//<xbar.image>https://i.imgur.com/3QYgZX0.png</xbar.image>
//<xbar.dependencies>go</xbar.dependencies>
//<xbar.abouturl>https://github.com/aaronedell/<xbar.abouturl>

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"github.com/m7shapan/njson"
)

type OptionsDetail struct {
	Strike    []string `njson:"optionChain.result.0.options.0.calls.#.strike"`
	LastPrice []string `njson:"optionChain.result.0.options.0.calls.#.lastPrice"`
}

func main() {

	//Get an API key and put it here (sign up for one for free at https://www.yahoofinanceapi.com)
	apikey := "XXXXXXXXXXXX"

	// Pick your strike price, date, and quote symbol (I've left some examples for you)
	os.Setenv("SYMBOL", "TSLA")
	os.Setenv("EXPIRATION_DATE", "1649894400")
	os.Setenv("STRIKE_PRICE", "975")
	symbol := os.Getenv("SYMBOL")
	expdate := os.Getenv("EXPIRATION_DATE")
	strikeprice := os.Getenv("STRIKE_PRICE")

	//API endpoint
	optionsURL := "https://yfapi.net/v7/finance/options/" + symbol + "?date=" + expdate

	//GET request
	client := &http.Client{}
	request, _ := http.NewRequest("GET", optionsURL, nil)
	request.Header.Set("X-API-KEY", apikey)
	response, err := client.Do(request)

	if err != nil {
		log.Fatal("Getting locations from the URL has failed for some reason")
	}

	//Wait until the end of this computer program to close this request
	defer response.Body.Close()

	//Convert the HTML type into a BYTE type
	responsebody, err := ioutil.ReadAll(response.Body)
	if err != nil {
		log.Fatalln(err)
	}

	//Unmarshall the BYTE into the structs
	var g OptionsDetail
	err = njson.Unmarshal(responsebody, &g)

	if err != nil {
		log.Fatalln(err)
	}

	//Iterate through all the API response data
	for i := 0; i < len(g.Strike); i++ {
		//fmt.Println(g.Strike[i])
		if g.Strike[i] == strikeprice {

			fmt.Println(symbol + " Quote: " + g.LastPrice[i] + " Strike: " + g.Strike[i])
		}

	}

}

//BYE
