#!/usr/bin/env gorun

// <xbar.title>Netatmo Weather Station</xbar.title>
// <xbar.version>v0.1</xbar.version>
// <xbar.author>Shinji Tanaka</xbar.author>
// <xbar.author.github>stanaka</xbar.author.github>
// <xbar.desc>Display information sent by Netatmo Weather Station</xbar.desc>
// <xbar.dependencies>go, gorun</xbar.dependencies>
// <xbar.image>https://raw.githubusercontent.com/stanaka/netatmo-xbar-plugin/main/netatmo-xbar-plugin.png</xbar.image>
// <xbar.abouturl>https://github.com/stanaka/netatmo-xbar-plugin/</xbar.abouturl>

// Variables

// <xbar.var>string(VAR_CLIENT_ID="client_id"): client id.</xbar.var>
// <xbar.var>string(VAR_CLIENT_SECRET="secret"): client secret.</xbar.var>
// <xbar.var>string(VAR_REFRESH_TOKEN="token"): client secret.</xbar.var>
// <xbar.var>select(VAR_STYLE="normal"): Which style to use. [small, normal, big]</xbar.var>

package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
)

type ResponseAccessToken struct {
	Scope        []string `json:"scope"`
	AccessToken  string   `json:"access_token"`
	RefreshToken string   `json:"refresh_token"`
	ExpiresIn    int      `json:"expires_in"`
}

func getAccessToken() string {

	clientId := os.Getenv("VAR_CLIENT_ID")
	clientSecret := os.Getenv("VAR_CLIENT_SECRET")
	refreshToken := os.Getenv("VAR_REFRESH_TOKEN")

	uri := "https://api.netatmo.com/oauth2/token"
	resp, err := http.PostForm(uri,
		url.Values{
			"grant_type":    {"refresh_token"},
			"client_id":     {clientId},
			"client_secret": {clientSecret},
			"refresh_token": {refreshToken},
		})
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	byteArray, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	// fmt.Println(string(byteArray))

	var response ResponseAccessToken
	if err := json.Unmarshal(byteArray, &response); err != nil {
		log.Fatal(err)
	}

	return response.AccessToken
}

type Data struct {
	Temperature   *float64 `json:"Temperature"`
	Co2           *float64 `json:"CO2"`
	Humidity      *float64 `json:"Humidity"`
	Noise         *float64 `json:"Noise"`
	Pressure      *float64 `json:"Pressure"`
	TempTrend     *string  `json:"temp_trend"`
	PressureTrend *string  `json:"pressure_trend"`
}

func (d Data) printOneline() string {
	var s string
	if d.Temperature != nil {
		s += fmt.Sprintf("%.1f°C ", *d.Temperature)
	}
	if d.Co2 != nil {
		s += fmt.Sprintf("%.0fppm ", *d.Co2)
	}
	if d.Humidity != nil {
		s += fmt.Sprintf("%.0f%% ", *d.Humidity)
	}
	return s
}

func (d Data) print() string {
	var s string
	if d.Temperature != nil {
		s += fmt.Sprintf("Temp: %.1f °C\n", *d.Temperature)
	}
	if d.Co2 != nil {
		s += fmt.Sprintf("CO2: %.0f ppm\n", *d.Co2)
	}
	if d.Humidity != nil {
		s += fmt.Sprintf("Humid: %.0f %%\n", *d.Humidity)
	}
	return s
}

type Module struct {
	Id            string `json:"_id"`
	ModuleName    string `json:"module_name"`
	DashboardData Data   `json:"dashboard_data"`
}

type Device struct {
	Id            string   `json:"_id"`
	ModuleName    string   `json:"module_name"`
	DashboardData Data     `json:"dashboard_data"`
	Modules       []Module `json:"modules"`
}

type Body struct {
	Devices []Device `json:"devices"`
}

type Response struct {
	Body Body `json:"body"`
}

func main() {
	accessToken := getAccessToken()

	url := "https://api.netatmo.com/api/getstationsdata?get_favorites=false"
	req, _ := http.NewRequest("GET", url, nil)
	req.Header.Set("Authorization", "Bearer "+accessToken)
	req.Header.Set("accept", "application/json")

	client := new(http.Client)
	resp, err := client.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	byteArray, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	var response Response
	if err := json.Unmarshal(byteArray, &response); err != nil {
		log.Fatal(err)
	}

	if len(response.Body.Devices) == 0 {
		fmt.Println(string(byteArray))
		log.Fatal("no device found")
	}

	fmt.Println(response.Body.Devices[0].DashboardData.printOneline())
	fmt.Println("---")
	fmt.Printf("%s | color=#ff0000\n", response.Body.Devices[0].ModuleName)
	fmt.Print(response.Body.Devices[0].DashboardData.print())
	for _, m := range response.Body.Devices[0].Modules {
		fmt.Printf("%s | color=#ff0000\n", m.ModuleName)
		fmt.Print(m.DashboardData.print())
	}
}
