/*
Salyangoz.me BitBar Plugin
# <bitbar.title>Salyangoz Recent Shared</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>s4l1h</bitbar.author>
# <bitbar.author.github>s4l1h</bitbar.author.github>
# <bitbar.desc>salyangoz.me bitbar plugin.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/zSqmkqs.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>http://www.salyangoz.me/</bitbar.abouturl>
*/
package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strconv"
	"time"
)

// Posts Nesnesi
type Posts struct {
	Posts  []Post `json:"posts"` // Post'lar
	result []byte // Response Sonuçu
	URL    string // Postların çekileceği bağlantı
}

// Post Nesnesi
type Post struct {
	ID         int    `json:"id"`
	URL        string `json:"url"`
	Title      string `json:"title"`
	VisitCount int    `json:"visit_count"`
	UpdatedAt  string `json:"updated_at"`
	User       User   `json:"user"`
}

// User Post User Nesnesi
type User struct {
	UserName     string `json:"user_name"`
	ProfileImage string `json:"profile_image"`
}

// Get postları almak için sunucuya istek yapar
func (p *Posts) Get() error {
	result, err := p.getURL(p.URL)
	if err != nil {
		return errors.New("Datalar Alınamadı")
	}
	p.result = result
	return nil
}

// Parse byte formatındaki json datasını Post objesine çevirir
func (p *Posts) Parse() error {
	if err := json.Unmarshal(p.result, p); err != nil {
		return errors.New("Alınan data parse edilmedi:")
	}
	return nil
}

// CheckLength bakalım post varmı
func (p *Posts) CheckLength() error {
	if len(p.Posts) == 0 {
		return errors.New("Post Bulunamadı")
	}
	return nil
}

// getURL Bu fonskiyon verilen url'yi çekip byte formatında return eder
func (p *Posts) getURL(url string) ([]byte, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	return body, nil
}

// POSTURL post'ların çekileceği adres
const POSTURL string = "http://salyangoz.me/recent.json"

// getURL Bu fonskiyon verilen url'yi çekip byte formatında return eder
func getURL(url string) ([]byte, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	return body, nil
}

// fromNow Bu fonksiyon string formatında aldığı post zamanınından şimdiki zamana kadar geçen süreyi hesaplar.
// Bu hesaplama için öncelikle zamanlar unixtime çevrilir.
func fromNow(newsTime string) string {

	newsDate, _ := time.Parse(
		time.RFC3339,
		newsTime)
	newsUnixDate := newsDate.Unix()
	nowUnixDate := time.Now().UTC().Unix()

	fark := (nowUnixDate - newsUnixDate)
	sonuc := ""
	if fark < 3600 {
		sonuc = strconv.FormatInt(fark/60, 10) + " minute"
	} else if fark < 86400 {
		sonuc = strconv.FormatInt(fark/3600, 10) + " hour"
	} else {
		sonuc = strconv.FormatInt(fark/86400, 10) + " day"
	}
	return sonuc
}

func main() {
	// Yeni bir post objesi
	var posts = new(Posts)
	posts.URL = POSTURL
	// Sunucudan json datasını alalım
	checkErr(posts.Get())
	// Parse edelim
	checkErr(posts.Parse())
	// Bakalım parse sonucu post varmı
	checkErr(posts.CheckLength())
	// İsteğe göre ekrana yazalım
	fmt.Println(`Salyangoz Recent Shared
---
:arrows_counterclockwise: Refresh | href=bitbar://refreshPlugin?name=salyangoz* color=#66cc00 size=16
`)
	for _, post := range posts.Posts {
		fmt.Printf("%s \n-- :bust_in_silhouette: %s :clock1: %s ago :dart: (%s) views | href=%s emojize=true\n",
			post.Title,
			post.User.UserName,
			fromNow(post.UpdatedAt),
			strconv.Itoa(post.VisitCount),
			post.URL,
		)
	}
}

// checkErr eğer gönderilen parametre nil değilse durdurup ekrana mesajı yazar
func checkErr(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
}
