package main

import (
	"crypto/tls"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/go-redis/redis"
	"gopkg.in/irc.v3"
)

var messageBeat chan bool

func main() {
	conn, err := tls.Dial("tcp", "bnc.irccloud.com:6697", nil)
	if err != nil {
		log.Fatalln(err)
	}

	messageBeat = make(chan bool)
	go ircKeepalive()

	redisc := redis.NewClient(&redis.Options{
		Addr:     fmt.Sprintf("127.0.0.1:%d", 6379),
		Password: "", // no password set
		DB:       0,  // use default DB
	})

	go func() {
		for {
			time.Sleep(time.Second)
			r := redisc.Ping()
			if r.Err() != nil {
				redisc = redis.NewClient(&redis.Options{
					Addr:     fmt.Sprintf("127.0.0.1:%d", 6379),
					Password: "", // no password set
					DB:       0,  // use default DB
				})
			}
		}
	}()

	config := irc.ClientConfig{
		Nick: "Benjojo",
		Pass: os.Getenv("IRCCLOUD"),
		User: "Benjojo",
		Name: "Ben Cox",
		Handler: irc.HandlerFunc(func(c *irc.Client, m *irc.Message) {
			b, _ := json.Marshal(m)
			// log.Printf("%#v", string(b))

			messageBeat <- true
			res := redisc.Publish("irccloud", string(b))
			if res.Err() != nil {
				log.Printf("Publish error! %#v", err)
			}
			// if m.Command == "001" {
			// 	// 001 is a welcome event, so we join channels there
			// 	// c.Write("JOIN #bot-test-chan")
			// } else if m.Command == "PRIVMSG" && c.FromChannel(m) {
			// 	// // Create a handler on all messages.
			// 	// c.WriteMessage(&irc.Message{
			// 	// 	Command: "PRIVMSG",
			// 	// 	Params: []string{
			// 	// 		m.Params[0],
			// 	// 		m.Trailing(),
			// 	// 	},
			// 	// })
			// }
		}),
	}

	// Create the client
	client := irc.NewClient(conn, config)
	err = client.Run()
	if err != nil {
		log.Fatalln(err)
	}
}

func ircKeepalive() {
	tt := time.NewTimer(time.Second)
	lastPing := time.Now()
	for {
		select {
		case <-tt.C:
			if time.Since(lastPing) > time.Minute*5 {
				log.Fatalf("It's been too long since the last IRC message, blowing up")
			}
			break
		case <-messageBeat:
			lastPing = time.Now()
		}
	}

}
