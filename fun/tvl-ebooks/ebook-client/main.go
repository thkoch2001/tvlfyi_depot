package main

import (
	"crypto/tls"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net"
	"sync"
	"time"

	"github.com/go-redis/redis"
	"gopkg.in/irc.v3"
)

var messageBeat chan bool
var firstMessage chan bool
var client *irc.Client
var safeLock sync.Mutex

func main() {
	nick := flag.String("nick", "NONE", "the ircnick you want")
	from := flag.String("ip", "[::1]", "src address")
	flag.Parse()

	localAddrDialier := &net.Dialer{
		LocalAddr: &net.TCPAddr{
			IP:   net.ParseIP(*from),
			Port: 0,
		},
	}

	conn, err := tls.DialWithDialer(localAddrDialier, "tcp", "chat.freenode.net:6697", &tls.Config{})
	if err != nil {
		log.Fatalln(err)
	}

	messageBeat = make(chan bool)
	firstMessage = make(chan bool, 10)
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
			redisc.Set(fmt.Sprintf("alive-%s", *nick), "yes", time.Second*5)
		}
	}()

	if *nick == "NONE" {
		log.Fatalf("You must set a nick")
	}

	go func() {
		<-firstMessage
		for {
			psub := redisc.Subscribe(fmt.Sprintf("irc-%s", *nick))

			for {
				msg, err := psub.ReceiveMessage()
				if err != nil {
					break
				}
				client.WriteMessage(&irc.Message{
					Command: "PRIVMSG",
					Params: []string{
						"##tvl-ebooks",
						msg.Payload,
					},
				})
			}
			time.Sleep(time.Second * 10)
		}

	}()

	go func() {
		<-firstMessage
		for {
			psub := redisc.Subscribe(fmt.Sprintf("raw-irc-%s", *nick))

			for {
				msg, err := psub.ReceiveMessage()
				if err != nil {
					break
				}
				im := irc.Message{}
				err = json.Unmarshal([]byte(msg.Payload), &im)
				if err == nil {
					client.WriteMessage(&im)
				}
			}
			time.Sleep(time.Second * 10)
		}

	}()

	seenMsgBefore := false
	config := irc.ClientConfig{
		Nick: *nick,
		User: *nick,
		Name: fmt.Sprintf("%s Ebooks", *nick),
		Handler: irc.HandlerFunc(func(c *irc.Client, m *irc.Message) {
			b, _ := json.Marshal(m)
			log.Printf("%#v", string(b))

			messageBeat <- true

			if !seenMsgBefore {
				firstMessage <- true
				seenMsgBefore = true
			}
			res := redisc.Publish("ebook", string(b))
			if res.Err() != nil {
				log.Printf("Publish error! %#v", err)
			}
			if m.Command == "001" {
				// 001 is a welcome event, so we join channels there
				c.Write("JOIN ##tvl-ebooks")
			}
			// else if m.Command == "PRIVMSG" && c.FromChannel(m) {
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
	client = irc.NewClient(conn, config)
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
