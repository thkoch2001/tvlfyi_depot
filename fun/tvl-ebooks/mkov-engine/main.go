package main

import (
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"strconv"
	"strings"
	"time"

	"github.com/go-redis/redis"
)

type incomingIRC struct {
	Command string   `json:"Command"`
	Host    string   `json:"Host"`
	Name    string   `json:"Name"`
	Params  []string `json:"Params"`
	User    string   `json:"User"`
}

var supressionUsernames map[string]bool
var noMkov map[string]bool

func main() {
	redisc := redis.NewClient(&redis.Options{
		Addr:     fmt.Sprintf("127.0.0.1:%d", 6379),
		Password: "", // no password set
		DB:       0,  // use default DB
	})

	fireaway := make(chan incomingIRC, 10)
	supressionUsernames = make(map[string]bool)

	supressionList := redisc.HGetAll("supressionList")
	supressionListA, _ := supressionList.Result()

	supressionListMap, _ := stringMaptoIntMap(supressionListA)
	for v, _ := range supressionListMap {
		supressionUsernames[v] = true
		supressionUsernames[strings.ToLower(v)] = true
	}

	noMkov = make(map[string]bool)

	noMkovRedis := redisc.HGetAll("nomkov")
	noMkovRedisA, _ := noMkovRedis.Result()

	noMkovMap, _ := stringMaptoIntMap(noMkovRedisA)
	for v, _ := range noMkovMap {
		noMkov[v] = true
		noMkov[strings.ToLower(v)] = true
	}

	go func() {
		for {
			irccloudFeed := redisc.Subscribe("irccloud")
			for {
				msg, err := irccloudFeed.ReceiveMessage()
				if err != nil {
					break
				}
				imsg := incomingIRC{}
				err = json.Unmarshal([]byte(msg.Payload), &imsg)
				if err != nil {
					log.Printf("Json decoding error from irccloud feed %s", err)
					continue
				}

				if imsg.Command == "PRIVMSG" {
					if len(imsg.Params) == 2 {
						if imsg.Params[0] == "##tvl" || imsg.Params[0] == "##tvlbot" {
							fireaway <- imsg
						}
					}
				}
			}
			time.Sleep(time.Second)
		}
	}()

	for msg := range fireaway {
		// Learn
		learnFromMessage(msg, redisc)
		msg2 := generateMesasge(msg, redisc)

		// Check if we have a active log in for that user
		ttl := redisc.TTL("alive-" + msg.Name + "-eb")
		ttld, err := ttl.Result()
		if err == nil {
			redisc.Publish("irc-"+msg.Name+"-eb", msg2)
			if ttld == 0 || ttld.Seconds() == -2 {
				redisc.Publish("irc-tvlebooks-eb", "<"+fmt.Sprintf("%s.%s", string(msg.Name[0]), msg.Name[1:])+"-eb> "+msg2)
			}
		} else {
			redisc.Publish("irc-tvlebooks-eb", "<"+fmt.Sprintf("%s.%s", string(msg.Name[0]), msg.Name[1:])+"-eb> "+msg2)
		}
	}
}

func generateMesasge(msg incomingIRC, redisc *redis.Client) string {
	text := msg.Params[1]
	username := strings.ToLower(msg.Name)
	supressionUsernames[username] = true
	supressionUsernames[username+":"] = true
	supressionUsernames[msg.Name] = true
	supressionUsernames[msg.Name+":"] = true
	redisc.HIncrBy("supressionList", msg.Name, 1)

	text = strings.ToLower(text)
	text = strings.Replace(text, ",", "", -1)
	text = strings.Replace(text, ",", "", -1)
	text = strings.Replace(text, ".", "", -1)
	text = strings.Replace(text, "!", "", -1)
	text = strings.Replace(text, "?", "", -1)

	words := strings.Split(text, " ")
	lastWord := propwords(msg.Name, words[0], redisc)

	if noMkov[username] {
		lastWord = blockoutWord(lastWord)
		words[0] = blockoutWord(words[0])
	}

	if supressionUsernames[words[0]] {
		if len(words[0]) < 2 {
			words[0] = "vee"
		}
		words[0] = fmt.Sprintf("%s.%s", string(words[0][0]), words[0][1:])
	}

	if lastWord == "_END_" {
		if noMkov[username] {
			return blockoutWord(words[0])
		}
		return words[0]
	}
	outputMsg := words[0] + " " + lastWord + " "

	for {
		lastWord = propwords(username, lastWord, redisc)
		if lastWord == "" || lastWord == "_END_" {
			return outputMsg
		}

		if noMkov[username] {
			lastWord = blockoutWord(lastWord)
		}

		if supressionUsernames[lastWord] {
			if len(lastWord) < 2 {
				lastWord = "vee"
			}
			lastWord = fmt.Sprintf("%s.%s", string(lastWord[0]), lastWord[1:])
		}

		outputMsg += lastWord + " "
		if len(outputMsg) > 100 {
			return outputMsg
		}
	}
}

func blockoutWord(in string) string {
	block := ""
	for i := 0; i < len(in); i++ {
		block += "█"
	}
	return block
}

func propwords(username string, start string, redisc *redis.Client) string {
	userHash := redisc.HGetAll(fmt.Sprintf("%s-%s", username, start))
	userHashMap, err := userHash.Result()
	if err != nil {
		genericHash := redisc.HGetAll(fmt.Sprintf("generic-%s", start))
		userHashMap, err = genericHash.Result()
	}

	userIntHashMap, totalVectors := stringMaptoIntMap(userHashMap)
	if totalVectors == 0 {
		return ""
	}
	targetRand := rand.Intn(totalVectors)
	progresRand := 0

	for k, v := range userIntHashMap {
		progresRand += v
		if targetRand > progresRand {
			return k
		}
	}

	for k, _ := range userIntHashMap {
		return k
	}

	return ""
}

func stringMaptoIntMap(in map[string]string) (outMap map[string]int, total int) {
	outMap = make(map[string]int)

	for k, v := range in {
		i, err := strconv.ParseInt(v, 10, 64)
		if err != nil {
			continue
		}
		total += int(i)
		outMap[k] = int(i)
	}

	return outMap, total
}

func learnFromMessage(msg incomingIRC, redisc *redis.Client) {
	text := msg.Params[1]

	text = strings.ToLower(text)
	text = strings.Replace(text, ",", "", -1)
	text = strings.Replace(text, ",", "", -1)
	text = strings.Replace(text, ".", "", -1)
	text = strings.Replace(text, "!", "", -1)
	text = strings.Replace(text, "?", "", -1)

	words := strings.Split(text, " ")
	username := msg.Name

	for k, word := range words {
		// HINCRBY myhash field 1
		nextWord := "_END_"
		if len(words)-1 != k {
			nextWord = words[k+1]
		}

		if !noMkov[username] {
			redisc.HIncrBy(fmt.Sprintf("%s-%s", username, word), nextWord, 1)
		}
		redisc.HIncrBy(fmt.Sprintf("generic-%s", word), nextWord, 1)
	}
}
