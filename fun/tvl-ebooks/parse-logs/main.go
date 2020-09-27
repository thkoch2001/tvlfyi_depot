package main

import (
	"bufio"
	"fmt"
	"log"
	"math/rand"
	"os"
	"regexp"
	"strconv"
	"strings"

	"github.com/go-redis/redis"
)

type incomingIRC struct {
	Command string   `json:"Command"`
	Host    string   `json:"Host"`
	Name    string   `json:"Name"`
	Params  []string `json:"Params"`
	User    string   `json:"User"`
}

var quicklogMatch = regexp.MustCompile(`<(\w+)> (.+)`)

func main() {
	redisc := redis.NewClient(&redis.Options{
		Addr:     fmt.Sprintf("127.0.0.1:%d", 6379),
		Password: "", // no password set
		DB:       0,  // use default DB
	})

	fireaway := make(chan incomingIRC, 10)
	go func() {
		f, err := os.Open("tvl.txt")
		if err != nil {
			log.Printf("aaa %v", err)
			os.Exit(0)
		}

		bio := bufio.NewReader(f)
		for {
			line, _, err := bio.ReadLine()
			if err != nil {
				break
			}

			sline := string(line)

			offset := strings.Index(sline, "]")

			notime := sline[offset+1:]

			if quicklogMatch.MatchString(notime) {
				bits := quicklogMatch.FindAllStringSubmatch(notime, -1)
				if len(bits) != 0 {
					if len(bits[0]) != 0 {
						a := make([]string, 2)
						a[1] = bits[0][2]
						ic := incomingIRC{
							Name:   bits[0][1],
							Params: a,
						}
						log.Printf("aa %#v", ic)

						fireaway <- ic
					}
				}
			}

		}

	}()

	for msg := range fireaway {
		// Learn
		learnFromMessage(msg, redisc)
		// os.Exit(0)
	}
}

func generateMesasge(msg incomingIRC, redisc *redis.Client) string {
	text := msg.Params[1]
	username := msg.Name

	text = strings.ToLower(text)
	text = strings.Replace(text, ",", "", -1)
	text = strings.Replace(text, ",", "", -1)
	text = strings.Replace(text, ".", "", -1)
	text = strings.Replace(text, "!", "", -1)
	text = strings.Replace(text, "?", "", -1)

	words := strings.Split(text, " ")
	lastWord := propwords(username, words[0], redisc)
	outputMsg := words[0] + " " + lastWord + " "

	for {
		lastWord = propwords(username, words[0], redisc)
		if lastWord == "" || lastWord == "_END_" {
			return outputMsg
		}

		outputMsg += lastWord + " "
		if len(outputMsg) > 100 {
			return outputMsg
		}
	}
}

func propwords(username string, start string, redisc *redis.Client) string {
	userHash := redisc.HGetAll(fmt.Sprintf("%s-%s", username, start))
	userHashMap, err := userHash.Result()
	if err != nil {
		genericHash := redisc.HGetAll(fmt.Sprintf("generic-%s", start))
		userHashMap, err = genericHash.Result()
	}

	userIntHashMap, totalVectors := stringMaptoIntMap(userHashMap)
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

		redisc.HIncrBy(fmt.Sprintf("%s-%s", username, word), nextWord, 1)
		redisc.HIncrBy(fmt.Sprintf("generic-%s", word), nextWord, 1)
	}
}
