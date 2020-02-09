// Supporting reading and writing key-value pairs to disk.
package kv

import (
	"encoding/json"
	"io/ioutil"
	"log"
)

const storePath = "./kv.json"

// Return the decoded store from disk.
func getStore() map[string]interface{} {
	b, err := ioutil.ReadFile(storePath)
	if err != nil {
		log.Fatal("Could not read store: ", err)
	}
	var state map[string]interface{}
	err = json.Unmarshal(b, &state)
	if err != nil {
		log.Fatal("Could not decode store as JSON: ", err)
	}
	return state
}

// Set `key` to `value` in the store.
func Set(key string, value interface{}) error {
	state := getStore()
	state[key] = value
	b, err := json.Marshal(state)
	if err != nil {
		log.Fatal("Could not encode state as JSON: ", err)
	}
	return ioutil.WriteFile(storePath, b, 0644)
}

// Get `key` from the store.
func Get(key string) interface{} {
	return getStore()[key]
}
