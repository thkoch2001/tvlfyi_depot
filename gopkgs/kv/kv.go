// Supporting reading and writing key-value pairs to disk.
package kv

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"path"
)

// Return the decoded store from disk.
func getStore(storePath string) map[string]interface{} {
	b, err := ioutil.ReadFile(path.Join(storePath, "kv.json"))
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
func Set(storePath string, key string, value interface{}) error {
	state := getStore(storePath)
	state[key] = value
	b, err := json.Marshal(state)
	if err != nil {
		log.Fatal("Could not encode state as JSON: ", err)
	}
	return ioutil.WriteFile(path.Join(storePath, "kv.json"), b, 0644)
}

// Get `key` from the store.
func Get(storePath string, key string) interface{} {
	return getStore(path.Join(storePath, "kv.json"))[key]
}
