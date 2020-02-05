package main

import (
	"fmt"
	"net/http"
)

// TODO: Support a version of this function that doesn't need the token
// parameter.
func monzoGet(token, string, endpoint string) {
	client := &http.Client{}
	req, err := http.NewRequest("GET", fmt.Sprintf("https://api.monzo.com/%s", endpoint), nil)
	failOn(err)
	req.Header.Add("Authorization", fmt.Sprintf("Bearer %s", token))
	res, err := client.Do(req)
	failOn(err)
	fmt.Println(res)
}
