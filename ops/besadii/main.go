// Copyright 2019-2020 Google LLC.
// SPDX-License-Identifier: Apache-2.0
//
// besadii is a small CLI tool that is invoked as a hook by various
// programs to cause CI-related actions.
//
// It supports the following modes & operations:
//
// Gerrit (ref-updated) hook:
// - Trigger Buildkite CI builds
// - Trigger SourceGraph (cs.tvl.fyi) repository index updates
//
// Buildkite (post-command) hook:
// - Submit CL verification status back to Gerrit
package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path"
	"strconv"
)

// buildTrigger represents the information passed to besadii when it
// is invoked as a Gerrit hook.
//
// https://gerrit.googlesource.com/plugins/hooks/+/HEAD/src/main/resources/Documentation/hooks.md
type buildTrigger struct {
	project   string
	ref       string
	commit    string
	submitter string
	email     string

	changeId *string
	patchset *string
}

type Author struct {
	Name  string `json:"name"`
	Email string `json:"email"`
}

// Build is the representation of a Buildkite build as described on
// https://buildkite.com/docs/apis/rest-api/builds#create-a-build
type Build struct {
	Commit string            `json:"commit"`
	Branch string            `json:"branch"`
	Author Author            `json:"author"`
	Env    map[string]string `json:"env"`
}

// BuildResponse is the representation of Buildkite's success response
// after triggering a build. This has many fields, but we only need
// one of them.
type buildResponse struct {
	WebUrl string `json:"web_url"`
}

// reviewInput is a struct representing the data submitted to Gerrit
// to post a review on a CL.
//
// https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#review-input
type reviewInput struct {
	Message                        string         `json:"message"`
	Labels                         map[string]int `json:"labels,omitempty"`
	OmitDuplicateComments          bool           `json:"omit_duplicate_comments"`
	IgnoreDefaultAttentionSetRules bool           `json:"ignore_default_attention_set_rules"`
	Tag                            string         `json:"tag"`
}

// updateGerrit posts a comment on a Gerrit CL to indicate the current build status.
func updateGerrit(review reviewInput, changeId, patchset string) {
	body, _ := json.Marshal(review)
	reader := ioutil.NopCloser(bytes.NewReader(body))

	url := fmt.Sprintf("https://cl.tvl.fyi/a/changes/%s/revisions/%s/review", changeId, patchset)
	req, err := http.NewRequest("POST", url, reader)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to create an HTTP request: %w", err)
		os.Exit(1)
	}

	req.SetBasicAuth("buildkite", gerritPassword())
	req.Header.Add("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		fmt.Errorf("failed to update CL on Gerrit: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		respBody, _ := ioutil.ReadAll(resp.Body)
		fmt.Fprintf(os.Stderr, "received non-success response from Gerrit: %s (%v)", respBody, resp.Status)
	} else {
		fmt.Printf("Added CI status comment on https://cl.tvl.fyi/c/depot/+/%s/%s", changeId, patchset)
	}
}

// Trigger a build of a given branch & commit on Buildkite
func triggerBuild(token string, trigger *buildTrigger) error {
	env := make(map[string]string)

	// Pass information about the originating Gerrit change to the
	// build, if it is for a patchset.
	//
	// This information is later used by besadii when invoked by Gerrit
	// to communicate the build status back to Gerrit.
	if trigger.changeId != nil && trigger.patchset != nil {
		env["GERRIT_CHANGE_ID"] = *trigger.changeId
		env["GERRIT_PATCHSET"] = *trigger.patchset
	}

	build := Build{
		Commit: trigger.commit,
		Branch: trigger.ref,
		Env:    env,
		Author: Author{
			Name:  trigger.submitter,
			Email: trigger.email,
		},
	}

	body, _ := json.Marshal(build)
	reader := ioutil.NopCloser(bytes.NewReader(body))

	req, err := http.NewRequest("POST", "https://api.buildkite.com/v2/organizations/tvl/pipelines/depot/builds", reader)
	if err != nil {
		return fmt.Errorf("failed to create an HTTP request: %w", err)
	}

	req.Header.Add("Authorization", "Bearer "+token)
	req.Header.Add("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		// This might indicate a temporary error on the Buildkite side.
		return fmt.Errorf("failed to send Buildkite request: %w", err)
	}
	defer resp.Body.Close()

	respBody, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return fmt.Errorf("failed to read Buildkite response body: %w", err)
	}

	if resp.StatusCode != http.StatusCreated {
		return fmt.Errorf("received non-success response from Buildkite: %s (%v)", respBody, resp.Status)
	}

	var buildResp buildResponse
	err = json.Unmarshal(respBody, &buildResp)
	if err != nil {
		return fmt.Errorf("failed to unmarshal build response: %w", err)
	}

	fmt.Printf("triggered build for ref %q at commit %q: %s", trigger.ref, trigger.commit, buildResp.WebUrl)

	// Report the status back to the Gerrit CL so that users can click
	// through to the running build.
	msg := fmt.Sprintf("Started build for patchset #%s of cl/%s: %s", *trigger.patchset, *trigger.changeId, buildResp.WebUrl)
	review := reviewInput{
		Message:               msg,
		OmitDuplicateComments: true,
		Tag:                   "autogenerated:buildkite~trigger",

		// Do not update the attention set for this comment.
		IgnoreDefaultAttentionSetRules: true,
	}
	updateGerrit(review, *trigger.changeId, *trigger.patchset)

	return nil
}

// Trigger a Sourcegraph repository index update on cs.tvl.fyi.
//
// https://docs.sourcegraph.com/admin/repo/webhooks
func triggerIndexUpdate(token string) error {
	req, err := http.NewRequest("POST", "https://cs.tvl.fyi/.api/repos/depot/-/refresh", nil)
	if err != nil {
		return err
	}

	req.Header.Add("Authorization", "token "+token)

	_, err = http.DefaultClient.Do(req)
	return err
}

// Gerrit passes more flags than we want, but Rob Pike decided[0] in
// 2013 that the Go art project will not allow users to ignore flags
// because he "doesn't like it". This function allows users to ignore
// flags.
//
// [0]: https://github.com/golang/go/issues/6112#issuecomment-66083768
func ignoreFlags(ignore []string) {
	var _ignore string
	for _, f := range ignore {
		flag.StringVar(&_ignore, f, "", "flag to ignore")
	}
}

// Extract the buildtrigger struct out of the flags passed to besadii
// when invoked as Gerrit's 'patchset-created' hook. This hook is used
// for triggering CI on in-progress CLs.
func buildTriggerFromPatchsetCreated() *buildTrigger {
	// Information that needs to be returned
	var trigger buildTrigger

	// Information that is only needed for parsing
	var targetBranch string

	flag.StringVar(&trigger.project, "project", "", "Gerrit project")
	flag.StringVar(&trigger.commit, "commit", "", "commit hash")
	flag.StringVar(&trigger.email, "change-owner", "", "Owner email")
	flag.StringVar(&trigger.submitter, "change-owner-username", "", "Owner username")
	flag.StringVar(trigger.changeId, "change", "", "CL ID")
	flag.StringVar(trigger.patchset, "patchset", "", "Patchset ID")

	flag.StringVar(&targetBranch, "branch", "", "CL target branch")

	// patchset-created also passes various flags which we don't need.
	ignoreFlags([]string{"kind", "change-url", "topic", "uploader", "uploader-username"})

	flag.Parse()

	// If the patchset is not for depot@canon then we can ignore it. It
	// might be some other kind of change (refs/meta/config or
	// Gerrit-internal), but it is not an error.
	if trigger.project != "depot" || targetBranch != "canon" {
		return nil
	}

	// Construct the CL ref from which the build should happen.
	changeId, _ := strconv.Atoi(*trigger.changeId)
	trigger.ref = fmt.Sprintf(
		"refs/changes/%02d/%s/%s",
		changeId%100, trigger.changeId, trigger.patchset,
	)

	return &trigger
}

// Extract the buildtrigger struct out of the flags passed to besadii
// when invoked as Gerrit's 'change-merged' hook. This hook is used
// for triggering canon builds after change submission.
func buildTriggerFromChangeMerged() *buildTrigger {
	// Information that needs to be returned
	var trigger buildTrigger

	// Information that is only needed for parsing
	var targetBranch string

	flag.StringVar(&trigger.project, "project", "", "Gerrit project")
	flag.StringVar(&trigger.commit, "newrev", "", "Commit hash")
	flag.StringVar(&trigger.submitter, "change-owner-username", "", "Owner username")
	flag.StringVar(&trigger.email, "change-owner", "", "Owner email")

	flag.StringVar(&targetBranch, "branch", "", "CL target branch")

	// Ignore extra flags passed by change-merged
	ignoreFlags([]string{"change", "change-url", "topic", "submitter", "submitter-username", "commit"})

	flag.Parse()

	// Skip builds for anything other than depot@canon
	if trigger.project != "depot" || trigger.ref != "canon" {
		return nil
	}

	trigger.ref = "refs/heads/canon"

	return &trigger
}

func gerritHookMain(trigger *buildTrigger) {
	if trigger == nil {
		// The hook was not for something we care about.
		os.Exit(0)
	}

	buildkiteToken, err := ioutil.ReadFile("/etc/secrets/buildkite-besadii")
	if err != nil {
		log.Fatalf("buildkite token could not be read: %s", err)
	}

	sourcegraphToken, err := ioutil.ReadFile("/etc/secrets/sourcegraph-token")
	if err != nil {
		log.Fatalf("sourcegraph token could not be read: %s", err)
	}

	err = triggerBuild(string(buildkiteToken), trigger)
	if err != nil {
		log.Printf("failed to trigger Buildkite build: %s", err)
	}

	err = triggerIndexUpdate(string(sourcegraphToken))
	if err != nil {
		log.Printf("failed to trigger sourcegraph index update: %s", err)
	}
	log.Print("triggered sourcegraph index update")
}

func gerritPassword() string {
	gerritPassword, err := ioutil.ReadFile("/etc/secrets/buildkite-gerrit")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Gerrit password could not be read: %s", err)
		os.Exit(1)
	}

	return string(gerritPassword)
}

func postCommandMain() {
	changeId := os.Getenv("GERRIT_CHANGE_ID")
	patchset := os.Getenv("GERRIT_PATCHSET")

	if changeId == "" || patchset == "" {
		// If these variables are unset, but the hook was invoked, the
		// build was most likely for a branch and not for a CL - no status
		// needs to be reported back to Gerrit!
		fmt.Println("This isn't a CL build, nothing to do. Have a nice day!")
		return
	}

	if os.Getenv("BUILDKITE_LABEL") != ":duck:" {
		// this is not the build stage, don't do anything.
		return
	}

	var verified int
	var verb string

	if os.Getenv("BUILDKITE_COMMAND_EXIT_STATUS") == "0" {
		verified = 1 // Verified: +1 in Gerrit
		verb = "passed"
	} else {
		verified = -1
		verb = "failed"
	}

	msg := fmt.Sprintf("Build of patchset %s %s: %s", patchset, verb, os.Getenv("BUILDKITE_BUILD_URL"))
	review := reviewInput{
		Message:               msg,
		OmitDuplicateComments: true,
		Labels: map[string]int{
			"Verified": verified,
		},

		// Update the attention set if we are failing this patchset.
		IgnoreDefaultAttentionSetRules: verified == 1,

		Tag: "autogenerated:buildkite~result",
	}
	updateGerrit(review, changeId, patchset)
}

func main() {
	log.Print("besadii invoked with %v", os.Args)

	bin := path.Base(os.Args[0])

	if bin == "patchset-created" {
		trigger := buildTriggerFromPatchsetCreated()
		gerritHookMain(trigger)
	} else if bin == "change-merged" {
		trigger := buildTriggerFromChangeMerged()
		gerritHookMain(trigger)
	} else if bin == "post-command" {
		postCommandMain()
	} else {
		fmt.Fprintf(os.Stderr, "besadii does not know how to be invoked as %q, sorry!", bin)
		os.Exit(1)
	}
}
