package gerritevents

import (
	"encoding/json"
	"fmt"
)

var events = map[string]func() Event{}

func registerEvent(e func() Event) {
	t := e().EventType()
	if _, ok := events[t]; ok {
		panic(fmt.Sprintf("%s already registered", t))
	}
	events[t] = e
}

// These events are taken from https://cl.tvl.fyi/Documentation/cmd-stream-events.html.

// Event is implemented by Gerrit event structs.
type Event interface {
	EventType() string
}

type simpleEvent struct {
	Type string `json:"type"`
}

// Parse parses a Gerrit event from JSON.
func Parse(bs []byte) (Event, error) {
	var s simpleEvent
	if err := json.Unmarshal(bs, &s); err != nil {
		return nil, fmt.Errorf("unmarshalling %q as Gerrit Event: %v", string(bs), err)
	}
	ef, ok := events[s.Type]
	if !ok {
		return nil, fmt.Errorf("unknown event type %q", s.Type)
	}
	e := ef()
	if err := json.Unmarshal(bs, e); err != nil {
		return nil, fmt.Errorf("unmarshalling %q as Gerrit Event %q: %v", string(bs), e.EventType(), err)
	}
	return e, nil
}

// AssigneeChanged indicates that a change's assignee has been changed.
type AssigneeChanged struct {
	Type           string  `json:"type"`
	Change         Change  `json:"change"`
	Changer        Account `json:"changer"`
	OldAssignee    Account `json:"oldAssignee"`
	EventCreatedOn Time    `json:"eventCreatedOn"`
}

// EventType implements Event.
func (AssigneeChanged) EventType() string { return "assignee-changed" }

func init() {
	registerEvent(func() Event { return &AssigneeChanged{} })
}

// ChangeAbandoned indicates that a change has been abandoned.
type ChangeAbandoned struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Abandoner      Account  `json:"abandoner"`
	Reason         string   `json:"reason"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (ChangeAbandoned) EventType() string { return "change-abandoned" }

func init() {
	registerEvent(func() Event { return &ChangeAbandoned{} })
}

// ChangeDeleted indicates that a change has been deleted.
type ChangeDeleted struct {
	Type    string  `json:"type"`
	Change  Change  `json:"change"`
	Deleter Account `json:"deleter"`
}

// EventType implements Event.
func (ChangeDeleted) EventType() string { return "change-deleted" }

func init() {
	registerEvent(func() Event { return &ChangeDeleted{} })
}

// ChangeMerged indicates that a change has been merged into the target branch.
type ChangeMerged struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Submitter      Account  `json:"submitter"`
	NewRev         string   `json:"newRev"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (ChangeMerged) EventType() string { return "change-merged" }

func init() {
	registerEvent(func() Event { return &ChangeMerged{} })
}

// ChangeRestored indicates a change has been restored (i.e. un-abandoned).
type ChangeRestored struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Restorer       Account  `json:"restorer"`
	Reason         string   `json:"reason"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (ChangeRestored) EventType() string { return "change-restored" }

func init() {
	registerEvent(func() Event { return &ChangeRestored{} })
}

// CommentAdded indicates someone has commented on a patchset.
type CommentAdded struct {
	Type           string     `json:"type"`
	Change         Change     `json:"change"`
	PatchSet       PatchSet   `json:"patchSet"`
	Author         Account    `json:"author"`
	Approvals      []Approval `json:"approvals"`
	Comment        string     `json:"comment"`
	EventCreatedOn Time       `json:"eventCreatedOn"`
}

// EventType implements Event.
func (CommentAdded) EventType() string { return "comment-added" }

func init() {
	registerEvent(func() Event { return &CommentAdded{} })
}

// DroppedOutput indicates that some events may be missing from the stream.
type DroppedOutput struct {
	Type string `json:"type"`
}

// EventType implements Event.
func (DroppedOutput) EventType() string { return "dropped-output" }

func init() {
	registerEvent(func() Event { return &DroppedOutput{} })
}

// HashtagsChanged indicates that someone has added or removed hashtags from a change.
type HashtagsChanged struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	Editor         Account  `json:"editor"`
	Added          []string `json:"added"`
	Removed        []string `json:"removed"`
	Hashtags       []string `json:"hashtags"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (HashtagsChanged) EventType() string { return "hashtags-changed" }

func init() {
	registerEvent(func() Event { return &HashtagsChanged{} })
}

// ProjectCreated indicates that a new project has been created.
type ProjectCreated struct {
	Type           string `json:"type"`
	ProjectName    string `json:"projectName"`
	ProjectHead    string `json:"projectHead"`
	EventCreatedOn Time   `json:"eventCreatedOn"`
}

// EventType implements Event.
func (ProjectCreated) EventType() string { return "project-created" }

func init() {
	registerEvent(func() Event { return &ProjectCreated{} })
}

// PatchSetCreated indicates that a new patchset has been added to a change.
type PatchSetCreated struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Uploader       Account  `json:"uploader"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (PatchSetCreated) EventType() string { return "patchset-created" }

func init() {
	registerEvent(func() Event { return &PatchSetCreated{} })
}

// RefUpdated indicates that a ref has been updated.
type RefUpdated struct {
	Type           string    `json:"type"`
	Submitter      Account   `json:"submitter"`
	RefUpdate      RefUpdate `json:"refUpdate"`
	EventCreatedOn Time      `json:"eventCreatedOn"`
}

// EventType implements Event.
func (RefUpdated) EventType() string { return "ref-updated" }

func init() {
	registerEvent(func() Event { return &RefUpdated{} })
}

// ReviewerAdded indicates that a reviewer has been added to a change.
type ReviewerAdded struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Reviewer       Account  `json:"reviewer"`
	Adder          Account  `json:"adder"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (ReviewerAdded) EventType() string { return "reviewer-added" }

func init() {
	registerEvent(func() Event { return &ReviewerAdded{} })
}

// ReviewerDeleted indicates that a reviewer has been removed from a change, possibly removing one or more approvals.
type ReviewerDeleted struct {
	Type           string     `json:"type"`
	Change         Change     `json:"change"`
	PatchSet       PatchSet   `json:"patchSet"`
	Reviewer       Account    `json:"reviewer"`
	Remover        Account    `json:"remover"`
	Approvals      []Approval `json:"approvals"`
	Comment        string     `json:"comment"`
	EventCreatedOn Time       `json:"eventCreatedOn"`
}

// EventType implements Event.
func (ReviewerDeleted) EventType() string { return "reviewer-deleted" }

func init() {
	registerEvent(func() Event { return &ReviewerDeleted{} })
}

// TopicChanged indicates that the topic attached to a change has been changed.
type TopicChanged struct {
	Type           string  `json:"type"`
	Change         Change  `json:"change"`
	Changer        Account `json:"changer"`
	OldTopic       string  `json:"oldTopic"`
	EventCreatedOn Time    `json:"eventCreatedOn"`
}

// EventType implements Event.
func (TopicChanged) EventType() string { return "topic-changed" }

func init() {
	registerEvent(func() Event { return &TopicChanged{} })
}

// WIPStateChanged indicates that the work-in-progress state of a change has changed.
type WIPStateChanged struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Changer        Account  `json:"changer"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (WIPStateChanged) EventType() string { return "wip-state-changed" }

func init() {
	registerEvent(func() Event { return &WIPStateChanged{} })
}

// PrivateStateChanged indicates that the private state of a change has changed.
type PrivateStateChanged struct {
	Type           string   `json:"type"`
	Change         Change   `json:"change"`
	PatchSet       PatchSet `json:"patchSet"`
	Changer        Account  `json:"changer"`
	EventCreatedOn Time     `json:"eventCreatedOn"`
}

// EventType implements Event.
func (PrivateStateChanged) EventType() string { return "private-state-changed" }

func init() {
	registerEvent(func() Event { return &PrivateStateChanged{} })
}

// VoteDeleted indicates that an approval vote has been deleted from a change.
type VoteDeleted struct {
	Type      string     `json:"type"`
	Change    Change     `json:"change"`
	PatchSet  PatchSet   `json:"patchSet"`
	Reviewer  Account    `json:"reviewer"`
	Remover   Account    `json:"remover"`
	Approvals []Approval `json:"approvals"`
	Comment   string     `json:"comment"`
}

// EventType implements Event.
func (VoteDeleted) EventType() string { return "vote-deleted" }

func init() {
	registerEvent(func() Event { return &VoteDeleted{} })
}
