package gerritevents

// Account is a Gerrit account (or just a Git name+email pair).
type Account struct {
	Name     string `json:"name"`
	Email    string `json:"email"`
	Username string `json:"username"`
}

// ChangeStatus represents the states a change can be in.
type ChangeStatus string

const (
	// ChangeStatusNew is the state a change is in during review.
	ChangeStatusNew ChangeStatus = "NEW"

	// ChangeStatusMerged indicates a change was merged to the target branch.
	ChangeStatusMerged ChangeStatus = "MERGED"

	// ChangeStatusAbandoned indicates a change was marked as abandoned.
	ChangeStatusAbandoned ChangeStatus = "ABANDONED"
)

// Message is a message left by a reviewer.
type Message struct {
	Timestamp Time    `json:"timestamp"`
	Reviewer  Account `json:"reviewer"`
	Message   string  `json:"message"`
}

// TrackingID allows storing identifiers from external systems, i.e. bug trackers.
type TrackingID struct {
	System string `json:"system"`
	ID     string `json:"id"`
}

// ChangeKind indicates the different changes that can be made to a change.
type ChangeKind string

const (
	// ChangeKindRework indicates a non-trivial content change.
	ChangeKindRework ChangeKind = "REWORK"

	// ChangeKindTrivialRebase indicates a conflict-free merge between the new parent and the prior patch set.
	ChangeKindTrivialRebase ChangeKind = "TRIVIAL_REBASE"

	// ChangeKindMergeFirstParentUpdate indicates a conflict-free change of the first parent of a merge commit.
	ChangeKindMergeFirstParentUpdate ChangeKind = "MERGE_FIRST_PARENT_UPDATE"

	// ChangeKindNoCodeChange indicates no code change (the tree and parent trees are unchanged) - commit message probably changed.
	ChangeKindNoCodeChange ChangeKind = "NO_CODE_CHANGE"

	// ChangeKindNoChange indicates nothing changes: the commit message, tree, and parent tree are unchanged.
	ChangeKindNoChange ChangeKind = "NO_CHANGE"
)

// Approval represents the current and past state of an approval label.
type Approval struct {
	Type        string   `json:"type"`
	Description string   `json:"description"`
	Value       string   `json:"value"`
	OldValue    *string  `json:"oldValue"`
	GrantedOn   *Time    `json:"grantedOn"`
	By          *Account `json:"by"`
}

// PatchSetComment is a single comment left on a patchset.
type PatchSetComment struct {
	File     string  `json:"file"`
	Line     int     `json:"line"`
	Reviewer Account `json:"reviewer"`
	Message  string  `json:"message"`
}

// FilePatchType represents the different modifications that can be made to a file by a patchset.
type FilePatchType string

const (
	// FilePatchTypeAdded indicates the file did not exist, and this patchset adds it to the tree.
	FilePatchTypeAdded FilePatchType = "ADDED"

	// FilePatchTypeModified indicates the file exists before and after this patchset.
	FilePatchTypeModified FilePatchType = "MODIFIED"

	// FilePatchTypeDeleted indicates the file is removed by this patchset.
	FilePatchTypeDeleted FilePatchType = "DELETED"

	// FilePatchTypeRenamed indicates the file has a different name before this patchset than after.
	FilePatchTypeRenamed FilePatchType = "RENAMED"

	// FilePatchTypeCopied indicates the file was copied from a different file.
	FilePatchTypeCopied FilePatchType = "COPIED"

	// FilePatchTypeRewrite indicates the file had a significant quantity of content changed.
	FilePatchTypeRewrite FilePatchType = "REWRITE"
)

// File represents a file in a patchset as well as how it is being modified.
type File struct {
	File    string        `json:"file"`
	FileOld string        `json:"fileOld"`
	Type    FilePatchType `json:"type"`
}

// PatchSet represents a single patchset within a change.
type PatchSet struct {
	Number         int               `json:"number"`
	Revision       string            `json:"revision"`
	Parents        []string          `json:"parents"`
	Ref            string            `json:"ref"`
	Uploader       Account           `json:"uploader"`
	Author         Account           `json:"author"`
	CreatedOn      Time              `json:"createdOn"`
	Kind           ChangeKind        `json:"kind"`
	Approvals      []Approval        `json:"approvals"`
	Comments       []PatchSetComment `json:"comments"`
	Files          []File            `json:"file"`
	SizeInsertions int               `json:"sizeInsertions"`
	SizeDeletions  int               `json:"sizeDeletions"`
}

// Dependency represents a change on which this change is dependent.
type Dependency struct {
	ID                string `json:"id"`
	Number            int    `json:"number"`
	Revision          string `json:"revision"`
	Ref               string `json:"ref"`
	IsCurrentPatchSet bool   `json:"isCurrentPatchSet"`
}

// SubmitStatus indicates whether this change has met the submit conditions and is ready to submit.
type SubmitStatus string

const (
	// SubmitStatusOK indicates this change is ready to submit - all submit requirements are met.
	SubmitStatusOK SubmitStatus = "OK"

	// SubmitStatusNotReady indicates this change cannot yet be submitted.
	SubmitStatusNotReady SubmitStatus = "NOT_READY"

	// SubmitStatusRuleError indicates the submit rules could not be evaluted. Administrator intervention is required.
	SubmitStatusRuleError SubmitStatus = "RULE_ERROR"
)

// LabelStatus indicates whether this label permits submission and if the label can be granted by anyone.
type LabelStatus string

const (
	// LabelStatusOK indicates that this label provides what is necessary for submission (e.g. CR+2).
	LabelStatusOK LabelStatus = "OK"

	// LabelStatusReject indicates this label prevents submission (e.g. CR-2).
	LabelStatusReject LabelStatus = "REJECT"

	// LabelStatusNeed indicates this label is required for submission, but has not been satisfied (e.g. CR0).
	LabelStatusNeed LabelStatus = "NEED"

	// LabelStatusMay indicates this label is not required for submission. It may or may not be set.
	LabelStatusMay LabelStatus = "MAY"

	// LabelStatusImpossible indicates this label is required for submission, but cannot be satisfied. The ACLs on this label may be set incorrectly.
	LabelStatusImpossible LabelStatus = "IMPOSSIBLE"
)

// Label represents the status of a particular label.
type Label struct {
	Label  string      `json:"label"`
	Status LabelStatus `json:"status"`
	By     Account     `json:"by"`
}

// Requirement represents a submit requirement.
type Requirement struct {
	FallbackText string `json:"fallbackText"`
	Type         string `json:"type"`
	// TODO(lukegb): data
}

// SubmitRecord represents the current submission state of a change.
type SubmitRecord struct {
	Status       SubmitStatus  `json:"status"`
	Labels       []Label       `json:"labels"`
	Requirements []Requirement `json:"requirements"`
}

// Change represents a Gerrit CL.
type Change struct {
	Project         string         `json:"project"`
	Branch          string         `json:"branch"`
	Topic           string         `json:"topic"`
	ID              string         `json:"id"`
	Number          int            `json:"number"`
	Subject         string         `json:"subject"`
	Owner           Account        `json:"owner"`
	URL             string         `json:"url"`
	CommitMessage   string         `json:"commitMessage"`
	CreatedOn       Time           `json:"createdOn"`
	LastUpdated     *Time          `json:"lastUpdated"`
	Open            bool           `json:"open"`
	Status          ChangeStatus   `json:"status"`
	Private         bool           `json:"private"`
	WIP             bool           `json:"wip"`
	Comments        []Message      `json:"comments"`
	TrackingIDs     []TrackingID   `json:"trackingIds"`
	CurrentPatchSet *PatchSet      `json:"currentPatchSet"`
	PatchSets       []PatchSet     `json:"patchSets"`
	DependsOn       []Dependency   `json:"dependsOn"`
	NeededBy        []Dependency   `json:"neededBy"`
	SubmitRecords   []SubmitRecord `json:"submitRecord"`
	AllReviewers    []Account      `json:"allReviewers"`
}

// RefUpdate represents a change in a ref.
type RefUpdate struct {
	OldRev  string `json:"oldRev"`
	NewRev  string `json:"newRev"`
	RefName string `json:"refName"`
	Project string `json:"project"`
}
