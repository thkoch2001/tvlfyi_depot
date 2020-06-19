package googlegroups

import (
	"encoding/json"
	"fmt"
	"net/http"
)

const (
	EndpointGroupGet     = `https://www.googleapis.com/admin/directory/v1/groups/%s`
	EndpointGroupMembers = `https://www.googleapis.com/admin/directory/v1/groups/%s/members`
)

// MembershipDelivery is documented at https://developers.google.com/admin-sdk/directory/v1/reference/members#resource
type MembershipDelivery string

// MembershipRole is documented at https://developers.google.com/admin-sdk/directory/v1/reference/members#resource
type MembershipRole string

// MembershipStatus is documented at https://developers.google.com/admin-sdk/directory/v1/reference/members#resource
type MembershipStatus string

// MembershipType is documented at https://developers.google.com/admin-sdk/directory/v1/reference/members#resource
type MembershipType string

// These constants are documented at https://developers.google.com/admin-sdk/directory/v1/reference/members#resource
const (
	DeliveryAllMail  MembershipDelivery = "ALL_MAIL"
	DeliveryDaily                       = "DAILY"
	DeliveryDigest                      = "DIGEST"
	DeliveryDisabled                    = "DISABLED"
	DeliveryNone                        = "NONE"

	RoleOwner   MembershipRole = "OWNER"
	RoleManager                = "MANAGER"
	RoleMember                 = "MEMBER"

	StatusActive    MembershipStatus = "ACTIVE"
	StatusArchived                   = "ARCHIVED"
	StatusSuspended                  = "SUSPENDED"
	StatusUnknown                    = "UNKNOWN"

	TypeCustomer MembershipType = "CUSTOMER"
	TypeExternal                = "EXTERNAL"
	TypeGroup                   = "GROUP"
	TypeUser                    = "USER"
)

// GroupMember is documented at https://developers.google.com/admin-sdk/directory/v1/reference/members#resource
type GroupMember struct {
	Kind             string             `json:"kind"`
	ETag             string             `json:"etag,omitempty"`
	ID               string             `json:"id,omitempty"`
	Email            string             `json:"email,omitempty"`
	Role             MembershipRole     `json:"role,omitempty"`
	Type             MembershipType     `json:"type,omitempty"`
	Status           MembershipStatus   `json:"status,omitempty"`
	DeliverySettings MembershipDelivery `json:"delivery_settings,omitempty"`
}

// SetKind fills the Kind field.
func (gm *GroupMember) SetKind() {
	gm.Kind = `admin#directory#member`
}

// AddMemberToGroup adds a GroupMember to the specified groupID.
//
// As documented at https://developers.google.com/admin-sdk/directory/v1/reference/members/insert
func AddMemberToGroup(ctx context.Context, httpClient Doer, groupID string, member *GroupMember) error {
	member.SetKind()

	u := fmt.Sprintf(EndpointGroupMembers, groupID)
	by, err := json.Marshal(member)
	if err != nil {
		return fmt.Errorf("add %s to group: json marshal: %w", member.Email, err)
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, u, bytes.NewReader(by))
	if err != nil {
		return fmt.Errorf("add %s to group: http req: %w", member.Email, err)
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		return fmt.Errorf("add %s to group: http req: %w", member.Email, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == 200 {
		return nil
	}

	by, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return fmt.Errorf("add %s to group: read resp body: %w", member.Email, err)
	}
	return fmt.Errorf("add %s to group: http error %d: %s", member.Email, resp.StatusCode, by)
}

// Type Doer is implemented by *http.Client.
type Doer interface {
	Do(*http.Request) (*http.Response, error)
}
