package main

import (
	json "encoding/json"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	lipgloss "github.com/charmbracelet/lipgloss"
	// termenv "github.com/muesli/termenv"
	// isatty "github.com/mattn/go-isatty"
)

// Keeps the full data structure and a path that indexes our current position into it.
type model struct {
	path []index
	data val
}

// an index into a value, uint for lists and string for maps.
// nil for any scalar value.
// TODO: use an actual interface for these
type index interface{}

/// recursive value that we can represent.
type val struct {
	// the “type” of value; see tag const belove
	tag tag
	// last known position of our cursor
	last_index index
	// documentation (TODO)
	doc string
	// the actual value;
	// the actual structure is behind a pointer so we can replace the struct.
	// determined by the tag
	// tagString -> *string
	// tagFloat -> *float64
	// tagList -> *[]val
	// tagMap -> *map[string]val
	val interface{}
}

type tag string

const (
	tagString tag = "string"
	tagFloat  tag = "float"
	tagList   tag = "list"
	tagMap    tag = "map"
)

// print a value, flat
func (v val) Render() string {
	s := ""
	switch v.tag {
	case tagString:
		s += *v.val.(*string)
	case tagFloat:
		s += fmt.Sprint(*v.val.(*float64))
	case tagList:
		s += "[ "
		vs := []string{}
		for _, enum := range v.enumerate() {
			vs = append(vs, enum.v.Render())
		}
		s += strings.Join(vs, ", ")
		s += " ]"
	case tagMap:
		s += "{ "
		vs := []string{}
		for _, enum := range v.enumerate() {
			vs = append(vs, fmt.Sprintf("%s: %s", enum.i.(string), enum.v.Render()))
		}
		s += strings.Join(vs, ", ")
		s += " }"
	default:
		s += fmt.Sprintf("<unknown: %v>", v)
	}
	return s
}

// render an index, depending on the type
func renderIndex(i index) (s string) {
	switch i := i.(type) {
	case nil:
		s = ""
	// list index
	case uint:
		s = "*"
	// map index
	case string:
		s = i + ":"
	}
	return
}

// take an arbitrary (within restrictions) go value and construct a val from it
func makeVal(i interface{}) val {
	var v val
	switch i := i.(type) {
	case string:
		v = val{
			tag:        tagString,
			last_index: index(nil),
			doc:        "",
			val:        &i,
		}
	case float64:
		v = val{
			tag:        tagFloat,
			last_index: index(nil),
			doc:        "",
			val:        &i,
		}
	case []interface{}:
		ls := []val{}
		for _, i := range i {
			ls = append(ls, makeVal(i))
		}
		v = val{
			tag:        tagList,
			last_index: pos1Inner(tagList, &ls),
			doc:        "",
			val:        &ls,
		}
	case map[string]interface{}:
		ls := map[string]val{}
		for k, i := range i {
			ls[k] = makeVal(i)
		}
		v = val{
			tag:        tagMap,
			last_index: pos1Inner(tagMap, &ls),
			doc:        "",
			val:        &ls,
		}
	default:
		log.Fatalf("makeVal: cannot read json of type %T", i)
	}
	return v
}

// return an index that points at the first entry in val
func (v val) pos1() index {
	return v.enumerate()[0].i
}

func pos1Inner(tag tag, v interface{}) index {
	return enumerateInner(tag, v)[0].i
}

type enumerate struct {
	i index
	v val
}

// enumerate gives us a stable ordering of elements in this val.
// for scalars it’s just a nil index & the val itself.
// Guaranteed to always return at least one element.
func (v val) enumerate() (e []enumerate) {
	e = enumerateInner(v.tag, v.val)
	if e == nil {
		e = append(e, enumerate{
			i: nil,
			v: v,
		})
	}
	return
}

// like enumerate, but returns an empty slice for scalars without inner vals.
func enumerateInner(tag tag, v interface{}) (e []enumerate) {
	switch tag {
	case tagString:
		fallthrough
	case tagFloat:
		e = nil
	case tagList:
		for i, v := range *v.(*[]val) {
			e = append(e, enumerate{i: index(uint(i)), v: v})
		}
	case tagMap:
		// map sorting order is not stable (actually randomized thank jabber)
		// so let’s sort them
		keys := []string{}
		m := *v.(*map[string]val)
		for k, _ := range m {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			e = append(e, enumerate{i: index(k), v: m[k]})
		}
	default:
		log.Fatalf("unknown val tag %s, %v", tag, v)
	}
	return
}

func (m model) PathString() string {
	s := "/ "
	var is []string
	for _, v := range m.path {
		is = append(is, fmt.Sprintf("%v", v))
	}
	s += strings.Join(is, " / ")
	return s
}

// walk the given path down in data, to get the value at that point.
// Assumes that all path indexes are valid indexes into data.
// Returns a pointer to the value at point, in order to be able to change it.
func walk(data *val, path []index) (*val, bool, error) {
	res := data
	atPath := func(index int) string {
		return fmt.Sprintf("at path %v", path[:index+1])
	}
	errf := func(ty string, val interface{}, index int) error {
		return fmt.Errorf("walk: can’t walk into %s %v %s", ty, val, atPath(index))
	}
	for i, p := range path {
		switch res.tag {
		case tagString:
			return nil, true, nil
		case tagFloat:
			return nil, true, nil
		case tagList:
			switch p := p.(type) {
			case uint:
				list := *res.val.(*[]val)
				if int(p) >= len(list) || p < 0 {
					return nil, false, fmt.Errorf("index out of bounds %s", atPath(i))
				}
				res = &list[p]
			default:
				return nil, false, fmt.Errorf("not a list index %s", atPath(i))
			}
		case tagMap:
			switch p := p.(type) {
			case string:
				m := *res.val.(*map[string]val)
				if a, ok := m[p]; ok {
					res = &a
				} else {
					return nil, false, fmt.Errorf("index %s not in map %s", p, atPath(i))
				}
			default:
				return nil, false, fmt.Errorf("not a map index %v %s", p, atPath(i))
			}

		default:
			return nil, false, errf(string(res.tag), res.val, i)
		}
	}
	return res, false, nil
}

// descend into the selected index. Assumes that the index is valid.
// Will not descend into scalars.
func (m model) descend() (model, error) {
	// TODO: two walks?!
	this, _, err := walk(&m.data, m.path)
	if err != nil {
		return m, err
	}
	newPath := append(m.path, this.last_index)
	_, bounce, err := walk(&m.data, newPath)
	if err != nil {
		return m, err
	}
	// only descend if we *can*
	if !bounce {
		m.path = newPath
	}
	return m, nil
}

// ascend to one level up. stops at the root.
func (m model) ascend() (model, error) {
	if len(m.path) > 0 {
		m.path = m.path[:len(m.path)-1]
		_, _, err := walk(&m.data, m.path)
		return m, err
	}
	return m, nil
}

/// go to the next item, or wraparound
func (min model) next() (m model, err error) {
	m = min
	this, _, err := walk(&m.data, m.path)
	if err != nil {
		return
	}
	enumL := this.enumerate()
	setNext := false
	for _, enum := range enumL {
		if setNext {
			this.last_index = enum.i
			setNext = false
			break
		}
		if enum.i == this.last_index {
			setNext = true
		}
	}
	// wraparound
	if setNext {
		this.last_index = enumL[0].i
	}
	return
}

/// go to the previous item, or wraparound
func (min model) prev() (m model, err error) {
	m = min
	this, _, err := walk(&m.data, m.path)
	if err != nil {
		return
	}
	enumL := this.enumerate()
	// last element, wraparound
	prevIndex := enumL[len(enumL)-1].i
	for _, enum := range enumL {
		if enum.i == this.last_index {
			this.last_index = prevIndex
			break
		}
		prevIndex = enum.i
	}
	return
}

/// bubbletea implementations

func (m model) Init() tea.Cmd {
	return nil
}

func initialModel(v interface{}) model {
	val := makeVal(v)
	return model{
		path: []index{},
		data: val,
	}
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var err error
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit

		case "up":
			m, err = m.prev()

		case "down":
			m, err = m.next()

		case "right":
			m, err = m.descend()

		case "left":
			m, err = m.ascend()

			// 	case "enter":
			// 		_, ok := m.selected[m.cursor]
			// 		if ok {
			// 			delete(m.selected, m.cursor)
			// 		} else {
			// 			m.selected[m.cursor] = struct{}{}
			// 		}
		}

	}
	if err != nil {
		log.Fatal(err)
	}
	return m, nil
}

var pathColor = lipgloss.NewStyle().
	// light blue
	Foreground(lipgloss.Color("12"))

var selectedColor = lipgloss.NewStyle().
	Bold(true)

func (m model) View() string {
	s := pathColor.Render(m.PathString())
	cur, _, err := walk(&m.data, m.path)
	if err != nil {
		log.Fatal(err)
	}
	s += cur.doc + "\n"
	s += "\n"
	for _, enum := range cur.enumerate() {
		is := renderIndex(enum.i)
		if is != "" {
			s += is + " "
		}
		if enum.i == cur.last_index {
			s += selectedColor.Render(enum.v.Render())
		} else {
			s += enum.v.Render()
		}
		s += "\n"
	}

	// s += fmt.Sprintf("%v\n", m)
	// s += fmt.Sprintf("%v\n", cur)

	return s
}

func main() {
	var input interface{}
	err := json.NewDecoder(os.Stdin).Decode(&input)
	if err != nil {
		log.Fatal("json from stdin: ", err)
	}
	p := tea.NewProgram(initialModel(input))
	if err := p.Start(); err != nil {
		log.Fatal("bubbletea TUI error: ", err)
	}
}
