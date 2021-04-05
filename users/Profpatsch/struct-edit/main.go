package main

import (
	"fmt"
	"os"
	"strings"
	json "encoding/json"
	"log"

	tea "github.com/charmbracelet/bubbletea"
	lipgloss "github.com/charmbracelet/lipgloss"
	// termenv "github.com/muesli/termenv"
	// isatty "github.com/mattn/go-isatty"
)

type model struct {
	path []index
	selectedIndex index
	data val
}

// an index into a value, uint for lists and string for maps.
// nil for any scalar value.
// TODO: use an actual interface for these
type index interface{}

type val struct {
	tag tag
	doc string
	val interface{}
}

type tag string

const (
	tagString tag = "string"
	tagFloat tag = "float"
	tagList tag = "list"
)

func (v val) Render() string {
	s := ""
	switch v.tag {
	case tagString:
		s += v.val.(string)
	case tagFloat:
		s += fmt.Sprint(v.val.(float64))
	case tagList:
		s += "[ "
		vs := []string{}
		for _, v := range v.val.([]val) {
			vs = append(vs, v.Render())
		}
		s += strings.Join(vs, ", ")
		s += " ]"
	default:
		s += fmt.Sprintf("<unknown: %v>", v)
	}
	return s
}

func renderIndex(i index) (s string) {
	switch i := i.(type) {
	case nil: s = ""
	// list index
	case uint: s = "*"
	// map index
	case string: s = i + ":"
	}
	return
}

func makeVal(i interface {}) val {
	var v val
	switch i := i.(type) {
	case string:
		v = val{
			tag: tagString,
			doc: "",
			val: i,
		}
	case float64:
		v = val{
			tag: tagFloat,
			doc: "",
			val: i,
		}
	case []interface{}:
		ls := []val{}
		for _, i := range i {
			ls = append(ls, makeVal(i))
		}
		v = val{
			tag: tagList,
			doc: "",
			val: ls,
		}
	default:
		log.Fatalf("makeVal: cannot read json of type %T", i)
	}
	return v
}

// return an index that point at the first entry in val
func (v val) pos1() index {
	switch v.tag {
	case tagList:
		return index(uint(0))
	default:
		return index(nil)
	}
}

type enumerate struct {
	i index
	v val
}

// enumerate gives us a stable ordering of elements in this val.
// for scalars it’s just a nil index & the val itself.
// Guaranteed to always return at least one element.
func (v val) enumerate() (e []enumerate) {
	switch v.tag {
	case tagString: fallthrough
	case tagFloat:
		e = []enumerate{enumerate{i: index(nil), v: v}}
	case tagList:
		for i, v := range v.val.([]val) {
			e = append(e, enumerate{i: index(uint(i)), v: v})
		}
        default:
		log.Fatalf("unknown val tag %s, %v", v.tag, v)
	}
	return
}

func initialModel(v interface{}) model {
	val := makeVal(v)
	return model{
		path: []index{},
		data: val,
		selectedIndex: val.pos1(),
	}
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
func walk(data val, path []index) (val, error) {
	atPath := func(index int) string {
		return fmt.Sprintf("at path %v", path[:index+1])
	}
	errf := func(ty string, val interface{}, index int) error {
		return fmt.Errorf("walk: can’t walk into %s %v %s", ty, val, atPath(index))
	}
	for i, p := range path {
		switch data.tag {
		case tagString:
			return data, errf("string", data.val, i)
		case tagFloat:
			return data, errf("float", data.val, i)
		case tagList:
			switch p := p.(type) {
			case uint:
				list := data.val.([]val)
				if int(p) >= len(list) || p < 0 {
					return data, fmt.Errorf("index out of bounds " + atPath(i))
				}
				data = list[p]
			default:
				return data, fmt.Errorf("not a list index " + atPath(i))
			}
		default:
			return data, errf(string(data.tag), data.val, i)
		}
	}
	return data, nil
}

// descend into the selected index. Assumes that the index is valid.
// Will not descend into scalars.
func (m model) descend() (model, error) {
	newPath := append(m.path, m.selectedIndex)
	lower, err := walk(m.data, newPath)
	// only descend if we *can* (TODO: can we distinguish bad errors from scalar?)
	if err == nil {
		m.path = newPath
		m.selectedIndex = lower.pos1()
	}
	return m, nil
}

// ascend to one level up. stops at the root.
func (m model) ascend() (model, error) {
	if len(m.path) > 0 {
	  m.path = m.path[:len(m.path)-1]
	  upper, err := walk(m.data, m.path)
	  m.selectedIndex = upper.pos1()
          return m, err
	}
	return m, nil
}

func (min model) next() (m model, err error) {
	m = min
	var this val
	this, err = walk(m.data, m.path)
	enumL := this.enumerate()
	setNext := false
	for _, enum := range enumL {
		if setNext {
			m.selectedIndex = enum.i
			setNext = false
			break
		}
		if enum.i == m.selectedIndex {
			setNext = true
		}
	}
	// wraparound
	if setNext {
		m.selectedIndex = enumL[0].i
	}
	return
}

func (min model) prev() (m model, err error) {
	m = min
	var this val
	this, err = walk(m.data, m.path)
	enumL := this.enumerate()
	// last element, wraparound
	prevIndex := enumL[len(enumL)-1].i
	for _, enum := range enumL {
		if enum.i == m.selectedIndex {
			m.selectedIndex = prevIndex
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

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var err error
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit

		case "up":
			m, err = m.ascend()

		case "down":
			m, err = m.descend()

		case "right":
			m, err = m.next()

		case "left":
			m, err = m.prev()


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
	cur, err := walk(m.data, m.path)
	if err != nil {
		log.Fatal(err)
	}
	s += "\n"
	for _, enum := range cur.enumerate() {
		is := renderIndex(enum.i)
		if is != "" {
			s += is + " "
		}
		if enum.i == m.selectedIndex {
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
