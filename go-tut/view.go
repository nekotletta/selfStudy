// what do i want to render to my terminal?
// returns a string? why though

package main

import (
	"github.com/charmbracelet/lipgloss"
	"strings"
)

// pre set styles
var (
	appNameStyle = lipgloss.NewStyle().Background(lipgloss.Color("99"))
	bodyStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("255")).Faint(true)
	indexStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("99")).MarginRight(1)
)

func (m model) View() string {
	st := appNameStyle.Render("Notes app") + "\n\n"

	// what the user should see if creating a note
	if m.state == titleView {
		st += "Note title:\n"
		st += m.textinput.View() + "\n\n"

		st += "enter - save note, esc - discard"
	}

	if m.state == bodyView {
		st += "Note:\n"
		st += m.textarea.View() + "\n\n"

		st += "crtl+s - save note, esc - discard"
	}

	// what the user should see if looking at notes
	if m.state == listView {
		for index, note := range m.notes {
			prefix := " "
			if index == m.listIndex {
				prefix = ">"
			}

			previewBody := strings.ReplaceAll(note.Body, "\n", " ")
			if len(previewBody) > 20 {
				previewBody = previewBody[:20]
			}

			st += indexStyle.Render(prefix) + note.Title + " | " + bodyStyle.Render(previewBody) + "\n\n"
		}
		st += "n - new note, q - quit"
	}
	
	return st
}