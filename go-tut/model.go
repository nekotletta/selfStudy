package main

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/bubbles/textinput"
	"log"
)


const (
	// all possible tabs i could be in
	listView uint = iota
	titleView
	bodyView
)

type model struct {
	state uint
	// db
	store *Store
	notes []Note
	currNote Note
	listIndex int

	textarea textarea.Model
	textinput textinput.Model
}

// update when a new note is created
// receive a store (all curr notes) and returns updated model
func newNoteModel(store *Store) model {

	notes, err := store.getNotes()
	if err != nil {
		log.Fatal("unable to get notes: %v", err)
	}
	return model{
		state: listView,
		store: store,
		notes: notes,

		textarea: textarea.New(),
		textinput: textinput.New(),
	}
}

// go generally has 3 methods: view, init, and update

// init -> do we need to do anythibg beforehand?
// it usually returns null
func (m model) Init() tea.Cmd {
	return nil
}

// updated -> what do i need to change?
// receive a message (usually keypress) and returns updated model and command
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	// we need to update our text areas as well
	var (
		cmds []tea.Cmd
		cmd tea.Cmd
	)

	m.textinput, cmd = m.textinput.Update(msg)
	cmds = append(cmds, cmd)

	m.textarea, cmd = m.textarea.Update(msg)
	cmds = append(cmds, cmd)

	// read key presses
	switch msg := msg.(type) {
	case tea.KeyMsg:
		key_pressed := msg.String()

		// which page am i on
		switch m.state {
			// what am i doing with the notes in my db
			case listView:
				switch key_pressed {
				case "q":
					return m, tea.Quit
				case "n": //new note
					m.textinput.SetValue("")
					m.textinput.Focus()
					m.currNote = Note{}
					// we're switching pages, so we update state
					m.state = titleView
				case "up", "k":
					if m.listIndex > 0 {
						m.listIndex--
					}
				case "down", "j":
					if m.listIndex < len(m.notes) -1 {
						m.listIndex++
					}
				case "enter":
					m.currNote = m.notes[m.listIndex]
					m.textarea.SetValue(m.currNote.Body)
					m.textarea.Focus()
					m.textarea.CursorEnd()
					m.state = bodyView
				}

			case titleView:
				switch key_pressed {
				case "enter":
					title := m.textinput.Value()
					if title != "" {
						m.currNote.Title = title
						m.textarea.SetValue("")
						m.textarea.Focus()
						m.textarea.CursorEnd()
						m.state = bodyView
					}
				case "esc": // go back
					m.state = listView
				}

			case bodyView:
				switch key_pressed {
				case "ctrl+s":
					body := m.textarea.Value()
					m.currNote.Body = body
					
					var err error
					if err = m.store.saveNote(m.currNote); err != nil {
						return m, tea.Quit
					}

					m.notes, err = m.store.getNotes()
					if err != nil {
						return m, tea.Quit
					}

					m.currNote = Note{}
					m.state = listView
				case "esc": // go back
					m.state = listView
			}

		}
	}
	return m, tea.Batch(cmds...)
}