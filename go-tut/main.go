package main

import (
	tea "github.com/charmbracelet/bubbletea"
	"log"
)

func main() {
	store := &Store{}
	if err := store.Init(); err != nil {
		log.Fatal("error initializing db: %v", err)
	}
	m := newNoteModel(store)
	p := tea.NewProgram(m)
	if _, err := p.Run(); err != nil {
		log.Fatal("error creating project: %v", err)
	}
}