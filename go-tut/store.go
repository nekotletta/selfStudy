package main

// you can have local database in go, nust import
import (
	"database/sql"
	// what does this do? fail silently? optional?
	_ "github.com/mattn/go-sqlite3"
	"time"
)
type Note struct {
	ID int64
	Title string
	Body string
}

// db
type Store struct {
	conn *sql.DB
}

// connect to db
func (s *Store) Init() error {
	var err error
	// where to store db = name
	// i need to look up how i'd connect to an external one much later
	s.conn, err = sql.Open("sqlite3", "./notes.db")
	if err != nil {
		return err
	}

	createTableQuery := `CREATE TABLE IF NOT EXISTS notes (
		id integer not null primary key,
		title varchar(40) not null,
		body text not null
	);`

	if _, err := s.conn.Exec(createTableQuery); err != nil {
		return err
	}
	return nil
}

// db methods
func (s *Store) getNotes() ([]Note, error) {
	rows, err := s.conn.Query("SELECT * FROM notes")
	if err != nil {
		return nil, err
	}

	// close connection
	// what does defer do
	defer rows.Close()

	notes := []Note{}

	for rows.Next(){
		var note_obj Note
		// in the vid scan checks that no fields are null
		// but i should look into what this actually does
		rows.Scan(&note_obj.ID, &note_obj.Title, &note_obj.Body)
		notes = append(notes, note_obj)
	}

	return notes, nil
}

func (s *Store) saveNote(note Note) error {
	// video whats to insert and update in the same function
	if note.ID == 0 {
		note.ID = time.Now().UTC().UnixNano()
	}

	// what does excluded do?
	upsertQuery := `INSERT INTO notes (id, title, body)
	VALUES (?, ?, ?)
	ON CONFLICT(id) DO UPDATE
	SET title=excluded.title, body=excluded.body, body=excluded.body;`

	if _, err := s.conn.Exec(upsertQuery, note.ID, note.Title, note.Body); err != nil{
		return err
	}

	return nil
}