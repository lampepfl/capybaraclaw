CREATE TABLE messages (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT    NOT NULL REFERENCES sessions(id),
  role       TEXT    NOT NULL,
  text       TEXT    NOT NULL,
  created_at INTEGER NOT NULL
);

CREATE INDEX messages_session_idx ON messages(session_id);
