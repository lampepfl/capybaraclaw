CREATE TABLE sessions (
  id            TEXT    PRIMARY KEY,
  workdir       TEXT    NOT NULL,
  created_at    INTEGER NOT NULL,
  last_activity INTEGER NOT NULL
);

CREATE INDEX sessions_last_activity_idx ON sessions(last_activity);

CREATE TABLE session_handles (
  session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  workdir    TEXT NOT NULL,
  kind       TEXT NOT NULL,
  value      TEXT NOT NULL,
  PRIMARY KEY (session_id, kind, value),
  UNIQUE (workdir, kind, value)
);
