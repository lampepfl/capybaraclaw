CREATE TABLE sessions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  port TEXT NOT NULL,
  thread TEXT NOT NULL
);

CREATE UNIQUE INDEX sessions_port_thread_idx ON sessions(port, thread);
