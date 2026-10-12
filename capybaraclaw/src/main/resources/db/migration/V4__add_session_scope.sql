-- Who may find a session through session search, set by its first message:
-- 'direct' with the starter's port and user id, or 'group' with the port and
-- channel id. NULL for sessions recorded before scopes, which nobody finds.
ALTER TABLE sessions ADD COLUMN scope_kind  TEXT;
ALTER TABLE sessions ADD COLUMN scope_port  TEXT;
ALTER TABLE sessions ADD COLUMN scope_value TEXT;

CREATE INDEX sessions_scope_idx ON sessions(workdir, scope_kind, scope_port, scope_value);
