-- Creates the initial schema required by finito-postgres.

CREATE TABLE machines (
  id UUID PRIMARY KEY,
  created TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  fsm TEXT NOT NULL,
  state JSONB NOT NULL
);

CREATE TABLE events (
  id UUID PRIMARY KEY,
  created TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  fsm TEXT NOT NULL,
  fsm_id UUID NOT NULL REFERENCES machines(id),
  event JSONB NOT NULL
);
CREATE INDEX idx_events_machines ON events(fsm_id);

CREATE TYPE ActionStatus AS ENUM (
  'Pending',
  'Completed',
  'Failed'
);

CREATE TABLE actions (
  id UUID PRIMARY KEY,
  created TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  fsm TEXT NOT NULL,
  fsm_id UUID NOT NULL REFERENCES machines(id),
  event_id UUID NOT NULL REFERENCES events(id),
  content JSONB NOT NULL,
  status ActionStatus NOT NULL,
  error JSONB
);

CREATE INDEX idx_actions_machines ON actions(fsm_id);
CREATE INDEX idx_actions_events ON actions(event_id);
