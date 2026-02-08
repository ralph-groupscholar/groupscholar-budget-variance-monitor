CREATE SCHEMA IF NOT EXISTS groupscholar_budget_variance_monitor;

CREATE TABLE IF NOT EXISTS groupscholar_budget_variance_monitor.runs (
  id BIGSERIAL PRIMARY KEY,
  run_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  input_file TEXT NOT NULL,
  variance_threshold NUMERIC(10,4) NOT NULL,
  variance_amount_threshold NUMERIC(14,2) NOT NULL DEFAULT 0.00,
  total_planned NUMERIC(14,2) NOT NULL,
  total_actual NUMERIC(14,2) NOT NULL
);

CREATE TABLE IF NOT EXISTS groupscholar_budget_variance_monitor.variances (
  id BIGSERIAL PRIMARY KEY,
  run_id BIGINT NOT NULL REFERENCES groupscholar_budget_variance_monitor.runs(id) ON DELETE CASCADE,
  group_type TEXT NOT NULL CHECK (group_type IN ('month', 'program')),
  group_key TEXT NOT NULL,
  planned_amount NUMERIC(14,2) NOT NULL,
  actual_amount NUMERIC(14,2) NOT NULL,
  variance_amount NUMERIC(14,2) NOT NULL,
  variance_pct NUMERIC(10,4) NOT NULL,
  status TEXT NOT NULL,
  variance_direction TEXT NOT NULL CHECK (variance_direction IN ('over', 'under', 'even')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS variances_run_id_idx
  ON groupscholar_budget_variance_monitor.variances(run_id);

CREATE INDEX IF NOT EXISTS variances_group_idx
  ON groupscholar_budget_variance_monitor.variances(group_type, group_key);
