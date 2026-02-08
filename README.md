# GroupScholar Budget Variance Monitor

Budget variance monitor is a Fortran CLI that compares planned vs. actual award disbursements, summarizes variance by month and program, and flags risk when variance exceeds a configurable threshold. It also supports optional run logging to the Group Scholar PostgreSQL database via `psql`.

## Features
- CSV ingestion for planned vs. actual awards
- Monthly and program-level aggregation
- Variance percentage alerting with configurable threshold
- Alert summary counts, directional overspend/underspend summary, and top variance ranking by month and program
- Optional production DB logging for run history and variance rows
- Deterministic, testable output

## Usage

Build:
```bash
make build
```

Run:
```bash
bin/gs-budget-variance-monitor data/sample_awards.csv config/default.cfg
```

Optional DB logging (production only):
```bash
export GS_DB_URL="postgresql://USER:PASSWORD@HOST:PORT/postgres"
export GS_DB_SCHEMA="groupscholar_budget_variance_monitor"
make build
bin/gs-budget-variance-monitor data/sample_awards.csv config/default.cfg
```

## Configuration
`config/default.cfg` contains:
- `variance_threshold`: decimal percentage (0.10 = 10%)
- `variance_amount_threshold`: absolute variance amount that triggers alerts (0 disables)
- `top_variance_limit`: number of rows to show in each top-variance section

## Output
The CLI prints monthly and program summaries, alert rollups, direction summaries, top-variance sections (overall, month, program),
top overspends/underspends, and overall totals.

## Database
Schema and seed data are in `db/schema.sql` and `db/seed.sql`. Apply with:
```bash
psql "$GS_DB_URL" -v ON_ERROR_STOP=1 -f db/schema.sql
psql "$GS_DB_URL" -v ON_ERROR_STOP=1 -f db/seed.sql
```

## Tests
```bash
make test
```

## Technologies
- Fortran (gfortran)
- PostgreSQL (optional logging via `psql`)
- Make

## Additional Scenarios
Run the alert-focused fixture:
```bash
bin/gs-budget-variance-monitor data/sample_awards_alerts.csv config/alerts.cfg
```
