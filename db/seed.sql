SET search_path TO groupscholar_budget_variance_monitor, public;

INSERT INTO runs (input_file, variance_threshold, variance_amount_threshold, total_planned, total_actual)
VALUES ('data/sample_awards.csv', 0.10, 1000.00, 77000.00, 77300.00);

INSERT INTO variances (run_id, group_type, group_key, planned_amount, actual_amount, variance_amount, variance_pct, status)
VALUES
  (1, 'month', '2026-01', 25000.00, 25000.00, 0.00, 0.0000, 'OK'),
  (1, 'month', '2026-02', 25000.00, 25700.00, 700.00, 0.0280, 'OK'),
  (1, 'month', '2026-03', 27000.00, 26600.00, -400.00, -0.0148, 'OK'),
  (1, 'program', 'Arts', 17500.00, 18100.00, 600.00, 0.0343, 'OK'),
  (1, 'program', 'Health', 18500.00, 18900.00, 400.00, 0.0216, 'OK'),
  (1, 'program', 'STEM', 41000.00, 40300.00, -700.00, -0.0171, 'OK');
