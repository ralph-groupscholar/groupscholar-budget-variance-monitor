program gs_budget_variance_monitor
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none

  integer, parameter :: max_items = 512
  type :: summary_item
    character(len=40) :: key = ""
    real(real64) :: planned = 0.0_real64
    real(real64) :: actual = 0.0_real64
  end type summary_item

  type(summary_item), dimension(max_items) :: month_summary
  type(summary_item), dimension(max_items) :: program_summary
  integer :: month_count, program_count
  integer :: ios
  character(len=256) :: csv_path
  character(len=256) :: cfg_path
  character(len=512) :: line
  character(len=64) :: date_str
  character(len=64) :: program
  character(len=7) :: month_key
  real(real64) :: planned_amount, actual_amount
  real(real64) :: variance_threshold
  real(real64) :: total_planned, total_actual

  call get_command_argument(1, csv_path)
  call get_command_argument(2, cfg_path)

  if (len_trim(csv_path) == 0 .or. len_trim(cfg_path) == 0) then
    call print_usage()
    stop 1
  end if

  variance_threshold = read_threshold(trim(cfg_path))
  month_count = 0
  program_count = 0
  total_planned = 0.0_real64
  total_actual = 0.0_real64

  open(unit=10, file=trim(csv_path), status='old', action='read', iostat=ios)
  if (ios /= 0) then
    write(*, '(a)') 'Error: unable to open CSV file.'
    stop 1
  end if

  read(10, '(A)', iostat=ios) line
  if (ios /= 0) then
    write(*, '(a)') 'Error: CSV file appears to be empty.'
    close(10)
    stop 1
  end if

  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    if (len_trim(line) == 0) cycle
    call parse_csv_line(trim(line), date_str, program, planned_amount, actual_amount)
    month_key = date_str(1:7)

    call add_summary_item(month_summary, month_count, trim(month_key), planned_amount, actual_amount)
    call add_summary_item(program_summary, program_count, trim(program), planned_amount, actual_amount)

    total_planned = total_planned + planned_amount
    total_actual = total_actual + actual_amount
  end do
  close(10)

  call sort_summary(month_summary, month_count)
  call sort_summary(program_summary, program_count)

  call print_report(trim(csv_path), variance_threshold, month_summary, month_count, &
                    program_summary, program_count, total_planned, total_actual)
  call maybe_log_to_db(trim(csv_path), variance_threshold, month_summary, month_count, &
                       program_summary, program_count, total_planned, total_actual)

contains

  subroutine print_usage()
    write(*, '(a)') 'Usage: gs-budget-variance-monitor <awards.csv> <config.cfg>'
  end subroutine print_usage

  real(real64) function read_threshold(cfg_file)
    character(len=*), intent(in) :: cfg_file
    character(len=256) :: cfg_line
    integer :: unit, ios, eq_pos
    read_threshold = 0.0_real64

    open(newunit=unit, file=cfg_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*, '(a)') 'Error: unable to open config file.'
      stop 1
    end if

    read(unit, '(A)', iostat=ios) cfg_line
    if (ios == 0) then
      eq_pos = index(cfg_line, '=')
      if (eq_pos > 0) then
        read(cfg_line(eq_pos+1:), *, iostat=ios) read_threshold
        if (ios /= 0) read_threshold = 0.0_real64
      end if
    end if
    close(unit)
  end function read_threshold

  subroutine parse_csv_line(csv_line, date_out, program_out, planned_out, actual_out)
    character(len=*), intent(in) :: csv_line
    character(len=64), intent(out) :: date_out
    character(len=64), intent(out) :: program_out
    real(real64), intent(out) :: planned_out
    real(real64), intent(out) :: actual_out
    character(len=512) :: tmp_line
    integer :: i

    tmp_line = csv_line
    do i = 1, len_trim(tmp_line)
      if (tmp_line(i:i) == ',') tmp_line(i:i) = ' '
    end do

    read(tmp_line, *) date_out, program_out, planned_out, actual_out
  end subroutine parse_csv_line

  subroutine add_summary_item(list, count, key, planned, actual)
    type(summary_item), dimension(:), intent(inout) :: list
    integer, intent(inout) :: count
    character(len=*), intent(in) :: key
    real(real64), intent(in) :: planned
    real(real64), intent(in) :: actual
    integer :: i

    do i = 1, count
      if (trim(list(i)%key) == trim(key)) then
        list(i)%planned = list(i)%planned + planned
        list(i)%actual = list(i)%actual + actual
        return
      end if
    end do

    count = count + 1
    list(count)%key = trim(key)
    list(count)%planned = planned
    list(count)%actual = actual
  end subroutine add_summary_item

  subroutine sort_summary(list, count)
    type(summary_item), dimension(:), intent(inout) :: list
    integer, intent(in) :: count
    integer :: i, j
    type(summary_item) :: temp

    do i = 1, count - 1
      do j = i + 1, count
        if (trim(list(j)%key) < trim(list(i)%key)) then
          temp = list(i)
          list(i) = list(j)
          list(j) = temp
        end if
      end do
    end do
  end subroutine sort_summary

  subroutine print_report(csv_file, threshold, month_list, month_count, program_list, &
                          program_count, total_plan, total_act)
    character(len=*), intent(in) :: csv_file
    real(real64), intent(in) :: threshold
    type(summary_item), dimension(:), intent(in) :: month_list
    type(summary_item), dimension(:), intent(in) :: program_list
    integer, intent(in) :: month_count
    integer, intent(in) :: program_count
    real(real64), intent(in) :: total_plan
    real(real64), intent(in) :: total_act

    write(*, '(a)') 'Budget Variance Monitor'
    write(*, '(a)') 'Input file: ' // trim(csv_file)
    write(*, '(a, f6.2)') 'Variance threshold: ', threshold
    write(*, '(a)') ''

    write(*, '(a)') 'Monthly Summary'
    write(*, '(a)') 'Month | Planned | Actual | Variance | Variance % | Status'
    call print_summary_rows(month_list, month_count, threshold)
    write(*, '(a)') ''

    write(*, '(a)') 'Program Summary'
    write(*, '(a)') 'Program | Planned | Actual | Variance | Variance % | Status'
    call print_summary_rows(program_list, program_count, threshold)
    write(*, '(a)') ''

    write(*, '(a)') 'Overall Totals'
    call print_total_row(total_plan, total_act, threshold)
  end subroutine print_report

  subroutine print_summary_rows(list, count, threshold)
    type(summary_item), dimension(:), intent(in) :: list
    integer, intent(in) :: count
    real(real64), intent(in) :: threshold
    integer :: i

    do i = 1, count
      call print_row(list(i)%key, list(i)%planned, list(i)%actual, threshold)
    end do
  end subroutine print_summary_rows

  subroutine print_row(key, planned, actual, threshold)
    character(len=*), intent(in) :: key
    real(real64), intent(in) :: planned
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: threshold
    real(real64) :: variance
    real(real64) :: variance_pct
    character(len=6) :: status

    variance = actual - planned
    if (planned /= 0.0_real64) then
      variance_pct = variance / planned
    else
      variance_pct = 0.0_real64
    end if

    if (abs(variance_pct) > threshold) then
      status = 'ALERT'
    else
      status = 'OK'
    end if

    write(*, '(a, " | ", f10.2, " | ", f10.2, " | ", f10.2, " | ", f10.3, " | ", a)') &
      trim(key), planned, actual, variance, variance_pct, trim(status)
  end subroutine print_row

  subroutine print_total_row(planned, actual, threshold)
    real(real64), intent(in) :: planned
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: threshold
    real(real64) :: variance
    real(real64) :: variance_pct
    character(len=6) :: status

    variance = actual - planned
    if (planned /= 0.0_real64) then
      variance_pct = variance / planned
    else
      variance_pct = 0.0_real64
    end if

    if (abs(variance_pct) > threshold) then
      status = 'ALERT'
    else
      status = 'OK'
    end if

    write(*, '(a, f10.2, a, f10.2, a, f10.2, a, f10.3, a, a)') &
      'Total | ', planned, ' | ', actual, ' | ', variance, ' | ', variance_pct, ' | ', trim(status)
  end subroutine print_total_row

  subroutine maybe_log_to_db(csv_file, threshold, month_list, month_count, program_list, &
                             program_count, total_plan, total_act)
    character(len=*), intent(in) :: csv_file
    real(real64), intent(in) :: threshold
    type(summary_item), dimension(:), intent(in) :: month_list
    type(summary_item), dimension(:), intent(in) :: program_list
    integer, intent(in) :: month_count
    integer, intent(in) :: program_count
    real(real64), intent(in) :: total_plan
    real(real64), intent(in) :: total_act

    character(len=256) :: db_url
    character(len=128) :: db_schema
    integer :: env_len
    integer :: unit
    integer :: i
    character(len=512) :: sql_path

    call get_environment_variable('GS_DB_URL', db_url, length=env_len)
    if (env_len == 0) return

    call get_environment_variable('GS_DB_SCHEMA', db_schema, length=env_len)
    if (env_len == 0) then
      db_schema = 'groupscholar_budget_variance_monitor'
    end if

    sql_path = 'out/last_run.sql'
    open(newunit=unit, file=trim(sql_path), status='replace', action='write')

    write(unit, '(a)') 'BEGIN;'
    write(unit, '(a)') 'SET search_path TO ' // trim(db_schema) // ', public;'
    write(unit, '(a)') 'INSERT INTO runs (input_file, variance_threshold, total_planned, total_actual)'
    write(unit, '(a)') 'VALUES (''' // trim(csv_file) // ''', ' // trim(real_to_str(threshold)) // &
      ', ' // trim(real_to_str(total_plan)) // ', ' // trim(real_to_str(total_act)) // ') RETURNING id \\\\gset'

    do i = 1, month_count
      call write_variance_insert(unit, 'month', month_list(i), threshold)
    end do

    do i = 1, program_count
      call write_variance_insert(unit, 'program', program_list(i), threshold)
    end do

    write(unit, '(a)') 'COMMIT;'
    close(unit)

    call execute_psql(trim(db_url), trim(sql_path))
  end subroutine maybe_log_to_db

  subroutine write_variance_insert(unit, group_type, item, threshold)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: group_type
    type(summary_item), intent(in) :: item
    real(real64), intent(in) :: threshold
    real(real64) :: variance
    real(real64) :: variance_pct
    character(len=6) :: status

    variance = item%actual - item%planned
    if (item%planned /= 0.0_real64) then
      variance_pct = variance / item%planned
    else
      variance_pct = 0.0_real64
    end if

    if (abs(variance_pct) > threshold) then
      status = 'ALERT'
    else
      status = 'OK'
    end if

    write(unit, '(a)') 'INSERT INTO variances (run_id, group_type, group_key, planned_amount, actual_amount, variance_amount, variance_pct, status)'
    write(unit, '(a)') 'VALUES (:id, ''' // trim(group_type) // ''', ''' // &
      trim(item%key) // ''', ' // trim(real_to_str(item%planned)) // ', ' // &
      trim(real_to_str(item%actual)) // ', ' // trim(real_to_str(variance)) // ', ' // &
      trim(real_to_str(variance_pct)) // ', ''' // trim(status) // ''');'
  end subroutine write_variance_insert

  function real_to_str(value) result(out)
    real(real64), intent(in) :: value
    character(len=32) :: out
    write(out, '(f12.6)') value
    out = adjustl(out)
  end function real_to_str

  subroutine execute_psql(db_url, sql_path)
    character(len=*), intent(in) :: db_url
    character(len=*), intent(in) :: sql_path
    character(len=512) :: command

    command = 'psql "' // trim(db_url) // '" -v ON_ERROR_STOP=1 -f ' // trim(sql_path)
    call execute_command_line(trim(command))
  end subroutine execute_psql

end program gs_budget_variance_monitor
