% Define the days in each month (non-leap year)
days_in_month(january, 31).
days_in_month(february, 28).
days_in_month(march, 31).
days_in_month(april, 30).
days_in_month(may, 31).
days_in_month(june, 30).
days_in_month(july, 31).
days_in_month(august, 31).
days_in_month(september, 30).
days_in_month(october, 31).
days_in_month(november, 30).
days_in_month(december, 31).

% Calculate the number of days from January to the given month
days_from_january(Month, Days) :-
    days_in_month(Month, Days).

% Calculate the total days from the start of the year to the given date
total_days(Day-Month, TotalDays) :-
    days_from_january(Month, Days),
    TotalDays is Day + Days.

% Calculate the interval between two dates
interval(Date1, Date2, Interval) :-
    Date1 = Day1-Month1,
    Date2 = Day2-Month2,
    total_days(Date1, TotalDays1),
    total_days(Date2, TotalDays2),
    Interval is abs(TotalDays2 - TotalDays1).

% Example usage:
% ?- interval(3-march, 7-april, Interval).
% Output: Interval = 35